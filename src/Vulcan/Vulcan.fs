module Vulcan

open System
open Aether
open Aether.Operators
open Hekate

// TODO: Pre/post-condition analysis
// TODO: Logging/Introspection mechanisms

(* Notes

   Type parameter names throughout the Vulcan implementation are used
   consistently, single character parameter names representing two specific
   concepts:
   - 'c for the type of Configuration
   - 'r for the type of result
   - 's for the type of State *)

(* Prelude *)

[<AutoOpen>]
module internal Prelude =

    type Identity<'a> =
        'a -> 'a

    type Pair<'a> =
        'a * 'a

    (* Equality/Comparison

       Functions for simplifying the customization of equality
       and comparison on types where this is required. *)

    let equalsOn f x (y: obj) =
        match y with
        | :? 'T as y -> (f x) = (f y)
        | _ -> false
 
    let hashOn f x =
        hash (f x)
 
    let compareOn f x (y: obj) =
        match y with
        | :? 'T as y -> compare (f x) (f y)
        | _ -> invalidArg "y" "cannot compare values of different types"

(* Types

   Core Vulcan types of various classifications, representing the common
   building blocks of Vulcan machinery which will regularly be exposed to users
   of the Vulcan system (i.e. those designing and implementing Vulcan
   components. *)

(* Decisions *)

type Decision<'s> =
    | Function of ('s -> Async<DecisionValue * 's>)
    | Literal of DecisionValue

    static member function_ : Epimorphism<Decision<'s>,('s -> Async<DecisionValue * 's>)> =
        (function | Function f -> Some f
                  | _ -> None), (Function)

    static member literal_ : Epimorphism<Decision<'s>,DecisionValue> =
        (function | Literal l -> Some l
                  | _ -> None), (Literal)

 and DecisionValue =
    | Right
    | Left

type DecisionConfigurator<'c,'s> =
    'c -> Decision<'s>

(* Specifications

   The Vulcan Specification model, a highly restricted abstraction of the
   conceptual decision graph, allowing only binary (Left|Right) Decisions or
   Terminals to exist as a Specification of a graph (where the left and right
   case of a Decision is - recursively - a Specification).

   This allows for graphs to be defined (including graphs with cycles if
   required through the use of rec bindings) and defined as reusable elements
   where convenient. *)

[<AutoOpen>]
module Specifications =

    (* Types

       Types for a specification model of a decision graph, using two simple
       primitive types from which any decision graph can be constructed and
       optimized, etc. *)

    type Specification<'c,'r,'s> =
        | Decision of SpecificationDecision<'c,'r,'s>
        | Terminal of SpecificationTerminal<'r,'s>

        static member decision_ : Epimorphism<Specification<'c,'r,'s>,SpecificationDecision<'c,'r,'s>> =
            (function | Decision d -> Some d
                      | _ -> None), (Decision)

        static member terminal_ : Epimorphism<Specification<'c,'r,'s>,SpecificationTerminal<'r,'s>> =
            (function | Terminal t -> Some t
                      | _ -> None), (Terminal)

     and SpecificationDecision<'c,'r,'s> =
        string list * DecisionConfigurator<'c,'s> * Pair<Specification<'c,'r,'s>>

     and SpecificationTerminal<'r,'s> =
        string list * ('s -> Async<'r * 's>)

    type SpecificationOperation<'c,'r,'s> =
        | Prepend of Identity<Specification<'c,'r,'s>>
        | Splice of string list * DecisionValue * Identity<Specification<'c,'r,'s>>

    (* Helpers *)

    let internal (|SpecificationName|) =
        function | Decision (n, _, _) -> n
                 | Terminal (n, _) -> n

    (* Specification

       The Vulcan user API for working with specifications, including specific
       functionality for working at the Decision and Terminal level, and for
       working with modifications and compositions at the specification level,
       enabling the combination of multiple specifications, given dependencies,
       precedence, pre/post-conditions, etc. *)

    [<RequireQualifiedAccess>]
    module Specification =

        (* Terminals

           Helpful shorthand functions for working with terminals, namely a
           function for creating a new named terminal given a Vulcan function
           of unit. *)

        [<RequireQualifiedAccess>]
        module Terminal =

            /// Create a new named terminal, given a Vulcan function returning
            /// unit, and with the appropriate state type.
            let create<'c,'r,'s> name f =
                Specification<'c,'r,'s>.Terminal (name, f)

            /// Create a new unnamed terminal with a no-op Vulcan function.
            let empty<'c,'r,'s> =
                Specification<'c,'r,'s>.Terminal ([], fun s -> async.Return (Unchecked.defaultof<'r>, s))

        (* Decisions

           Helpful shorthand functions for creating decisions, including literal
           decisions where the decision of Left or Right is known at compile
           time, and the unselected decision is assigned a simple null terminal.

           This can be useful for defining components which effectively act as a
           pipeline and only one logical connection point is coherent for the
           end user of the component. It is assumed that dead (unreachable) paths
           will be optimized out of the final executable form of the resultant
           decision graph in general use. *)

        [<RequireQualifiedAccess>]
        module Decision =

            /// Create a new named decision, given a suitable configuration
            /// function and specifications for the subsequent left and right
            /// trees.
            let create<'c,'r,'s> name configure (p: Pair<Specification<'c,'r,'s>>) =
                Decision (name, configure, p)

            /// Create a new named left decision, which will always evaluate to
            /// Left, and where the specification of the right tree will be an
            /// effective no-operation terminal.
            let left<'c,'r,'s> name (l: Specification<'c,'r,'s>) =
                create name (fun _ -> Literal Left) (l, Terminal.empty)

            /// Create a new named right decision, which will always evaluate to
            /// Right, and where the specification of the left tree will be an
            /// effective no-operation terminal.
            let right<'c,'r,'s> name (r: Specification<'c,'r,'s>) =
                create name (fun _ -> Literal Right) (Terminal.empty, r)

(* Models

   The Vulcan Model implementation, providing a way to package and share a
   Specification via operations on the existing state of a notional
   specification, along with metadata relating to that specification, such
   as name, description, etc. plus technical metadata such as dependencies
   and precedence according to the composition model. *)

[<AutoOpen>]
module Models =

    module S = Specifications

    (* Types

       Types defining an individual Vulcan Component (pre-composition) and a
       Vulcan Model, the combined result of a set of orderable Vulcan
       Components, and including metadata from each in a meaningful way.

       The list of constituent Vulcan Components represents the applied
       ordering. *)

    type Model<'c,'r,'s> =
        { Components: ComponentMetadata list
          Specification: Specification<'c,'r,'s> }

     and Component<'c,'r,'s> =
        { Metadata: ComponentMetadata
          Requirements: ComponentRequirements
          Operations: SpecificationOperation<'c,'r,'s> list }

        static member metadata_ : Lens<Component<'c,'r,'s>,ComponentMetadata> =
            (fun x -> x.Metadata), (fun m x -> { x with Metadata = m })

        static member requirements_ : Lens<Component<'c,'r,'s>,ComponentRequirements> =
            (fun x -> x.Requirements), (fun r x -> { x with Requirements = r })

        static member operations_ : Lens<Component<'c,'r,'s>,SpecificationOperation<'c,'r,'s> list> =
            (fun x -> x.Operations), (fun o x -> { x with Operations = o })

        static member private Comparable (x: Component<'c,'r,'s>) =
            x.Metadata.Name.ToLowerInvariant ()

        override x.Equals y =
            equalsOn Component<_,_,_>.Comparable x y

        override x.GetHashCode () =
            hashOn Component<_,_,_>.Comparable x

        interface IComparable with

            member x.CompareTo y =
                compareOn Component<_,_,_>.Comparable x y

     and ComponentMetadata =
        { Name: string
          Description: string option }

        static member name_ =
            (fun x -> x.Name), (fun n x -> { x with Name = n })

     and ComponentRequirements =
        { Required: Set<string>
          Preconditions: string list }

        static member required_ =
            (fun x -> x.Required), (fun r x -> { x with Required = r })

    (* Order

       Functions to order a set of Vulcan Components given the defined
       dependencies as supplied in the Vulcan Component Requirements. An
       ordering may or may not be possible, and the function will throw on
       failure.

       The implementation is a simple topological sort using Kahn's algorithm,
       made simpler by the fact that Hekate will automatically handle the
       removal of appropriate edges on node removal. *)

    [<RequireQualifiedAccess>]
    module internal Order =

        let private nodes<'c,'r,'s> =
                List.map (fun m ->
                    Optic.get (
                            Component<'c,'r,'s>.metadata_ 
                        >-> ComponentMetadata.name_) m, m)

        let private edges<'c,'r,'s> =
                List.map (fun m ->
                    Optic.get (Component<'c,'r,'s>.metadata_ >-> ComponentMetadata.name_) m,
                    Optic.get (Component<'c,'r,'s>.requirements_ >-> ComponentRequirements.required_) m)
             >> List.map (fun (n, rs) ->
                    List.map (fun n' -> n', n, ()) (Set.toList rs))
             >> List.concat

        let private graph<'c,'r,'s> modules =
            Graph.create (nodes<'c,'r,'s> modules) (edges<'c,'r,'s> modules)

        let private independent graph =
            Graph.Nodes.toList graph
            |> List.tryFind (fun (v, _) -> Graph.Nodes.inwardDegree v graph = Some 0)
            |> Option.map (fun (v, l) -> l, Graph.Nodes.remove v graph)

        let rec private order modules graph =
            match independent graph with
            | Some (m, graph) -> order (modules @ [ m ]) graph
            | _ when Graph.isEmpty graph -> modules
            | _ -> failwith "A valid Vulcan Model order cannot be determined."

        let apply<'c,'r,'s> =
                Set.toList
             >> graph<'c,'r,'s>
             >> order []

    (* Operation

       Functions to process the logical operations defined on Vulcan
       Specifications, combining two specifications in specific and controlled
       ways. *)

    [<RequireQualifiedAccess>]
    module internal Operation =

        let private prepend<'c,'r,'s> (f: Identity<Specification<'c,'r,'s>>) specification =
            f specification

        // TODO: Test and fix this in the case of cyclic specifications
        // TODO: Test and fix this in the case of duplicate specification names

        let rec private splice<'c,'r,'s> name value (f: Identity<Specification<'c,'r,'s>>) specification =
            match specification with
            | Decision (n, c, (l, r)) when n = name && value = Left -> Decision (n, c, (f l, r))
            | Decision (n, c, (l, r)) when n = name && value = Right -> Decision (n, c, (l, f r))
            | Decision (n, c, (l, r)) -> Decision (n, c, (splice name value f l, splice name value f r))
            | Terminal (n, f) -> Terminal (n, f)

        let apply<'c,'r,'s> (s: Specification<'c,'r,'s>) =
            function | Prepend f -> prepend f s
                     | Splice (n, v, f) -> splice n v f s

    (* Model

       The Vulcan user API for working with Models, specifically the
       composition of Vulcan Components to Vulcan Models. This function may
       fail. *)

    [<RequireQualifiedAccess>]
    module Model =

        let private components<'c,'r,'s> =
                List.map (Optic.get Component<'c,'r,'s>.metadata_)

        let private specification<'c,'r,'s> : (Component<'c,'r,'s> list -> Specification<'c,'r,'s>) =
                List.map (fun c -> c.Operations)
             >> List.concat
             >> List.fold Operation.apply Specification.Terminal.empty

        let private model<'c,'r,'s> (m: Component<'c,'r,'s> list) =
            { Components = components m
              Specification = specification m }

        let create<'c,'r,'s> =
                Order.apply<'c,'r,'s>
             >> model

(* Machines *)

[<AutoOpen>]
module Machines =

    (* Graphs

       The Vulcan Graph implementation, turning the logical Vulcan
       Specification representation of a decision graph in to an optimized
       executable form (given appropriate configuration data to configure a
       graph before optimization).

       The resultant graph effectively behaves as an Async State monad while
       being transparent and modifiable - potentially supporting differing
       forms of optimization, error handling etc. via mapping of the graph
       (including a possible plugin model which would act as a "plugin-based
       graph compiler").

       The graph implementation is not a Vulcan user-facing API, and is wrapped
       within the Vulcan Machine API. *)

    [<AutoOpen>]
    module internal Graphs =

        (* Common *)

        let RootName =
            [ "root" ]

        type Node<'r,'s> =
            | Root
            | Terminal of ('s -> Async<'r * 's>)

         and Edge =
            | Undefined
            | Value of DecisionValue

        (* Translation *)

        [<RequireQualifiedAccess>]
        module Translation =

            module S = Specifications

            (* Types

               A translated graph, parameterized by state type, and by the type
               of the configuration data which will be passed to translation
               decisions to effectively reify to a Vulcan Decision.

               Translated graphs are the result of the translation from the
               Vulcan Specification model to a phase of the pipeline to produce
               an executable graph model. *)


            type TranslatedGraph<'c,'r,'s> =
                | Graph of TranslatedGraphType<'c,'r,'s>

                static member graph_ : Isomorphism<TranslatedGraph<'c,'r,'s>,TranslatedGraphType<'c,'r,'s>> =
                    (fun (Graph x) -> x), (Graph)

             and TranslatedGraphType<'c,'r,'s> =
                Graph<string list,TranslatedNode<'c,'r,'s>,Edge>

             and TranslatedNode<'c,'r,'s> =
                | Node of Node<'r,'s>
                | TranslatedDecision of DecisionConfigurator<'c,'s>

            (* Translation

               Functions for translating a Vulcan Specification to a graph based
               representation of the equivalent (unmodified) decision graph, prior
               to applying instance specific configuration to the graph. *)

            let private left n s =
                n, (|SpecificationName|) s, Value Left

            let private right n s =
                n, (|SpecificationName|) s, Value Right

            let rec private nodes ns =
                function | S.Decision (n, c, (l, r)) -> (n, TranslatedDecision c) :: nodes [] l @ nodes [] r @ ns
                         | S.Terminal (n, f) -> (n, Node (Terminal f)) :: ns

            let rec private edges es =
                function | S.Decision (n, _, (l, r)) -> (left n l) :: (right n r) :: edges [] l @ edges [] r @ es
                         | S.Terminal _ -> es

            let translate s =
                Graph (
                    Graph.create
                        ((RootName, Node Root) :: nodes [] s)
                        ((RootName, (|SpecificationName|) s, Undefined) :: edges [] s))

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module Configuration =

            module T = Translation

            (* Types

               A configured graph, parameterized solely by the state type which
               will be threaded through the computation. *)

            type ConfiguredGraph<'r,'s> =
                | Graph of ConfiguredGraphType<'r,'s>

                static member graph_ : Isomorphism<ConfiguredGraph<'r,'s>,ConfiguredGraphType<'r,'s>> =
                    (fun (Graph x) -> x), (Graph)

             and ConfiguredGraphType<'r,'s> =
                Graph<string list,ConfiguredNode<'r,'s>,Edge>

             and ConfiguredNode<'r,'s> =
                | Node of Node<'r,'s>
                | ConfiguredDecision of Decision<'s>

                static member node_ : Epimorphism<ConfiguredNode<'r,'s>,Node<'r,'s>> =
                    (function | Node n -> Some n
                              | _ -> None), (Node)

                static member decision_ : Epimorphism<ConfiguredNode<'r,'s>,Decision<'s>> =
                    (function | ConfiguredDecision d -> Some d
                              | _ -> None), (ConfiguredDecision)

            (* Node Mapping *)

            let private map<'c,'r,'s> configuration : T.TranslatedGraphType<'c,'r,'s> -> ConfiguredGraphType<'r,'s> =
                Graph.Nodes.map (fun _ ->
                    function | T.Node n -> Node n
                             | T.TranslatedDecision f -> ConfiguredDecision (f configuration))

            (* Configure *)

            let configure<'c,'r,'s> configuration =
                    Optic.get (Lens.ofIsomorphism T.TranslatedGraph<'c,'r,'s>.graph_)
                 >> map<'c,'r,'s> configuration
                 >> Graph

        (* Optimization *)

        [<RequireQualifiedAccess>]
        module Optimization =

            module C = Configuration

            (* Types

               An optimized graph, having optimized away all Vulcan Decisions
               which result in literals, removing the choice point and directly
               connecting the input edges to the appropriate next node given
               the literal value. *)

            type OptimizedGraph<'r,'s> =
                | Graph of OptimizedGraphType<'r,'s>

                static member graph_ : Isomorphism<OptimizedGraph<'r,'s>,OptimizedGraphType<'r,'s>> =
                    (fun (Graph x) -> x), (Graph)

             and OptimizedGraphType<'r,'s> =
                Graph<string list, OptimizedNode<'r,'s>,Edge>

             and OptimizedNode<'r,'s> =
                | Node of Node<'r,'s>
                | OptimizedDecision of ('s -> Async<DecisionValue * 's>)

            (* Literal Node Elimination *)

            let private node<'r,'s> : C.ConfiguredGraphType<'r,'s> -> (string list * DecisionValue) option =
                    Graph.Nodes.toList
                 >> List.tryPick (function | n, C.ConfiguredDecision (Literal v) -> Some (n, v)
                                           | _ -> None)

            let private target<'r,'s> n v : C.ConfiguredGraphType<'r,'s> -> string list =
                    Graph.Nodes.outward n
                 >> Option.get
                 >> List.pick (function | _, t, Value v' when v = v' -> Some t
                                        | _ -> None)

            let private edges<'r,'s> n t : C.ConfiguredGraphType<'r,'s> -> LEdge<string list,Edge> list =
                    Graph.Nodes.inward n
                 >> Option.get
                 >> List.map (fun (f, _, v) -> (f, t, v))

            let rec private eliminateLiterals<'r,'s> (graph: C.ConfiguredGraphType<'r,'s>) =
                match node graph with
                | Some (n, v) ->
                    eliminateLiterals (
                        graph
                        |> Graph.Nodes.remove n
                        |> Graph.Edges.addMany (edges n (target n v graph) graph))
                | _ ->
                    graph

            (* Subgraph Elimination *)

            let private subgraph<'r,'s> (graph: C.ConfiguredGraphType<'r,'s>) =
                 Graph.Nodes.toList graph
                 |> List.tryPick (function | _, C.Node Root -> None
                                           | n, _ when Graph.Nodes.inwardDegree n graph = Some 0 -> Some n
                                           | _ -> None)

            let rec private eliminateSubgraphs<'r,'s> (graph: C.ConfiguredGraphType<'r,'s>) =
                match subgraph graph with
                | Some n -> eliminateSubgraphs (Graph.Nodes.remove n graph)
                | _ -> graph

            (* Node Mapping *)

            let private map<'r,'s> : C.ConfiguredGraphType<'r,'s> -> OptimizedGraphType<'r,'s> =
                Graph.Nodes.map (fun _ ->
                    function | C.Node n -> Node n
                             | C.ConfiguredDecision (Function f) -> OptimizedDecision f
                             | _ -> failwith "Unexpected Node during Optimization.")

            (* Optimization *)

            let optimize<'r,'s> =
                   Optic.get (Lens.ofIsomorphism C.ConfiguredGraph<'r,'s>.graph_)
                >> eliminateLiterals
                >> eliminateSubgraphs
                >> map
                >> Graph

        (* Evaluation *)

        [<RequireQualifiedAccess>]
        module Evaluation =

            module O = Optimization

            let private current<'r,'s> n : O.OptimizedGraphType<'r,'s> -> (string list * O.OptimizedNode<'r,'s>) =
                    Graph.Nodes.find n

            let private next<'r,'s> n e : O.OptimizedGraphType<'r,'s> -> string list =
                    Graph.Nodes.successors n
                 >> Option.get
                 >> List.pick (function | n, e' when e = e' -> Some n
                                        | _ -> None)

            let rec private eval<'r,'s> n s (g: O.OptimizedGraphType<'r,'s>) =
                match current n g with
                | n, O.OptimizedDecision f -> async.Bind (f s, fun (v, s) -> eval (next n (Value v) g) s g)
                | n, O.Node Root -> eval (next n Undefined g) s g
                | _, O.Node (Terminal f) -> f s

            let evaluate<'r,'s> state (graph: Optimization.OptimizedGraph<'r,'s>) =
                match graph with
                | Optimization.Graph graph -> eval RootName state graph

    (* Types *)

    type Machine<'r,'s> =
        | Machine of ('s -> Async<'r * 's>)

     and MachinePrototype<'c,'r,'s> =
        | MachinePrototype of ('c -> Machine<'r,'s>)

    (* Machine *)

    [<RequireQualifiedAccess>]
    module Machine =

        module C = Configuration
        module E = Evaluation
        module O = Optimization
        module T = Translation

        (* Initialization *)

        let private machine<'r,'s> (graph: O.OptimizedGraph<'r,'s>) =
            Machine (fun (state: 's) ->
                E.evaluate state graph)

        let private machinePrototype<'c,'r,'s> (graph: T.TranslatedGraph<'c,'r,'s>) =
            MachinePrototype (fun (configuration: 'c) ->
                machine ((C.configure configuration >> O.optimize) graph))

        (* Operations *)

        let prototype<'c,'r,'s> (m: Model<'c,'r,'s>) =
            machinePrototype (T.translate m.Specification)

        let configure<'c,'r,'s> (MachinePrototype (f : ('c -> Machine<'r,'s>))) configuration =
            f configuration

        let run<'r,'s> (Machine (f: ('s -> Async<'r * 's>))) state =
            f state