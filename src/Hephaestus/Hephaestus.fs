module Hephaestus

open System
open Aether
open Aether.Operators
open Anat
open Anat.Operators
open Hekate

// TODO: Pre/post-condition analysis
// TODO: Logging/Introspection mechanisms (to be completed for execution - async)

(* Notes

   Type parameter names throughout the Hephaestus implementation are used
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

    let flip f =
        fun a b -> f b a

    let swap (a, b) =
        (b, a)

    let tuple a b =
        (a, b)

    let uncurry f =
        fun (a, b) -> f a b

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

   Core Hephaestus types of various classifications, representing the common
   building blocks of Hephaestus machinery which will regularly be exposed to users
   of the Hephaestus system (i.e. those designing and implementing Hephaestus
   components. *)

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

type TerminalConfigurator<'c,'r,'s> =
    'c -> 's -> Async<'r * 's>

(* Specifications

   The Hephaestus Specification model, a highly restricted abstraction of the
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
        | Terminal of SpecificationTerminal<'c,'r,'s>

        static member decision_ : Epimorphism<Specification<'c,'r,'s>,SpecificationDecision<'c,'r,'s>> =
            (function | Decision d -> Some d
                      | _ -> None), (Decision)

        static member terminal_ : Epimorphism<Specification<'c,'r,'s>,SpecificationTerminal<'c,'r,'s>> =
            (function | Terminal t -> Some t
                      | _ -> None), (Terminal)

     and SpecificationDecision<'c,'r,'s> =
        string list * DecisionConfigurator<'c,'s> * Pair<Specification<'c,'r,'s>>

     and SpecificationTerminal<'c,'r,'s> =
        string list * TerminalConfigurator<'c,'r,'s>

    type SpecificationOperation<'c,'r,'s> =
        | Prepend of Identity<Specification<'c,'r,'s>>
        | Splice of string list * DecisionValue * Identity<Specification<'c,'r,'s>>

    (* Helpers *)

    let internal (|SpecificationName|) =
        function | Decision (n, _, _) -> n
                 | Terminal (n, _) -> n

    (* Specification

       The Hephaestus user API for working with specifications, including specific
       functionality for working at the Decision and Terminal level, and for
       working with modifications and compositions at the specification level,
       enabling the combination of multiple specifications, given dependencies,
       precedence, pre/post-conditions, etc. *)

    [<RequireQualifiedAccess>]
    module Specification =

        (* Terminals

           Helpful shorthand functions for working with terminals, namely a
           function for creating a new named terminal given a Hephaestus function
           of unit. *)

        [<RequireQualifiedAccess>]
        module Terminal =

            /// Create a new named terminal, given a Hephaestus function returning
            /// unit, and with the appropriate state type.
            let create<'c,'r,'s> name configure =
                Specification<'c,'r,'s>.Terminal (name, configure)

            /// Create a new unnamed terminal with a no-op Hephaestus function.
            let empty<'c,'r,'s> =
                Specification<'c,'r,'s>.Terminal ([], fun _ s -> async.Return (Unchecked.defaultof<'r>, s))

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

   The Hephaestus Model implementation, providing a way to package and share a
   Specification via operations on the existing state of a notional
   specification, along with metadata relating to that specification, such
   as name, description, etc. plus technical metadata such as dependencies
   and precedence according to the composition model. *)

[<AutoOpen>]
module Models =

    module S = Specifications

    (* Types

       Types defining an individual Hephaestus Component (pre-composition) and a
       Hephaestus Model, the combined result of a set of orderable Hephaestus
       Components, and including metadata from each in a meaningful way.

       The list of constituent Hephaestus Components represents the applied
       ordering. *)

    type Model<'c,'r,'s> =
        { Components: ComponentMetadata list
          Specification: Specification<'c,'r,'s> }

        static member components_ : Lens<Model<'c,'r,'s>,ComponentMetadata list> =
            (fun x -> x.Components), (fun c x -> { x with Components = c })

        static member specification_ : Lens<Model<'c,'r,'s>,Specification<'c,'r,'s>> =
            (fun x -> x.Specification), (fun s x -> { x with Specification = s })

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

       Functions to order a set of Hephaestus Components given the defined
       dependencies as supplied in the Hephaestus Component Requirements. An
       ordering may or may not be possible, and the function will throw on
       failure.

       The implementation is a simple topological sort using Kahn's algorithm,
       made simpler by the fact that Hekate will automatically handle the
       removal of appropriate edges on node removal. *)

    [<RequireQualifiedAccess>]
    module internal Order =

        (* Optics *)

        let private name_<'c,'r,'s> =
                Component<'c,'r,'s>.metadata_
            >-> ComponentMetadata.name_

        let private required_<'c,'r,'s> =
                Component<'c,'r,'s>.requirements_
            >-> ComponentRequirements.required_

        (* Functions *)

        let private nodes<'c,'r,'s> =
                List.map (Optic.get name_<'c,'r,'s> &&& id)

        let private edges<'c,'r,'s> =
                List.map (Optic.get name_<'c,'r,'s> &&& Optic.get required_<'c,'r,'s>)
            >>> List.map (fun (n, rs) -> List.map (fun n' -> n', n, ()) (Set.toList rs))
            >>> List.concat

        let private graph<'c,'r,'s> =
                nodes<'c,'r,'s> &&& edges<'c,'r,'s>
            >>> uncurry Graph.create

        let private independent graph =
            Graph.Nodes.toList graph
            |> List.tryFind (fun (v, _) -> Graph.Nodes.inwardDegree v graph = Some 0)
            |> Option.map (fun (v, l) -> l, Graph.Nodes.remove v graph)

        let rec private order modules graph =
            match independent graph with
            | Some (m, graph) -> order (modules @ [ m ]) graph
            | _ when Graph.isEmpty graph -> modules
            | _ -> failwith "A valid Hephaestus Model order cannot be determined."

        let apply<'c,'r,'s> =
                Set.toList
             >> graph<'c,'r,'s>
             >> order []

    (* Operation

       Functions to process the logical operations defined on Hephaestus
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

       The Hephaestus user API for working with Models, specifically the
       composition of Hephaestus Components to Hephaestus Models. This function may
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

       The Hephaestus Graph implementation, turning the logical Hephaestus
       Specification representation of a decision graph in to an optimized
       executable form (given appropriate configuration data to configure a
       graph before optimization).

       The resultant graph effectively behaves as an Async State monad while
       being transparent and modifiable - potentially supporting differing
       forms of optimization, error handling etc. via mapping of the graph
       (including a possible plugin model which would act as a "plugin-based
       graph compiler").

       The graph implementation is not a user-facing API (although the types
       are exposed), and is wrapped by the Hephaestus Prototype and Machine
       APIs. *)

    [<AutoOpen>]
    module Graphs =

        (* Common *)

        [<RequireQualifiedAccess>]
        module Common =

            (* Keys *)

            let RootName =
                [ "root" ]

            (* Types *)

            type Node =
                | Node

             and Edge =
                | Undefined
                | Value of DecisionValue

            (* Functions *)

            let inline log l f =
                Option.map (Optic.map l f)

            let internal increment =
                fun x -> x + 1

        (* Translation

            A translated graph, parameterized by state type, and by the type
            of the configuration data which will be passed to translation
            decisions to effectively reify to a Hephaestus Decision.

            Translated graphs are the result of the translation from the
            Hephaestus Specification model to a phase of the pipeline to produce
            an executable graph model. *)

        [<RequireQualifiedAccess>]
        module Translation =

            (* Types *)

            type Translated<'c,'r,'s> =
                | Translated of GraphType<'c,'r,'s>

                static member translated_ : Isomorphism<Translated<'c,'r,'s>,GraphType<'c,'r,'s>> =
                    (fun (Translated x) -> x), (Translated)

             and GraphType<'c,'r,'s> =
                Graph<string list,Node<'c,'r,'s>,Common.Edge>

             and Node<'c,'r,'s> =
                | Node of Common.Node
                | Decision of DecisionConfigurator<'c,'s>
                | Terminal of TerminalConfigurator<'c,'r,'s>

            (* Log *)

            type Log =
                { Operations: Operation list
                  Statistics: Statistics }

                static member empty =
                    { Operations = List.empty
                      Statistics = Statistics.empty }

                static member operations_ =
                    (fun x -> x.Operations), (fun o x -> { x with Log.Operations = o })

                static member statistics_ =
                    (fun x -> x.Statistics), (fun s x -> { x with Log.Statistics = s })

             and Operation =
                | DecisionTranslated of string list
                | TerminalTranslated of string list

             and Statistics =
                { Nodes: Nodes
                  Edges: int }

                static member empty =
                    { Nodes = Nodes.empty
                      Edges = 0 }

                static member edges_ =
                    (fun x -> x.Edges), (fun e x -> { x with Edges = e })

                static member nodes_ =
                    (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

             and Nodes =
                { Decisions: int
                  Terminals: int }

                static member empty =
                    { Decisions = 0
                      Terminals = 0 }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

            (* Logging *)

            let private operations_ =
                    Log.operations_

            let private decisions_ =
                    Log.statistics_
                >-> Statistics.nodes_
                >-> Nodes.decisions_

            let private terminals_ =
                    Log.statistics_
                >-> Statistics.nodes_
                >-> Nodes.terminals_

            let private edges_ =
                    Log.statistics_
                >-> Statistics.edges_

            let private decisionTranslated k =
                    Common.log operations_ (fun os -> os @ [ DecisionTranslated k ])
                 >> Common.log decisions_ Common.increment

            let private terminalTranslated k =
                    Common.log operations_ (fun os -> os @ [ TerminalTranslated k ])
                 >> Common.log terminals_ Common.increment

            let private edgeTranslated =
                    Common.log edges_ Common.increment

            (* Translation

               Functions for translating a Hephaestus Specification to a graph based
               representation of the equivalent (unmodified) decision graph, prior
               to applying instance specific configuration to the graph. *)

            (* Nodes *)

            let rec private nodes =
                    function | Specification.Decision (n, c, (l, r)) -> decision n c l r
                             | Specification.Terminal (n, c) -> terminal n c

            and private decision n c l r =
                    nodes l
                >>> nodes r
                >>> Graph.Nodes.add (n, Decision c) *** decisionTranslated n

            and private terminal n c =
                    Graph.Nodes.add (n, Terminal c) *** terminalTranslated n

            (* Edges *)

            let rec private edges =
                    function | Specifications.Decision (n, _, (l, r)) -> edge n l r
                             | Specification.Terminal _ -> id

            and private edge n l r =
                    edges l
                >>> edges r
                >>> Graph.Edges.add (n, (|SpecificationName|) l, Common.Value Left) *** edgeTranslated
                >>> Graph.Edges.add (n, (|SpecificationName|) r, Common.Value Right) *** edgeTranslated

            (* Graph *)

            let private empty _ =
                    Graph.empty

            let private graph s =
                    nodes s
                >>> edges s
                >>> first (Graph.Nodes.add (Common.RootName, Node Common.Node))
                >>> first (Graph.Edges.add (Common.RootName, (|SpecificationName|) s, Common.Undefined))

            (* Translate *)

            let internal translate _ =
                    id *** tuple Graph.empty
                >>> uncurry graph
                >>> first Translated

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module Configuration =

            (* Types *)

            type Configured<'r,'s> =
                | Configured of GraphType<'r,'s>

                static member graph_ : Isomorphism<Configured<'r,'s>,GraphType<'r,'s>> =
                    (fun (Configured x) -> x), (Configured)

             and GraphType<'r,'s> =
                Graph<string list,Node<'r,'s>,Common.Edge>

             and Node<'r,'s> =
                | Node of Common.Node
                | Decision of Decision<'s>
                | Terminal of ('s -> Async<'r * 's>)

            type Log =
                { Operations: Operation list
                  Statistics: Statistics }

                static member empty =
                    { Operations = List.empty
                      Statistics = Statistics.empty }

                static member operations_ =
                    (fun x -> x.Operations), (fun o x -> { x with Log.Operations = o })

                static member statistics_ =
                    (fun x -> x.Statistics), (fun s x -> { x with Log.Statistics = s })

             and Operation =
                | DecisionConfigured of string list * Result
                | TerminalConfigured of string list

             and Result =
                | LiteralConfigured of DecisionValue
                | FunctionConfigured

             and Statistics =
                { Decisions: Decisions
                  Terminals: int }

                static member empty =
                    { Decisions = Decisions.empty
                      Terminals = 0 }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Statistics.Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Statistics.Terminals = t })

             and Decisions =
                { Functions: int
                  Literals: int }

                static member empty =
                    { Functions = 0
                      Literals = 0 }

                static member functions_ =
                    (fun x -> x.Functions), (fun f x -> { x with Functions = f })

                static member literals_ =
                    (fun x -> x.Literals), (fun l x -> { x with Literals = l })

            (* Logging

               Types and functions for generating logs of configuration during
               the Machine creation process. A log of operations and a set of
               general statistics are generated when given an existent
               configuration report. *)

            (* Optics *)

            let private operations_ =
                    Log.operations_

            let private literals_ =
                    Log.statistics_
                >-> Statistics.decisions_
                >-> Decisions.literals_

            let private functions_ =
                    Log.statistics_
                >-> Statistics.decisions_
                >-> Decisions.functions_

            let private terminals_ =
                    Log.statistics_
                >-> Statistics.terminals_

            (* Functions *)

            let private literalConfigured k l =
                    Common.log operations_ (fun os -> os @ [ DecisionConfigured (k, LiteralConfigured l) ])
                 >> Common.log literals_ Common.increment

            let private functionConfigured k =
                    Common.log operations_ (fun os -> os @ [ DecisionConfigured (k, FunctionConfigured) ])
                 >> Common.log functions_ Common.increment

            let private terminalConfigured k =
                    Common.log operations_ (fun os -> os @ [ TerminalConfigured (k) ])
                 >> Common.log terminals_ Common.increment

            (* Configuration

               Configuration of a graph, involving the application of
               configurators to given node types, using supplied configuration,
               resulting in literal or function decisions. Reporting is
               generated as part of the process (see above). *)

            (* Functions *)

            let private node l =
                function | Common.Node -> Node (Common.Node), l

            let private decision l k =
                function | Literal v -> Decision (Literal v), literalConfigured k v l
                         | Function f -> Decision (Function f), functionConfigured k l

            let private terminal l k =
                function | f -> Terminal f, terminalConfigured k l

            let private nodes c l k =
                function | Translation.Node n -> node l n
                         | Translation.Decision f -> decision l k (f c)
                         | Translation.Terminal f -> terminal l k (f c)

            let private graph c =
                    uncurry (flip (Graph.Nodes.mapFold (nodes c)))
                 >> swap

            (* Configure *)

            let internal configure c =
                    graph c
                >>> first Configured

        (* Optimization *)

        [<RequireQualifiedAccess>]
        module Optimization =

            (* Types

                An optimized graph, having optimized away all Hephaestus Decisions
                which result in literals, removing the choice point and directly
                connecting the input edges to the appropriate next node given
                the literal value. *)

            type Optimized<'r,'s> =
                | Optimized of GraphType<'r,'s>

                static member graph_ : Isomorphism<Optimized<'r,'s>,GraphType<'r,'s>> =
                    (fun (Optimized x) -> x), (Optimized)

             and GraphType<'r,'s> =
                Graph<string list, Node<'r,'s>,Common.Edge>

             and Node<'r,'s> =
                | Node of Common.Node
                | Decision of ('s -> Async<DecisionValue * 's>)
                | Terminal of ('s -> Async<'r * 's>)

            type Log =
                { Operations: Operation list
                  Statistics: Statistics }

                static member empty =
                     { Operations = List.empty
                       Statistics = Statistics.empty }

                static member operations_ =
                    (fun x -> x.Operations), (fun o x -> { x with Log.Operations = o })

                static member statistics_ =
                    (fun x -> x.Statistics), (fun s x -> { x with Log.Statistics = s })

             and Operation =
                | LiteralEliminated of string list
                | SubgraphRootEliminated of string list

             and Statistics =
                { Literals: int
                  SubgraphRoots: int }

                static member empty =
                    { Literals = 0
                      SubgraphRoots = 0 }

                static member literals_ =
                    (fun x -> x.Literals), (fun l x -> { x with Statistics.Literals = l })

                static member subgraphRoots_ =
                    (fun x -> x.SubgraphRoots), (fun s x -> { x with Statistics.SubgraphRoots = s })

            (* Logging *)

            let private operations_ =
                    Log.operations_

            let private literals_ =
                    Log.statistics_
                >-> Statistics.literals_

            let private subgraphRoots_ =
                    Log.statistics_
                >-> Statistics.subgraphRoots_

            let private eliminatedLiteral k =
                    Common.log operations_ (fun os -> LiteralEliminated k :: os)
                 >> Common.log literals_ Common.increment

            let private eliminatedSubgraphRoot k =
                    Common.log operations_ (fun os -> SubgraphRootEliminated k :: os)
                 >> Common.log subgraphRoots_ Common.increment

            (* Literal Node Elimination *)

            let private outward<'r,'s> n v : Configuration.GraphType<'r,'s> -> string list =
                    Graph.Nodes.outward n
                 >> Option.get
                 >> List.pick (function | _, t, Common.Value v' when v = v' -> Some t
                                        | _ -> None)

            let private inward<'r,'s> n t : Configuration.GraphType<'r,'s> -> LEdge<string list,Common.Edge> list =
                    Graph.Nodes.inward n
                 >> Option.get
                 >> List.map (fun (f, _, v) -> (f, t, v))

            let rec private eliminateLiterals (graph: Configuration.GraphType<'r,'s>, l) =
                match findLiteral graph with
                | Some (n, v) -> eliminateLiterals (bypassLiteral n v graph, eliminatedLiteral n l)
                | _ -> graph, l

            and private findLiteral (graph: Configuration.GraphType<'r,'s>) =
                Graph.Nodes.toList graph
                |> List.tryPick (function | n, Configuration.Decision (Literal v) -> Some (n, v)
                                          | _ -> None)

            and private bypassLiteral n v =
                    ((outward n v &&& id) >>> uncurry (inward n)) &&& id >>> uncurry Graph.Edges.addMany
                >>> Graph.Nodes.remove n

            (* Subgraph Elimination *)

            let rec private eliminateSubgraphs (graph: Configuration.GraphType<'r,'s>, l) =
                match findSubgraph graph with
                | Some n -> eliminateSubgraphs (Graph.Nodes.remove n graph, eliminatedSubgraphRoot n l)
                | _ -> graph, l

            and private findSubgraph (graph: Configuration.GraphType<'r,'s>) =
                Graph.Nodes.toList graph
                |> List.tryPick (function | n, _ when Graph.Nodes.inwardDegree n graph = Some 0 -> Some n
                                          | _ -> None)

            (* Node Conversion *)

            let private convert<'r,'s> : Configuration.GraphType<'r,'s> -> GraphType<'r,'s> =
                Graph.Nodes.map (fun _ ->
                    function | Configuration.Node n -> Node n
                             | Configuration.Decision (Function f) -> Decision f
                             | Configuration.Terminal (f) -> Terminal f
                             | _ -> failwith "Unexpected Node during Optimization.")

            (* Optimization *)

            let private graph g =
                    tuple g
                 >> eliminateLiterals
                 >> eliminateSubgraphs

            let internal optimize _ =
                    uncurry graph
                >>> first convert
                >>> first Optimized

        (* Execution *)

        [<RequireQualifiedAccess>]
        module Execution =

            let private next<'r,'s> n e : Optimization.GraphType<'r,'s> -> string list =
                    Graph.Nodes.successors n
                 >> Option.get
                 >> List.pick (function | n, e' when e = e' -> Some n
                                        | _ -> None)

            let rec private eval<'r,'s> n s (g: Optimization.GraphType<'r,'s>) =
                match Graph.Nodes.find n g with
                | n, Optimization.Node Common.Node -> eval (next n Common.Undefined g) s g
                | n, Optimization.Decision f -> async.Bind (f s, fun (v, s) -> eval (next n (Common.Value v) g) s g)
                | _, Optimization.Terminal f -> f s

            let execute<'r,'s> state (Optimization.Optimized (g: Optimization.GraphType<'r,'s>)) =
                eval Common.RootName state g

    (* Prototype

       Prototypes are the graph representation of a model from which no Machine
       has yet been created. They are the result of translation from a model
       specification form to a graph form. The public creation API allows the
       passing of an option report parameter which will be populated if a
       report is passed. *)

    [<RequireQualifiedAccess>]
    module Prototype =

        (* Types *)

        type Prototype<'c,'r,'s> =
            | Prototype of Translation.Translated<'c,'r,'s>

        type PrototypeCreationLog =
            { Translation: Translation.Log }

            static member empty =
                { Translation = Translation.Log.empty }

            static member translation_ =
                (fun x -> x.Translation), (fun t x -> { x with Translation = t })

        (* Creation *)

        [<RequireQualifiedAccess>]
        module private Create =

            (* Optics *)

            let private translation_ =
                    Option.value_
                >?> PrototypeCreationLog.translation_

            (* Patterns *)

            let private specification =
                    function { Specification = s } -> s

            (* Functions *)

            let private translate _ =
                    specification*** Optic.get translation_ >>> Translation.translate () &&& snd
                >>> function | (t, Some l), log -> t, Optic.set translation_ l log
                             | (t, _), log -> t, log

            let create _ =
                    translate ()
                >>> first Prototype

        (* Functions *)

        let create<'c,'r,'s> (model: Model<'c,'r,'s>) =
            Create.create () (model, None) |> fst

        let createLogged<'c,'r,'s> (model: Model<'c,'r,'s>) =
            Create.create () (model, Some PrototypeCreationLog.empty) |> second Option.get

    (* Machine

       Machines are configured and optimized graphs, prepared for execution
       given appropriate state. They are created from a prepared prototype.
       As with the public prototype API, the creation function allows the
       passing of an option report parameter which will be populated if a
       report is passed.

       Execution may also take an option report parameter, which will be
       returned asynchronously as part of the result. *)

    [<RequireQualifiedAccess>]
    module Machine =

        (* Types *)

        type Machine<'r,'s> =
            | Machine of Optimization.Optimized<'r,'s>

        type MachineCreationLog =
            { Configuration: Configuration.Log
              Optimization: Optimization.Log }

            static member empty =
                { Configuration = Configuration.Log.empty
                  Optimization = Optimization.Log.empty }

            static member configuration_ =
                (fun x -> x.Configuration), (fun c x -> { x with Configuration = c })

            static member optimization_ =
                (fun x -> x.Optimization), (fun o x -> { x with Optimization = o })

        (* Creation *)

        [<RequireQualifiedAccess>]
        module private Create =

            (* Optics *)

            let private configuration_ =
                    Option.value_
                >?> MachineCreationLog.configuration_

            let private optimization_ =
                    Option.value_
                >?> MachineCreationLog.optimization_

            (* Patterns *)

            let private translated =
                    function | Translation.Translated t -> t

            let private configured =
                    function | Configuration.Configured c -> c

            (* Functions *)

            let private configure c =
                    translated *** Optic.get configuration_ >>> Configuration.configure c &&& snd
                >>> function | (c, Some l), log -> c, Optic.set configuration_ l log
                             | (c, _), log -> c, log

            let private optimize _ =
                    configured *** Optic.get optimization_ >>> Optimization.optimize () &&& snd
                >>> function | (o, Some l), log -> o, Optic.set optimization_ l log
                             | (o, _), log -> o, log

            let create c =
                    configure c
                >>> optimize ()
                >>> first Machine

        (* Functions *)

        let create<'c,'r,'s> (Prototype.Prototype (t: Translation.Translated<'c,'r,'s>)) configuration =
            Create.create configuration (t, None) |> fst

        let createLogged<'c,'r,'s> (Prototype.Prototype (t: Translation.Translated<'c,'r,'s>)) configuration =
            Create.create configuration (t, Some MachineCreationLog.empty) |> second Option.get

        let execute<'r,'s> (Machine (g: Optimization.Optimized<'r,'s>)) state =
            Execution.execute state g