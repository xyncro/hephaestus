module Hephaestus

open System
open Aether
open Aether.Operators
open Anat
open Anat.Operators

// TODO: Pre/post-condition analysis
// TODO: Logging/Introspection mechanisms (to be completed for execution - async)

(* Notes

   Type parameter names throughout the Hephaestus implementation are used
   consistently, single character parameter names representing two specific
   concepts:
   - 'c for the type of Configuration
   - 'r for the type of result
   - 's for the type of State *)

(* Aliases

   Rather than opening Hekate at the root, we open sub-modules explicitly as
   aliases, to avoid clashing with our Graph type later on. *)

module Graph = Hekate.Graph
module Edges = Hekate.Graph.Edges
module Nodes = Hekate.Graph.Nodes

(* Prelude *)

[<AutoOpen>]
module internal Prelude =

    type Homomorphism<'a> =
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

type Key =
    | Key of string list

    static member key_ =
        (fun (Key x) -> x), (Key)

    static member empty =
        Key []

    override x.ToString () =
        (function | Key x -> String.Join (".", Array.ofList x)) x

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
        | Decision of Key * ('c -> Decision<'s>) * Pair<Specification<'c,'r,'s>>
        | Terminal of Key * ('c -> 's -> Async<'r * 's>)

        static member decision_ =
            (function | Decision (k, c, s) -> Some (k, c, s)
                      | _ -> None), (Decision)

        static member terminal_ =
            (function | Terminal (k, c) -> Some (k, c)
                      | _ -> None), (Terminal)

    type SpecificationOperation<'c,'r,'s> =
        | Prepend of Homomorphism<Specification<'c,'r,'s>>
        | Splice of Key * DecisionValue * Homomorphism<Specification<'c,'r,'s>>

    (* Helpers *)

    let internal (|Key|) =
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
                Specification<'c,'r,'s>.Terminal (Key [], fun _ s -> async.Return (Unchecked.defaultof<'r>, s))

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

        let private prepend<'c,'r,'s> (f: Homomorphism<Specification<'c,'r,'s>>) specification =
            f specification

        // TODO: Test and fix this in the case of cyclic specifications
        // TODO: Test and fix this in the case of duplicate specification names

        let rec private splice<'c,'r,'s> name value (f: Homomorphism<Specification<'c,'r,'s>>) specification =
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

        (* Keys *)

        let RootKey =
            Key [ "root" ]

        (* Types

           Types representing the graph underlying the machine with nodes of
           varying types representing the different states in which the graph
           may exist during construction, configuration, optimization, etc.
           (effectively compilation passes).

           Types for storing reasonably structured logs of the varying passes
           are defined, along with a simple representation of the graphs after
           each pass. *)

        (* Graph *)

        type Graph<'c,'r,'s> =
            | Graph of Hekate.Graph<Key,Node<'c,'r,'s>,Edge>

         and Node<'c,'r,'s> =
            | Node
            | Constructed of Constructed<'c,'r,'s>
            | Configured of Configured<'r,'s>
            | Optimized of Optimized<'r,'s>

         and Constructed<'c,'r,'s> =
            | Decision of ('c -> Decision<'s>)
            | Terminal of ('c -> 's -> Async<'r * 's>)

         and Configured<'r,'s> =
            | Decision of Decision<'s>
            | Terminal of ('s -> Async<'r * 's>)

         and Optimized<'r,'s> =
            | Decision of ('s -> Async<DecisionValue * 's>)
            | Terminal of ('s -> Async<'r * 's>)

         and Edge =
            | Undefined
            | Value of DecisionValue

        (* Logs *)

        type Log =
            | Log of Pass list

            static member passes_ =
                (fun (Log x) -> x), (Log)

            static member empty =
                Log []

         and Pass =
            | Pass of string * Operation list

            static member name_ =
                (fun (Pass (n, _)) -> n), (fun n (Pass (_, o)) -> Pass (n, o))

            static member operations_ =
                (fun (Pass (_, o)) -> o), (fun o (Pass (n, _)) -> Pass (n, o))

         and Operation =
            | Operation of string * Map<string,string>

            static member name_ =
                (fun (Operation (n, _)) -> n), (fun n (Operation (_, p)) -> Operation (n, p))

            static member properties_ =
                (fun (Operation (_, p)) -> p), (fun p (Operation (n, _)) -> Operation (n, p))

        (* Logging

           Functions for logging passes and operations when given a Log
           instance. Logging a pass will prepend a new pass to the list of
           passes, and logging an operation will prepend an operation to the
           list of operations of the current head pass. *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module private Log =

            let private passes_ =
                    Lens.ofIsomorphism Log.passes_

            let private operations_ =
                    passes_
                >-> List.head_
                >?> Pass.operations_

            let pass name =
                Option.map (
                    Optic.map passes_ (fun ps ->
                        Pass (name, []) :: ps))

            let operation o =
                Option.map (
                    Optic.map operations_ (fun os ->
                        o :: os))

        (* Construction

           A constructed graph, parameterized by state type, and by the type
           of the configuration data which will be passed to construction
           decisions to effectively reify to a Hephaestus Decision.

           Constructed graphs are the result of the translation from the
           Hephaestus Specification model to a phase of the pipeline to produce
           an executable graph model. *)

        [<RequireQualifiedAccess>]
        module Construction =

            (* Logging *)

            let private logDecision k =
                Log.operation (
                    Operation ("decision-constructed",
                               Map.ofList [ "decision", string k ]))

            let private logTerminal k =
                Log.operation (
                    Operation ("terminal-constructed",
                               Map.ofList [ "terminal", string k ]))

            let private logEdge k1 k2 =
                Log.operation (
                    Operation ("edge-constructed",
                               Map.ofList [ "from", string k1
                                            "to", string k2 ]))

            (* Construction

               Functions for constructing a Hephaestus Specification to a graph based
               representation of the equivalent (unmodified) decision graph, prior
               to applying instance specific configuration to the graph. *)

            (* Nodes *)

            let rec private nodes =
                    function | Specification.Decision (k, c, (l, r)) -> decision k c l r
                             | Specification.Terminal (k, c) -> terminal k c

            and private decision k c l r =
                    nodes l
                >>> nodes r
                >>> Graph.Nodes.add (k, Constructed (Constructed.Decision c)) *** logDecision k

            and private terminal k c =
                    Graph.Nodes.add (k, Constructed (Constructed.Terminal c)) *** logTerminal k

            (* Edges *)

            let rec private edges =
                    function | Specifications.Decision (k, _, (l, r)) -> edge k l r
                             | Specification.Terminal _ -> id

            and private edge k l r =
                    edges l
                >>> edges r
                >>> Graph.Edges.add (k, (|Key|) l, Value Left) *** logEdge k ((|Key|) l)
                >>> Graph.Edges.add (k, (|Key|) r, Value Right) *** logEdge k ((|Key|) r)

            (* Graph *)

            let private empty _ =
                    Graph.empty

            let private graph s =
                    nodes s
                >>> edges s
                >>> first (Graph.Nodes.add (RootKey, Node))
                >>> first (Graph.Edges.add (RootKey, (|Key|) s, Undefined))

            (* Construct *)

            let internal construct _ =
                    second (Log.pass "construction")
                >>> second (tuple Graph.empty)
                >>> uncurry graph
                >>> first Graph

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module Configuration =

            (* Logging *)

            let private logLiteral k v =
                Log.operation (
                    Operation ("literal-decision-configured",
                               Map.ofList [ "decision", string k
                                            "value", string v ]))

            let private logFunction k =
                Log.operation (
                    Operation ("function-decision-configured",
                               Map.ofList [ "decision", string k ]))

            let private logTerminal k =
                Log.operation (
                    Operation ("terminal-configured",
                               Map.ofList [ "terminal", string k ]))

            (* Configuration

               Configuration of a graph, involving the application of
               configurators to given node types, using supplied configuration,
               resulting in literal or function decisions. Reporting is
               generated as part of the process (see above). *)

            (* Functions *)

            let private decision l k =
                function | Literal v -> Configured (Configured.Decision (Literal v)), logLiteral k v l
                         | Function f -> Configured (Configured.Decision (Function f)), logFunction k l

            let private terminal l k =
                function | f -> Configured (Configured.Terminal f), logTerminal k l

            let private nodes c l k =
                function | Constructed (Constructed.Decision f) -> decision l k (f c)
                         | Constructed (Constructed.Terminal f) -> terminal l k (f c)
                         | n -> n, l

            let private graph c =
                    uncurry (flip (Graph.Nodes.mapFold (nodes c)))
                 >> swap

            (* Configure *)

            let internal configure c =
                    second (Log.pass "configuration")
                >>> graph c
                >>> first Graph

        (* Optimization *)

        [<RequireQualifiedAccess>]
        module Optimization =

            (* Logging *)

            let private logLiteral k =
                Log.operation (
                    Operation ("literal-eliminated",
                               Map.ofList [ "decision", string k ]))

            let private logDirect k =
                Log.operation (
                    Operation ("direct-eliminated",
                               Map.ofList [ "decision", string k ]))

            let private logSubgraph k =
                Log.operation (
                    Operation ("subgraph-root-eliminated",
                               Map.ofList [ "decision", string k ]))

            (* Common Operations *)

            let private outward n v =
                    Graph.Nodes.outward n
                 >> Option.get
                 >> List.pick (function | _, t, Value v' when v = v' -> Some t
                                        | _ -> None)

            let private inward n t  =
                    Graph.Nodes.inward n
                 >> Option.get
                 >> List.map (fun (f, _, v) -> (f, t, v))

            let private reconnect n v =
                    ((outward n v &&& id) >>> uncurry (inward n)) &&& id >>> uncurry Graph.Edges.addMany
                >>> Graph.Nodes.remove n

            (* Literal Node Elimination *)

            let rec private eliminateLiterals (graph, l) =
                match findLiteral graph with
                | Some (n, v) -> eliminateLiterals (reconnect n v graph, logLiteral n l)
                | _ -> graph, l

            and private findLiteral graph =
                Graph.Nodes.toList graph
                |> List.tryPick (function | n, Configured (Configured.Decision (Literal v)) -> Some (n, v)
                                          | _ -> None)

            (* Direct Node Elimination *)

            let rec private eliminateDirect (graph, l) =
                match findDirect graph with
                | Some (n, v) -> eliminateDirect (reconnect n v graph, logDirect n l)
                | _ -> graph, l

            and private findDirect graph =
                Graph.Nodes.toList graph
                |> List.tryPick (fun (key, _) ->
                    match Graph.Nodes.outward key graph with
                    | Some ([ (_, _, Value v) ]) when key <> RootKey -> Some (key, v)
                    | _ -> None)

            (* Subgraph Elimination *)

            let rec private eliminateSubgraphs (graph, l) =
                match findSubgraph graph with
                | Some n -> eliminateSubgraphs (Graph.Nodes.remove n graph, logSubgraph n l)
                | _ -> graph, l

            and private findSubgraph graph =
                Graph.Nodes.toList graph
                |> List.tryPick (function | n, _ when n = RootKey -> None
                                          | n, _ when Graph.Nodes.inwardDegree n graph = Some 0 -> Some n
                                          | _ -> None)

            (* Node Conversion *)

            let private convert<'c,'r,'s> : Homomorphism<Hekate.Graph<Key,Node<'c,'r,'s>,Edge>> =
                Graph.Nodes.map (fun _ ->
                    function | Node -> Node
                             | Configured (Configured.Decision (Function f)) -> Optimized (Optimized.Decision f)
                             | Configured (Configured.Terminal (f)) -> Optimized (Optimized.Terminal f)
                             | _ -> failwith "Unexpected Node during Optimization.")


            (* Optimization *)

            let private graph g =
                    tuple g
                 >> eliminateLiterals
                 >> eliminateDirect
                 >> eliminateSubgraphs

            let internal optimize _ =
                    second (Log.pass "optimization")
                >>> uncurry graph
                >>> first convert
                >>> first Graph

        (* Execution *)

        [<RequireQualifiedAccess>]
        module Execution =

            let private next n e =
                    Graph.Nodes.successors n
                 >> Option.get
                 >> List.pick (function | n, e' when e = e' -> Some n
                                        | _ -> None)

            let rec private traverse n s g =
                match Graph.Nodes.find n g with
                | n, Node -> traverse (next n Undefined g) s g
                | n, Optimized (Optimized.Decision f) -> async.Bind (f s, fun (v, s) -> traverse (next n (Value v) g) s g)
                | _, Optimized (Optimized.Terminal f) -> f s
                | _ -> failwith ""

            let execute state (Graph g) =
                traverse RootKey state g

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
            | Prototype of Graph<'c,'r,'s>

        (* Creation *)

        [<RequireQualifiedAccess>]
        module private Create =

            (* Patterns *)

            let private specification =
                    function | { Specification = s } -> s

            (* Functions *)

            let prototype _ =
                    first specification
                >>> Construction.construct ()
                >>> first Prototype

        (* Functions *)

        let create model =
            Create.prototype () (model, None) |> fst

        let createLogged model =
            Create.prototype () (model, Some Log.empty) |> second Option.get

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

        type Machine<'c,'r,'s> =
            | Machine of Graph<'c,'r,'s>

        (* Creation *)

        [<RequireQualifiedAccess>]
        module private Create =

            (* Patterns *)

            let private graph =
                    function | Graph g -> g

            (* Functions *)

            let private configure c =
                    first graph
                >>> Configuration.configure c

            let private optimize _ =
                    first graph
                >>> Optimization.optimize ()

            let machine c =
                    configure c
                >>> optimize ()
                >>> first Machine

        (* Functions *)

        let create (Prototype.Prototype g) configuration =
            Create.machine configuration (g, None) |> fst

        let createLogged (Prototype.Prototype g) configuration =
            Create.machine configuration (g, Some Log.empty) |> second Option.get

        let execute (Machine g) state =
            Execution.execute state g