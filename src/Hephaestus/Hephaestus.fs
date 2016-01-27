module Hephaestus

open System
open Aether
open Aether.Operators
open Anat
open Anat.Operators
open Hekate

// TODO: Pre/post-condition analysis
// TODO: Logging/Introspection mechanisms
// TODO: Simplify with Arrows where possible

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

    let swap (a, b) =
        (b, a)

    let tuple a b =
        (a, b)

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

        (* Types *)

        let RootName =
            [ "root" ]

        type Node =
            | Node

         and Edge =
            | Undefined
            | Value of DecisionValue

        (* Common *)

        [<RequireQualifiedAccess>]
        module Common =

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

        (* Graph *)

        type TranslatedGraph<'c,'r,'s> =
            | Graph of TranslatedGraphType<'c,'r,'s>

            static member graph_ : Isomorphism<TranslatedGraph<'c,'r,'s>,TranslatedGraphType<'c,'r,'s>> =
                (fun (Graph x) -> x), (Graph)

         and TranslatedGraphType<'c,'r,'s> =
            Graph<string list,TranslatedNode<'c,'r,'s>,Edge>

         and TranslatedNode<'c,'r,'s> =
            | TranslatedNode of Node
            | TranslatedDecision of DecisionConfigurator<'c,'s>
            | TranslatedTerminal of TerminalConfigurator<'c,'r,'s>

        (* Log *)

        type TranslationLog =
            { Operations: TranslationOperation list
              Statistics: TranslationStatistics }

            static member empty =
                { Operations = List.empty
                  Statistics = TranslationStatistics.empty }

            static member operations_ =
                (fun x -> x.Operations), (fun o x -> { x with TranslationLog.Operations = o })

            static member statistics_ =
                (fun x -> x.Statistics), (fun s x -> { x with TranslationLog.Statistics = s })

         and TranslationOperation =
            | TranslatedDecision of string list
            | TranslatedTerminal of string list

         and TranslationStatistics =
            { Nodes: TranslationNodes
              Edges: int }

            static member empty =
                { Nodes = TranslationNodes.empty
                  Edges = 0 }

            static member edges_ =
                (fun x -> x.Edges), (fun e x -> { x with Edges = e })

            static member nodes_ =
                (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

         and TranslationNodes =
            { Decisions: int
              Terminals: int }

            static member empty =
                { Decisions = 0
                  Terminals = 0 }

            static member decisions_ =
                (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

            static member terminals_ =
                (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        [<RequireQualifiedAccess>]
        module Translation =

            (* Logging *)

            let private operations_ =
                    TranslationLog.operations_

            let private decisions_ =
                    TranslationLog.statistics_
                >-> TranslationStatistics.nodes_
                >-> TranslationNodes.decisions_

            let private terminals_ =
                    TranslationLog.statistics_
                >-> TranslationStatistics.nodes_
                >-> TranslationNodes.terminals_

            let private edges_ =
                    TranslationLog.statistics_
                >-> TranslationStatistics.edges_

            let private translatedDecision k =
                    Common.log operations_ (fun os -> os @ [TranslatedDecision k ])
                 >> Common.log decisions_ Common.increment

            let private translatedTerminal k =
                    Common.log operations_ (fun os -> os @ [ TranslatedTerminal k ])
                 >> Common.log terminals_ Common.increment

            let private translatedEdge =
                    Common.log edges_ Common.increment

            (* Translation

               Functions for translating a Hephaestus Specification to a graph based
               representation of the equivalent (unmodified) decision graph, prior
               to applying instance specific configuration to the graph. *)

            (* Nodes *)

            let private nodeRoot =
                RootName, TranslatedNode Node

            let private nodeDecision n c =
                n, TranslatedNode.TranslatedDecision c

            let private nodeTerminal n c =
                n, TranslatedNode.TranslatedTerminal c

            let rec private nodes =
                function | Specification.Decision (n, c, (l, r)) -> decision n c l r
                         | Specification.Terminal (n, c) -> terminal n c

            and private decision n c l r =
                    Graph.Nodes.add (nodeDecision n c) *** translatedDecision n
                >>> nodes l
                >>> nodes r

            and private terminal n c =
                    Graph.Nodes.add (nodeTerminal n c) *** translatedTerminal n

            (* Edges *)

            let private edgeRoot s =
                RootName, (|SpecificationName|) s, Undefined

            let private edgeLeft n s =
                n, (|SpecificationName|) s, Value Left

            let private edgeRight n s =
                n, (|SpecificationName|) s, Value Right

            let rec private edges =
                function | Specifications.Decision (n, _, (l, r)) -> edge n l r
                         | Specification.Terminal _ -> id

            and private edge n l r =
                    Graph.Edges.add (edgeLeft n l) *** translatedEdge
                >>> Graph.Edges.add (edgeRight n r) *** translatedEdge
                >>> edges l
                >>> edges r

            (* Graph *)

            let private graph s =
                    nodes s
                >>> edges s
                >>> first (Graph.Nodes.add nodeRoot)
                >>> first (Graph.Edges.add (edgeRoot s))

            (* Translate *)

            let internal translate s =
                    fun l -> graph s (Graph.empty, l)
                >>> first Graph

        (* Configuration *)

        (* Types *)

        type ConfiguredGraph<'r,'s> =
            | Graph of ConfiguredGraphType<'r,'s>

            static member graph_ : Isomorphism<ConfiguredGraph<'r,'s>,ConfiguredGraphType<'r,'s>> =
                (fun (Graph x) -> x), (Graph)

         and ConfiguredGraphType<'r,'s> =
            Graph<string list,ConfiguredNode<'r,'s>,Edge>

         and ConfiguredNode<'r,'s> =
            | ConfiguredNode of Node
            | ConfiguredDecision of Decision<'s>
            | ConfiguredTerminal of ('s -> Async<'r * 's>)

        type ConfigurationLog =
            { Operations: ConfigurationOperation list
              Statistics: ConfigurationStatistics }

            static member empty =
                { Operations = List.empty
                  Statistics = ConfigurationStatistics.empty }

            static member operations_ =
                (fun x -> x.Operations), (fun o x -> { x with ConfigurationLog.Operations = o })

            static member statistics_ =
                (fun x -> x.Statistics), (fun s x -> { x with ConfigurationLog.Statistics = s })

         and ConfigurationOperation =
            | ConfiguredDecision of string list * ConfigurationResult
            | ConfiguredTerminal of string list

         and ConfigurationResult =
            | ConfiguredLiteral of DecisionValue
            | ConfiguredFunction

         and ConfigurationStatistics =
            { Decisions: ConfigurationDecisions
              Terminals: int }

            static member empty =
                { Decisions = ConfigurationDecisions.empty
                  Terminals = 0 }

            static member decisions_ =
                (fun x -> x.Decisions), (fun d x -> { x with ConfigurationStatistics.Decisions = d })

            static member terminals_ =
                (fun x -> x.Terminals), (fun t x -> { x with ConfigurationStatistics.Terminals = t })

         and ConfigurationDecisions =
            { Functions: int
              Literals: int }

            static member empty =
                { Functions = 0
                  Literals = 0 }

            static member functions_ =
                (fun x -> x.Functions), (fun f x -> { x with Functions = f })

            static member literals_ =
                (fun x -> x.Literals), (fun l x -> { x with Literals = l })

        [<RequireQualifiedAccess>]
        module Configuration =

            (* Logging

               Types and functions for generating logs of configuration during
               the Machine creation process. A log of operations and a set of
               general statistics are generated when given an existent
               configuration report. *)

            (* Optics *)

            let private operations_ =
                    ConfigurationLog.operations_

            let private literals_ =
                    ConfigurationLog.statistics_
                >-> ConfigurationStatistics.decisions_
                >-> ConfigurationDecisions.literals_

            let private functions_ =
                    ConfigurationLog.statistics_
                >-> ConfigurationStatistics.decisions_
                >-> ConfigurationDecisions.functions_

            let private terminals_ =
                    ConfigurationLog.statistics_
                >-> ConfigurationStatistics.terminals_

            (* Functions *)

            let private configuredDecisionLiteral k l =
                    Common.log operations_ (fun os -> os @ [ ConfiguredDecision (k, ConfiguredLiteral l) ])
                 >> Common.log literals_ Common.increment

            let private configuredDecisionFunction k =
                    Common.log operations_ (fun os -> os @ [ ConfiguredDecision (k, ConfiguredFunction) ])
                 >> Common.log functions_ Common.increment

            let private configuredTerminal k =
                    Common.log operations_ (fun os -> os @ [ ConfiguredTerminal (k) ])
                 >> Common.log terminals_ Common.increment

            (* Configuration

               Configuration of a graph, involving the application of
               configurators to given node types, using supplied configuration,
               resulting in literal or function decisions. Reporting is
               generated as part of the process (see above). *)

            (* Functions *)

            let private node l =
                function | Node -> ConfiguredNode (Node), l

            let private decision k l =
                function | Literal v -> ConfiguredNode.ConfiguredDecision (Literal v), configuredDecisionLiteral k v l
                         | Function f -> ConfiguredNode.ConfiguredDecision (Function f), configuredDecisionFunction k l

            let private terminal k l =
                function | f -> ConfiguredNode.ConfiguredTerminal f, configuredTerminal k l

            let private nodes k c l =
                function | TranslatedNode n -> node l n
                         | TranslatedNode.TranslatedDecision f -> decision k l (f c)
                         | TranslatedNode.TranslatedTerminal f -> terminal k l (f c)

            let private graph c l =
                    Graph.Nodes.mapFold (fun l k n -> nodes k c l n) l
                 >> swap

            (* Configure *)

            let internal configure<'c,'r,'s> (TranslatedGraph.Graph (g: TranslatedGraphType<'c,'r,'s>)) c =
                    (fun l -> graph c l g)
                >>> first Graph

        (* Optimization *)

        (* Types

            An optimized graph, having optimized away all Hephaestus Decisions
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
            | OptimizedNode of Node
            | OptimizedDecision of ('s -> Async<DecisionValue * 's>)
            | OptimizedTerminal of ('s -> Async<'r * 's>)

        type OptimizationLog =
            { Operations: OptimizationOperation list
              Statistics: OptimizationStatistics }

            static member empty =
                 { Operations = List.empty
                   Statistics = OptimizationStatistics.empty }

            static member operations_ =
                (fun x -> x.Operations), (fun o x -> { x with OptimizationLog.Operations = o })

            static member statistics_ =
                (fun x -> x.Statistics), (fun s x -> { x with OptimizationLog.Statistics = s })

         and OptimizationOperation =
            | EliminatedLiteral of string list
            | EliminatedSubgraphRoot of string list

         and OptimizationStatistics =
            { Literals: int
              SubgraphRoots: int }

            static member empty =
                { Literals = 0
                  SubgraphRoots = 0 }

            static member literals_ =
                (fun x -> x.Literals), (fun l x -> { x with OptimizationStatistics.Literals = l })

            static member subgraphRoots_ =
                (fun x -> x.SubgraphRoots), (fun s x -> { x with OptimizationStatistics.SubgraphRoots = s })

        [<RequireQualifiedAccess>]
        module Optimization =

            (* Logging *)

            let private operations_ =
                    OptimizationLog.operations_

            let private literals_ =
                    OptimizationLog.statistics_
                >-> OptimizationStatistics.literals_

            let private subgraphRoots_ =
                    OptimizationLog.statistics_
                >-> OptimizationStatistics.subgraphRoots_

            let private eliminatedLiteral k =
                    Common.log operations_ (fun os -> EliminatedLiteral k :: os)
                 >> Common.log literals_ Common.increment

            let private eliminatedSubgraphRoot k =
                    Common.log operations_ (fun os -> EliminatedSubgraphRoot k :: os)
                 >> Common.log subgraphRoots_ Common.increment

            (* Literal Node Elimination *)

            let private literal<'r,'s> : ConfiguredGraphType<'r,'s> -> (string list * DecisionValue) option =
                    Graph.Nodes.toList
                 >> List.tryPick (function | n, ConfiguredNode.ConfiguredDecision (Literal v) -> Some (n, v)
                                           | _ -> None)

            let private target<'r,'s> n v : ConfiguredGraphType<'r,'s> -> string list =
                    Graph.Nodes.outward n
                 >> Option.get
                 >> List.pick (function | _, t, Value v' when v = v' -> Some t
                                        | _ -> None)

            let private edges<'r,'s> n t : ConfiguredGraphType<'r,'s> -> LEdge<string list,Edge> list =
                    Graph.Nodes.inward n
                 >> Option.get
                 >> List.map (fun (f, _, v) -> (f, t, v))

            let rec private eliminateLiterals<'r,'s> (graph: ConfiguredGraphType<'r,'s>, l) =
                match literal graph with
                | Some (n, v) ->
                    eliminateLiterals (
                        (graph
                         |> Graph.Nodes.remove n
                         |> Graph.Edges.addMany (edges n (target n v graph) graph)), eliminatedLiteral n l)
                | _ ->
                    graph, l

            (* Subgraph Elimination *)

            let private subgraph<'r,'s> (graph: ConfiguredGraphType<'r,'s>) =
                 Graph.Nodes.toList graph
                 |> List.tryPick (function | _, ConfiguredNode Node -> None
                                           | n, _ when Graph.Nodes.inwardDegree n graph = Some 0 -> Some n
                                           | _ -> None)

            let rec private eliminateSubgraphs<'r,'s> (graph: ConfiguredGraphType<'r,'s>, l) =
                match subgraph graph with
                | Some n -> eliminateSubgraphs (Graph.Nodes.remove n graph, eliminatedSubgraphRoot n l)
                | _ -> graph, l

            (* Node Conversion *)

            let private convert<'r,'s> : ConfiguredGraphType<'r,'s> -> OptimizedGraphType<'r,'s> =
                Graph.Nodes.map (fun _ ->
                    function | ConfiguredNode n -> OptimizedNode n
                             | ConfiguredNode.ConfiguredDecision (Function f) -> OptimizedDecision f
                             | ConfiguredNode.ConfiguredTerminal (f) -> OptimizedTerminal f
                             | _ -> failwith "Unexpected Node during Optimization.")

            (* Optimization *)

            let private graph g =
                    tuple g
                 >> eliminateLiterals
                 >> eliminateSubgraphs

            let internal optimize<'r,'s> (ConfiguredGraph.Graph (g: ConfiguredGraphType<'r,'s>)) =
                    graph g
                >>> first convert
                >>> first Graph

        (* Execution *)

        [<RequireQualifiedAccess>]
        module Execution =

            let private next<'r,'s> n e : OptimizedGraphType<'r,'s> -> string list =
                    Graph.Nodes.successors n
                 >> Option.get
                 >> List.pick (function | n, e' when e = e' -> Some n
                                        | _ -> None)

            let rec private eval<'r,'s> n s (g: OptimizedGraphType<'r,'s>) =
                match Graph.Nodes.find n g with
                | n, OptimizedNode Node -> eval (next n Undefined g) s g
                | n, OptimizedDecision f -> async.Bind (f s, fun (v, s) -> eval (next n (Value v) g) s g)
                | _, OptimizedTerminal f -> f s

            let execute<'r,'s> state (graph: OptimizedGraph<'r,'s>) =
                match graph with
                | OptimizedGraph.Graph graph -> eval RootName state graph

    (* Prototype

       Prototypes are the graph representation of a model from which no Machine
       has yet been created. They are the result of translation from a model
       specification form to a graph form. The public creation API allows the
       passing of an option report parameter which will be populated if a
       report is passed. *)

    type Prototype<'c,'r,'s> =
        | Prototype of TranslatedGraph<'c,'r,'s>

    type PrototypeCreationLog =
        { Translation: TranslationLog }

        static member empty =
            { Translation = TranslationLog.empty }

    [<RequireQualifiedAccess>]
    module Prototype =

        let private translate model log =
            Translation.translate model.Specification (Option.map (fun r -> r.Translation) log)

        let create<'c,'r,'s> (model: Model<'c,'r,'s>) log =
            translate model log
            |> fun (g, l) -> Prototype g, Option.map (fun r -> { r with Translation = Option.get l }) log

    (* Machine

       Machines are configured and optimized graphs, prepared for execution
       given appropriate state. They are created from a prepared prototype.
       As with the public prototype API, the creation function allows the
       passing of an option report parameter which will be populated if a
       report is passed.

       Execution may also take an option report parameter, which will be
       returned asynchronously as part of the result. *)

    type Machine<'r,'s> =
        | Machine of OptimizedGraph<'r,'s>

    type MachineCreationLog =
        { Configuration: ConfigurationLog
          Optimization: OptimizationLog }

        static member empty =
            { Configuration = ConfigurationLog.empty
              Optimization = OptimizationLog.empty }

    [<RequireQualifiedAccess>]
    module Machine =

        let private configure graph configuration log =
            Configuration.configure graph configuration (Option.map (fun l -> l.Configuration) log)

        let private optimize graph log =
            Optimization.optimize graph (Option.map (fun l -> l.Optimization) log)

        // TODO: Nicer

        let create<'c,'r,'s> (Prototype (g: TranslatedGraph<'c,'r,'s>)) configuration log =
            configure g configuration log
            |> fun (g, lc) -> optimize g log, lc
            |> fun ((g, lo), lc) -> Machine g, (Option.map (fun r ->
                { r with Configuration = Option.get lc
                         Optimization = Option.get lo }) log)

        let execute<'r,'s> (Machine (g: OptimizedGraph<'r,'s>)) state =
            Execution.execute state g
