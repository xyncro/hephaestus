module Hephaestus

open System
open Aether
open Aether.Operators
open Anat
open Anat.Operators
open Hopac

// TODO: Rework commentary
// TODO: Pre/post-condition analysis

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

type DecisionValue<'s> =
    | Function of ('s -> Job<DecisionResult * 's>)
    | Literal of DecisionResult

    static member function_ : Epimorphism<DecisionValue<'s>,('s -> Job<DecisionResult * 's>)> =
        (function | Function f -> Some f
                  | _ -> None), (Function)

    static member literal_ : Epimorphism<DecisionValue<'s>,DecisionResult> =
        (function | Literal l -> Some l
                  | _ -> None), (Literal)

 and DecisionResult =
    | Right
    | Left

(* Keys *)

let private rootKey =
    Key [ "root" ]

(* Logs *)

[<AutoOpen>]
module Logs =

    (* Types *)

    type Log =
        | Log of Pass list

        static member passes_ =
            (fun (Log x) -> x), (Log)

        static member empty =
            Log []

     and Pass =
        | Pass of string * Hekate.Graph<Key,Descriptor,DecisionResult option> * Operation list

        static member graph_ =
            (fun (Pass (_, g, _)) -> g), (fun g (Pass (n, _, o)) -> Pass (n, g, o))

        static member operations_ =
            (fun (Pass (_, _, o)) -> o), (fun o (Pass (n, g, _)) -> Pass (n, g, o))

     and Descriptor =
        | Descriptor of string * Map<string,string>

     and Operation =
        | Operation of string * Map<string,string>

    (* Logging

        Functions for logging passes and operations when given a Log
        instance. Logging a pass will prepend a new pass to the list of
        passes, and logging an operation will prepend an operation to the
        list of operations of the current head pass. *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal Log =

        let private passes_ =
                Lens.ofIsomorphism Log.passes_

        let private pass_ =
                passes_
            >-> List.head_

        let private graph_ =
                pass_
            >?> Pass.graph_

        let private operations_ =
                pass_
            >?> Pass.operations_

        let pass n =
            Option.map (
                Optic.map passes_ (fun ps ->
                    Pass (n, Graph.empty, []) :: ps))

        let graph g =
            Option.map (
                Optic.map graph_ (fun _ ->
                    g))

        let operation o =
            Option.map (
                Optic.map operations_ (fun os ->
                    o :: os))

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
        | Decision of Key * ('c -> DecisionValue<'s>) * Pair<Specification<'c,'r,'s>>
        | Terminal of Key * ('c -> 's -> Job<'r * 's>)

        static member decision_ =
            (function | Decision (k, c, s) -> Some (k, c, s)
                      | _ -> None), (Decision)

        static member terminal_ =
            (function | Terminal (k, c) -> Some (k, c)
                      | _ -> None), (Terminal)

    type SpecificationOperation<'c,'r,'s> =
        | Prepend of Homomorphism<Specification<'c,'r,'s>>
        | Splice of Key * DecisionResult * Homomorphism<Specification<'c,'r,'s>>

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

            let private name () =
                sprintf "empty-%A" (Guid.NewGuid ())

            /// Create a new named terminal, given a Hephaestus function returning
            /// unit, and with the appropriate state type.
            let create<'c,'r,'s> name configure =
                Specification<'c,'r,'s>.Terminal (name, configure)

            /// Create a new unnamed terminal with a no-op Hephaestus function.
            let empty<'c,'r,'s> =
                Specification<'c,'r,'s>.Terminal (Key [], fun _ s -> Job.result (Unchecked.defaultof<'r>, s))

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
            Nodes.toList graph
            |> List.tryFind (fun (v, _) -> Nodes.inwardDegree v graph = Some 0)
            |> Option.map (fun (v, l) -> l, Nodes.remove v graph)

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

(* Prototypes

   Prototypes are the graph representation of a model from which no Machine has
   yet been created. They are the result of translation from a model
   specification form to a graph form. The public creation API allows for
   logged or unlogged creation, supporting runtime debugging, analysis, etc. *)

[<AutoOpen>]
module Prototypes =

    (* Types *)

    type Prototype<'c,'r,'s> =
        | Prototype of Hekate.Graph<Key,Node<'c,'r,'s>,DecisionResult option>

     and Node<'c,'r,'s> =
        | Node
        | Decision of ('c -> DecisionValue<'s>)
        | Terminal of ('c -> 's -> Job<'r * 's>)

    (* Creation *)

    [<AutoOpen>]
    module internal Creation =

        (* Construction *)

        [<AutoOpen>]
        module Construction =

            (* Logging *)

            let private logPass =
                Log.pass "construction"

            let private logDecision k =
                Log.operation (
                    Operation ("decision construction",
                        Map.ofList [ "key", string k ]))

            let private logTerminal k =
                Log.operation (
                    Operation ("terminal construction",
                        Map.ofList [ "key", string k ]))

            let private logEdge k1 k2 =
                Log.operation (
                    Operation ("edge construction",
                        Map.ofList [ "from", string k1
                                     "to",   string k2 ]))

            let private logGraph (g, l) =
                g, Log.graph (
                    Nodes.map (fun _ ->
                        function | Node -> Descriptor ("", Map.empty)
                                 | Decision _ -> Descriptor ("decision", Map.empty)
                                 | Terminal _ -> Descriptor ("terminal", Map.empty)) g) l

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
                >>> fun (g, log) ->
                        match Nodes.contains k g with
                        | false -> Nodes.add (k, Decision c) g, logDecision k log
                        | _ -> g, log

            and private terminal k c =
                    fun (g, log) ->
                        match Nodes.contains k g with
                        | false -> Nodes.add (k, Terminal c) g, logTerminal k log
                        | _ -> g, log

            (* Edges *)

            let rec private edges =
                    function | Specifications.Decision (k, _, (l, r)) -> edge k l r
                             | Specification.Terminal _ -> id

            and private edge k l r =
                    edges l
                >>> edges r
                >>> fun (g, log) ->
                        match Edges.contains k ((|Key|) l) g with
                        | false -> Edges.add (k, (|Key|) l, Some Left) g, logEdge k ((|Key|) l) log
                        | _ -> g, log
                >>> fun (g, log) ->
                        match Edges.contains k ((|Key|) r) g with
                        | false -> Edges.add (k, (|Key|) r, Some Right) g, logEdge k ((|Key|) r) log
                        | _ -> g, log

            (* Graph *)

            let private graph s =
                    nodes s
                >>> edges s
                >>> first (Nodes.add (rootKey, Node))
                >>> first (Edges.add (rootKey, (|Key|) s, None))

            (* Construct *)

            let construct _ =
                    second logPass
                >>> second (tuple Graph.empty)
                >>> uncurry graph
                >>> logGraph

        (* Creation*)

        let create _ =
                first (function | { Specification = s } -> s)
            >>> construct ()
            >>> first Prototype

    (* Prototype *)

    [<RequireQualifiedAccess>]
    module Prototype =

        (* Creation *)

        let createLogged model =
            create () (model, Some Log.empty) |> second Option.get

        let create model =
            create () (model, None) |> fst

(* Machines *)

[<AutoOpen>]
module Machines =

    (* Types *)

    type Machine<'r,'s> =
        | Decision of Key * ('s -> Job<DecisionResult * 's>) * Pair<Machine<'r,'s>>
        | Terminal of Key * ('s -> Job<'r * 's>)

    (* Creation *)

    [<AutoOpen>]
    module internal Creation =

        (* Configuration *)

        [<AutoOpen>]
        module Configuration =

            (* Types *)

            type Node<'r,'s> =
                | Node
                | Decision of DecisionValue<'s>
                | Terminal of ('s -> Job<'r * 's>)

            (* Logging *)

            let private logPass =
                Log.pass "configuration"

            let private logLiteral k v =
                Log.operation (
                    Operation ("decision literal configuration",
                                Map.ofList [ "key", string k
                                             "value", string v ]))

            let private logFunction k =
                Log.operation (
                    Operation ("decision function configuration",
                                Map.ofList [ "key", string k ]))

            let private logTerminal k =
                Log.operation (
                    Operation ("terminal configuration",
                                Map.ofList [ "key", string k ]))

            let private logGraph (g, l) =
                g, Log.graph (
                    Nodes.map (fun _ ->
                        function | Node -> Descriptor ("", Map.empty)
                                 | Decision (Literal l) -> Descriptor ("literal", Map.ofList [ "result", string l ])
                                 | Decision _ -> Descriptor ("function", Map.empty)
                                 | Terminal _ -> Descriptor ("terminal", Map.empty)) g) l

            (* Configuration

                Configuration of a graph, involving the application of
                configurators to given node types, using supplied configuration,
                resulting in literal or function decisions. Reporting is
                generated as part of the process (see above). *)

            (* Functions *)

            let private decision l k =
                function | Literal v -> Decision (Literal v), logLiteral k v l
                         | Function f -> Decision (Function f), logFunction k l

            let private terminal l k =
                function | f -> Terminal f, logTerminal k l

            let private nodes c l k =
                function | Prototypes.Decision f -> decision l k (f c)
                         | Prototypes.Terminal f -> terminal l k (f c)
                         | Prototypes.Node -> Node, l

            let private graph c =
                    uncurry (flip (Nodes.mapFold (nodes c)))
                 >> swap

            (* Configure *)

            let configure c =
                    second logPass
                >>> graph c
                >>> logGraph

        (* Optimization *)

        [<AutoOpen>]
        module Optimization =

            (* Types *)

            type Node<'r,'s> =
                | Node
                | Decision of ('s -> Job<DecisionResult * 's>)
                | Terminal of ('s -> Job<'r * 's>)

            (* Logging *)

            let private logGraph (g, l) =
                g, Log.graph (
                    Nodes.map (fun _ ->
                        function | Node -> Descriptor ("", Map.empty)
                                 | Decision _ -> Descriptor ("decision", Map.empty)
                                 | Terminal _ -> Descriptor ("terminal", Map.empty)) g) l

            (* Common *)

            let private outward k v =
                    Nodes.outward k
                 >> Option.get
                 >> List.pick (function | _, t, Some v' when v = v' -> Some t
                                        | _ -> None)

            let private inward k t =
                    Nodes.inward k
                 >> Option.get
                 >> List.map (fun (f, _, v) -> (f, t, v))

            let private reconnect k v =
                    ((outward k v &&& id) >>> uncurry (inward k)) &&& id >>> uncurry Edges.addMany
                >>> Nodes.remove k

            let private remove k =
                    Nodes.remove k

            (* Literal Elimination *)

            [<RequireQualifiedAccess>]
            module private LiteralElimination =

                (* Logging *)

                let private logPass =
                    Log.pass "literal elimination optimization"

                let private logRootLiteral k =
                    Log.operation (
                        Operation ("root literal elimination",
                                   Map.ofList [ "key", string k ]))

                let private logChildLiteral k =
                    Log.operation (
                        Operation ("child literal elimination",
                                   Map.ofList [ "key", string k ]))

                (* Elimination *)

                let rec private eliminateLiterals (g, l) =
                    match g with
                    | RootLiteral (k, _) -> eliminateLiterals (remove k g, logRootLiteral k l)
                    | ChildLiteral (k, v) -> eliminateLiterals (reconnect k v g, logChildLiteral k l)
                    | _ -> g, l

                and private (|RootLiteral|_|) g =
                    literal (root) g

                and private (|ChildLiteral|_|) g =
                    literal (fun g k -> not (root g k)) g

                and private literal p g =
                    Nodes.toList g
                    |> List.tryPick (function | k, Configuration.Decision (Literal v) when (p g k) -> Some (k, v)
                                              | _ -> None)

                and private root g k =
                    match Nodes.inwardDegree k g with
                    | Some 0 | None -> true
                    | _ -> false

                (* Mapping *)

                let private map g =
                    Nodes.map (fun _ n ->
                        match n with
                        | Configuration.Node -> Node
                        | Configuration.Decision (Function v) -> Decision v
                        | Configuration.Terminal (v) -> Terminal v
                        | _ -> failwith "") g

                (* Optimization *)

                let optimize _ =
                        second logPass
                    >>> eliminateLiterals
                    >>> first map
                    >>> logGraph

            (* Unary Elimination *)

            [<RequireQualifiedAccess>]
            module UnaryElimination =

                (* Logging *)

                let private logPass =
                    Log.pass "unary elimination optimization"

                let private logUnary k =
                    Log.operation (
                        Operation ("unary elimination",
                                   Map.ofList [ "key", string k ]))

                (* Elimination *)

                let rec private eliminateUnary (g, l) =
                    match findUnary g with
                    | Some (k, v) -> eliminateUnary (reconnect k v g, logUnary k l)
                    | _ -> g, l

                and private findUnary g =
                    Nodes.toList g
                    |> List.tryPick (fun (k, _) ->
                        match Nodes.outward k g with
                        | Some ([ (_, _, Some v) ]) when k <> rootKey -> Some (k, v)
                        | _ -> None)

                (* Optimization *)

                let optimize _ =
                        second logPass
                    >>> eliminateUnary
                    >>> logGraph

            (* Subgraph Elimination *)

            [<RequireQualifiedAccess>]
            module private SubgraphElimination =

                (* Logging *)

                let private logPass =
                    Log.pass "subgraph elimination optimization"

                let private logSubgraph k =
                    Log.operation (
                        Operation ("subgraph elimination",
                                   Map.ofList [ "key", string k ]))

                (* Elimination *)

                let rec private eliminateSubgraphs (graph, l) =
                    match findSubgraph graph with
                    | Some k -> eliminateSubgraphs (Nodes.remove k graph, logSubgraph k l)
                    | _ -> graph, l

                and private findSubgraph graph =
                    Nodes.toList graph
                    |> List.tryPick (function | k, _ when k = rootKey -> None
                                              | k, _ when Nodes.inwardDegree k graph = Some 0 -> Some k
                                              | _ -> None)

                (* Optimization *)

                let optimize _ =
                        second logPass
                    >>> eliminateSubgraphs
                    >>> logGraph

            (* Optimization *)

            let private optimizations g =
                    tuple g
                 >> LiteralElimination.optimize ()
                 >> UnaryElimination.optimize ()
                 >> SubgraphElimination.optimize ()

            let optimize _ =
                    uncurry optimizations

        (* Deconstruction *)

        [<AutoOpen>]
        module Deconstruction =

            let private find k e =
                    Nodes.successors k
                 >> Option.get
                 >> List.pick (function | k, e' when e = e' -> Some k
                                        | _ -> None)

            let rec private machine k g cache =
                match Map.tryFind k cache with
                | Some m ->
                    m, cache
                | _ ->
                    match Nodes.find k g with
                    | _, Optimization.Decision f -> decision k f g cache
                    | _, Optimization.Terminal f -> terminal k f cache
                    | _ -> failwith ""

            and private decision k f g cache =
                let l, cache = machine (find k (Some Left) g) g cache
                let r, cache = machine (find k (Some Right) g) g cache
                let m = Machine.Decision (k, f, (l, r))

                m, Map.add k m cache

            and private terminal k f cache =
                let m = Machine.Terminal (k, f)

                m, Map.add k m cache

            let deconstruct _ =
                    (fun g -> fst (machine (find rootKey None g) g Map.empty)) *** id

        (* Creation *)

        let create c =
                first (function | Prototype.Prototype p -> p)
            >>> configure c
            >>> optimize ()
            >>> deconstruct ()

    (* Execution *)

    [<AutoOpen>]
    module internal Execution =

        (* Evaluation *)

        [<AutoOpen>]
        module Evaluation =

            (* Logging *)

            let private logPass =
                Log.pass "evaluation"

            let private logDecision k v =
                Log.operation (
                    Operation ("decision",
                               Map.ofList [ "key", string k
                                            "value", string v ]))

            let private logTerminal k =
                Log.operation (
                    Operation ("terminal",
                               Map.ofList [ "key", string k ]))

            (* Evaluation *)

            let rec private eval s =
                function | Machine.Decision (k, f, p), l -> decision s k f p l
                         | Machine.Terminal (k, f), l -> terminal s k f l

            and private decision s k f p l =
                Job.bind (
                    function | Left, s' -> eval s' (fst p, logDecision k Left l)
                             | Right, s' -> eval s' (snd p, logDecision k Right l)) (f s)

            and private terminal s k f l =
                Job.bind (fun (v, s) ->
                    Job.result ((v, s), logTerminal k l)) (f s)

            let evaluate s =
                    second logPass
                >>> eval s

        (* Execution *)

        let execute s =
                evaluate s

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

        (* Creation *)

        let createLogged prototype configuration =
            create configuration (prototype, Some Log.empty) |> second Option.get

        let create prototype configuration =
            create configuration (prototype, None) |> fst

        (* Execution *)

        let executeLogged machine state =
            Job.bind (fun ((v, s), l) ->
                Job.result ((v, Option.get l), s)) (execute state (machine, Some Log.empty))

        let execute machine state =
            Job.bind (fun ((v, s), _) ->
                Job.result (v, s)) (execute state (machine, None))
