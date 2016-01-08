module Vulcan

open System
open Aether
open Aether.Operators
open Hekate

// TODO: Specification composition model (append, prepend, splice)
// TODO: Pre/post-condition analysis
// TODO: Optimization
// TODO: Machine wrapper
// TODO: Logging/Introspection mechanisms

(* Notes

   Type parameter names throughout the Vulcan implementation are used
   consistently, single character parameter names representing two specific
   concepts:
   - 'c for the type of Configuration
   - 's for the type of State *)

(* Prelude *)

[<AutoOpen>]
module internal Prelude =

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

(* Core *)

type Vulcan<'a,'s> =
    's -> Async<'a * 's>

(* Decisions *)

type VulcanDecision<'s> =
    | Function of Vulcan<VulcanDecisionValue,'s>
    | Literal of VulcanDecisionValue

    static member function_ : Epimorphism<VulcanDecision<'s>,Vulcan<VulcanDecisionValue,'s>> =
        (function | Function f -> Some f
                  | _ -> None), (Function)

    static member literal_ : Epimorphism<VulcanDecision<'s>,VulcanDecisionValue> =
        (function | Literal l -> Some l
                  | _ -> None), (Literal)

 and VulcanDecisionValue =
    | Right
    | Left

type VulcanDecisionConfigurator<'c,'s> =
    'c -> VulcanDecision<'s>

(* Results *)

type VulcanResult<'a> =
    | Success of 'a
    | Failure of string

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

    type VulcanSpecification<'c,'s> =
        | Decision of VulcanSpecificationDecision<'c,'s>
        | Terminal of VulcanSpecificationTerminal<'s>

        static member decision_ : Epimorphism<VulcanSpecification<'c,'s>,VulcanSpecificationDecision<'c,'s>> =
            (function | Decision d -> Some d
                      | _ -> None), (Decision)

        static member terminal_ : Epimorphism<VulcanSpecification<'c,'s>,VulcanSpecificationTerminal<'s>> =
            (function | Terminal t -> Some t
                      | _ -> None), (Terminal)

     and VulcanSpecificationDecision<'c,'s> =
        string * VulcanDecisionConfigurator<'c,'s> * (VulcanSpecification<'c,'s> * VulcanSpecification<'c,'s>)

     and VulcanSpecificationTerminal<'s> =
        string * Vulcan<unit,'s>

    (* Specification

       The Vulcan user API for working with specifications, including specific
       functionality for working at the Decision and Terminal level, and for
       working with modifications and compositions at the specification level,
       enabling the combination of multiple specifications, given dependencies,
       precedence, pre/post-conditions, etc. *)

    [<RequireQualifiedAccess>]
    module Specification =

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

            let private configure value =
                fun _ -> Literal value

            // TODO: Investigate what happens when this just becomes Terminal.empty

            let private terminal () =
                Terminal (sprintf "%A" (Guid.NewGuid ()), fun s -> async.Return ((), s))

            /// Create a new named decision, given a suitable configuration
            /// function and specifications for the subsequent left and right
            /// trees.
            let create name configure left right =
                Decision (name, configure, (left, right))

            /// Create a new named left decision, which will always evaluate to
            /// Left, and where the specification of the right tree will be an
            /// effective no-operation terminal.
            let left name left =
                Decision (name, configure Left, (left, terminal ()))

            /// Create a new named right decision, which will always evaluate to
            /// Right, and where the specification of the left tree will be an
            /// effective no-operation terminal.
            let right name right =
                Decision (name, configure Right, (terminal (), right))

        (* Terminals

           Helpful shorthand functions for working with terminals, namely a
           function for creating a new named terminal given a Vulcan function
           of unit. *)

        [<RequireQualifiedAccess>]
        module Terminal =

            /// Create a new named terminal, given a Vulcan function returning
            /// unit, and with the appropriate state type.
            let create name f =
                Terminal (name, f)

            /// Create a new unnamed terminal with a no-op Vulcan function.
            let empty =
                Terminal ("", fun s -> async { return (), s })

        (* Helpers *)

        let (|SpecificationName|) =
            function | Decision (n, _, _) -> n
                     | Terminal (n, _) -> n

        (* Operations*)

        type VulcanSpecificationOperation<'c,'s> =
            | Prepend of VulcanSpecificationMap<'c,'s>
            | Splice of string * VulcanDecisionValue * VulcanSpecificationMap<'c,'s>

         and VulcanSpecificationMap<'c,'s> =
            VulcanSpecification<'c,'s> -> VulcanSpecification<'c,'s>

(* Modules

   The Vulcan Module implementation, providing a way to package and share a
   Specification via operations on the existing state of a notional
   specification, along with metadata relating to that specification, such
   as name, description, etc. plus technical metadata such as dependencies
   and precedence according to the module composition model. *)

[<AutoOpen>]
module Modules =

    (* Types

       Types defining an individual Vulcan Module (pre-composition) and a
       Vulcan Module Composition, the combined result of a set of orderable
       Vulcan Modules, and including metadata from each in a meaningful way.

       The list of constituent Vulcan Modules represents the applied
       ordering. *)

    type VulcanModuleComposition<'c,'s> =
        { Modules: VulcanModuleMetadata list
          Specification: Specifications.VulcanSpecification<'c,'s> }

     and VulcanModule<'c,'s> =
        { Metadata: VulcanModuleMetadata
          Requirements: VulcanModuleRequirements
          Operations: Specification.VulcanSpecificationOperation<'c,'s> list }

        static member metadata_ : Lens<VulcanModule<'c,'s>,VulcanModuleMetadata> =
            (fun x -> x.Metadata), (fun m x -> { x with Metadata = m })

        static member requirements_ : Lens<VulcanModule<'c,'s>,VulcanModuleRequirements> =
            (fun x -> x.Requirements), (fun r x -> { x with Requirements = r })

        static member operations_ : Lens<VulcanModule<'c,'s>,Specification.VulcanSpecificationOperation<'c,'s> list> =
            (fun x -> x.Operations), (fun o x -> { x with Operations = o })

        static member private Comparable (x: VulcanModule<'c,'s>) =
            x.Metadata.Name.ToLowerInvariant ()

        override x.Equals y =
            equalsOn VulcanModule<_,_>.Comparable x y

        override x.GetHashCode () =
            hashOn VulcanModule<_,_>.Comparable x

        interface IComparable with

            member x.CompareTo y =
                compareOn VulcanModule<_,_>.Comparable x y

     and VulcanModuleMetadata =
        { Name: string
          Description: string option }

        static member name_ =
            (fun x -> x.Name), (fun n x -> { x with Name = n })

     and VulcanModuleRequirements =
        { Required: Set<string>
          Preconditions: string list }

        static member required_ =
            (fun x -> x.Required), (fun r x -> { x with Required = r })

    (* Order

       Functions to order a set of Vulcan Modules given the defined dependencies
       as supplied in the Vulcan Module Requirements. An ordering may or may not
       be possible, as represented by the use of the Vulcan Result type.

       The implementation is a simple topological sort using Kahn's algorithm,
       made simpler by the fact that Hekate will automatically handle the
       removal of appropriate edges on node removal. *)

    [<RequireQualifiedAccess>]
    module internal Order =

        let private nodes<'c,'s> =
                List.map (fun m ->
                    Optic.get (
                            VulcanModule<'c,'s>.metadata_ 
                        >-> VulcanModuleMetadata.name_) m, m)

        let private edges<'c,'s> =
                List.map (fun m ->
                    Optic.get (VulcanModule<'c,'s>.metadata_ >-> VulcanModuleMetadata.name_) m,
                    Optic.get (VulcanModule<'c,'s>.requirements_ >-> VulcanModuleRequirements.required_) m)
             >> List.map (fun (n, rs) ->
                    List.map (fun n' -> n', n, ()) (Set.toList rs))
             >> List.concat

        let private graph<'c,'s> modules =
            Graph.create (nodes<'c,'s> modules) (edges<'c,'s> modules)

        let private independent graph =
            Graph.Nodes.toList graph
            |> List.tryFind (fun (v, _) -> Graph.Nodes.inwardDegree v graph = Some 0)
            |> Option.map (fun (v, l) -> l, Graph.Nodes.remove v graph)

        let rec private order modules graph =
            match independent graph with
            | Some (m, graph) -> order (modules @ [ m ]) graph
            | _ when Graph.isEmpty graph -> Success modules
            | _ -> Failure "A valid Vulcan Module order cannot be determined."

        let apply<'c,'s> =
                Set.toList
             >> graph<'c,'s>
             >> order []

    (* Operation

       Functions to process the logical operations defined on Vulcan
       Specifications, combining two specifications in specific and controlled
       ways. *)

    [<RequireQualifiedAccess>]
    module internal Operation =

        type private M<'c,'s> =
            Specification.VulcanSpecificationMap<'c,'s>

        let private prepend<'c,'s> (f: M<'c,'s>) specification =
            f specification

        // TODO: Test and fix this in the case of cyclic specifications

        let rec private splice<'c,'s> name value (f: M<'c,'s>) specification =
            match specification with
            | Decision (n, c, (l, r)) when n = name && value = Left -> Decision (n, c, (f l, r))
            | Decision (n, c, (l, r)) when n = name && value = Right -> Decision (n, c, (l, f r))
            | Decision (n, c, (l, r)) -> Decision (n, c, (splice name value f l, splice name value f r))
            | Terminal (n, f) -> Terminal (n, f)

        let apply<'c,'s> specification =
            function | Specification.Prepend (f) -> prepend<'c,'s> f specification
                     | Specification.Splice (n, v, f) -> splice<'c,'s> n v f specification

    (* Composition

       Functions for combining a set of Vulcan Modules to produce a Vulcan
       Module Composition, detailing the ordered metadata of the Modules used
       to create the composition, and the resulting Vulcan Specification. *)

    [<RequireQualifiedAccess>]
    module internal Composition =

        let private modules<'c,'s> =
                List.map (fun m ->
                    Optic.get VulcanModule<'c,'s>.metadata_ m)

        let private specification<'c,'s> =
                List.map (fun m ->
                    Optic.get VulcanModule<'c,'s>.operations_ m)
             >> List.concat
             >> List.fold Operation.apply Specification.Terminal.empty

        let apply<'c,'s> m =
            { Modules = modules m
              Specification = specification<'c,'s> m }

    (* Module

       The Vulcan user API for working with Modules, specifically the
       composition of Vulcan Modules to Vulcan Module Compositions. This
       function may fail for various logical reasons and so the return
       type is represented as a Vulcan Result. *)

    [<RequireQualifiedAccess>]
    module Module =

        let compose modules =
            match Order.apply modules with
            | Success modules -> Success (Composition.apply modules)
            | Failure f -> Failure f

(* Graphs

   The Vulcan Graph implementation, turning the logical Vulcan Specification
   representation of a decision graph in to an optimized executable form (given
   appropriate configuration data to configure a graph before optimization).

   The resultant graph effectively behaves as an Async State monad while being
   transparent and modifiable - potentially supporting differing forms of
   optimization, error handling etc. via mapping of the graph (including a
   possible plugin model which would act as a "plugin-based graph compiler").

   The graph implementation is not a Vulcan user-facing API, and is wrapped
   within the Vulcan Machine API. *)

[<AutoOpen>]
module internal Graphs =

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        let [<Literal>] RootKey =
            "root"

        type Node<'s> =
            | Root
            | Terminal of Vulcan<unit,'s>

         and Edge =
            | Undefined
            | Value of VulcanDecisionValue

    (* Translation *)

    [<RequireQualifiedAccess>]
    module Translation =

        (* Types

           A translated graph, parameterized by state type, and by the type of
           the configuration data which will be passed to translation decisions
           to effectively reify to a Vulcan Decision.

           Translated graphs are the result of the translation from the Vulcan
           Specification model to a phase of the pipeline to produce an
           executable graph model. *)

        type TranslatedGraph<'c,'s> =
            | Graph of TranslatedGraphType<'c,'s>

            static member graph_ : Isomorphism<TranslatedGraph<'c,'s>,TranslatedGraphType<'c,'s>> =
                (fun (Graph x) -> x), (Graph)

         and TranslatedGraphType<'c,'s> =
            Graph<string,TranslatedNode<'c,'s>,Common.Edge>

         and TranslatedNode<'c,'s> =
            | Node of Common.Node<'s>
            | Configure of VulcanDecisionConfigurator<'c,'s>

        (* Translation

           Functions for translating a Vulcan Specification to a graph based
           representation of the equivalent (unmodified) decision graph, prior
           to applying instance specific configuration to the graph. *)

        let private left =
            Common.Value Left

        let private right =
            Common.Value Right

        let private sn =
            Specification.(|SpecificationName|)

        let rec private nodes ns =
            function | Decision (n, c, (l, r)) -> (n, Configure c) :: nodes [] l @ nodes [] r @ ns
                     | Terminal (n, f) -> (n, Node (Common.Terminal f)) :: ns

        let rec private edges es =
            function | Decision (n, _, (l, r)) -> (n, sn l, left) :: (n, sn r, right) :: edges [] l @ edges [] r @ es
                     | Terminal _ -> es

        let translate s =
            Graph (
                Graph.create
                    ((Common.RootKey, Node Common.Root) :: nodes [] s)
                    ((Common.RootKey, sn s, Common.Undefined) :: edges [] s))

    (* Configuration *)

    [<RequireQualifiedAccess>]
    module Configuration =

        (* Types

           A configured graph, parameterized solely by the state type which will
           be threaded through the computation. *)

        type ConfiguredGraph<'s> =
            | Graph of ConfiguredGraphType<'s>

            static member graph_ : Isomorphism<ConfiguredGraph<'s>,ConfiguredGraphType<'s>> =
                (fun (Graph x) -> x), (Graph)

         and ConfiguredGraphType<'s> =
            Graph<string,ConfiguredNode<'s>,Common.Edge>

         and ConfiguredNode<'s> =
            | Node of Common.Node<'s>
            | Decision of VulcanDecision<'s>

            static member node_ : Epimorphism<ConfiguredNode<'s>,Common.Node<'s>> =
                (function | Node n -> Some n
                          | _ -> None), (Node)

            static member decision_ : Epimorphism<ConfiguredNode<'s>,VulcanDecision<'s>> =
                (function | Decision d -> Some d
                          | _ -> None), (Decision)

        (* Configuration

           Functions for applying configuration data to an unconfigured graph,
           giving a configured graph. *)

        let configure<'c,'s> configuration =
                Optic.get (Lens.ofIsomorphism Translation.TranslatedGraph<'c,'s>.graph_)
             >> Graph.Nodes.map (fun _ ->
                    function | Translation.Node n -> Node n
                             | Translation.Configure f -> Decision (f configuration))
             >> Graph