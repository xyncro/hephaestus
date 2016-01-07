module Vulcan

open System
open Aether
open Hekate

// TODO: Graph root model
// TODO: Specification package model
// TODO: Specification composition model (append, prepend, splice)
// TODO: Dependency and pre/post-condition analysis
// TODO: Optimization
// TODO: Machine wrapper
// TODO: Logging/Introspection mechanisms

(* Notes

   Type parameter names are used consistently:
   - 'c for the type of Configuration
   - 's for the type of State *)

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

            let private terminal () =
                Terminal (sprintf "%A" (Guid.NewGuid ()), fun s -> async { return (), s })

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

        [<RequireQualifiedAccess>]
        module Terminal =

            /// Create a new named terminal, given a Vulcan function returning
            /// unit, and with the appropriate state type.
            let create name f =
                Terminal (name, f)

(* Graphs

   The Vulcan Graph implementation, turning the logical Vulcan Specification
   representation of a decision graph in to an optimized executable form (given
   appropriate configuration data to configure a graph before optimization).

   The resultant graph effectively behaves as an Async State monad while being
   transparent and modifiable - potentially supporting differing forms of
   optimization, error handling etc. via mapping of the graph (including a
   possible plugin model which would act as a "plugin-based graph compiler").

   The graph implementation is not a Vulcan user-facing API, and is wrapped
   within the Machine API. *)

[<AutoOpen>]
module Graphs =

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        // TODO: Determine if Empty edge still required

        type Node<'s> =
            | Terminal of Vulcan<unit,'s>

         and Edge =
            | Empty
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

        let private cn =
            function | Decision (n, _, _)
                     | Terminal (n, _) -> n

        let rec private nodes ns =
            function | Decision (n, c, (l, r)) -> (n, Configure c) :: nodes [] l @ nodes [] r @ ns
                     | Terminal (n, f) -> (n, Node (Common.Terminal f)) :: ns

        let rec private edges es =
            function | Decision (n, _, (l, r)) -> (n, cn l, left) :: (n, cn r, right) :: edges [] l @ edges [] r @ es
                     | Terminal _ -> es

        let translate c =
            Graph (Graph.create (nodes [] c) (edges [] c))

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