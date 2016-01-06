module Vulcan

open System
open Aether
open Hekate

(* Notes

   Type parameter names are used consistently:
   - 'c for the type of Configuration
   - 's for the type of State *)

(* Types

   Core Vulcan types of various classifications, representing the common
   building blocks of Vulcan machinery which will regularly be exposed to
   users of the Vulcan system (i.e. those designing and implementing
   Vulcan components. *)

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

(* Components

   The Vulcan Component model, a highly restricted abstraction of the
   conceptual decision graph, allowing only binary (Left|Right) Decisions or
   Terminals to exist as a Component of a graph (where the left and right case
   of a Decision is - recursively - a Component).

   This allows for graphs to be
   defined (including graphs with cycles if required through the use of rec
   bindings) and defined as reusable elements where convenient. *)

[<AutoOpen>]
module Components =

    (* Types

       Types for a component model of a decision graph, using two simple primitive
       types from which any decision graph can be constructed and optimized,
       etc. *)

    type VulcanComponent<'c,'s> =
        | Decision of VulcanComponentDecision<'c,'s>
        | Terminal of VulcanComponentTerminal<'s>

        static member decision_ : Epimorphism<VulcanComponent<'c,'s>,VulcanComponentDecision<'c,'s>> =
            (function | Decision d -> Some d
                      | _ -> None), (Decision)

        static member terminal_ : Epimorphism<VulcanComponent<'c,'s>,VulcanComponentTerminal<'s>> =
            (function | Terminal t -> Some t
                      | _ -> None), (Terminal)

     and VulcanComponentDecision<'c,'s> =
        string * VulcanDecisionConfigurator<'c,'s> * (VulcanComponent<'c,'s> * VulcanComponent<'c,'s>)

     and VulcanComponentTerminal<'s> =
        string * Vulcan<unit,'s>

    (* Decisions

       Helpful shorthand functions for creating decisions, including literal decisions
       where the decision of Left or Right is known at compile time, and the unselected
       decision is assigned a simple null terminal.

       This can be useful for defining components which effectively act as a pipeline
       and only one logical connection point is coherent for the end user of the
       component. It is assumed that dead (unreachable) paths will be optimized out
       of the final executable form of the resultant decision graph in general use. *)

    [<RequireQualifiedAccess>]
    module Decision =

        let private configure value =
            fun _ -> Literal value

        let private terminal () =
            Terminal (sprintf "terminal.%A" (Guid.NewGuid ()), fun s -> async { return (), s })

        let decision name configure left right =
            Decision (name, configure, (left, right))

        let left name left =
            Decision (name, configure Left, (left, terminal ()))

        let right name right =
            Decision (name, configure Right, (terminal (), right))

    (* Terminals *)

    [<RequireQualifiedAccess>]
    module Terminal =

        let terminal name f =
            Terminal (name, f)

(* Graphs

   The Vulcan Graph implementation, turning the logical Vulcan Component representation
   of a decision graph in to an optimized executable form (given appropriate
   configuration data to configure a graph before optimization).
   
   The resultant graph effectively behaves as an Async State monad while being
   transparent and modifiable - potentially supporting differing forms of optimization,
   error handling etc. via mapping of the graph (including a possible plugin model
   which would act as a "plugin-based graph compiler"). *)

[<AutoOpen>]
module Graphs =

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        type Node<'s> =
            | Terminal of Vulcan<unit,'s>

         and Edge =
            | Empty
            | Value of VulcanDecisionValue

    (* Translation *)

    [<RequireQualifiedAccess>]
    module Translation =

        (* Types

           A translation graph, parameterized by state type, and by the type
           of the configuration data which will be passed to translation
           decisions to effectively reify to a Vulcan Decision.

           Translation graphs are the result of the Translation translation from the Vulcan
           Component model to a more general graph model. *)

        type TranslationGraph<'c,'s> =
            | Graph of TranslationGraphType<'c,'s>

            static member graph_ : Isomorphism<TranslationGraph<'c,'s>,TranslationGraphType<'c,'s>> =
                (fun (Graph x) -> x), (Graph)

         and TranslationGraphType<'c,'s> =
            Graph<string,TranslationNode<'c,'s>,Common.Edge>

         and TranslationNode<'c,'s> =
            | Node of Common.Node<'s>
            | Configure of VulcanDecisionConfigurator<'c,'s>

        (* Translation

           Functions for translating a Vulcan Component to a graph based representation
           of the equivalent (unmodified) decision graph, prior to applying instance specific
           configuration to the graph. *)

        let rec private nodes ns =
            function | Decision (n, c, (l, r)) -> (n, Configure c) :: nodes [] l @ nodes [] r @ ns
                     | Terminal (n, f) -> (n, Node (Common.Terminal f)) :: ns

        let translate c =
            Graph (Graph.create (nodes [] c) [])

    (* Configuration *)

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

           Functions for applying configuration data to an unconfigured graph, giving
           a configured graph. *)

        let private (|Node|_|) =
            function | Translation.TranslationNode.Node n -> Some n
                     | _ -> None

        let private (|Configure|_|) =
            function | Translation.Configure c -> Some c
                     | _ -> None

        let configure<'c,'s> configuration =
                Optic.get (Lens.ofIsomorphism Translation.TranslationGraph<'c,'s>.graph_)
             >> Graph.Nodes.map (fun _ ->
                    function | Node node -> Node node
                             | Configure configure -> Decision (configure configuration)
                             | _ -> failwith "")
             >> ConfiguredGraph<'s>.Graph