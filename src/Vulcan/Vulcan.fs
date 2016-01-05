module Vulcan

open Aether
open Hekate

(* Functions *)

type Vulcan<'a,'s> =
    's -> Async<'a * 's>

(* Types *)

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

(* Common *)

module Common =

    type Edge =
        | Empty
        | Direction of VulcanDecisionValue

    type Node<'s> =
        | Nullary of Vulcan<unit,'s>
        | Unary of Vulcan<unit,'s>

(* Configuration *)

module Configuration =

    (* Types

       Graph types for the two states of the configuration process:
       - a configured graph which has had the configuration data applied
         to relevant nodes, and
       - an unconfigured graph, where decision nodes are not yet configured
         and are functions which will take the configuration data to provide
         a configured decision (which may already be final, or may need further
         evaluation at runtime).
         
       Type parameters are used consistently:
       - 'c for the type of Configuration
       - 's for the type of State *)

    (* Configured

       A configured graph, parameterized solely by the state type which will
       be threaded through the computation. *)

    type ConfiguredGraph<'s> =
        | Graph of Graph<string,ConfiguredNode<'s>,Common.Edge>

        static member graph_ : Isomorphism<ConfiguredGraph<'s>,Graph<string,ConfiguredNode<'s>,Common.Edge>> =
            (fun (Graph x) -> x), (Graph)

     and ConfiguredNode<'s> =
        | Node of Common.Node<'s>
        | Decision of ConfiguredDecision<'s>

        static member node_ : Epimorphism<ConfiguredNode<'s>,Common.Node<'s>> =
            (function | Node n -> Some n
                      | _ -> None), (Node)

        static member decision_ : Epimorphism<ConfiguredNode<'s>,ConfiguredDecision<'s>> =
            (function | Decision d -> Some d
                      | _ -> None), (Decision)

     and ConfiguredDecision<'s> =
        | Decision of VulcanDecision<'s>

        static member decision_ : Isomorphism<ConfiguredDecision<'s>,VulcanDecision<'s>> =
            (fun (Decision d) -> d), (Decision)

    (* Unconfigured

       An unconfigured graph, parameterized by state type, and by the type
       of the configuration data which will be passed to unconfigured
       Decisions to effectively reify to a configured Decision. *)

    type UnconfiguredGraph<'c,'s> =
        | Graph of Graph<string,UnconfiguredNode<'c,'s>,Common.Edge>

        static member graph_ : Isomorphism<UnconfiguredGraph<'c,'s>,Graph<string,UnconfiguredNode<'c,'s>,Common.Edge>> =
            (fun (Graph x) -> x), (Graph)

     and UnconfiguredNode<'c,'s> =
        | Node of Common.Node<'s>
        | Decision of UnconfiguredDecision<'c,'s>

     and UnconfiguredDecision<'c,'s> =
        | Configure of ('c -> ConfiguredDecision<'s>)

    (* Configuration

       Functions for applying configuration data to an unconfigured graph, giving
       a configured graph. *)

    let configure<'c,'s> configuration =
            Optic.get (Lens.ofIsomorphism UnconfiguredGraph<'c,'s>.graph_)
         >> Graph.Nodes.map (fun _ ->
                function | Node n -> ConfiguredNode.Node n
                         | Decision (Configure apply) -> ConfiguredNode.Decision (apply configuration))
         >> ConfiguredGraph<'s>.Graph