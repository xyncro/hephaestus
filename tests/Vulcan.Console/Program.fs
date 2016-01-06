module Vulcan.Console

open Vulcan

(* Configuration *)

type ExampleConfiguration =
    { AllowEven: bool }

(* State *)

type ExampleState =
    { Value: int }

(* Terminals *)

let printer name =
    Terminal.terminal name (fun s ->
        async {
            printfn "%s Value: %i" name s.Value
            return (), s })

(* Decisions *)

let filterEven =
    Decision.decision "filter even" (fun configuration ->
        match configuration.AllowEven with
        | true ->
            Literal Right
        | _ ->
            Function (fun state ->
                async {
                    return match state.Value with
                           | value when value % 2 <> 0 -> Right, state
                           | _ -> Left, state }))

(* Composition *)

let protocol =
    filterEven (printer "not allowed") (printer "complete")

(* Main *)

[<EntryPoint>]
let main _ =

    let translated = Graphs.Translation.translate protocol
    let configured = Graphs.Configuration.configure { AllowEven = false } translated

    0
