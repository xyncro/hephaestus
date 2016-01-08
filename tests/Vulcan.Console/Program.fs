module Vulcan.Console

open Vulcan

(* Configuration *)

type HttpConfiguration =
    { ServiceAvailable: bool option }

(* State *)

type HttpState =
    { Method: string }

(* Terminals *)

let response name =
    Specification.Terminal.create name (fun s ->
        async {
            printfn "%s Method: %s" name s.Method
            return (), s })

(* Core *)

module HttpCore =

    (* Decisions *)

    let private serviceAvailable =
        Specification.Decision.create "service available?" (fun configuration ->
            match configuration.ServiceAvailable with
            | None ->
                Literal Right
            | Some available ->
                Function (fun state ->
                    async {
                        return match available with
                               | true -> Right, state
                               | _ -> Left, state }))

    (* Composition *)

    let httpCore =
        serviceAvailable (response "service unavailable") (response "ok")

    (* Module *)

    let exports =
        { Metadata =
            { Name = "http.core"
              Description = None }
          Requirements =
            { Required = set []
              Preconditions = [] }
          Operations =
            [ Specification.Prepend (fun _ -> httpCore) ] }

module HttpOptions =

    (* Decisions *)

    let private methodOptions =
        Specification.Decision.create "method options?" (fun (_: HttpConfiguration) ->
            Function (fun state ->
                async {
                    return match state.Method with
                           | m when m = "options" -> Right, state
                           | _ -> Left, state }))

    (* Composition *)

    let httpOptions =
        fun x -> methodOptions x (response "options")

    (* Module *)

    let exports =
        { Metadata =
            { Name = "http.options"
              Description = None }
          Requirements =
            { Required = set [ "http.core" ]
              Preconditions = [] }
          Operations =
            [ Specification.Splice ("service available?", Right, httpOptions) ] }

(* Main *)

[<EntryPoint>]
let main _ =

    let composition = Module.compose (set [ HttpOptions.exports; HttpCore.exports ])

    printfn "composition:\n%A\n" composition

    let _ = System.Console.ReadLine ()

    0
