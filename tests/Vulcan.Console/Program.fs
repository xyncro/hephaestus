module Vulcan.Console

open Vulcan

(* Classifications *)

[<Literal>]
let Decision =
    "decision"

let Response =
    "response"

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
            printfn "%A Method: %s" name s.Method
            return (), s })

(* Core *)

module HttpCore =

    [<Literal>]
    let Name =
        "http.core"

    (* Decisions *)

    let private decision name =
        Specification.Decision.create [ Name; Decision; name ]

    let private serviceAvailable =
        decision "service.available" (fun configuration ->
            match configuration.ServiceAvailable with
            | None ->
                Literal Right
            | Some available ->
                Function (fun state ->
                    async {
                        return match available with
                               | true -> Right, state
                               | _ -> Left, state }))

    (* Responses *)

    let private response name =
        response [ Name; Response; name ]

    (* Composition *)

    let httpCore =
        serviceAvailable (response "service.unavailable") (response "ok")

    (* Module *)

    let exports =
        { Metadata =
            { Name = Name
              Description = None }
          Requirements =
            { Required = set []
              Preconditions = [] }
          Operations =
            [ Specification.Prepend (fun _ -> httpCore) ] }

(* Options *)

module HttpOptions =

    [<Literal>]
    let Name =
        "http.options"

    (* Decisions *)

    let private decision name =
        Specification.Decision.create [ Name; Decision; name ]

    let private methodOptions =
        decision "method.options" (fun (_: HttpConfiguration) ->
            Function (fun state ->
                async {
                    return match state.Method with
                           | m when m = "options" -> Right, state
                           | _ -> Left, state }))

    (* Responses *)

    let private response name =
        response [ Name; Response; name ]

    (* Composition *)

    let httpOptions =
        fun x -> methodOptions x (response "options")

    (* Module *)

    let exports =
        { Metadata =
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ HttpCore.Name ]
              Preconditions = [] }
          Operations =
            [ Specification.Splice ([ HttpCore.Name; Decision; "service.available" ], Right, httpOptions) ] }

(* Main *)

[<EntryPoint>]
let main _ =

    let composition = Module.compose (set [ HttpOptions.exports; HttpCore.exports ])

    printfn "composition:\n%A\n" composition

    let _ = System.Console.ReadLine ()

    0
