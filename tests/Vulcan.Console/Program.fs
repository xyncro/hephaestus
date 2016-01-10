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

    (* Component *)

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

    (* Component *)

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

    let configuration = { ServiceAvailable = None }
    let state = { Method = "get" }

    let components = set [ HttpOptions.exports; HttpCore.exports ]
    let create = Module.create components
    let httpModule = create |> function | Success composition -> composition
                                        | Failure f -> failwith f

    printfn "http:\n%A\n" httpModule

    let translated = Machines.Graphs.Translation.translate httpModule.Specification
    let configured = Machines.Graphs.Configuration.configure configuration translated
    let optimized = Machines.Graphs.Optimization.optimize configured

    printfn "translated:\n%A\n" translated
    printfn "configured:\n%A\n" configured
    printfn "optimized:\n%A\n" optimized

    let _ = System.Console.ReadLine ()

    0
