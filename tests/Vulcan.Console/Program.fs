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

    let serviceAvailable =
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

    let httpCoreModule =
        { Metadata =
            { Name = "http.core"
              Description = None }
          Requirements =
            { Required = []
              Preconditions = [] }
          Operations =
            [ Specification.Prepend (fun _ -> httpCore) ] }

module HttpOptions =

    (* Decisions *)

    let methodOptions =
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

    let httpOptionsModule =
        { Metadata =
            { Name = "http.options"
              Description = None }
          Requirements =
            { Required = [ "http.core" ]
              Preconditions = [] } 
          Operations =
            [ Specification.Splice ("service available?", Right, httpOptions) ] }

(* Operations *)

let private prepend f specification =
    f specification

let private splice name value f specification =
    specification

let apply s =
    function | Specification.Prepend (f) -> prepend f s
             | Specification.Splice (n, v, f) -> splice n v f s





(* Main *)

[<EntryPoint>]
let main _ =

    let translated = Translation.translate HttpCore.httpCore
    let configured = Configuration.configure { ServiceAvailable = Some true } translated
    
    let operations = HttpCore.httpCoreModule.Operations @ HttpOptions.httpOptionsModule.Operations
    let composed = List.fold apply (Specification.Terminal.create "" (fun s -> async { return (), s })) operations

    printfn "translated:\n%A\n" translated
    printfn "configured:\n%A\n" configured
    printfn "composed:\n%A\n" composed

    let _ = System.Console.ReadLine ()

    0
