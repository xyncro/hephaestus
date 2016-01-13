(* Overview

   A very simple standard build script using Fake and Paket. The script is
   used to build the simple Xyncro projects such as Aether etc. which share
   a common conventional layout, etc.
   
   The build file is included via a Git submodule under the /build path
   within a repository, and is then called from a project specific build
   command or batch file which is expected to have restored any packages
   required to begin the build process (generally using command line
   Paket tooling).

   The build files MUST pass in the "product" environment variable. *)
open System
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#I "../../../../packages/build/FAKE/tools"
#r "../../../../packages/build/FAKE/tools/FakeLib.dll"
Environment.CurrentDirectory  <- "../../../../"
open Fake
open Fake.Testing

(* Solution

   Build script parameterised by product, which will look for a matching
   solution file in the root of the build current working directory. *)

let solutionFile =
    sprintf "%s.sln" (environVar "product")

(* Paths

   Paths for commonly used sources and destinations. The current simple build
   uses a temporary directory for build artifacts, usually generated NuGet
   packages in a standard build. *)

let tempDir =
    "temp"

(* Clean

   Clean all standard paths before running any further build steps,
   preventing the possible duplication of generated build artifacts,
   etc. *)

Target "Clean" <| fun _ ->
    CleanDirs [ tempDir ]

(* Build

   A standard Release build of the main solution file (parameterised), using
   default properties for optimization, debug symbols, etc. *)

let private properties =
    [ "Optimize",      environVarOrDefault "Build.Optimize"      "True"
      "DebugSymbols",  environVarOrDefault "Build.DebugSymbols"  "True"
      "Configuration", environVarOrDefault "Build.Configuration" "Release" ]

let private targets =
    [ "Build" ]

Target "Build" <| fun _ ->
    build (fun x ->
        { x with
            Properties = properties
            Targets = targets
            Verbosity = Some Quiet }) solutionFile

(* Test

   Convention based testing based on release binaries and using an
   assumption that the testing tools used will be controlled by a top level
   of XUnit 2 tests. XUnit 2 integration handles pushing of results etc. to
   relevant CI systems when running in an appropriate environment. *)

Target "Test" <| fun _ ->
    !! "tests/**/bin/Release/*.Tests.dll"
    |> xUnit2 (fun p ->
        { p with
            HtmlOutputPath = None
            Parallel = All })

(* Package

   Paket based packaging and publishing (publishing - push - only being
   called when the nugetkey environment variable is present). Packaging is
   based on the documented Paket conventions of template files, etc. *)

Target "Pack" <| fun _ ->
    Paket.Pack (fun p ->
        { p with
            OutputPath = tempDir })

Target "Push" <| fun _ ->
    Paket.Push (fun p ->
        { p with
            WorkingDir = tempDir })

(* Dependencies

   Dependency set for a standard build, with a simple clean, build, test,
   publish workflow. *)

Target "Default" DoNothing

"Clean"
    ==> "Build"
    ==> "Test"
    ==> "Pack"
    =?> ("Push", Option.isSome (environVarOrNone "nugetkey"))
    ==> "Default"

RunTargetOrDefault "Default"
