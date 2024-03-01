module BoleroProjectHost.Server.ProjectView

open System
open Bolero
open FSharp.Compiler.Interactive.Shell
open FSharp.Data.LiteralProviders


type Model = { component: Type option; project: FSI_Host.Project }
type Message = AppUpdate of Type option
open Elmish
open Bolero.Html

let fsi =
    FSI_Host.Project(TextFile.``..``.``BoleroProjectHost.Client``.``BoleroProjectHost.Client.fsproj``.Path)
    
#if DEBUG
type Project() =
    inherit ProgramComponent<Model, Message>()
    
    let initModel js =
        { component = None; project = fsi }, Cmd.ofEffect <| fun dispatch ->
            let find_app (fsi: FsiEvaluationSession) =
                fsi.DynamicAssemblies
                |> Seq.map _.DefinedTypes
                |> Seq.collect id
                |> Seq.sortByDescending _.FullName
                |> Seq.tryFind (fun t -> t.FullName.EndsWith "BoleroProjectHost.Client.Main+MyApp")
                |> Option.map _.UnderlyingSystemType
            dispatch <| AppUpdate (find_app fsi.Session)
            fsi.OnRestart <- fun fsi ->
                dispatch <| AppUpdate (find_app fsi)
                
            
    let update msg model =
        match msg with
        | AppUpdate app -> { model with component = app }, []
    
    let view model dispatch =
        div {
            // comp<MyApp>
            cond model.component <| function
                | Some t ->
                    Bolero.Node(fun c b i ->
                        b.OpenComponent(i, t)
                        b.CloseComponent()
                        i + 1
                    )
                | None ->
                    code { text "Loading..." }
        }

    override this.Program =
        Program.mkProgram
            (fun (a: ProgramComponent<_,_>) ->
                let r = a.JSRuntime
                initModel r
            )
            update view
#endif
