module BoleroProjectHost.Client.Main

open System.Diagnostics
open Elmish
open Bolero
open Bolero.Html
open Thoth.Json.Net

type Model =
    {
        text: string
        count: int
    }

let statePath = "state.json"
let init _ =
    try
        Decode.Auto.unsafeFromString<Model> (System.IO.File.ReadAllText statePath)
    with error ->
        printfn $"{error}"
        {
            text = ""
            count = 0
        }

type Message =
    | Clicked
    | TextInput of string
    | Debug

let update message model =
    match message with
    | Clicked -> { model with count = model.count + 1}
    | TextInput s -> { model with text = s }
    | Debug ->
        if Debugger.IsAttached then
            Debugger.Break()
        model

let view model dispatch =
    div {
        p { button { on.click (fun _ -> dispatch Debug); text "Debug" } }
        p {
            on.click (fun _ -> dispatch Clicked)
            $"Times clicked: {model.count}, Text Length: {model.text.Length}"
        }
        p {
            input {
                attr.``type`` "text"
                attr.value model.text
                on.input (fun e -> dispatch (TextInput (string e.Value)))
            }
        }
    }

type MyApp() =
    
    inherit ProgramComponent<Model, Message>()
    override this.Program =
        Program.mkSimple init update view
        |> Program.withTrace (fun msg model subIds ->
            try System.IO.File.WriteAllText (statePath, Encode.Auto.toString model)
            with error -> printfn $"{error}"
        )
