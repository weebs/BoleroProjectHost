module BoleroProjectHost.Server.FSI_Host

open System
open System.Runtime.Loader
open System.Text
open FSharp.Compiler.Interactive
open System.Text
open FSharp.Compiler.Interactive.Shell

let inline throttleFn (time: int) (f: 'a -> unit) =
    let mutable t = None
    fun args ->
        if Option.isNone t then
            let a = async { 
                do! Async.Sleep time
                f args
                t <- None;
            }
            t <- Some a
            a |> Async.StartAsTask |> ignore

module Text =
    let mutable ignoreInteractiveOutput = 
        (Environment.GetCommandLineArgs() |> Array.contains "--mute")
        || (Environment.GetEnvironmentVariable "MUTE_FSI" <> null)
    type ConsoleWriter(?callback: (char -> unit)) =
        inherit IO.TextWriter()

        let print: char -> unit =
            callback |> Option.defaultValue (fun c -> printf $"{c}")
            // ignore
        override this.Write(c: char) =
            if not ignoreInteractiveOutput then
                print c
        override this.Encoding = Encoding.Default



type Session =
    static member create (outStream: IO.TextWriter, ?errorStream: IO.TextWriter) =
        // Initialize output and input streams
        //let sbOut = new StringBuilder()
        let sbErr = new StringBuilder()
        let inStream = new IO.StringReader("")
        let outStream = 
            outStream //|> Option.defaultWith (fun () -> new IO.StringWriter(sbOut))
        let errStream =
            // errorStream |> Option.defaultValue (new Text.ConsoleWriter())
            outStream

        // Build command line arguments & start FSI session
        let argv = [|
            ""
        |]

        let allArgs =
            //Array.append argv [| "--quiet" |]
            Array.append argv  [| "--multiemit-" |] //[| "--noninteractive" |]

        let fsiConfig =
            FsiEvaluationSession.GetDefaultConfiguration()

        printfn $"Starting with args: %A{allArgs}"
        let fsiSession =
            FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream, collectible=true)

        fsiSession, (fsiConfig, outStream, sbErr)


open FSharp.Data

let transformModuleDecl (text: string) =
    let lines = 
        text.Trim().Split("\n") 
    let fileModuleDeclarationLine =
        lines |> Array.tryFindIndex (fun l -> 
        // todo: handle comments that contain an =
            // todo: namespace
            l.StartsWith "module" && l.Contains('=') = false)
    match fileModuleDeclarationLine with
    | Some index -> 
        lines.[index] <- lines.[index] + " ="
        for i in (index + 1)..(lines.Length - 1) do
            lines.[i] <- "    " + lines.[i]
    | _ -> ()
    System.String.Join("\n", lines)

module prelude =
    // open FSharp.Data.LiteralProviders
    let [<Literal>] projectSample = """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net6.0</TargetFramework>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
    <OutputType>exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="models.fs" />
    <Compile Include="host.fs" />
    <!-- <Compile Include="prelude.gen.fsx" /> -->
    <Compile Include="server.fs" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="FSharp.Compiler.Service" Version="41.0.7" />
    <PackageReference Include="FSharp.Data" Version="5.0.2" />
    <PackageReference Include="FSharp.Data.LiteralProviders" Version="1.0.3" />
    <PackageReference Include="WatsonWebsocket" Version="4.0.3" />
    <PackageReference Include="Newtonsoft.Json" />
    <Reference Include="/some/path.dll" />
    <Reference Include="project_dependency.dll" />
  </ItemGroup>
  <ItemGroup>
    <ProjectReference Include="..\fsi-interactive-host-test-proj\fsi-interactive-host-test-proj.fsproj" />
    <ProjectReference Include="../foo.fsproj" />
  </ItemGroup>
</Project>"""
    // let [<Literal>] sample = TextFile.``host.fsproj``.Text
    type ProjType = XmlProvider<projectSample>
    let writeFiles dir prelude files =
        System.IO.File.WriteAllText(IO.Path.Join(dir, "prelude.fsx"), prelude)
        System.IO.File.AppendAllText(IO.Path.Join(dir, "prelude.fsx"), "\n" + files)
        System.IO.File.WriteAllText(IO.Path.Join(dir, "start.fsx"), System.String.Join("\n", files))
    let resolvePath (projPath: string) (itemPath: string) =
         let projPath = projPath.Replace("\\", "/")
         let itemPath = itemPath.Replace("\\", "/")
         if not <| itemPath.StartsWith "/" then
             let projDir = IO.Path.GetDirectoryName projPath
             let itemPath = IO.Path.Join(projDir, itemPath) |> IO.Path.GetFullPath
             itemPath
         else
             itemPath
    let resolveRefProj (projPath: string) (refProj: string) =
        if refProj.StartsWith "/" then
            refProj
        else
            let refProj = refProj.Replace("\\", "/")
            let projPath = projPath.Replace("\\", "/")
            let projDir = IO.Path.GetDirectoryName projPath
            let projDir = IO.Path.GetDirectoryName (IO.Path.Join(projDir, refProj))
            IO.Path.GetFullPath(IO.Path.Join(projDir, IO.Path.GetFileName refProj))
    let assemblyReferences projPath : string[] =
        let projText = IO.File.ReadAllText(projPath)
        let project = 
            ProjType.Load(new IO.MemoryStream(System.Text.Encoding.ASCII.GetBytes(projText)))
        [|
            for group in project.ItemGroups do
                for reference in group.References do
                    resolvePath projPath reference.Include
        |]
    let allFiles projPath : string[] =
        let projText = IO.File.ReadAllText(projPath)
        let project = 
            ProjType.Load(new IO.MemoryStream(System.Text.Encoding.ASCII.GetBytes(projText)))

        [| for group in project.ItemGroups do
               for compile in group.Compiles do
                   resolvePath projPath compile.Include |]
    let startFilePath projPath : string =
        allFiles projPath |> Array.last
    let rec allFilesSolution projPath : string[] =
        let projText = IO.File.ReadAllText(projPath)
        let project = 
            ProjType.Load(new IO.MemoryStream(System.Text.Encoding.ASCII.GetBytes(projText)))
        let solutionSourceFiles = [|
            for group in project.ItemGroups do
                for projRef in group.ProjectReferences do
                    let projRefPath = resolveRefProj projPath projRef.Include
                    yield! allFilesSolution projRefPath
        |]
        [| yield! solutionSourceFiles
           for group in project.ItemGroups do
               for compile in group.Compiles do
                   resolvePath projPath compile.Include |]
        
    let rec referencesForSolution (projPath: string) : string[] =
        let projText = IO.File.ReadAllText(projPath)
        let project = 
            ProjType.Load(new IO.MemoryStream(System.Text.Encoding.ASCII.GetBytes(projText)))
        let solutionReferences = [|
            for group in project.ItemGroups do
                for projRef in group.ProjectReferences do
                    yield! referencesForSolution (resolveRefProj projPath projRef.Include)
        |]
        [|
            yield! solutionReferences
            for group in project.ItemGroups do
                for package in group.PackageReferences do
                    match package.Version with
                    // todo: PackageReference Update ?
                    | Some version -> yield $"#r \"nuget: {package.Include}, {version}\""
                    | None -> yield $"#r \"nuget: {package.Include}\""
                for ref in group.References do
                    let reference_include : string = ref.Include |> resolvePath projPath
                    // printfn $"System.Reflection.Assembly.Load(System.IO.File.ReadAllBytes(\"{reference_include}\"))\""
                    yield $"""#r "{reference_include.Replace("\\", "/")}" """
                    
        |]
        
        
    let getImportsAndStartup (projPath: string) : string * list<string> * string =
        // console.log $"%A{Environment.GetCommandLineArgs()}"
        let projDir = IO.Path.GetDirectoryName projPath
        let projText = IO.File.ReadAllText(projPath)
        let project = 
            ProjType.Load(new IO.MemoryStream(System.Text.Encoding.ASCII.GetBytes(projText)))
        let includes = referencesForSolution projPath
        let prelude = System.String.Join("\n", includes)
        let compileIncludes = project.ItemGroups |> Array.collect (fun group -> group.Compiles)
        let files = [
            let files = allFilesSolution projPath
            yield! files |> Array.take (files.Length - 1)
        ]
        let start =
            if compileIncludes.Length > 0 then
                transformModuleDecl (IO.File.ReadAllText(IO.Path.Join(IO.Path.GetDirectoryName(projPath), (Array.last compileIncludes).Include)))
            else
                ""
        let project_fsx_parts = prelude, files, start
        project_fsx_parts

type console =
    static member inline log _ = ()

type System.Environment with
    static member CurrentLine = Environment.StackTrace.Split(char '\n').[1]
type time =
    static member milli =
        string DateTime.Now.Second + " " + string DateTime.Now.Millisecond
let printfn (s: string) =
    if not (Text.ignoreInteractiveOutput) then
        Console.WriteLine $"{time.milli}: {s}"

let sessionLock = obj()

let setupWatch (file_path: string) callback =
    let watch = new IO.FileSystemWatcher()
    watch.Path <- IO.Path.GetDirectoryName(file_path)
    console.log $"Adding file watch for {watch.Path}"
    watch.NotifyFilter <- IO.NotifyFilters.LastWrite
    watch.Filter <- "*.*"
    let mutable refreshTask = None
    watch.Changed.AddHandler(fun sender event ->
        console.log $"file changed: {event.FullPath}"
        callback event.FullPath
    )
    watch.EnableRaisingEvents <- true
    watch
let setupAsyncFileWatcher (projectPath: string) callback =
    Threading.Thread(Threading.ThreadStart(fun () ->
    // async {
        let files =
            [| yield! prelude.allFilesSolution projectPath; projectPath |]
            |> Array.filter (fun file -> file.EndsWith(".fs") || file = projectPath)
            |> Array.append (prelude.assemblyReferences projectPath)
        let getFiles () =
            files
            // +endswith
            // ^endswith
        let createCache files =
            let mutable fileContents = Map.ofArray [|
                for file in files do
                    file, (IO.File.ReadAllText(file), IO.File.GetLastWriteTime(file))
            |]
            let mutable lastChecked = Map.ofArray [|
                for file in files do yield file, DateTime.Now
            |]
            fileContents, lastChecked
        let mutable fileContents, lastRead = getFiles () |> createCache
        while true do
            try
                Threading.Thread.Sleep 200
                let mutable index = 0
                let mutable fileChanged = false
                let files = getFiles ()
                while index < files.Length && not fileChanged do
                // for filePath in files () do
                    // todo: Change to <> comparison
                    let filePath = files[index]
                    index <- index + 1
                    if lastRead.[filePath] < IO.File.GetLastWriteTime(filePath) then
                        let newContents = IO.File.ReadAllText(filePath)
                        if newContents <> fst fileContents.[filePath] then

                            fileChanged <- true

                            if filePath = projectPath then
                                let files = getFiles ()
                                    // IO.Directory.GetFiles(IO.Path.GetDirectoryName(projectPath))
                                    // |> Array.filter (fun file -> file.EndsWith(".fs") || file = projectPath)
                                let (files, _lastRead) = createCache files
                                fileContents <- files
                                lastRead <- _lastRead
                                callback filePath
                            else
                                fileContents <- fileContents.Add (filePath, (newContents, IO.File.GetLastWriteTime(filePath)))
                                console.log $"==================== {filePath} changed ========================="
                                callback filePath
            with ex ->
                printfn $"{ex}"
    // } |> Async.Start
    )).Start()

let printStuffs (_fsi: Shell.FsiEvaluationSession) =
    let letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for c in letters do
        let completions = _fsi.GetCompletions(string c) |> Array.ofSeq
        console.log <| sprintf "%A" completions
    for c in letters.ToLower() do
        let completions = _fsi.GetCompletions(string c) |> Array.ofSeq
        console.log <| sprintf "%A" completions
    let completions = _fsi.GetCompletions("System.") |> Array.ofSeq
    console.log <| sprintf "%A" completions
    // let completions = _fsi.GetCompletions("b") |> Array.ofSeq
    // console.log "%A" completions
    let boundValues = _fsi.GetBoundValues()
    for value in boundValues do
        console.log $"==== {value.Name}:  {value.Value.ReflectionType.FullName} ====="
let inline log any = printfn $"{any}"
let inline log2 (any, any1) = printfn $"{any}, {any1}"
type Project(projectPath: string, ?onRestart: Shell.FsiEvaluationSession -> unit, ?initAction: Shell.FsiEvaluationSession -> unit, ?writer: char -> unit, ?muteFsi: bool) as this =

    let mutable onRestart = onRestart |> Option.defaultValue ignore
    do if muteFsi = Some true then Text.ignoreInteractiveOutput <- true
    // TODO: Create compiler arguments
    // https://blog.nojaf.com/2023/02/02/my-fsharp-compiler-scripts/
    let writer = writer |> Option.defaultValue Console.Write
    let initAction = initAction |> Option.defaultValue ignore
    let projectPath = IO.Path.GetFullPath projectPath
    do 
        try Environment.CurrentDirectory <- IO.Path.GetDirectoryName(projectPath)
        with error -> log error
    let mutable _imports = ""
    let write_prelude (fsi: Shell.FsiEvaluationSession) =
        let (imports, files, start_fsx) = prelude.getImportsAndStartup projectPath
        _imports <- imports
        printfn $"{imports}"
        printfn "Writing prelude"
        for import in imports.Split "\n" do
            fsi.EvalInteractionNonThrowing(import)
            |> sprintf "%A" |> printfn
        printfn "Wrote prelude"
    let call_init_action (fsi: Shell.FsiEvaluationSession) =
        printfn "Calling initAction"
        initAction fsi
        printfn "FSI session init complete"
    let run_script (fsi: Shell.FsiEvaluationSession) module_start =
        let module_start_text = IO.File.ReadAllText(module_start)
        let filename =
            IO.Path.Join(IO.Path.GetDirectoryName(module_start), IO.Path.GetFileNameWithoutExtension(module_start) + ".fsiproject.fs")
        let module_suffix = IO.Path.GetFileName(module_start).Replace(".", "_")
        let replaceFileHeadersWithoutModuleOrNamespace = true
        // todo: Handle files that don't start with a namespace or module in multi-file projects
        if not (module_start_text.Split('\n') |> Array.exists (fun line -> line.Trim().ToLower().StartsWith("module") ||
                                                                                  line.Trim().ToLower().StartsWith("namespace")))
                && not (IO.File.Exists(filename))
           && replaceFileHeadersWithoutModuleOrNamespace then
            try
                IO.File.WriteAllText(filename, $"module FSI_{module_suffix}\n\n" + module_start_text)
                printfn $"""%A{fsi.EvalScriptNonThrowing (filename)}"""
                IO.File.Delete(filename)
            with e -> printfn $"{e}"
        else
            printfn $"%A{fsi.EvalScriptNonThrowing module_start}"
    let load_project_files (fsi: Shell.FsiEvaluationSession) =
        let (imports, files, start_fsx) = prelude.getImportsAndStartup projectPath
        printfn $"Loading project files: %A{files}"
        files |> List.iter (run_script fsi) 
        printfn "Finished loading project files"
    let mutable _nextSession, _nextConfig = Session.create (new Text.ConsoleWriter(writer))
    let mutable sw = Unchecked.defaultof<_>
    let mutable fileChangedResults = []
    do
        write_prelude _nextSession
        call_init_action _nextSession

        let projName = IO.Path.GetFileNameWithoutExtension(projectPath)
        let dll_path = IO.Path.Join(IO.Path.GetDirectoryName(projectPath), $"bin/Debug/net6.0/{projName}.dll")

        load_project_files _nextSession
        printfn "Writing start script"
        let (imports, files, start_fsx) = prelude.getImportsAndStartup projectPath
        run_script _nextSession (prelude.startFilePath projectPath)
        
        printfn "Wrote start script"
        async {
            do! Async.Sleep(2 * 1000)
            let mutable _fsi, _fsiConfig = _nextSession, _nextConfig
            
            let fileChanged (filePath: string) =
                let (_, files, _) = prelude.getImportsAndStartup projectPath
                let files = files |> List.map IO.Path.GetFullPath
                if filePath.EndsWith ".fsx" then
                    console.log $"File changed {filePath}"
                else
                    printfn "Cleaning previous session"
                    let (fsi, fsiConfig) =
                        // todo: Fix multiple sessions for changing nuget packages etc
                        if false && filePath = projectPath then
                            printfn "Project changed, reloading packages and .NET references"
                            let temp = Session.create (new Text.ConsoleWriter(writer))
                            write_prelude (fst temp)
                            call_init_action (fst temp)
                            load_project_files (fst temp)
                            temp
                        else
                            _nextSession, _nextConfig

                    printfn "looking for index"
                    let filePath = IO.Path.GetFullPath(filePath)
                    printfn $"{filePath}"
                    printfn $"{files}"
                    match files |> List.tryFindIndex (fun file -> file = filePath) with
                    | None ->
                        if filePath.EndsWith ".dll" then
                            // fsi.EvalInteractionNonThrowing $"System.Reflection.Assembly.Load(System.IO.File.ReadAllBytes(\"{filePath}\"))"
                            fsi.EvalInteractionNonThrowing $"#r \"{filePath}\""
                            |> sprintf "%A" |> printfn
                    | Some index ->
                        printfn "found index"
                        printfn $"{files.[index]}"
                        for i in index..(files.Length - 1) do
                            printfn $"Writing {files.[i]}"
                            match fsi.EvalScriptNonThrowing(files.[i]) with
                            | (Choice1Of2 unit, _) -> ()
                            | Choice2Of2 ex, diagnostics ->
                                printfn $"{ex}"
                                for d in diagnostics do
                                    printfn $"%A{d}"
                            |> ignore
                    
                    call_init_action fsi

                    let projectFiles = prelude.allFiles projectPath
                    let module_start = projectFiles[projectFiles.Length - 1]
                    run_script fsi module_start
                    
                    printfn "Started!"
                    onRestart fsi
                ()
            sw <- setupAsyncFileWatcher projectPath (throttleFn 100 fileChanged)
        } |> Async.Start

    member this.OnRestart with set value = onRestart <- value
    member this.ConsoleWaitLoopStart() : unit = 
        let rec loop (input: string) =
            match input.Trim() with
            | "q" -> exit 0
            | "mute-fsi" -> 
                Text.ignoreInteractiveOutput <- true
            | "unmute-fsi" -> 
                Text.ignoreInteractiveOutput <- false
            | _ -> ()
            Console.WriteLine "Type q<Enter> to exit...";
            loop <| Console.ReadLine()
        Console.WriteLine "Type q<Enter> to exit...";
        loop <| Console.ReadLine()
    member this.Session = _nextSession
    // interface IDisposable with
    //     member _.Dispose() = (_nextSession :> IDisposable).Dispose()