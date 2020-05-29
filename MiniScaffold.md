# Changing to MiniScaffold

- [ ] Get the latest Github version of your lib.

- [ ]  Create a clone in a different folder (as backup).

- [ ]  Remove all files and directories from your lib folder.

- [ ]  Run `dotnet new mini-scaffold` in your lib folder.

- [ ]  Update the `paket.dependencies` folder.

Use these additions:

```yml
    framework: auto-detect
    redirects: on
```

remove the pinned versions from:
- FSharp.Core
- All the Fake libs

Add your own lib dependencies

- [ ] Update the `paket.reference` files with you own dependencies.

- [ ] Change the .net versions.

Change to netstandard2.1, netcoreapp3.1; net472

##### Lib proj

```xml
    <TargetFrameworks>netstandard2.1;net472</TargetFrameworks>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
```

##### Test proj

```xml
    <TargetFrameworks>netcoreapp3.1</TargetFrameworks>
    <GenerateProgramFile>false</GenerateProgramFile>
```

Note that the `GenerateProgramFile<false>` is because when updating the test runners 
result in this issue: https://andrewlock.net/fixing-the-error-program-has-more-than-one-entry-point-defined-for-console-apps-containing-xunit-tests/

- [ ]  Change the `build.fsx`.

Make sure that FSharp.Core is ignored and force removal of temp AltCover folders.

```fsharp
let dotnetTest ctx =
    let excludeCoverage =
        !! testsGlob
        |> Seq.map IO.Path.GetFileNameWithoutExtension
        // ignore FSharp.Core
        |> Seq.append ["FSharp.Core"]
        |> String.concat "|"
    
    let args =
        [
            "--no-build"
            // Make sure temp folder is removed
            "/p:AltCoverForce=true"
            sprintf "/p:AltCover=%b" (not disableCodeCoverage)
            sprintf "/p:AltCoverThreshold=%d" coverageThresholdPercent
            sprintf "/p:AltCoverAssemblyExcludeFilter=%s" excludeCoverage
        ]
    DotNet.test(fun c ->

        { c with
            Configuration = configuration (ctx.Context.AllExecutingTargets)
            Common =
                c.Common
                |> DotNet.Options.withAdditionalArgs args
            }) sln

```

Disable the anayzers target

```fsharp
// Issue: https://github.com/ionide/FSharp.Analyzers.SDK/issues/22
//    ==> "FSharpAnalyzers"
```

You might also need to lower the `coverageThresholdPercent` to a lower setting.

- [ ] Update dependencies.

    dotnet tool restore
    dotnet paket update

- [ ] Fix problem in the docsTool.

```fsharp
    let openBrowser url =
        let waitForExit (proc : Process) =
            proc.WaitForExit()
            if proc.ExitCode <> 0 then printfn "opening browser failed"
```

So a `printfn` instead of `failwith`.

- [ ] Update the `.travis.yml`

`dotnet: 3.1`

- [ ] Move the content of the `RELEASE_NOTES.md` to the `CHANGELOG.md`

- [ ] Make sure that the `Assembly.info` files contain the right names and entries

- [ ] Use an opt-in approach in the `.gitignore` file.

You can check in Visual Studio in the folders view which files will be add to the repository.

- [ ] Run `/build.sh` or `build.cmd`.
