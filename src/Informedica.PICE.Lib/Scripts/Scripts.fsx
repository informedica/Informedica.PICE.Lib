
#r "../../../libs/Informedica.PimPrism.Lib.dll"
#r "nuget: ExcelProvider"
#r "nuget: Newtonsoft.Json"
#r "nuget: Markdig"
#r "nuget: Microsoft.Data.SqlClient"

#load "../StartUp.fs"
#load "../NullCheck.fs"
#load "../String.fs"
#load "../StringBuilder.fs"
#load "../File.fs"
#load "../Cache.fs"
#load "../Markdown.fs"
#load "../Result.fs"
#load "../Types.fs"
#load "../Utils.fs"
#load "../Database.fs"
#load "../MRDM.fs"
#load "../Options.fs"
#load "../Patient.fs"
#load "../Validation.fs"
#load "../Parsing.fs"

open Informedica.PICE.Lib

let pats, msgs =
    Parsing.parseMRDM ()
    |> function
    | Ok pats ->
        pats

    | Error errs ->
        errs
        |> String.concat "\n"
        |> failwith


