
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


open System
open System.IO
open Informedica.PICE.Lib

fsi.AddPrinter<DateTime> (fun dt -> dt.ToString("dd-MM-yyyy"))

[<Literal>]
let cachePath = __SOURCE_DIRECTORY__ +  "./../../../mrdm/data.cache"
[<Literal>]
let exportPath = __SOURCE_DIRECTORY__ +  "./../../../mrdm/Export_PICE.xlsx"

File.exists exportPath


let pats, msgs =
    Parsing.parseMRDM exportPath cachePath
    |> function
    | Ok pats ->
        pats
    | Error errs ->
        errs
        |> String.concat "\n"
        |> failwith


pats
|> Seq.length



type Validation =
    | AgeInYears of float
    | LengthCm of float
    | WeightKg of float


module Validator =

    let validate v =
    match v with
    | AgeInYears a -> a >= 0. && a <= 120.
    | LengthCm l   -> l >= 30. && l <= 300.
    | WeightKg w   -> w >= 0.2 && w <= 500.
    |> fun b ->
        if b |> not then
            $"{v} is not valid"
            |> printfn "%s"
        b


pats
|> Array.filter (fun pat ->
    let ages =
        pat.HospitalAdmissions
        |> List.collect (fun ha ->
            ha.PICUAdmissions
            |> List.map (fun pa ->
                match pat.BirthDate, pa.AdmissionDate with
                | Some bd, Some dt -> (dt - bd).TotalDays |> Some
                | _ -> None
            )
            |> List.filter Option.isSome
            |> List.map (Option.get >> (fun a -> a / 356.) >> AgeInYears)
        )

    let wghts =
        [
            pat.BirthWeight
//            |> Option.map (fun v -> v / 1000.)

            yield!
                pat.HospitalAdmissions
                |> List.collect (fun ha ->
                    ha.PICUAdmissions
                    |> List.map (fun pa -> pa.AdmissionWeight)
                )

        ]
        |> List.filter Option.isSome
        |> List.map (Option.get >> WeightKg)

    let hgths =
        pat.HospitalAdmissions
        |> List.collect (fun ha ->
            ha.PICUAdmissions
            |> List.map (fun pa -> pa.AdmissionLength)
        )
        |> List.filter Option.isSome
        |> List.map (Option.get >> float >> LengthCm)

    ages @ wghts @ hgths
    |> List.exists (Validator.validate >> not)
)
|> Seq.length


