
#r "System.Data.Linq"
#load "../../../.paket/load/net472/main.group.fsx"

#load "../StartUp.fs"
#load "../NullCheck.fs"
#load "../String.fs"
#load "../StringBuilder.fs"
#load "../File.fs"
#load "../Cache.fs"
#load "../Markdown.fs"
#load "../Result.fs"
#load "../Utils.fs"
#load "../Types.fs"
#load "../Click.fs"
#load "../MRDM.fs"
#load "../PIM.fs"
#load "../PRISM.fs"
#load "../Patient.fs"
#load "../Validation.fs"
#load "../Parsing.fs"
#load "../Statistics.fs"
#load "../Export.fs"


#time


open System
open Informedica.PICE.Lib


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

fsi.AddPrinter<DateTime>(sprintf "%A")


let pats = Parsing.parseMRDM ()


pats
|> Result.valueOrDefault (fun _ -> [||])
|> Array.toList
|> List.collect (fun p ->
    p.HospitalAdmissions
    |> List.collect (fun ha ->
        ha.PICUAdmissions
        |> List.map (fun pa -> p, pa)
    )
)
|> List.filter (fun (_, pa) ->
    pa.PIM.RiskDiagnosis
    |> List.exists (fun d -> d = Types.PIM.CardiacArrestInHospital || d = Types.PIM.CardiacArrestPreHospital)
)
|> List.iter (fun (p, pa) ->
    let cpr =
        pa.PIM.RiskDiagnosis
        |> List.filter (fun d -> d = Types.PIM.CardiacArrestInHospital || d = Types.PIM.CardiacArrestPreHospital)
        |> List.map (sprintf "%A")
        |> String.concat " "
    printfn "%s, %s, %s, %s" ((pa.AdmissionDate |> Option.get).ToString("yyyy/MM/dd")) p.HospitalNumber cpr pa.DischargeReason
)
