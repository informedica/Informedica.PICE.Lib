
#r "System.Data.Linq"
#load "../../../.paket/load/net45/main.group.fsx"

open System
open System.IO

Environment.CurrentDirectory <- Path.Combine(__SOURCE_DIRECTORY__, "../")
#r "../bin/Debug/net45/Informedica.PICE.Lib.dll"

#time

open Informedica.PICE.Lib



PICE.get () |> ignore


let pats = PICE.get ()


pats
|> List.filter (fun p ->
    let dtfrom = DateTime(2019, 1, 1)
    let dtto = DateTime(2019, 12, 31)
    p.HospitalAdmissions
    |> List.exists (fun ha ->
        ha.PICUAdmissions
        |> List.exists (fun pa ->
            let b =
                match pa.DischargeDate with
                | None -> true
                | Some dd -> dd >= dtfrom
            pa.AdmissionDate <= dtto && b
        )
    )
)
|> fun pats ->
    printfn "# PICE Rapportage 2019"
    pats
    |> List.length
    |> printfn "### Opgenomen patienten in 2019: %i"

    pats
    |> List.collect (fun p ->
        p.HospitalAdmissions
    )
    |> fun has ->
        has
        |> List.length
        |> printfn "### Aantal ziekenhuis opnames: %i"

    let pas =
        pats
        |> List.collect (fun p ->
            p.HospitalAdmissions
            |> List.collect (fun ha ->
                ha.PICUAdmissions
                |> List.map (fun pa -> p.HospitalNumber, pa)
            )
        )
        |> List.filter (fun (_, pa) ->
            pa.AdmissionDate >= DateTime(2019, 1, 1) && pa.AdmissionDate <= DateTime(2019, 12, 31)
        )

    pas
    |> fun pas ->
        pas
        |> List.length
        |> printfn "### Aantal PICU opnames in 2019: %i"

    let pas =
        pas
        |> List.filter (fun (hn, pa) ->
            pa.DischargeDate.IsSome &&
            match pats |> List.tryFind (fun p -> p.HospitalNumber = hn) with
            | Some p -> p.BirthDate >= (DateTime(2019, 1, 1).AddYears(-25))
            | None   -> false
        )
        
    pas
    |> List.length
    |> printfn "### Aantal PICU opnames met ontslag datum: %i"

    printfn "### Aantal opnames per maand:"
    [1..12]
    |> List.iter (fun m ->
        pas
        |> List.filter (fun (_, pa) ->
            pa.AdmissionDate.Month = m
        )
        |> List.length
        |> printfn "* %s: %i" (DateTime(2019, m, 1).ToString("MMMM"))
    )

    let adms =
        pats
        |> List.collect (fun p ->
            p.HospitalAdmissions
            |> List.collect (fun ha ->
                ha.PICUAdmissions
                |> List.map (fun pa -> p.HospitalNumber, pa)
            )
        )
        |> List.filter (fun (_, pa) ->
            let b =
                match pa.DischargeDate with
                | Some dt -> dt >= DateTime(2019, 1, 1)
                | None -> true
            pa.AdmissionDate <= DateTime(2019, 12, 31) &&  b
        )

    adms
    |> fun pas ->
        pas
        |> List.length
        |> printfn "### Aantal PICU opgenomen in 2019: %i"

    printfn "### Aantal opgenomen per maand:"
    [1..12]
    |> List.iter (fun m ->
        adms
        |> List.filter (fun (_, pa) ->
            pa.AdmissionDate.Month = m
        )
        |> List.length
        |> printfn "* %s: %i" (DateTime(2019, m, 1).ToString("MMMM"))
    )

    adms
    |> fun pas ->
        pas
        |> List.map (fun (_, pa) ->
            let dtto =
                match pa.DischargeDate with
                | None -> DateTime(2019, 12, 31)
                | Some dd -> dd
            (dtto - pa.AdmissionDate).Days
        )
        |> List.reduce (+)
        |> printfn "### Totaal aantal verpleegdagen: %i"


    printfn "### Aantal verpleegdagen per maand:"
    [1..12]
    |> List.iter (fun m ->
        adms
        |> List.filter (fun (_, pa) ->
            pa.AdmissionDate.Month = m
        )
        |> List.map (fun (_, pa) ->
            let dtto =
                match pa.DischargeDate with
                | None -> DateTime(2019, 12, 31)
                | Some dd -> dd
            (dtto - pa.AdmissionDate).Days
        )
        |> List.reduce (+)
        |> printfn "* %s: %i" (DateTime(2019, m, 1).ToString("MMMM"))
    )
