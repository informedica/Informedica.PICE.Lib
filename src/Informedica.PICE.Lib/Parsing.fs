namespace Informedica.PICE.Lib



module Parsing =

    open System
    open System.Diagnostics
    open System.Globalization

    open Informedica.PimPrism.Lib
    open Types
    open MRDM
    open Patient

    open Result.Operators

    let isNullOrWhiteSpace (s : String) = s |> String.IsNullOrWhiteSpace
    let notNullOrWhiteSpace = isNullOrWhiteSpace >> not


    module Parsers =

        let parseInt (s : string) =
            match Int32.TryParse(s) with
            | true,  x -> Some x
            | false, _ -> None


        let parseFloat (s : string) =
            match Double.TryParse(s,NumberStyles.Any,  CultureInfo.InvariantCulture) with
            | true, x  -> Some x
            | false, _ -> None


        let parseDate s =
            try
                DateTime.Parse(s, DateTimeFormatInfo.InvariantInfo)
            with
            | _ ->
                sprintf "could not parse date %s" s
                |> failwith


        let parseDateOpt (s : string) =
            if s |> String.IsNullOrWhiteSpace then None
            else
                s
                |> parseDate
                |> Some


        let mapRiscDiagnosis d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs =
            match d with
            | s when s = "0" -> []
            | s when s = "1" -> [ PIM.Croup ]
            | s when s = "2" -> [ PIM.ObstructiveSleepApnea ]
            | s when s = "3" -> [ PIM.Bronchiolitis ]
            | s when s = "4" -> [ PIM.Asthma ]
            | s when s = "5" -> [ PIM.LiverFailure ]
            | s when s = "6" -> [ PIM.SeizureDisorder ]
            | s when s = "7" -> [ PIM.DiabeticKetoacidosis ]
            | s when s = "8" -> [ PIM.LiverFailure ]
            | s when s = "9" -> [ PIM.NecrotizingEnterocolitis ]
            | s when s = "10" -> [ PIM.DiabeticKetoacidosis ]
//            | s when s = "11" -> [ PIM.CardiomyopathyOrMyocarditis ]
            | _ -> []
            |> List.append [
                if leukemia = "1" then PIM.LeukemiaorLymphoma
                if bmt = "1"      then PIM.BoneMarrowTransplant
                if cva = "1"      then PIM.CerebralHemorrhage
                if card = "1"     then PIM.CardiomyopathyOrMyocarditis
                if scid = "1"     then PIM.SevereCombinedImmuneDeficiency
                if hiv = "1"      then PIM.HIVPositive
                if neuro = "1"    then PIM.NeurodegenerativeDisorder
                if hlhs = "1"     then PIM.HypoplasticLeftHeartSyndrome
                if cprpre = "1"   then PIM.CardiacArrestPreHospital
                if cprin = "1"    then PIM.CardiacArrestInHospital
            ]


        let mapUrgency = function
            | s when s = "10" -> PIM.NotElective
            | s when s = "11" -> PIM.Elective
            | _ -> PIM.UnknownUrgency


        let mapPupils = function
            | s when s = "1" -> PIM.FixedDilated
            | s when s = "0" -> PIM.NormalPupils
            | _ -> PIM.UnknownPupils


        let mapPatientState = function
            | s when s = "0" -> Alive
            | s when s = "1" -> Dead
            | _ -> UnknownPatientState


        let mapAdmissionType = function
            | s when s = "1" -> Medical
            | s when s = "2" -> Surgery
            | s when s = "3" -> DOA
            | _ -> UnknownAdmissionType


        let mapGender = function
            | s when s = "1" -> Male
            | s when s = "2" -> Female
            | _ -> UnknownGender


        //129	Directe opname van buiten eigen UMC
        //103	Volwassen-IC /CCU
        //114	Zorgafdeling via OK
        //106	(Zorg)Afdeling (zonder extra bewaking)
        //115	SEH via OK
        //107	SEH
        //109	Recovery
        //105	Afdeling met extra bewaking (HDU/HighCare)
        //110	Kraamafdeling
        //77	Overig
        //108	OK
        //104	Longstay-ic
        //102	NICU (IC-Neonatologie)
        //99	Onbekend
        let mapAdmissionSource = function
            | s when s = "109"  -> PRISM.Recovery
            | s when s = "129"  -> PRISM.AnotherHospital
            | s when s = "115" || s = "107" -> PRISM.EmergencyUnit
            | s when s = "99"  || s = "77"  -> PRISM.UnknownAdmissionSource
            | _ -> PRISM.InHospital


        let mapLowRiskPRISM s =
            [
                "7"
                "10"
                "11"
            ]
            |> List.exists ((=) s)


        let parseDiagnose n id =
            id
            |> Options.find n
            |> function
            | None -> []
            | Some s ->
                s.Label
                |> String.replace "(" ""
                |> String.replace ")" "|"
                |> String.split "|"
                |> function
                | [g;d] -> [ { Id = id; Group = g |> String.trim |> String.toLower; Name = d |> String.trim |> String.toLower} ]
                | _     -> []


    let findOk n d =
        Options.find n d
        |> function
        | Some s -> s |> Some
        | None   -> None
        |> Result.ok


    let parseDateOpt s =
        if s |> isNullOrWhiteSpace then None |> Result.ok
        else s |> Result.tryWithOk Parsers.parseDate

    let parseBool = ((=) "1") >> Result.ok


    let parseFloat s =
        if String.IsNullOrWhiteSpace(s) then Result.ok None
        else Result.okIfNone [| sprintf "couldn't parse float %s" s |] (Parsers.parseFloat s)


    let parseInt s =
        if String.IsNullOrWhiteSpace(s) then Result.ok None
        else Result.okIfNone [| sprintf "couldn't parse int %s" s |] (Parsers.parseInt s)


    let parsePatient (hospData : MRDMHospital.Row[]) (d : MRDMPatient.Row) =
        let getHospNum (data : MRDMHospital.Row[]) =
            let errs, hn =
                data
                |> Array.filter (fun hd -> hd.patient_uri = d.patient_uri)
                |> Array.map (fun hd -> hd.``ziekenhuis-episode-upn``)
                |> Array.distinct
                |> function
                | [||]   ->
                    [| sprintf "no hospitalnumber for: %A" d |],
                    ""
                | [|hn|] -> [||], hn
                | xs ->
                    let msg =
                        xs
                        |> Array.map (sprintf "%s")
                        |> Array.append [| sprintf "multiple hospitalnumbers for %s:" d.idcode |]
                        |> String.concat "\n"

                    [| msg |] , ""
            if errs |> Array.isEmpty then hn |> Result.ok
            else errs |> Result.error

        let mapPatientState = Parsers.mapPatientState >> Result.ok

        Patient.create
        <!> Result.ok d.patient_uri
        <*> getHospNum hospData
        <*> Result.ok d.naam
        <*> Result.ok d.voornaam
        <*> parseDateOpt d.gebdat
        <*> (Parsers.mapGender >> Result.ok) d.geslacht
        <*> parseFloat d.``pat-weight-of-birth``
        <*> parseInt d.``pat-zwduur``
        <*> mapPatientState d.status
        <*> parseDateOpt d.datovl
        <*> findOk "adm-deathmodeid" d.``adm-deathmodeid``
        <*> findOk "adm-deceasedwhereid" d.``adm-deceasedwhereid``


    let parseHospAdm (hospData: MRDMHospital.Row[]) =
        let fErr msgs = msgs |> Result.Error
        let fOk ((p : Patient), msgs1) =
            hospData
            |> Array.filter (fun d -> d.patient_uri = p.Id)
            |> Array.map (fun d ->
                Patient.createHospitalAdmission
                <!> Result.ok d.``ziekenhuis-episode_uri``
                <*> Result.ok d.``ziekenhuis-episode-upn``
                <*> parseDateOpt d.``adm-hosp-admdate``
                <*> (findOk "herk-tran-door" d.``herk-tran-door``)
                <*> (findOk "adm-transport-adm" d.``adm-transport-adm``)
                <*> (findOk "adm-desthospunitid" d.``adm-desthospunitid``)
                <*> parseDateOpt d.``adm-hosp-disdate``
            )
            |> Result.foldOk
            |> function
            | Result.Ok (adms, msgs2) ->
                msgs1 |> Array.append msgs2 |> Result.okMsg ({ p with HospitalAdmissions = adms |> Array.toList })
            | Result.Error msgs2  -> msgs1 |> Array.append msgs2 |> Result.error

        Result.either fOk fErr


    let addPICUAdmissions (admissions : Result<PICUAdmission[] * string[], _>)
                          (diagnoses : {| pi : string; dn : string |}[]) =

        let calcPRISM bdt adt prism =
            match prism with
            | None -> None
            | Some prism ->
                {
                    prism with
                        Age = bdt
                }
                |> fun prism ->
                    match adt with
                    | Some dt ->
                        prism
                        |> PRISM.mapPRISMtoInput
                        |> PRISM.calculate dt
                        |> PRISM.mapInputToPRISM prism
                        |> Some
                    | None    -> prism |> Some

        let fErr msgs = msgs |> Result.Error
        let fOk ((p : Patient), msgs1) =
            match admissions with
            | Result.Ok (xs , msgs2) ->
                let p =
                    {
                        p with
                            HospitalAdmissions =
                                p.HospitalAdmissions
                                |> List.map (fun ha ->
                                    { ha with
                                        PICUAdmissions =
                                            xs
                                            |> Array.filter (fun pa ->
                                                ha.Id = pa.HospitalAdmissionId
                                            )
                                            |> Array.map (fun pa ->
                                                let diagnoses =
                                                    diagnoses
                                                    |> Array.filter (fun d ->
                                                        d.pi = pa.Id
                                                    )
                                                { pa with
                                                    HospitalNumber = p.HospitalNumber
                                                    PRISM24 =
                                                        pa.PRISM24
                                                        |> calcPRISM p.BirthDate pa.AdmissionDate
                                                    PRISM12 =
                                                        pa.PRISM12
                                                        |> calcPRISM p.BirthDate pa.AdmissionDate
                                                    PRISM4 =
                                                        pa.PRISM4
                                                        |> calcPRISM p.BirthDate pa.AdmissionDate
                                                    Diagnoses =
                                                        diagnoses
                                                        |> Array.toList
                                                        |> List.collect (fun d ->
                                                            d.dn
                                                            |> Parsers.parseDiagnose "bijkomende-diagnose"
                                                        )
                                                }
                                            )
                                            |> Array.toList
                                    }
                                )
                    }

                Result.okMsg p (msgs1 |> Array.append msgs2)
            | Result.Error msgs2 -> Result.okMsg p (msgs1 |> Array.append msgs2)

        Result.either fOk fErr


    let filterDuplicateOrMore (results : Result<Patient * string[], string[]> array) =
        results
        |> Result.foldOk
        |> function
        | Result.Error msgs       -> msgs |> Result.error
        | Result.Ok (pats, msgs1) ->
            printfn "start detecting duplicates"
            // Detect records with the same hospital number
            let pats, msgs2 =
                let distPats =
                    pats
                    |> Array.distinctBy (fun p -> p.HospitalNumber)

                let msgs =
                    pats
                    |> Array.filter (fun p ->
                        distPats
                        |> Array.exists ((=) p) |> not
                    )
                    |> Array.mapi (fun i p -> sprintf "%i. dupuplicate patient %s\n" i p.HospitalNumber)

                distPats, msgs

            Result.okMsg pats (msgs1 |> Array.append msgs2)


    let parsePICUAdmissions (picuData : MRDMPicu.Row[]) =
        let mapAdmType = Parsers.mapAdmissionType >> Result.ok
        let mapUrgency = Parsers.mapUrgency >> Result.ok
        let mapRisk d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs =
            Parsers.mapRiscDiagnosis d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs
            |> Result.ok
        let mapPupils  = Parsers.mapPupils >> Result.ok
        let mapAdmissionSource = Parsers.mapAdmissionSource >> Result.ok
        let mapLowRiskPRISM = Parsers.mapLowRiskPRISM >> Result.ok
        let getDiagn n s  =
            Parsers.parseDiagnose n s
            |> Result.ok
        let mapReadm s =
            s = "13"
            |> Result.ok

        let prism (d: MRDM.MRDMPicu.Row) =
            Patient.createPRISM
            <!> parseFloat d.``sbp-0``
            <*> parseFloat d.``t-min12``
            <*> parseFloat d.``t-max12``
            <*> parseInt d.``adm-emv``
            <*> parseInt d.``hr-max12``
            <*> parseInt d.admpupils
            <*> parseFloat d.``ph-min12``
            <*> parseFloat d.``ph-max12``
            <*> parseFloat d.``bicarbonate-min12``
            <*> parseFloat d.``bicarbonate-max12``
            <*> parseFloat d.``paco2-max12``
            <*> parseFloat d.``pao2-0``
            <*> parseFloat d.``glucose-max12``
            <*> parseFloat d.``k-max12``
            <*> parseFloat d.``creatinine-max12``
            <*> parseFloat d.``ureum-max12``
            <*> parseFloat d.``leuco-min12``
            <*> parseFloat d.``pt-max12``
            <*> parseFloat d.``ptt-max12``
            <*> parseFloat d.``thrombo-min12``
            <*> mapAdmissionSource d.``adm-sourceunitid``
            <*> parseBool d.contrean12
            <*> parseBool d.cancer
            <*> mapLowRiskPRISM d.``risicodiag-hoofd``

        let pim (d : MRDM.MRDMPicu.Row) =
            let cardiacSurg g =
                let d1 =
                    d.diagnose1
                    |> Parsers.parseDiagnose "diagnose1"
                    |> List.exists (fun d -> d.Group = g)
                let d2 =
                    d.diagnose2
                    |> Parsers.parseDiagnose "diagnose2"
                    |> List.exists (fun d -> d.Group = g)
                d1 || d2

            Patient.createPIM
            <!> mapUrgency d.``adm-typeid-urgentie``
            <*> parseBool d.recovery
            <*> parseBool d.bypass
            <*> Result.ok (cardiacSurg "hartchirurgie")
            <*> (mapRisk d.``risicodiag-hoofd``
                         d.``cprprehosp-riskpim``
                         d.``cprprepicu-riskpim``
                         d.``leukemie-riskpim``
                         d.``bmtrecipient-riskpim``
                         d.``sponthersenbl-riskpim``
                         d.``cardmyopath-riskpim``
                         d.``scid-riskpim``
                         d.``hiv-riskpim``
                         d.``neurodegen-riskpim``
                         d.``hypoplast-riskpim``)
            <*> parseBool d.ventilated
            <*> mapPupils d.admpupils
            <*> parseFloat d.``pao2-0``
            <*> parseFloat d.``fio2-0``
            <*> parseFloat d.``be-0``
            <*> parseFloat d.``sbp-0``

        picuData
        |> Array.map (fun d ->
            let find n c =
                if c |> isNullOrWhiteSpace then Result.ok None
                else
                    match Options.find n c with
                    | Some d -> d |> Some |> Result.ok
                    | None ->
                        [| sprintf "couldn't find code %s with name %s" c n |]
                        |> Result.error

            Patient.createPICUAdmission
            <!> Result.ok d.``picu-episode_uri``
            <*> Result.ok d.``ziekenhuis-episode_uri``
            <*> Result.ok d.``adm-ic-id``
            <*> Result.ok "" //d.``ziekenhuis-episode-upn``
            <*> mapReadm d.``adm-readmtypeid``
            <*> parseDateOpt d.``adm-ic-admdate``
            <*> parseDateOpt d.``adm-ic-disdate``
            <*> find "adm-disreasonid" d.``adm-disreasonid``
            <*> mapAdmType d.``adm-typeid-soort``
            <*> find "adm-indication" d.``adm-indication``
            <*> find "adm-refspecialism" d.``adm-refspecialism``
            <*> getDiagn "diagnose1" d.diagnose1
            <*> getDiagn "diagnose2" d.diagnose2
            <*> parseFloat d.gewicht
            <*> parseInt d.``adm-length``
            <*> parseBool d.contrean12
            <*> parseBool d.``septische-shock``
            <*> parseBool d.canule
            <*> pim d
            <*> Result.ok None
            <*> prism d
            <*> Result.ok None
        )
        |> Result.foldOk


    let parseMRDM exportPath cachePath : Result<(Types.Patient [] * string []), string []> =
        match cachePath |> Cache.getCache<Result<(Types.Patient [] * string []), string []>> with
        | Some pats -> pats
        | None ->
            let pats =
                printfn "Start parsing, this can take a while ..."
                let timer = new Stopwatch ()
                timer.Start ()

                let hospData = (getMrdmHospital exportPath).Data |> Seq.toArray
                let picuData = (getMrdmPicu exportPath).Data |> Seq.toArray
                let picuAdms =
                    printfn "parsing picu admissions"
                    parsePICUAdmissions picuData
//                let clickData = Click.pimprismHist.Data |> Seq.toArray

                let diagnoses =
                    (getMrdmDiagnose exportPath).Data
                    |> Seq.toArray
                    |> Array.map (fun r ->
                        {|
                            pi = r.``picu-episode_uri``
                            dn = r.``bijkomende-diagnose``
                        |}
                    )

                let parsePat i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i parse patient" i
                    parsePatient hospData

                let parseHosp i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i parse hospital admission" i
                    parseHospAdm hospData

                let addPICU i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i add picu admission" i
                    addPICUAdmissions picuAdms diagnoses

                // let validClick i =
                //     timer.ElapsedMilliseconds
                //     |> printfn "%i: %i validated click data" i
                //     validateWithClickData clickData

                let filter xs =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: starting filtering duplicates"
                    let xs = xs |> filterDuplicateOrMore
                    timer.ElapsedMilliseconds
                    |> printfn "%i: finished filtering duplicates"
                    xs

                (getMrdmPatient exportPath).Data
                |> Seq.toArray
                |> Array.mapi parsePat
                |> Array.mapi parseHosp
                |> Array.mapi addPICU
//                |> Array.mapi validClick
                |> filter

            pats |> Cache.cache cachePath
            pats





