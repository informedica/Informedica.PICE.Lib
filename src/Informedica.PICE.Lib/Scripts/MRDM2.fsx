
#r "System.Data.Linq"
#load "../../../.paket/load/net472/main.group.fsx"


open System
open System.IO
open System.Globalization

open FSharp.Data
open FSharp.Interop.Excel

#time

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

fsi.AddPrinter<DateTime>(sprintf "%A")



module File =

    open System.IO

    let enumerate path =
        seq { for file in (new DirectoryInfo(path)).EnumerateFiles() do yield file }    

    let readAllLines path = File.ReadAllLines(path)

    let readAllLinesAsync path = 
        async {
            use stream = File.OpenText(path)
            
            return File.ReadAllLines(path) |> Array.toList
        }

    let writeTextToFile path text =
        File.WriteAllText(path, text) 

    let exists path =
        File.Exists(path)



module Markdown =

    open System
    
    open Markdig


    let toHtml s =
        let pipeline = Markdig.MarkdownPipelineBuilder().UseAdvancedExtensions().Build()
        Markdown.ToHtml(s, pipeline)


    let htmlToBrowser html =
        let proc = new System.Diagnostics.Process()
        proc.EnableRaisingEvents <- false

        let tmp = IO.Path.GetTempPath() + "/temp.html"

        html
        |> File.writeTextToFile tmp

        proc.StartInfo.FileName <- tmp

        proc.Start() |> ignore
        proc.Close()


    let toBrowser s =
        s
        |> toHtml
        |> htmlToBrowser



module Result =

    let ok x = Result.Ok (x, [||])

    let okMsg x msgs = Result.Ok (x, msgs)

    let error msgs = Result.Error msgs

    let tryWith f x =
        try
            f x
            |> ok
        with
        | e ->
            [ e.Message ]
            |> error


    let tryWithOk f x =
        try
            Some (f x)
            |> ok
        with
        | e ->
            [| e.Message |]
            |> okMsg None


    /// A function that applies either fSuccess or fFailure 
    /// depending on the case.
    let either fOk fErr = function
        | Result.Ok (x, msgs) -> fOk (x, msgs) 
        | Result.Error msgs -> fErr msgs 

    /// merge messages with a result
    let mergeMessages msgs result =
        let fOk (x, msgs2) = 
            msgs @ msgs2 |> okMsg x 
        let fErr errs = 
            error (errs @ msgs) 
        either fOk fErr result

    /// given a function that generates a new RopResult
    /// apply it only if the result is on the Success branch
    /// merge any existing messages with the new result
    let bindR f result =
        let fOk (x,msgs) = 
            f x |> mergeMessages msgs
        let fErr errs = 
            Result.Error errs 
        either fOk fErr result

    let bindL result f = bindR f result

    /// given a function wrapped in a result
    /// and a value wrapped in a result
    /// apply the function to the value only if both are Success
    let applyR f result =
        match f, result with
        | Result.Ok (f, msgs1), Result.Ok (x, msgs2) -> 
            msgs1 |> Array.append msgs2 |> okMsg (f x)
        | Result.Error msgs1,    Result.Ok (_, msgs2) 
        | Result.Ok (_, msgs2),  Result.Error msgs1 -> 
            msgs1 |> Array.append msgs2 |> error
        | Result.Error msgs1,   Result.Error msgs2 -> 
            msgs1 |> Array.append msgs2 |> error 

    /// given a function that transforms a value
    /// apply it only if the result is on the Success branch
    let liftR f result =
        let f' =  f |> ok
        applyR f' result 

    /// given two values wrapped in results apply a function to both
    let lift2R f result1 result2 =
        let f' = liftR f result1
        applyR f' result2 

    /// given three values wrapped in results apply a function to all
    let lift3R f result1 result2 result3 =
        let f' = lift2R f result1 result2 
        applyR f' result3

    /// given four values wrapped in results apply a function to all
    let lift4R f result1 result2 result3 result4 =
        let f' = lift3R f result1 result2 result3 
        applyR f' result4


    /// synonym for liftR
    let mapR = liftR


    /// given an RopResult, call a unit function on the success branch
    /// and pass thru the result
    let okTee f result = 
        let fOk (x, msgs) = 
            f (x, msgs)
            okMsg x msgs 
        let fErr msgs = msgs |> error
        either fOk fErr result

    /// given an RopResult, call a unit function on the failure branch
    /// and pass thru the result
    let errorTee f result = 
        let fOk (x, msgs) = msgs |> okMsg x 
        let fErr msgs = 
            f msgs
            msgs |> error
        either fOk fErr result

    /// given an RopResult, map the messages to a different error type
    let mapMessagesR f result = 
        match result with 
        | Result.Ok (x,  msgs) -> 
            List.map f msgs
            |> okMsg x
        | Result.Error msgs -> 
            List.map f msgs 
            |> error

    /// given an RopResult, in the success case, return the value.
    /// In the failure case, determine the value to return by 
    /// applying a function to the errors in the failure case
    let valueOrDefault f result = 
        match result with 
        | Result.Ok (x,_) -> x
        | Result.Error msgs -> f msgs

    /// lift an option to a RopResult.
    /// Return Success if Some
    /// or the given message if None
    let errIfNone msgs = function
        | Some x -> ok x
        | None -> msgs |> error 

    /// given an RopResult option, return it
    /// or the given message if None
    let errorIfNoneR msgs = function
        | Some result -> result
        | None -> error msgs 


    let okIfNone msgs = function
        | Some x -> ok (Some x)
        | None -> msgs |> okMsg None 


    let resultIs bf = function 
        | Result.Error _ -> bf |> not
        | Result.Ok _ -> bf


    let isError r = resultIs false r
    let isOk r = resultIs true r


    let getMessages = function
        | Result.Ok(_, msgs) | Result.Error msgs -> msgs


    let foldOk results =
        results
        |> Seq.fold (fun acc result ->
            match result with
            | Result.Ok (x, msgs1) ->
                match acc with
                | Result.Ok (xs, msgs2) -> okMsg ([|x|] |> Array.append xs) (msgs1 |> Array.append msgs2)
                | Result.Error _ -> acc
            | Result.Error msgs1 ->
                match acc with
                | Result.Ok (xs, msgs2) -> okMsg xs (msgs1 |> Array.append msgs2)
                | Result.Error _ -> acc
        ) (okMsg [||] [||])


    module Operators =

        /// infix version of bindL
        let (>>=) = bindL

        let (<<=) = bindR

        /// infix version of apply
        let (<*>) = applyR

        /// infix version of liftR
        let (<!>) = liftR



module Types =

    type Item =
        | BloodPressure
        | Temperature
        | MentalStatus
        | HeartRate
        | Creatinine
        | Urea
        | ProthPT
        | ProthPTT
        | Pupils
        | Ph
        | TotalCO2
        | PCO2
        | PaO2
        | Glucose
        | Potassium
        | WBC
        | Platelets

    type AgePRISM3 = | Neonate | Infant | Child | Adolescent | AllMinNeonate | AnyAge

    type AgePRISM4 = | TwoWeeks | OneMonth | OneYear | EightTeen | UnknownAge

    type AdmissionSource =
        | Recovery
        | AnotherHospital
        | InHospital
        | EmergencyUnit
        | UnknownAdmissionSource

    type Value =
        | NoValue
        | OneValue of float
        | TwoValues of float * float

    type PRISM =
        {
            Age : DateTime option
            SystolicBloodPressure : Value
            Temperature : Value
            MentalStatus : Value
            HeartRate : Value
            PupilsFixed : Value
            PH : Value
            TotalCO2 : Value
            PCO2 : Value
            PaO2 : Value
            Glucose : Value
            Potassium : Value
            Creatinine : Value
            Urea : Value
            WhiteBloodCount : Value
            PT : Value
            PTT : Value
            Platelets : Value
            AdmissionSource : AdmissionSource
            CPR24HourBefore : bool
            Cancer : bool
            LowRiskPrimary : bool
            PRISM3Score : int option
            PRISM3Neuro : int option
            PRISM4Mortality : float option
        }

    type PIM =
        {
            Urgency: AdmissionUrgency
            Recovery: bool
            RiskDiagnosis: RiskDiagnosis list
            CardiacByPass: bool
            CardiacNonByPass: bool
            NonCardiacProcedure: bool
            Ventilated: bool
            AdmissionPupils: PupilResponse
            PaO2: float option
            FiO2: float option
            BaseExcess: float option
            SystolicBloodPressure: float option
            PIM2Score : float option
            PIM2Mortality : float option
            PIM3Score : float option
            PIM3Mortality : float option
        }

    and AdmissionUrgency =
        | Elective
        | NotElective
        | UnknownUrgency

    and AdmissionType =
        | Medical
        | Surgery
        | DOA
        | UnknownAdmissionType

    and PupilResponse =
        | FixedDilated
        | NormalPupils
        | UnknownPupils

    and RiskDiagnosis =
        | Asthma
        | BoneMarrowTransplant
        | Bronchiolitis
        | CardiacArrest
        | CardiomyopathyOrMyocarditis
        | CerebralHemorrhage
        | Croup
        | DiabeticKetoacidosis
        | HIVPositive
        | HypoplasticLeftHeartSyndrome
        | LeukemiaorLymphoma
        | LiverFailure
        | NecrotizingEnterocolitis
        | NeurodegenerativeDisorder
        | ObstructiveSleepApnea
        | SeizureDisorder
        | SevereCombinedImmuneDeficiency

    type Patient =
        {
            HospitalNumber : string
            BirthDate : DateTime option
            PatientState : PatientState
            DateOfDeath : DateTime option
            DeathMode : string
            HospitalAdmissions : HospitalAdmission list
        }
    and PatientState = Alive | Dead | UnknownPatientState
    and HospitalAdmission =
        {
            HospitalNumber : string
            AdmissionDate : DateTime option
            DischargeDate : DateTime option
            PICUAdmissions : PICUAdmission list
        }
    and PICUAdmission =
        {
            HospitalNumber : string
            AdmissionDate : DateTime option
            DischargeDate : DateTime option
            DischargeReason : string
            Urgency : AdmissionUrgency
            AdmissionType : AdmissionType
            Recovery : bool
            AdmissionIndication : string
            ReferingSpecialism : string
            AdmissionWeight : float option
            AdmissionLength : int option
            TempMin12 : float option
            TempMax12 : float option
            RiskDiagnosis : RiskDiagnosis list
            PIM : PIM
            PRISM : PRISM
        }

    type ParsingError =
        | ParseError of string
        | NoParseError

    type ValidationError =
        | NotValid of Patient * string
        | IsValid




module MRDM =

    [<Literal>]
    let path = "../../../mrdm/Export_PICE.xlsx"

    type MRDMPatient = ExcelFile<path, SheetName = "patient", HasHeaders = true, ForceString = true>
    type MRDMHospital = ExcelFile<path, SheetName = "ziekenhuis-episode", HasHeaders = true, ForceString = true>
    type MRDMPicu = ExcelFile<path, SheetName = "pat-zkh-episode-picu-episode", HasHeaders = true, ForceString = true>
    type MRDMDiagnos = ExcelFile<path, SheetName = "pat-zkh-epi-picu-epi-bijk-diagn", HasHeaders = true, ForceString = true>

    let mrdmPatient = MRDMPatient ()
    let mrdmHospital = MRDMHospital ()
    let mrdmPicu = MRDMPicu ()
    let mrdmDiagnos = MRDMDiagnos ()


    module Codes =

        let eqs (s1 : String) (s2 : String) =
            let canon (s: String) =
                s.ToLower()
                 .Trim()
                 .Replace("_", "")
                 .Replace("-", "")

            s1 |> canon = (s2 |> canon)

        let codes =
            [|
                ("adm-desthospitalid-umc", "103", "Academisch Medisch Centrum, Amsterdam")
                ("adm-desthospitalid-umc", "104", "VU Medisch Centrum, Amsterdam")
                ("adm-desthospitalid-umc", "101", "Universitair Medisch Centrum Groningen, Groningen")
                ("adm-desthospitalid-umc", "106", "Leids Universitair Medisch Centrum, Leiden")
                ("adm-desthospitalid-umc", "110", "Radboudumc, Nijmegen")
                ("adm-desthospitalid-umc", "113", "Academisch Ziekenhuis Maastricht, Maastricht")
                ("adm-desthospitalid-umc", "109", "Erasmus Medisch Centrum, Rotterdam")
                ("adm-desthospitalid-umc", "107", "UMC Utrecht, Utrecht")
                ("adm-sourcehospitalid-umc", "103", "Academisch Medisch Centrum, Amsterdam")
                ("adm-sourcehospitalid-umc", "104", "VU Medisch Centrum, Amsterdam")
                ("adm-sourcehospitalid-umc", "101", "Universitair Medisch Centrum Groningen, Groningen")
                ("adm-sourcehospitalid-umc", "106", "Leids Universitair Medisch Centrum, Leiden")
                ("adm-sourcehospitalid-umc", "110", "Radboudumc, Nijmegen")
                ("adm-sourcehospitalid-umc", "113", "Academisch Ziekenhuis Maastricht, Maastricht")
                ("adm-sourcehospitalid-umc", "109", "Erasmus Medisch Centrum, Rotterdam")
                ("adm-sourcehospitalid-umc", "107", "UMC Utrecht, Utrecht")
                ("ziekenhuis-episode-id", "103", "Academisch Medisch Centrum, Amsterdam")
                ("ziekenhuis-episode-id", "113", "Academisch Ziekenhuis Maastricht, Maastricht")
                ("ziekenhuis-episode-id", "172", "Admiraal de Ruyter Ziekenhuis, Vlissingen")
                ("ziekenhuis-episode-id", "156", "Albert Schweitzer Ziekenhuis, Dordrecht ")
                ("ziekenhuis-episode-id", "266", "Alrijne Zorggroep, Leiderdorp")
                ("ziekenhuis-episode-id", "111", "Amphia, Breda")
                ("ziekenhuis-episode-id", "118", "Amstelland Ziekenhuis, Amstelveen ")
                ("ziekenhuis-episode-id", "500", "Antoni van Leeuwenhoek Ziekenhuis, Amsterdam")
                ("ziekenhuis-episode-id", "162", "Antonius Ziekenhuis Zuid West Friesland, Sneek")
                ("ziekenhuis-episode-id", "181", "Beatrix Ziekenhuis (Rivas Zorggroep), Gorinchem")
                ("ziekenhuis-episode-id", "133", "Bernhoven Ziekenhuis, Uden")
                ("ziekenhuis-episode-id", "149", "Bethesda Ziekenhuis, Hoogeveen")
                ("ziekenhuis-episode-id", "119", "BovenIJ Ziekenhuis, Amsterdam")
                ("ziekenhuis-episode-id", "241", "Bravis Ziekenhuis, Roosendaal")
                ("ziekenhuis-episode-id", "139", "Canisius-Wilhelmina Ziekenhuis, Nijmegen")
                ("ziekenhuis-episode-id", "112", "Catharina Ziekenhuis, Eindhoven")
                ("ziekenhuis-episode-id", "123", "Deventer Ziekenhuis, Deventer")
                ("ziekenhuis-episode-id", "164", "Diaconessenhuis, Meppel")
                ("ziekenhuis-episode-id", "193", "Diakonessenhuis, Utrecht")
                ("ziekenhuis-episode-id", "600", "Elisabeth TweeSteden Ziekenhuis, Tilburg")
                ("ziekenhuis-episode-id", "170", "Elkerliek Ziekenhuis, Helmond")
                ("ziekenhuis-episode-id", "109", "Erasmus Medisch Centrum, Rotterdam")
                ("ziekenhuis-episode-id", "131", "Flevoziekenhuis, Almere")
                ("ziekenhuis-episode-id", "196", "Gelderse Vallei, Ede")
                ("ziekenhuis-episode-id", "1561", "Gelre Ziekenhuizen, Apeldoorn")
                ("ziekenhuis-episode-id", "151", "Groene Hart Ziekenhuis, Gouda")
                ("ziekenhuis-episode-id", "601", "Haaglanden Medisch Centrum, Den Haag")
                ("ziekenhuis-episode-id", "152", "HagaZiekenhuis, Den Haag")
                ("ziekenhuis-episode-id", "202", "Havenziekenhuis, Rotterdam")
                ("ziekenhuis-episode-id", "158", "Hofpoort Ziekenhuis, Woerden")
                ("ziekenhuis-episode-id", "173", "IJsselland Ziekenhuis, Capelle a/d Ijssel")
                ("ziekenhuis-episode-id", "116", "Ijsselmeerziekenhuizen, Lelystad")
                ("ziekenhuis-episode-id", "177", "Ikazia Ziekenhuis, Rotterdam")
                ("ziekenhuis-episode-id", "121", "Isala Klinieken, Zwolle")
                ("ziekenhuis-episode-id", "155", "Jeroen Bosch Ziekenhuis, s-Hertogenbosch")
                ("ziekenhuis-episode-id", "137", "Kennemer Gasthuis, Haarlem")
                ("ziekenhuis-episode-id", "194", "Lange Land Ziekenhuis, Zoetermeer")
                ("ziekenhuis-episode-id", "163", "Laurentius Ziekenhuis, Roermond")
                ("ziekenhuis-episode-id", "106", "Leids Universitair Medisch Centrum, Leiden")
                ("ziekenhuis-episode-id", "211", "Maasstad Ziekenhuis, Rotterdam")
                ("ziekenhuis-episode-id", "142", "Maasziekenhuis, Beugen")
                ("ziekenhuis-episode-id", "147", "Martini Ziekenhuis, Groningen")
                ("ziekenhuis-episode-id", "132", "Máxima Medisch Centrum, Veldhoven")
                ("ziekenhuis-episode-id", "175", "Meander Medisch Centrum, Amersfoort")
                ("ziekenhuis-episode-id", "148", "Medisch Centrum Leeuwarden, Leeuwarden")
                ("ziekenhuis-episode-id", "124", "Medisch Spectrum Twente, Enschede")
                ("ziekenhuis-episode-id", "157", "Nij Smellinghe, Drachten")
                ("ziekenhuis-episode-id", "618", "Noordwest Ziekenhuisgroep, Alkmaar")
                ("ziekenhuis-episode-id", "183", "Ommelander Ziekenhuis Groep, Delfzijl/Winschoten")
                ("ziekenhuis-episode-id", "110", "Radboudumc, Nijmegen")
                ("ziekenhuis-episode-id", "501", "Refaja Ziekenhuis, Stadskanaal")
                ("ziekenhuis-episode-id", "166", "Reinier de Graaf Groep, Delft")
                ("ziekenhuis-episode-id", "128", "Rijnstate Ziekenhuis, Arnhem")
                ("ziekenhuis-episode-id", "192", "Rode Kruis Ziekenhuis, Beverwijk")
                ("ziekenhuis-episode-id", "199", "Röpcke Zweers Ziekenhuis, Hardenberg")
                ("ziekenhuis-episode-id", "203", "Ruwaard van Putten Ziekenhuis, Spijkenisse")
                ("ziekenhuis-episode-id", "160", "Scheperziekenhuis, Emmen")
                ("ziekenhuis-episode-id", "184", "Sint Anna Ziekenhuis, Geldrop")
                ("ziekenhuis-episode-id", "108", "Sint Antonius Ziekenhuis, Nieuwegein")
                ("ziekenhuis-episode-id", "114", "Sint Franciscus Gasthuis, Rotterdam")
                ("ziekenhuis-episode-id", "138", "Sint Jansdal Ziekenhuis, Harderwijk")
                ("ziekenhuis-episode-id", "197", "Sint Jansgasthuis, Weert")
                ("ziekenhuis-episode-id", "165", "Slingeland Ziekenhuis, Doetinchem")
                ("ziekenhuis-episode-id", "129", "Slotervaart Ziekenhuis, Amsterdam")
                ("ziekenhuis-episode-id", "617", "Spaarne Gasthuis, Hoofddorp")
                ("ziekenhuis-episode-id", "122", "Spaarne Ziekenhuis, Hoofddorp")
                ("ziekenhuis-episode-id", "1403", "Stichting Onze Lieve Vrouwe Gasthuis, Amsterdam")
                ("ziekenhuis-episode-id", "201", "Stichting ZorgSaam Zeeuws Vlaanderen, Terneuzen")
                ("ziekenhuis-episode-id", "146", "Streekziekenhuis Koningin Beatrix, Winterswijk")
                ("ziekenhuis-episode-id", "182", "Tergooi Ziekenhuizen, Hilversum")
                ("ziekenhuis-episode-id", "169", "Tjongerschans Ziekenhuis, Heerenveen")
                ("ziekenhuis-episode-id", "107", "UMC Utrecht, Utrecht")
                ("ziekenhuis-episode-id", "101", "Universitair Medisch Centrum Groningen, Groningen")
                ("ziekenhuis-episode-id", "204", "Van Weel Ziekenhuis, Dirksland")
                ("ziekenhuis-episode-id", "140", "VieCuri Medisch Centrum, Venlo")
                ("ziekenhuis-episode-id", "150", "Vlietland Ziekenhuis, Schiedam/Vlaardingen")
                ("ziekenhuis-episode-id", "104", "VU Medisch Centrum, Amsterdam")
                ("ziekenhuis-episode-id", "171", "Waterland Ziekenhuis, Purmerend")
                ("ziekenhuis-episode-id", "120", "Westfries Gasthuis, Hoorn")
                ("ziekenhuis-episode-id", "179", "Wilhelminaziekenhuis, Assen")
                ("ziekenhuis-episode-id", "117", "Zaans Medisch Centrum, Zaandam ")
                ("ziekenhuis-episode-id", "168", "Ziekenhuis Rivierenland, Tiel")
                ("ziekenhuis-episode-id", "505", "Ziekenhuisgroep Twente, Hengelo / Almelo")
                ("ziekenhuis-episode-id", "807", "Zuyderland Medisch Centrum, Heerlen")
                ("adm-typeid-soort", "1", "Medisch")
                ("adm-typeid-soort", "2", "Chirurgisch")
                ("adm-typeid-soort", "3", "Vóór opname reeds overleden")
                ("adm-typeid-urgentie", "10", "Ongeplande opname")
                ("adm-typeid-urgentie", "11", "Geplande opname")
                ("adm-refspecialism", "1", "Geen")
                ("adm-refspecialism", "2", "Anaesthesiologie (ANS)")
                ("adm-refspecialism", "3", "AIDS (APT)")
                ("adm-refspecialism", "4", "Cardio Pulmonale Chirurgie (CAP)")
                ("adm-refspecialism", "5", "Cardiologie (CAR)")
                ("adm-refspecialism", "6", "Algemene Chirurgie (CHI)")
                ("adm-refspecialism", "7", "Plastische en reconstructieve Chirurgie (CHP)")
                ("adm-refspecialism", "8", "Endocrinologie (END)")
                ("adm-refspecialism", "9", "Gynaecologie (GYN)")
                ("adm-refspecialism", "10", "Huidziekten (HDZ)")
                ("adm-refspecialism", "11", "Hematologie (HEM)")
                ("adm-refspecialism", "12", "Hemostase (HST)")
                ("adm-refspecialism", "13", "Infectieziekten (INF)")
                ("adm-refspecialism", "14", "Algemene Interne Geneeskunde (INT)")
                ("adm-refspecialism", "15", "Kinder-cardiologie (KCA)")
                ("adm-refspecialism", "16", "Kinder-cardiochirurgie (KCC)")
                ("adm-refspecialism", "17", "Kinder-chirurgie (KCH)")
                ("adm-refspecialism", "18", "Kinder-endocrinologie (KEC)")
                ("adm-refspecialism", "19", "Kinder-gastro-enterologie (KGE)")
                ("adm-refspecialism", "20", "Kinder-hematologie/immunologie (KHI)")
                ("adm-refspecialism", "21", "Kinder-huidziekten (KHZ)")
                ("adm-refspecialism", "22", "Algemene Paediatrie / Kindergeneeskunde (KIN / AKG)")
                ("adm-refspecialism", "23", "Kinder-KNO (KKN)")
                ("adm-refspecialism", "24", "Kinder-metabole stoornissen (KMB)")
                ("adm-refspecialism", "25", "Kinder-neurochirurgie (KNC)")
                ("adm-refspecialism", "26", "Kinder-nefrologie (KNE)")
                ("adm-refspecialism", "27", "Kinder-neurologie (KNL)")
                ("adm-refspecialism", "28", "Keel, Neus en Oorheelkunde (KNO)")
                ("adm-refspecialism", "29", "Kinder-oogheelkunde (KOG)")
                ("adm-refspecialism", "30", "Kinder-oncologie (KON)")
                ("adm-refspecialism", "31", "Kinder-orthopaedie (KOR)")
                ("adm-refspecialism", "32", "Kinder-plastische en reconstructieve chirurgie (KPC)")
                ("adm-refspecialism", "33", "Kinder-psychiatrie (KPS)")
                ("adm-refspecialism", "34", "Kinder-pulmonologie (KPU)")
                ("adm-refspecialism", "35", "Kinder-urologie (KUR)")
                ("adm-refspecialism", "36", "Longziekten (LON)")
                ("adm-refspecialism", "37", "Maag-, Darm- en Leverziekten (MDL)")
                ("adm-refspecialism", "38", "Mondziekten en Kaakchirurgie (MZK)")
                ("adm-refspecialism", "39", "Neurochirurgie (NEC)")
                ("adm-refspecialism", "40", "Neonatologie (NEO)")
                ("adm-refspecialism", "41", "Neurologie (NEU)")
                ("adm-refspecialism", "42", "Niertransplantatie (NIT)")
                ("adm-refspecialism", "43", "Nierziekten (NIZ)")
                ("adm-refspecialism", "44", "Nucleaire geneeskunde (NUC)")
                ("adm-refspecialism", "45", "Oogheelkunde (OOG)")
                ("adm-refspecialism", "46", "Orthopaedie (ORT)")
                ("adm-refspecialism", "47", "Oncotherapie (OTH)")
                ("adm-refspecialism", "48", "Psychiatrie (PSY)")
                ("adm-refspecialism", "49", "Reumatologie (REU)")
                ("adm-refspecialism", "50", "Revalidatie (REV)")
                ("adm-refspecialism", "51", "Radiotherapie (RTH)")
                ("adm-refspecialism", "52", "Traumatologie (TRA)")
                ("adm-refspecialism", "53", "Urologie (URO)")
                ("adm-refspecialism", "54", "Vaatziekten (VAA)")
                ("adm-refspecialism", "55", "Verloskunde (VRV)")
                ("adm-refspecialism", "56", "Zuigelingen (ZUI)")
                ("adm-refspecialism", "57", "Kinder Intensive Care (ICK)")
                ("adm-refspecialism", "77", "Overig (niet nader gespecificeerd) (DIV)")
                ("adm-sourcetypeid", "116", "Ander UMC")
                ("adm-sourcetypeid", "102", "Ander Ziekenhuis (Nederland)")
                ("adm-sourcetypeid", "103", "Ander Ziekenhuis (Buitenland)")
                ("adm-sourcetypeid", "104", "Huis")
                ("adm-sourcetypeid", "106", "Plaats van ongeval")
                ("adm-sourcetypeid", "105", "Zorginstelling")
                ("adm-sourcetypeid", "109", "Buitenland (Overig)")
                ("adm-sourcetypeid", "77", "Overig")
                ("adm-sourcetypeid", "99", "Onbekend")
                ("adm-length", "999", "Onbekend")
                ("gewicht", "999", "Onbekend")
                ("adm-indication", "1", "1.0 Respiratoire insufficiëntie")
                ("adm-indication", "2", "1.1 Bovenste luchtwegprobleem")
                ("adm-indication", "3", "1.2 Lagere luchtwegprobleem (bronchiolitis, astma etc)")
                ("adm-indication", "4", "1.3 Parenchym afwijking (pneumonie, ARDS)")
                ("adm-indication", "5", "1.4 Pneumothorax")
                ("adm-indication", "6", "1.5 Neuromusculair")
                ("adm-indication", "7", "2.0 Circulatoire insufficiëntie")
                ("adm-indication", "8", "2.1 Shock")
                ("adm-indication", "9", "2.2 Ritmestoornissen")
                ("adm-indication", "10", "2.3 Hypertensie")
                ("adm-indication", "11", "2.3.1 Systemisch")
                ("adm-indication", "12", "2.3.2 Pulmonaal")
                ("adm-indication", "13", "2.4 Hartafwijking (congenitaal)")
                ("adm-indication", "14", "3.0 Cerebrale insufficiëntie")
                ("adm-indication", "15", "3.1 Status epilepticus")
                ("adm-indication", "16", "3.2 Coma / gedaald of wisselend bewustzijn")
                ("adm-indication", "18", "4.0 Metabole ontregeling")
                ("adm-indication", "19", "4.1 Diabetische ketoacidose")
                ("adm-indication", "20", "4.2 Nier insufficiëntie (acuut)")
                ("adm-indication", "21", "4.3 Lever insufficiëntie (acuut)")
                ("adm-indication", "22", "5.0 Monitoring")
                ("adm-indication", "23", "5.1 Bijzondere medicatie / procedure")
                ("adm-indication", "24", "5.2 Post CPR")
                ("adm-indication", "25", "5.3 Postoperatief")
                ("adm-indication", "26", "5.4 Preoperatief")
                ("adm-indication", "27", "5.5 Intoxicatie")
                ("adm-indication", "35", "5.6.1 ALTE")
                ("adm-indication", "36", "5.6.2 polysomnografie")
                ("adm-indication", "37", "5.6.3 instellen chronische beademing")
                ("adm-indication", "29", "6.0 Trauma")
                ("adm-indication", "30", "6.1 verdrinking")
                ("adm-indication", "33", "6.2 Multi-trauma")
                ("adm-indication", "17", "6.3 (ernstig) traumatisch schedel-hersenletsel")
                ("adm-indication", "34", "7.0 Palliatieve/terminale zorg")
                ("adm-pimloc", "1", "in PICU")
                ("adm-pimloc", "2", "buiten PICU in eigen ZH")
                ("adm-pimloc", "3", "buiten eigen ZH")
                ("risicodiag-hoofd", "8", "Leverfalen, exclusief opnames na recovery levertransplantatie [HR PIM2 & VHR PIM3]")
                ("risicodiag-hoofd", "5", "Leverfalen, incl levertransplantatie (ook met ‘Recovery’) [HR PIM2]")
                ("risicodiag-hoofd", "9", "Necrotiserende enterocolitis [HR PIM3]")
                ("risicodiag-hoofd", "1", "Pseudokroep [LowRisk PIM2&3]")
                ("risicodiag-hoofd", "2", "Obstructieve slaap-apnoe (kan ook met ‘Recovery’) [LowRisk PIM2&3]")
                ("risicodiag-hoofd", "3", "Bronchiolitis [LowRisk PIM2&3]")
                ("risicodiag-hoofd", "4", "Astma [LowRisk PIM2&3]")
                ("risicodiag-hoofd", "6", "Convulsies [LowRisk PIM3]")
                ("risicodiag-hoofd", "7", "Diabetische keto-acidose [LowRisk PIM2&3] (betekent ook 'acute diabtes' [PRISM3])")
                ("risicodiag-hoofd", "10", "Acute Diabetes [PRISM3]")
                ("risicodiag-hoofd", "11", "Niet-operatieve cardiovasculaire aandoeningen (bv: cardiomyopathie/myocarditis) [PRISM3]")
                ("risicodiag-hoofd", "0", "geen van bovenstaande hoofdredenen")
                ("diagnose1", "40", "(Letsel) Anafylaxie")
                ("diagnose1", "50", "(Letsel) Brandwonden")
                ("diagnose1", "60", "(Letsel) Koolmonoxide vergiftiging")
                ("diagnose1", "70", "(Letsel) Geneesmiddel toxiciteit-iatrogeen")
                ("diagnose1", "80", "(Letsel) Electrocutie")
                ("diagnose1", "90", "(Letsel) Vergiftiging")
                ("diagnose1", "100", "(Letsel) Verhanging of verwurging")
                ("diagnose1", "110", "(Letsel) Hyperthermie")
                ("diagnose1", "120", "(Letsel) Hypothermie")
                ("diagnose1", "130", "(Letsel) Verdrinking")
                ("diagnose1", "150", "(Letsel) Rook inhalatie")
                ("diagnose1", "170", "(Letsel) Trauma - Abdomen")
                ("diagnose1", "180", "(Letsel) Trauma - Borstkas")
                ("diagnose1", "190", "(Letsel) Trauma - Gelaat")
                ("diagnose1", "200", "(Letsel) Trauma - Hoofd")
                ("diagnose1", "210", "(Letsel) Trauma - Skelet")
                ("diagnose1", "220", "(Letsel) Trauma - Wervelkolom")
                ("diagnose1", "160", "(Letsel) Trauma - Overig")
                ("diagnose1", "142", "(Letsel) Ingestie - drug")
                ("diagnose1", "144", "(Letsel) Ingestie - non drug")
                ("diagnose1", "30", "(Letsel) Letsel/vergiftiging/externe oorzaak-overig")
                ("diagnose1", "400", "(Cardiovasculair) Afwezige Pulmonaal Klep")
                ("diagnose1", "410", "(Cardiovasculair) Anomalie Coronair Arterie")
                ("diagnose1", "420", "(Cardiovasculair) Aorta-insufficientie")
                ("diagnose1", "430", "(Cardiovasculair) Aortastenose")
                ("diagnose1", "440", "(Cardiovasculair) ASD")
                ("diagnose1", "450", "(Cardiovasculair) AVSD")
                ("diagnose1", "460", "(Cardiovasculair) Coarctatio aortae")
                ("diagnose1", "470", "(Cardiovasculair) Cor triatriatum")
                ("diagnose1", "480", "(Cardiovasculair) Ebstein anomalie")
                ("diagnose1", "490", "(Cardiovasculair) Hypoplastisch Linker Hart Syndroom")
                ("diagnose1", "500", "(Cardiovasculair) Interruptie of Hypoplastische Aortaboog")
                ("diagnose1", "510", "(Cardiovasculair) Mitraal Insufficientie")
                ("diagnose1", "520", "(Cardiovasculair) Mitraal Stenose")
                ("diagnose1", "530", "(Cardiovasculair) Open Ductus Botalli")
                ("diagnose1", "540", "(Cardiovasculair) Pulmonaal Atresie of Stenose")
                ("diagnose1", "550", "(Cardiovasculair) Monoventrikel")
                ("diagnose1", "560", "(Cardiovasculair) Totaal Abnornaal Pulmonaal Veneuze Return")
                ("diagnose1", "570", "(Cardiovasculair) Tetralogie van Fallot")
                ("diagnose1", "580", "(Cardiovasculair) Transpositie Grote Arterien")
                ("diagnose1", "590", "(Cardiovasculair) Tricuspidaal Atresie of Stenose")
                ("diagnose1", "600", "(Cardiovasculair) Tricuspidaal Insufficientie")
                ("diagnose1", "610", "(Cardiovasculair) Truncus Arteriosus")
                ("diagnose1", "620", "(Cardiovasculair) VSD")
                ("diagnose1", "630", "(Cardiovasculair) Aortopulmonaal Venster")
                ("diagnose1", "640", "(Cardiovasculair) Arterioveneuze Malformatie")
                ("diagnose1", "650", "(Cardiovasculair) Double Outlet Rechter Ventrikel")
                ("diagnose1", "660", "(Cardiovasculair) Linker Ventrikel Uitstroom Obstructie")
                ("diagnose1", "670", "(Cardiovasculair) Pulmonaal Insufficientie")
                ("diagnose1", "680", "(Cardiovasculair) Rechter Ventrikel Uitstroom Obstructie")
                ("diagnose1", "682", "(Cardiovasculair) Hypoplastische Li ventrikel (géén HLHS)")
                ("diagnose1", "684", "(Cardiovasculair) Hypoplastische Re ventrikel")
                ("diagnose1", "686", "(Cardiovasculair) l-TGA (Levo Transpositie grote arterien)")
                ("diagnose1", "390", "(Cardiovasculair) Cardiovasculair - Congenitaal - Overige")
                ("diagnose1", "710", "(Cardiovasculair) Hartfalen")
                ("diagnose1", "720", "(Cardiovasculair) Cardiale Tumor")
                ("diagnose1", "730", "(Cardiovasculair) Cardiomyopathie/Myocarditis")
                ("diagnose1", "740", "(Cardiovasculair) Dysritmie - Supraventriculair")
                ("diagnose1", "750", "(Cardiovasculair) Dysritmie - Ventriculair")
                ("diagnose1", "760", "(Cardiovasculair) Endocarditis")
                ("diagnose1", "770", "(Cardiovasculair) Hypertensie - Pulmonaal")
                ("diagnose1", "780", "(Cardiovasculair) Hypertensie - Systemisch")
                ("diagnose1", "790", "(Cardiovasculair) Kawasaki, Ziekte van")
                ("diagnose1", "800", "(Cardiovasculair) Pericardiale Effusie of Tamponade")
                ("diagnose1", "810", "(Cardiovasculair) Vasculaire Trombose")
                ("diagnose1", "820", "(Cardiovasculair) Vasculitis")
                ("diagnose1", "830", "(Cardiovasculair) Hart-Long Transplantatie, Status na")
                ("diagnose1", "840", "(Cardiovasculair) Harttransplantatie, Status na")
                ("diagnose1", "842", "(Cardiovasculair) Rheumatic Heart Disease")
                ("diagnose1", "844", "(Cardiovasculair) Hartoperatie in het verleden")
                ("diagnose1", "846", "(Cardiovasculair) Longembolie")
                ("diagnose1", "848", "(Cardiovasculair) Myocard (infarct/Ischemie)")
                ("diagnose1", "700", "(Cardiovasculair) Cardiovasculair - Verworven- Overige")
                ("diagnose1", "870", "(Neurologisch) Botulisme")
                ("diagnose1", "880", "(Neurologisch) Hersenabces")
                ("diagnose1", "890", "(Neurologisch) Hersen AV Malformatie")
                ("diagnose1", "900", "(Neurologisch) Hersendood")
                ("diagnose1", "910", "(Neurologisch) Herseninfarct of Beroerte")
                ("diagnose1", "920", "(Neurologisch) Hersentumor")
                ("diagnose1", "930", "(Neurologisch) CZS Shunt Dysfunctie of Infectie")
                ("diagnose1", "940", "(Neurologisch) Encefalitis")
                ("diagnose1", "950", "(Neurologisch) Encefalopathie, Acuut - Hypoxisch Ischaemisch")
                ("diagnose1", "960", "(Neurologisch) Encefalopatie, Acuut - Overige")
                ("diagnose1", "970", "(Neurologisch) Encefalopatie, Chronisch Degeneratief")
                ("diagnose1", "980", "(Neurologisch) Encefalopatie, Chronisch Statisch")
                ("diagnose1", "990", "(Neurologisch) Guillain Barre Syndroom")
                ("diagnose1", "1000", "(Neurologisch) Hydrocefalus")
                ("diagnose1", "1011", "(Neurologisch) Intracraniele Bloeding (spontaan)")
                ("diagnose1", "1020", "(Neurologisch) Intracraniele Hypertensie")
                ("diagnose1", "1030", "(Neurologisch) Meningitis")
                ("diagnose1", "1040", "(Neurologisch) Meningomyelocele of Spina Bifida")
                ("diagnose1", "1050", "(Neurologisch) Myopathie")
                ("diagnose1", "1060", "(Neurologisch) Neuropathie")
                ("diagnose1", "1070", "(Neurologisch) Convulsies")
                ("diagnose1", "1080", "(Neurologisch) Myelum, Beschadiging")
                ("diagnose1", "1090", "(Neurologisch) Veneuze Sinus Trombose")
                ("diagnose1", "1100", "(Neurologisch) Cerebraal Aneurysma")
                ("diagnose1", "1110", "(Neurologisch) Spierdystrofie")
                ("diagnose1", "1120", "(Neurologisch) Myastenia Gravis")
                ("diagnose1", "1130", "(Neurologisch) Tetanus")
                ("diagnose1", "1135", "(Neurologisch) Arnold Chiari Malformation")
                ("diagnose1", "1136", "(Neurologisch) Hersencyste")
                ("diagnose1", "1137", "(Neurologisch) Epilepsie (comorbiditeit)")
                ("diagnose1", "1138", "(Neurologisch) Hersenoedeem")
                ("diagnose1", "1139", "(Neurologisch) Intracraniele bloeding (Traumatisch)")
                ("diagnose1", "1131", "(Neurologisch) Acute disseminated encephalomyelitis (ADEM)")
                ("diagnose1", "1132", "(Neurologisch) Congenitale hersenafwijking")
                ("diagnose1", "1133", "(Neurologisch) Febrile convulsion")
                ("diagnose1", "1134", "(Neurologisch) Transverse Myelitis")
                ("diagnose1", "860", "(Neurologisch) Neurologisch - Overig")
                ("diagnose1", "1170", "(Respiratoir) Choanen Atresie of Stenose")
                ("diagnose1", "1180", "(Respiratoir) Epiglottitis")
                ("diagnose1", "1190", "(Respiratoir) Vreemdlichaam - Inhalatie")
                ("diagnose1", "1200", "(Respiratoir) Laryngotracheobronchitis")
                ("diagnose1", "1210", "(Respiratoir) Obstructief Slaap Apnoe Syndroom")
                ("diagnose1", "1220", "(Respiratoir) Pierre Robin Syndroom")
                ("diagnose1", "1230", "(Respiratoir) Retrofaryngeaal Abces")
                ("diagnose1", "1240", "(Respiratoir) Subglottische Stenose")
                ("diagnose1", "1250", "(Respiratoir) Tracheitis")
                ("diagnose1", "1260", "(Respiratoir) Bovenste Luchtwegobstructie - Overige")
                ("diagnose1", "1270", "(Respiratoir) Bovenste Luchtweginfectie - Overige")
                ("diagnose1", "1280", "(Respiratoir) Stembandparese")
                ("diagnose1", "1290", "(Respiratoir) Subglottisch Hemangioom")
                ("diagnose1", "1295", "(Respiratoir) Larygomalacia")
                ("diagnose1", "1160", "(Respiratoir) Bovenste Luchtwegen - Overige")
                ("diagnose1", "1320", "(Respiratoir) Astma")
                ("diagnose1", "1330", "(Respiratoir) Bronchiolitis")
                ("diagnose1", "1340", "(Respiratoir) Chronische Longziekte")
                ("diagnose1", "1350", "(Respiratoir) Malacie - Trachea en/of Bronchus")
                ("diagnose1", "1360", "(Respiratoir) Mediastinale Massa")
                ("diagnose1", "1370", "(Respiratoir) Stenose - Trachea en/of Bronchus")
                ("diagnose1", "1380", "(Respiratoir) Tracheo-oesofageale Fistel")
                ("diagnose1", "1390", "(Respiratoir) Vaatring")
                ("diagnose1", "1391", "(Respiratoir) Bronchiectasis")
                ("diagnose1", "1310", "(Respiratoir) Lage luchtwegen - Overige")
                ("diagnose1", "1420", "(Respiratoir) Luchtlek Syndroom")
                ("diagnose1", "1430", "(Respiratoir) Apnoe - Centraal")
                ("diagnose1", "1440", "(Respiratoir) ARDS")
                ("diagnose1", "1450", "(Respiratoir) Aspiratie")
                ("diagnose1", "1460", "(Respiratoir) Chylothorax")
                ("diagnose1", "1470", "(Respiratoir) Congenitale Hernia Diafragmatic")
                ("diagnose1", "1480", "(Respiratoir) Congenitale Longafwijking")
                ("diagnose1", "1490", "(Respiratoir) Cystic Fibrosis")
                ("diagnose1", "1500", "(Respiratoir) Empyeem")
                ("diagnose1", "1510", "(Respiratoir) Hyaliene Membraanziekte")
                ("diagnose1", "1520", "(Respiratoir) Hypoventilatie - Centraal")
                ("diagnose1", "1530", "(Respiratoir) Longabces")
                ("diagnose1", "1540", "(Respiratoir) Meconium Aspiratie Syndroom")
                ("diagnose1", "1550", "(Respiratoir) Pleura Effusie")
                ("diagnose1", "1560", "(Respiratoir) Pneumonie of Pneumonitis")
                ("diagnose1", "1570", "(Respiratoir) Long Hypoplasie")
                ("diagnose1", "1580", "(Respiratoir) Pulmonaal Oedeem")
                ("diagnose1", "1590", "(Respiratoir) Respiratoir Falen")
                ("diagnose1", "1600", "(Respiratoir) Lage Luchtweginfectie - Overige")
                ("diagnose1", "1610", "(Respiratoir) Kinkhoest")
                ("diagnose1", "1620", "(Respiratoir) Longtransplantatie, Status na")
                ("diagnose1", "1630", "(Respiratoir) Voorbijgaande Tachypnoe van de Pasgeborene")
                ("diagnose1", "1632", "(Respiratoir) Atelectase")
                ("diagnose1", "1410", "(Respiratoir) Ademhaling - Overige")
                ("diagnose1", "1660", "(Renaal) Hemolytisch Uremisch Syndroom")
                ("diagnose1", "1670", "(Renaal) Nefrotisch en/of Nefritisch Syndroom")
                ("diagnose1", "1680", "(Renaal) Nierfalen - Acuut")
                ("diagnose1", "1690", "(Renaal) Nierfalen - Chronisch")
                ("diagnose1", "1700", "(Renaal) Niertransplantatie, Status na")
                ("diagnose1", "1710", "(Renaal) Urineweg Infectie")
                ("diagnose1", "1712", "(Renaal) Hydronefrose")
                ("diagnose1", "1650", "(Renaal) Renaal - Overige")
                ("diagnose1", "1740", "(MDL) Darmobstructie")
                ("diagnose1", "1750", "(MDL) Colitis")
                ("diagnose1", "1760", "(MDL) Gastroenteritis")
                ("diagnose1", "1770", "(MDL) Gastrointestinale Bloeding")
                ("diagnose1", "1780", "(MDL) Gastroschizis of Exomphalus")
                ("diagnose1", "1790", "(MDL) Hepatitis")
                ("diagnose1", "1800", "(MDL) Invaginatie")
                ("diagnose1", "1820", "(MDL) Leverfalen - Acuut")
                ("diagnose1", "1830", "(MDL) Leverfalen - Chronisch")
                ("diagnose1", "1810", "(MDL) Leverziekte - Overige")
                ("diagnose1", "1840", "(MDL) Necrotiserende Enterocolitis")
                ("diagnose1", "1850", "(MDL) Oesofagus Atresie")
                ("diagnose1", "1860", "(MDL) Pancreatitis")
                ("diagnose1", "1870", "(MDL) Peritonitis")
                ("diagnose1", "1880", "(MDL) Pylorus Stenose")
                ("diagnose1", "1890", "(MDL) Short Bowel Syndroom")
                ("diagnose1", "1900", "(MDL) Ulcus - Duodenum")
                ("diagnose1", "1910", "(MDL) Ulcus - Maag of Gastritis")
                ("diagnose1", "1920", "(MDL) Varices - Oesofagus of Maag")
                ("diagnose1", "1930", "(MDL) Galgang Atresie")
                ("diagnose1", "1940", "(MDL) Darmperforatie")
                ("diagnose1", "1950", "(MDL) Hirschsprung, Ziekte van")
                ("diagnose1", "1960", "(MDL) Neonatale Icterus")
                ("diagnose1", "1970", "(MDL) Oesofageaal Vreemdlichaam")
                ("diagnose1", "1980", "(MDL) Portale Hyperternsie")
                ("diagnose1", "1990", "(MDL) Levertransplantatie, Status na")
                ("diagnose1", "2000", "(MDL) Volvulus")
                ("diagnose1", "2002", "(MDL) Gastro-oesophagiale reflux")
                ("diagnose1", "2004", "(MDL) Veno Occlusive Disease (VOD)")
                ("diagnose1", "2006", "(MDL) Chylus Effusie")
                ("diagnose1", "1730", "(MDL) Gastrointestinaal, Overige")
                ("diagnose1", "2400", "(Diversen) ALTE")
                ("diagnose1", "2410", "(Diversen) Hartstilstand - In Ziekenhuis")
                ("diagnose1", "2420", "(Diversen) Hartstilstand - Buiten Ziekenhuis")
                ("diagnose1", "2430", "(Diversen) Chromosoom Afwijking")
                ("diagnose1", "2440", "(Diversen) Stollingsstoornis")
                ("diagnose1", "2450", "(Diversen) Dehydratie")
                ("diagnose1", "2460", "(Diversen) Dermatologische Afwijking")
                ("diagnose1", "2470", "(Diversen) Diabetes Insipidus")
                ("diagnose1", "2480", "(Diversen) Diabetes Mellitus met Ketoacidose")
                ("diagnose1", "2490", "(Diversen) Diabetes Mellitus zonder Ketoacidose")
                ("diagnose1", "2500", "(Diversen) Electroliet verstoring")
                ("diagnose1", "2510", "(Diversen) Endocriene Afwijking")
                ("diagnose1", "2520", "(Diversen) Gasgangreen")
                ("diagnose1", "2530", "(Diversen) Thuisbeademing Patient")
                ("diagnose1", "2540", "(Diversen) Hypoglycemie")
                ("diagnose1", "2550", "(Diversen) IC Diagnostische Monitoring - Electief")
                ("diagnose1", "2560", "(Diversen) IC Procedure")
                ("diagnose1", "2570", "(Diversen) Immunodeficientie - Congenitaal")
                ("diagnose1", "2580", "(Diversen) Immunosuppressie - Verworven")
                ("diagnose1", "2590", "(Diversen) Stofwisselingsziekte, Aangeboren")
                ("diagnose1", "2600", "(Diversen) Leukemie of Lymfoom")
                ("diagnose1", "2610", "(Diversen) Necrotiserende Fasciitis")
                ("diagnose1", "2620", "(Diversen) Neutropenie")
                ("diagnose1", "2630", "(Diversen) Pancytopenie")
                ("diagnose1", "2640", "(Diversen) Feochromocytoom")
                ("diagnose1", "2650", "(Diversen) Prematuriteit (< 37/4 & < 12 mnd)")
                ("diagnose1", "2660", "(Diversen) Ademstilstand - In Ziekenhuis")
                ("diagnose1", "2670", "(Diversen) Ademstilstand - Buiten Ziekenhuis")
                ("diagnose1", "2680", "(Diversen) Sepsis")
                ("diagnose1", "2690", "(Diversen) Shock - Cardiogeen")
                ("diagnose1", "2700", "(Diversen) Shock - Hypovolemisch")
                ("diagnose1", "2710", "(Diversen) Shock - Septisch")
                ("diagnose1", "2685", "(Diversen) Shock (ongespecificeerd)")
                ("diagnose1", "2720", "(Diversen) SIRS")
                ("diagnose1", "2730", "(Diversen) Neoplasma Solide Orgaan - Maligne")
                ("diagnose1", "2740", "(Diversen) Neoplasma Solide Orgaan -Niet Maligne")
                ("diagnose1", "2750", "(Diversen) Syndroom of Malformatie")
                ("diagnose1", "2760", "(Diversen) Toxisch Shock Syndroom")
                ("diagnose1", "2770", "(Diversen) Beenmergtransplantatie")
                ("diagnose1", "2780", "(Diversen) Craniosynostose")
                ("diagnose1", "2790", "(Diversen) Hydrops Foetalis")
                ("diagnose1", "2800", "(Diversen) Neonaat - Kind van Diabetische Moeder")
                ("diagnose1", "2810", "(Diversen) Neonaat - Intrauteriene Groeivertraging")
                ("diagnose1", "2820", "(Diversen) Beenmergtransplantatie, Status na")
                ("diagnose1", "2830", "(Diversen) Scoliose")
                ("diagnose1", "2840", "(Diversen) Tumorlysis Syndroom")
                ("diagnose1", "2850", "(Diversen) Wondinfectie")
                ("diagnose1", "2860", "(Diversen) Hematologische Ziekte")
                ("diagnose1", "2870", "(Diversen) Orgaandonatie")
                ("diagnose1", "2871", "(Diversen) Graft vs Host disease")
                ("diagnose1", "2872", "(Diversen) Cystic Hygroma")
                ("diagnose1", "2873", "(Diversen) Ademstilstand (op de PICU)")
                ("diagnose1", "2878", "(Diversen) Hartstilstand op PICU")
                ("diagnose1", "2874", "(Diversen) Down syndroom")
                ("diagnose1", "2875", "(Diversen) Velo Cardio Faciaal Syndroom (22q11.2)")
                ("diagnose1", "2876", "(Diversen) Diabetes (comorbiditeit)")
                ("diagnose1", "2877", "(Diversen) SIADH")
                ("diagnose1", "6000", "(Diversen) Osteomyelitis")
                ("diagnose1", "6001", "(Diversen) Rhabdomyolysis")
                ("diagnose1", "2390", "(Diversen) Diversen - Overige")
                ("diagnose1", "3010", "(Chir.-Div/Anesth) Anesthetische Complicatie")
                ("diagnose1", "3020", "(Chir.-Div/Anesth) Hartkatheterisatie")
                ("diagnose1", "3030", "(Chir.-Div/Anesth) Ex-prematuur, na Algehele Anesthesie")
                ("diagnose1", "3040", "(Chir.-Div/Anesth) Invasieve Radiologie Procedure")
                ("diagnose1", "3050", "(Chir.-Div/Anesth) Massale Intraoperatieve Transfusie")
                ("diagnose1", "3060", "(Chir.-Div/Anesth) Hartkatheterisatie - Ballon Septostomie")
                ("diagnose1", "3070", "(Chir.-Div/Anesth) Hartkatheterisatie - Interventie")
                ("diagnose1", "3075", "(Chir.-Div/Anesth) Postoperatieve bloeding")
                ("diagnose1", "3000", "(Chir.-Div/Anesth) Na Procedure - Overige")
                ("diagnose1", "3450", "(Neurochirurgie) Craniotomie - Fossa Anterior")
                ("diagnose1", "3460", "(Neurochirurgie) Craniotomie - Fossa Posterior")
                ("diagnose1", "3470", "(Neurochirurgie) CZS Shunt - Insertie of revisie")
                ("diagnose1", "3480", "(Neurochirurgie) Decompressie - Craniaal")
                ("diagnose1", "3490", "(Neurochirurgie) Decompressie - Myelum")
                ("diagnose1", "3500", "(Neurochirurgie) Hemisferectomie of Lobectomie")
                ("diagnose1", "3510", "(Neurochirurgie) ICP monitor of Ventrikeldrain Insertie")
                ("diagnose1", "3520", "(Neurochirurgie) Intracraniele Bloeding, Evacuatie")
                ("diagnose1", "3522", "(Neurochirurgie) Myelomeningocoele reparatie")
                ("diagnose1", "3524", "(Neurochirurgie) Decompressie - Craniocervicaal")
                ("diagnose1", "3440", "(Neurochirurgie) Neurochirurgie - Overige")
                ("diagnose1", "3550", "(Thoraxchirurgie) Diafragma Pliceren")
                ("diagnose1", "3560", "(Thoraxchirurgie) Diafragma Herstel")
                ("diagnose1", "3570", "(Thoraxchirurgie) Longbiopsie")
                ("diagnose1", "3580", "(Thoraxchirurgie) Long Decorticatie")
                ("diagnose1", "3590", "(Thoraxchirurgie) Oesofagus Atresie Herstel")
                ("diagnose1", "3600", "(Thoraxchirurgie) Pneumectomie of Lobectomie")
                ("diagnose1", "3610", "(Thoraxchirurgie) Thoracale Tumor, Resectie")
                ("diagnose1", "3620", "(Thoraxchirurgie) Tracheo-oesofageale Fistel, Herstel")
                ("diagnose1", "3630", "(Thoraxchirurgie) Tracheopexie")
                ("diagnose1", "3632", "(Thoraxchirurgie) VATS procedure")
                ("diagnose1", "3540", "(Thoraxchirurgie) Thoraxchirurgie - Overige")
                ("diagnose1", "3660", "(KNO chirurgie) Adenotomie en/of Tonsillectomie")
                ("diagnose1", "3670", "(KNO chirurgie) Choanen Atresie, Herstel")
                ("diagnose1", "3680", "(KNO chirurgie) Cricoid Split")
                ("diagnose1", "3690", "(KNO chirurgie) Laryngeale Reconstructie")
                ("diagnose1", "3700", "(KNO chirurgie) Laryngobronchoscopie")
                ("diagnose1", "3710", "(KNO chirurgie) Tracheostomie")
                ("diagnose1", "3712", "(KNO chirurgie) Retopharyngeal abces drainage")
                ("diagnose1", "3714", "(KNO chirurgie) Laryngoplastie")
                ("diagnose1", "3715", "(KNO chirurgie) Trachea reconstructie/ Tracheaplastiek")
                ("diagnose1", "3650", "(KNO chirurgie) KNO - Overige")
                ("diagnose1", "3740", "(Alg. chirurgie) Abdominale Tumor, Resectie")
                ("diagnose1", "3750", "(Alg. chirurgie) Appendectomie")
                ("diagnose1", "3760", "(Alg. chirurgie) Extrophia Vesicae, Herstel")
                ("diagnose1", "3770", "(Alg. chirurgie) Brandwonden Chirurgie")
                ("diagnose1", "3780", "(Alg. chirurgie) Fundoplicatie")
                ("diagnose1", "3790", "(Alg. chirurgie) Gastroschizis of Exomphalos Herstel")
                ("diagnose1", "3800", "(Alg. chirurgie) GI endoscopie en/of Sclerotherapie")
                ("diagnose1", "3810", "(Alg. chirurgie) Intussusceptie Herstel")
                ("diagnose1", "3820", "(Alg. chirurgie) Kasai")
                ("diagnose1", "3830", "(Alg. chirurgie) Laparotomie - Overige")
                ("diagnose1", "3840", "(Alg. chirurgie) Transplantatie - Nier")
                ("diagnose1", "3850", "(Alg. chirurgie) Transplantatie - Lever")
                ("diagnose1", "3860", "(Alg. chirurgie) Transplantatie - Dunne Darm")
                ("diagnose1", "3870", "(Alg. chirurgie) Urogenitale Chirurgie - Overige")
                ("diagnose1", "3880", "(Alg. chirurgie) Laparotomie - Darmobstructie")
                ("diagnose1", "3890", "(Alg. chirurgie) Laparotomie - Darmperforatie")
                ("diagnose1", "3900", "(Alg. chirurgie) Laparotomie - GI Bloeding")
                ("diagnose1", "3910", "(Alg. chirurgie) Laparotomie - Necrotiserende Enterocolitis")
                ("diagnose1", "3920", "(Alg. chirurgie) Laparotomie - Peritonitis")
                ("diagnose1", "3930", "(Alg. chirurgie) Laparotomie - Trauma")
                ("diagnose1", "3931", "(Alg. chirurgie) Aorta chirurgie (Buik)")
                ("diagnose1", "3932", "(Alg. chirurgie) Wound Debridement")
                ("diagnose1", "3730", "(Alg. chirurgie) Algemene Chirurgie - Overige")
                ("diagnose1", "3960", "(Craniofaciale chir.) Schedel - Reconstructie")
                ("diagnose1", "3970", "(Craniofaciale chir.) Dentale Chirurgie")
                ("diagnose1", "3980", "(Craniofaciale chir.) Cheiloschizis herstel")
                ("diagnose1", "3990", "(Craniofaciale chir.) Mandibulaire Mobilisatie")
                ("diagnose1", "4000", "(Craniofaciale chir.) Midface Mobilisatie")
                ("diagnose1", "4010", "(Craniofaciale chir.) Palatoschizis Herstel")
                ("diagnose1", "3950", "(Craniofaciale chir.) Craniofaciale Chirurgie - Overige")
                ("diagnose1", "4040", "(Orthoped. chir.) Fractuur Fixatie")
                ("diagnose1", "4050", "(Orthoped. chir.) Spinale Instrumentatie")
                ("diagnose1", "4030", "(Orthoped. chir.) Orthopedische Chirurgie - Overige")
                ("diagnose1", "5040", "(Hartchirurgie) Annuloplasty")
                ("diagnose1", "5050", "(Hartchirurgie) Coronair arterie herstel met intrapulmonaire tunnel (Takeuchi)")
                ("diagnose1", "5060", "(Hartchirurgie) Coronair arterie herstel zonder intrapulmonaire tunnel")
                ("diagnose1", "5070", "(Hartchirurgie) Pulmonale arterie - reimplantatie")
                ("diagnose1", "5080", "(Hartchirurgie) Aorta klep vervanging")
                ("diagnose1", "5110", "(Hartchirurgie) Aortopexie")
                ("diagnose1", "5120", "(Hartchirurgie) Aortoplastie (not arch repair or graft)")
                ("diagnose1", "5130", "(Hartchirurgie) AP window repair")
                ("diagnose1", "5140", "(Hartchirurgie) Arterial switch operation")
                ("diagnose1", "5150", "(Hartchirurgie) Arterial switch operation with pulmonary artery band removal")
                ("diagnose1", "5160", "(Hartchirurgie) Arterial switch operation with repair of sub PS")
                ("diagnose1", "5170", "(Hartchirurgie) Arterial switch operation with VSD closure")
                ("diagnose1", "5180", "(Hartchirurgie) ASD and VSD repair")
                ("diagnose1", "5190", "(Hartchirurgie) ASD primum repair")
                ("diagnose1", "5200", "(Hartchirurgie) ASD surgery (including ASD secundum, sinus venosus ASD, patent forament ovale closure)")
                ("diagnose1", "5210", "(Hartchirurgie) Atrial septectomy")
                ("diagnose1", "5220", "(Hartchirurgie) Atrial switch operation")
                ("diagnose1", "5230", "(Hartchirurgie) Atrial switch operation with repair of sub PS")
                ("diagnose1", "5240", "(Hartchirurgie) Atrial switch operation with VSD closure")
                ("diagnose1", "5260", "(Hartchirurgie) Coarction repair > 30 dgn")
                ("diagnose1", "5270", "(Hartchirurgie) Coarction repair <= 30 dgn")
                ("diagnose1", "5290", "(Hartchirurgie) Common atrium Closure")
                ("diagnose1", "5310", "(Hartchirurgie) Cor triatriatum repair")
                ("diagnose1", "5320", "(Hartchirurgie) Coronary AV fistula repair")
                ("diagnose1", "5330", "(Hartchirurgie) Damus-Kaye-Stansel procedure")
                ("diagnose1", "5340", "(Hartchirurgie) Double switch")
                ("diagnose1", "5350", "(Hartchirurgie) Double-outlet right ventricle repair with or without repair of right ventricular obstruction")
                ("diagnose1", "5360", "(Hartchirurgie) Fontan procedure")
                ("diagnose1", "5370", "(Hartchirurgie) Glenn shunt")
                ("diagnose1", "5380", "(Hartchirurgie) Hypoplastic or interrupted arch repair with VSD closure")
                ("diagnose1", "5390", "(Hartchirurgie) Hypoplastic or interrupted arch repair without VSD closure")
                ("diagnose1", "5400", "(Hartchirurgie) Intracardiac tumour excision")
                ("diagnose1", "5410", "(Hartchirurgie) Konno procedure")
                ("diagnose1", "5420", "(Hartchirurgie) Left ventricular outflow tract patch")
                ("diagnose1", "5430", "(Hartchirurgie) Left ventricular to pulmonary artery conduit")
                ("diagnose1", "5440", "(Hartchirurgie) Left ventricular to right atrial shunt repair")
                ("diagnose1", "5450", "(Hartchirurgie) Mitral valve replacement")
                ("diagnose1", "5470", "(Hartchirurgie) Pacemaker insertion/replacement")
                ("diagnose1", "5480", "(Hartchirurgie) Partially anomalous pulmonary venous connection surgery")
                ("diagnose1", "5490", "(Hartchirurgie) PDA surgery >30d of age")
                ("diagnose1", "5500", "(Hartchirurgie) PDA surgery ≤30d of age")
                ("diagnose1", "5510", "(Hartchirurgie) Pulmonary artery banding")
                ("diagnose1", "5520", "(Hartchirurgie) Pulmonary artery stenosis repair")
                ("diagnose1", "5530", "(Hartchirurgie) Pulmonary outflow tract augmentation")
                ("diagnose1", "5540", "(Hartchirurgie) Pulmonary valve replacement")
                ("diagnose1", "5550", "(Hartchirurgie) Right ventricular infundibulectomy")
                ("diagnose1", "5560", "(Hartchirurgie) Right ventricular to pulmonary artery conduit")
                ("diagnose1", "5570", "(Hartchirurgie) Ross procedure")
                ("diagnose1", "5580", "(Hartchirurgie) Semilunar valve closure")
                ("diagnose1", "5590", "(Hartchirurgie) Septal defect repair (unspecified)")
                ("diagnose1", "5600", "(Hartchirurgie) Stage 1 repair of hypoplatsic left heart syndrome (Norwood)")
                ("diagnose1", "5610", "(Hartchirurgie) Stage 1 repair of nonhypoplastic left heart syndrome conditions")
                ("diagnose1", "5620", "(Hartchirurgie) Subaortic stenosis resection")
                ("diagnose1", "5630", "(Hartchirurgie) Systemic to pulmonary artery shunt")
                ("diagnose1", "5640", "(Hartchirurgie) Tetralogy of Fallot total repair")
                ("diagnose1", "5650", "(Hartchirurgie) Tetralogy of Fallot with pulmonary atresia repair")
                ("diagnose1", "5660", "(Hartchirurgie) Total repair of anamalous pulmonary veins >30d of age")
                ("diagnose1", "5670", "(Hartchirurgie) Total repair of anamalous pulmonary veins ≤30d of age")
                ("diagnose1", "5680", "(Hartchirurgie) Transection of pulmonary artery")
                ("diagnose1", "5690", "(Hartchirurgie) Transitional or complete atrioventricular canal repair with or without valve replacement")
                ("diagnose1", "5700", "(Hartchirurgie) Transplant – Heart")
                ("diagnose1", "5710", "(Hartchirurgie) Transplant – Heart Lung")
                ("diagnose1", "5720", "(Hartchirurgie) Transplant – Lung")
                ("diagnose1", "5730", "(Hartchirurgie) Transposition-VSD sub PS repair (Rastelli)")
                ("diagnose1", "5740", "(Hartchirurgie) Transverse arch graft")
                ("diagnose1", "5750", "(Hartchirurgie) Tricuspid valve replacement")
                ("diagnose1", "5760", "(Hartchirurgie) Tricuspid valve repositioning for Ebstein anomaly >30d of age")
                ("diagnose1", "5770", "(Hartchirurgie) Tricuspid valve repositioning for neonatal Ebstein ≤30d of age")
                ("diagnose1", "5780", "(Hartchirurgie) Truncus arteriosus and interrupted arch repair")
                ("diagnose1", "5790", "(Hartchirurgie) Truncus arteriosus repair")
                ("diagnose1", "5800", "(Hartchirurgie) Unifocalization for tetralogy of Fallot - pulmonary atresia")
                ("diagnose1", "5810", "(Hartchirurgie) Valvectomy of tricuspid valve")
                ("diagnose1", "5820", "(Hartchirurgie) Valvotomy - valvuloplasty (Aortic) ≤30d of age")
                ("diagnose1", "5830", "(Hartchirurgie) Valvotomy - valvuloplasty (Aortic) >30d of age")
                ("diagnose1", "5840", "(Hartchirurgie) Valvotomy - valvuloplasty (Mitral)")
                ("diagnose1", "5850", "(Hartchirurgie) Valvotomy - valvuloplasty (Pulmonary)")
                ("diagnose1", "5860", "(Hartchirurgie) Valvotomy - valvuloplasty (Tricuspid)")
                ("diagnose1", "5870", "(Hartchirurgie) Vascular ring surgery")
                ("diagnose1", "5880", "(Hartchirurgie) Ventriculomyotomy")
                ("diagnose1", "5900", "(Hartchirurgie) VSD closure and coarction repair")
                ("diagnose1", "5910", "(Hartchirurgie) VSD closure and pulmonary artery band removal")
                ("diagnose1", "5920", "(Hartchirurgie) VSD closure and pulmonary valvotomy or infundibular resection")
                ("diagnose1", "5930", "(Hartchirurgie) VSD enlargement for repair of complex anomaly (single ventricle)")
                ("diagnose1", "5940", "(Hartchirurgie) VSD repair")
                ("diagnose1", "5950", "(Hartchirurgie) Chest closure")
                ("diagnose1", "5960", "(Hartchirurgie) ECMO cannulation/exploration")
                ("diagnose1", "5970", "(Hartchirurgie) Emergency chest opening")
                ("diagnose1", "5980", "(Hartchirurgie) Pulmonary venous stenosis repair")
                ("diagnose1", "5020", "(Hartchirurgie) Hartoperatie Gesloten - Overige")
                ("diagnose1", "5030", "(Hartchirurgie) Hartoperatie Open - Overige")
                ("diagnose2", "40", "(Letsel) Anafylaxie")
                ("diagnose2", "50", "(Letsel) Brandwonden")
                ("diagnose2", "60", "(Letsel) Koolmonoxide vergiftiging")
                ("diagnose2", "70", "(Letsel) Geneesmiddel toxiciteit-iatrogeen")
                ("diagnose2", "80", "(Letsel) Electrocutie")
                ("diagnose2", "90", "(Letsel) Vergiftiging")
                ("diagnose2", "100", "(Letsel) Verhanging of verwurging")
                ("diagnose2", "110", "(Letsel) Hyperthermie")
                ("diagnose2", "120", "(Letsel) Hypothermie")
                ("diagnose2", "130", "(Letsel) Verdrinking")
                ("diagnose2", "150", "(Letsel) Rook inhalatie")
                ("diagnose2", "170", "(Letsel) Trauma - Abdomen")
                ("diagnose2", "180", "(Letsel) Trauma - Borstkas")
                ("diagnose2", "190", "(Letsel) Trauma - Gelaat")
                ("diagnose2", "200", "(Letsel) Trauma - Hoofd")
                ("diagnose2", "210", "(Letsel) Trauma - Skelet")
                ("diagnose2", "220", "(Letsel) Trauma - Wervelkolom")
                ("diagnose2", "160", "(Letsel) Trauma - Overig")
                ("diagnose2", "142", "(Letsel) Ingestie - drug")
                ("diagnose2", "144", "(Letsel) Ingestie - non drug")
                ("diagnose2", "30", "(Letsel) Letsel/vergiftiging/externe oorzaak-overig")
                ("diagnose2", "400", "(Cardiovasculair) Afwezige Pulmonaal Klep")
                ("diagnose2", "410", "(Cardiovasculair) Anomalie Coronair Arterie")
                ("diagnose2", "420", "(Cardiovasculair) Aorta-insufficientie")
                ("diagnose2", "430", "(Cardiovasculair) Aortastenose")
                ("diagnose2", "440", "(Cardiovasculair) ASD")
                ("diagnose2", "450", "(Cardiovasculair) AVSD")
                ("diagnose2", "460", "(Cardiovasculair) Coarctatio aortae")
                ("diagnose2", "470", "(Cardiovasculair) Cor triatriatum")
                ("diagnose2", "480", "(Cardiovasculair) Ebstein anomalie")
                ("diagnose2", "490", "(Cardiovasculair) Hypoplastisch Linker Hart Syndroom")
                ("diagnose2", "500", "(Cardiovasculair) Interruptie of Hypoplastische Aortaboog")
                ("diagnose2", "510", "(Cardiovasculair) Mitraal Insufficientie")
                ("diagnose2", "520", "(Cardiovasculair) Mitraal Stenose")
                ("diagnose2", "530", "(Cardiovasculair) Open Ductus Botalli")
                ("diagnose2", "540", "(Cardiovasculair) Pulmonaal Atresie of Stenose")
                ("diagnose2", "550", "(Cardiovasculair) Monoventrikel")
                ("diagnose2", "560", "(Cardiovasculair) Totaal Abnornaal Pulmonaal Veneuze Return")
                ("diagnose2", "570", "(Cardiovasculair) Tetralogie van Fallot")
                ("diagnose2", "580", "(Cardiovasculair) Transpositie Grote Arterien")
                ("diagnose2", "590", "(Cardiovasculair) Tricuspidaal Atresie of Stenose")
                ("diagnose2", "600", "(Cardiovasculair) Tricuspidaal Insufficientie")
                ("diagnose2", "610", "(Cardiovasculair) Truncus Arteriosus")
                ("diagnose2", "620", "(Cardiovasculair) VSD")
                ("diagnose2", "630", "(Cardiovasculair) Aortopulmonaal Venster")
                ("diagnose2", "640", "(Cardiovasculair) Arterioveneuze Malformatie")
                ("diagnose2", "650", "(Cardiovasculair) Double Outlet Rechter Ventrikel")
                ("diagnose2", "660", "(Cardiovasculair) Linker Ventrikel Uitstroom Obstructie")
                ("diagnose2", "670", "(Cardiovasculair) Pulmonaal Insufficientie")
                ("diagnose2", "680", "(Cardiovasculair) Rechter Ventrikel Uitstroom Obstructie")
                ("diagnose2", "682", "(Cardiovasculair) Hypoplastische Li ventrikel (géén HLHS)")
                ("diagnose2", "684", "(Cardiovasculair) Hypoplastische Re ventrikel")
                ("diagnose2", "686", "(Cardiovasculair) l-TGA (Levo Transpositie grote arterien)")
                ("diagnose2", "390", "(Cardiovasculair) Cardiovasculair - Congenitaal - Overige")
                ("diagnose2", "710", "(Cardiovasculair) Hartfalen")
                ("diagnose2", "720", "(Cardiovasculair) Cardiale Tumor")
                ("diagnose2", "730", "(Cardiovasculair) Cardiomyopathie/Myocarditis")
                ("diagnose2", "740", "(Cardiovasculair) Dysritmie - Supraventriculair")
                ("diagnose2", "750", "(Cardiovasculair) Dysritmie - Ventriculair")
                ("diagnose2", "760", "(Cardiovasculair) Endocarditis")
                ("diagnose2", "770", "(Cardiovasculair) Hypertensie - Pulmonaal")
                ("diagnose2", "780", "(Cardiovasculair) Hypertensie - Systemisch")
                ("diagnose2", "790", "(Cardiovasculair) Kawasaki, Ziekte van")
                ("diagnose2", "800", "(Cardiovasculair) Pericardiale Effusie of Tamponade")
                ("diagnose2", "810", "(Cardiovasculair) Vasculaire Trombose")
                ("diagnose2", "820", "(Cardiovasculair) Vasculitis")
                ("diagnose2", "830", "(Cardiovasculair) Hart-Long Transplantatie, Status na")
                ("diagnose2", "840", "(Cardiovasculair) Harttransplantatie, Status na")
                ("diagnose2", "842", "(Cardiovasculair) Rheumatic Heart Disease")
                ("diagnose2", "844", "(Cardiovasculair) Hartoperatie in het verleden")
                ("diagnose2", "846", "(Cardiovasculair) Longembolie")
                ("diagnose2", "848", "(Cardiovasculair) Myocard (infarct/Ischemie)")
                ("diagnose2", "700", "(Cardiovasculair) Cardiovasculair - Verworven- Overige")
                ("diagnose2", "870", "(Neurologisch) Botulisme")
                ("diagnose2", "880", "(Neurologisch) Hersenabces")
                ("diagnose2", "890", "(Neurologisch) Hersen AV Malformatie")
                ("diagnose2", "900", "(Neurologisch) Hersendood")
                ("diagnose2", "910", "(Neurologisch) Herseninfarct of Beroerte")
                ("diagnose2", "920", "(Neurologisch) Hersentumor")
                ("diagnose2", "930", "(Neurologisch) CZS Shunt Dysfunctie of Infectie")
                ("diagnose2", "940", "(Neurologisch) Encefalitis")
                ("diagnose2", "950", "(Neurologisch) Encefalopathie, Acuut - Hypoxisch Ischaemisch")
                ("diagnose2", "960", "(Neurologisch) Encefalopatie, Acuut - Overige")
                ("diagnose2", "970", "(Neurologisch) Encefalopatie, Chronisch Degeneratief")
                ("diagnose2", "980", "(Neurologisch) Encefalopatie, Chronisch Statisch")
                ("diagnose2", "990", "(Neurologisch) Guillain Barre Syndroom")
                ("diagnose2", "1000", "(Neurologisch) Hydrocefalus")
                ("diagnose2", "1011", "(Neurologisch) Intracraniele Bloeding (spontaan)")
                ("diagnose2", "1020", "(Neurologisch) Intracraniele Hypertensie")
                ("diagnose2", "1030", "(Neurologisch) Meningitis")
                ("diagnose2", "1040", "(Neurologisch) Meningomyelocele of Spina Bifida")
                ("diagnose2", "1050", "(Neurologisch) Myopathie")
                ("diagnose2", "1060", "(Neurologisch) Neuropathie")
                ("diagnose2", "1070", "(Neurologisch) Convulsies")
                ("diagnose2", "1080", "(Neurologisch) Myelum, Beschadiging")
                ("diagnose2", "1090", "(Neurologisch) Veneuze Sinus Trombose")
                ("diagnose2", "1100", "(Neurologisch) Cerebraal Aneurysma")
                ("diagnose2", "1110", "(Neurologisch) Spierdystrofie")
                ("diagnose2", "1120", "(Neurologisch) Myastenia Gravis")
                ("diagnose2", "1130", "(Neurologisch) Tetanus")
                ("diagnose2", "1135", "(Neurologisch) Arnold Chiari Malformation")
                ("diagnose2", "1136", "(Neurologisch) Hersencyste")
                ("diagnose2", "1137", "(Neurologisch) Epilepsie (comorbiditeit)")
                ("diagnose2", "1138", "(Neurologisch) Hersenoedeem")
                ("diagnose2", "1139", "(Neurologisch) Intracraniele bloeding (Traumatisch)")
                ("diagnose2", "1131", "(Neurologisch) Acute disseminated encephalomyelitis (ADEM)")
                ("diagnose2", "1132", "(Neurologisch) Congenitale hersenafwijking")
                ("diagnose2", "1133", "(Neurologisch) Febrile convulsion")
                ("diagnose2", "1134", "(Neurologisch) Transverse Myelitis")
                ("diagnose2", "860", "(Neurologisch) Neurologisch - Overig")
                ("diagnose2", "1170", "(Respiratoir) Choanen Atresie of Stenose")
                ("diagnose2", "1180", "(Respiratoir) Epiglottitis")
                ("diagnose2", "1190", "(Respiratoir) Vreemdlichaam - Inhalatie")
                ("diagnose2", "1200", "(Respiratoir) Laryngotracheobronchitis")
                ("diagnose2", "1210", "(Respiratoir) Obstructief Slaap Apnoe Syndroom")
                ("diagnose2", "1220", "(Respiratoir) Pierre Robin Syndroom")
                ("diagnose2", "1230", "(Respiratoir) Retrofaryngeaal Abces")
                ("diagnose2", "1240", "(Respiratoir) Subglottische Stenose")
                ("diagnose2", "1250", "(Respiratoir) Tracheitis")
                ("diagnose2", "1260", "(Respiratoir) Bovenste Luchtwegobstructie - Overige")
                ("diagnose2", "1270", "(Respiratoir) Bovenste Luchtweginfectie - Overige")
                ("diagnose2", "1280", "(Respiratoir) Stembandparese")
                ("diagnose2", "1290", "(Respiratoir) Subglottisch Hemangioom")
                ("diagnose2", "1295", "(Respiratoir) Larygomalacia")
                ("diagnose2", "1160", "(Respiratoir) Bovenste Luchtwegen - Overige")
                ("diagnose2", "1320", "(Respiratoir) Astma")
                ("diagnose2", "1330", "(Respiratoir) Bronchiolitis")
                ("diagnose2", "1340", "(Respiratoir) Chronische Longziekte")
                ("diagnose2", "1350", "(Respiratoir) Malacie - Trachea en/of Bronchus")
                ("diagnose2", "1360", "(Respiratoir) Mediastinale Massa")
                ("diagnose2", "1370", "(Respiratoir) Stenose - Trachea en/of Bronchus")
                ("diagnose2", "1380", "(Respiratoir) Tracheo-oesofageale Fistel")
                ("diagnose2", "1390", "(Respiratoir) Vaatring")
                ("diagnose2", "1391", "(Respiratoir) Bronchiectasis")
                ("diagnose2", "1310", "(Respiratoir) Lage luchtwegen - Overige")
                ("diagnose2", "1420", "(Respiratoir) Luchtlek Syndroom")
                ("diagnose2", "1430", "(Respiratoir) Apnoe - Centraal")
                ("diagnose2", "1440", "(Respiratoir) ARDS")
                ("diagnose2", "1450", "(Respiratoir) Aspiratie")
                ("diagnose2", "1460", "(Respiratoir) Chylothorax")
                ("diagnose2", "1470", "(Respiratoir) Congenitale Hernia Diafragmatic")
                ("diagnose2", "1480", "(Respiratoir) Congenitale Longafwijking")
                ("diagnose2", "1490", "(Respiratoir) Cystic Fibrosis")
                ("diagnose2", "1500", "(Respiratoir) Empyeem")
                ("diagnose2", "1510", "(Respiratoir) Hyaliene Membraanziekte")
                ("diagnose2", "1520", "(Respiratoir) Hypoventilatie - Centraal")
                ("diagnose2", "1530", "(Respiratoir) Longabces")
                ("diagnose2", "1540", "(Respiratoir) Meconium Aspiratie Syndroom")
                ("diagnose2", "1550", "(Respiratoir) Pleura Effusie")
                ("diagnose2", "1560", "(Respiratoir) Pneumonie of Pneumonitis")
                ("diagnose2", "1570", "(Respiratoir) Long Hypoplasie")
                ("diagnose2", "1580", "(Respiratoir) Pulmonaal Oedeem")
                ("diagnose2", "1590", "(Respiratoir) Respiratoir Falen")
                ("diagnose2", "1600", "(Respiratoir) Lage Luchtweginfectie - Overige")
                ("diagnose2", "1610", "(Respiratoir) Kinkhoest")
                ("diagnose2", "1620", "(Respiratoir) Longtransplantatie, Status na")
                ("diagnose2", "1630", "(Respiratoir) Voorbijgaande Tachypnoe van de Pasgeborene")
                ("diagnose2", "1632", "(Respiratoir) Atelectase")
                ("diagnose2", "1410", "(Respiratoir) Ademhaling - Overige")
                ("diagnose2", "1660", "(Renaal) Hemolytisch Uremisch Syndroom")
                ("diagnose2", "1670", "(Renaal) Nefrotisch en/of Nefritisch Syndroom")
                ("diagnose2", "1680", "(Renaal) Nierfalen - Acuut")
                ("diagnose2", "1690", "(Renaal) Nierfalen - Chronisch")
                ("diagnose2", "1700", "(Renaal) Niertransplantatie, Status na")
                ("diagnose2", "1710", "(Renaal) Urineweg Infectie")
                ("diagnose2", "1712", "(Renaal) Hydronefrose")
                ("diagnose2", "1650", "(Renaal) Renaal - Overige")
                ("diagnose2", "1740", "(MDL) Darmobstructie")
                ("diagnose2", "1750", "(MDL) Colitis")
                ("diagnose2", "1760", "(MDL) Gastroenteritis")
                ("diagnose2", "1770", "(MDL) Gastrointestinale Bloeding")
                ("diagnose2", "1780", "(MDL) Gastroschizis of Exomphalus")
                ("diagnose2", "1790", "(MDL) Hepatitis")
                ("diagnose2", "1800", "(MDL) Invaginatie")
                ("diagnose2", "1820", "(MDL) Leverfalen - Acuut")
                ("diagnose2", "1830", "(MDL) Leverfalen - Chronisch")
                ("diagnose2", "1810", "(MDL) Leverziekte - Overige")
                ("diagnose2", "1840", "(MDL) Necrotiserende Enterocolitis")
                ("diagnose2", "1850", "(MDL) Oesofagus Atresie")
                ("diagnose2", "1860", "(MDL) Pancreatitis")
                ("diagnose2", "1870", "(MDL) Peritonitis")
                ("diagnose2", "1880", "(MDL) Pylorus Stenose")
                ("diagnose2", "1890", "(MDL) Short Bowel Syndroom")
                ("diagnose2", "1900", "(MDL) Ulcus - Duodenum")
                ("diagnose2", "1910", "(MDL) Ulcus - Maag of Gastritis")
                ("diagnose2", "1920", "(MDL) Varices - Oesofagus of Maag")
                ("diagnose2", "1930", "(MDL) Galgang Atresie")
                ("diagnose2", "1940", "(MDL) Darmperforatie")
                ("diagnose2", "1950", "(MDL) Hirschsprung, Ziekte van")
                ("diagnose2", "1960", "(MDL) Neonatale Icterus")
                ("diagnose2", "1970", "(MDL) Oesofageaal Vreemdlichaam")
                ("diagnose2", "1980", "(MDL) Portale Hyperternsie")
                ("diagnose2", "1990", "(MDL) Levertransplantatie, Status na")
                ("diagnose2", "2000", "(MDL) Volvulus")
                ("diagnose2", "2002", "(MDL) Gastro-oesophagiale reflux")
                ("diagnose2", "2004", "(MDL) Veno Occlusive Disease (VOD)")
                ("diagnose2", "2006", "(MDL) Chylus Effusie")
                ("diagnose2", "1730", "(MDL) Gastrointestinaal, Overige")
                ("diagnose2", "2400", "(Diversen) ALTE")
                ("diagnose2", "2410", "(Diversen) Hartstilstand - In Ziekenhuis")
                ("diagnose2", "2420", "(Diversen) Hartstilstand - Buiten Ziekenhuis")
                ("diagnose2", "2430", "(Diversen) Chromosoom Afwijking")
                ("diagnose2", "2440", "(Diversen) Stollingsstoornis")
                ("diagnose2", "2450", "(Diversen) Dehydratie")
                ("diagnose2", "2460", "(Diversen) Dermatologische Afwijking")
                ("diagnose2", "2470", "(Diversen) Diabetes Insipidus")
                ("diagnose2", "2480", "(Diversen) Diabetes Mellitus met Ketoacidose")
                ("diagnose2", "2490", "(Diversen) Diabetes Mellitus zonder Ketoacidose")
                ("diagnose2", "2500", "(Diversen) Electroliet verstoring")
                ("diagnose2", "2510", "(Diversen) Endocriene Afwijking")
                ("diagnose2", "2520", "(Diversen) Gasgangreen")
                ("diagnose2", "2530", "(Diversen) Thuisbeademing Patient")
                ("diagnose2", "2540", "(Diversen) Hypoglycemie")
                ("diagnose2", "2550", "(Diversen) IC Diagnostische Monitoring - Electief")
                ("diagnose2", "2560", "(Diversen) IC Procedure")
                ("diagnose2", "2570", "(Diversen) Immunodeficientie - Congenitaal")
                ("diagnose2", "2580", "(Diversen) Immunosuppressie - Verworven")
                ("diagnose2", "2590", "(Diversen) Stofwisselingsziekte, Aangeboren")
                ("diagnose2", "2600", "(Diversen) Leukemie of Lymfoom")
                ("diagnose2", "2610", "(Diversen) Necrotiserende Fasciitis")
                ("diagnose2", "2620", "(Diversen) Neutropenie")
                ("diagnose2", "2630", "(Diversen) Pancytopenie")
                ("diagnose2", "2640", "(Diversen) Feochromocytoom")
                ("diagnose2", "2650", "(Diversen) Prematuriteit (< 37/4 & < 12 mnd)")
                ("diagnose2", "2660", "(Diversen) Ademstilstand - In Ziekenhuis")
                ("diagnose2", "2670", "(Diversen) Ademstilstand - Buiten Ziekenhuis")
                ("diagnose2", "2680", "(Diversen) Sepsis")
                ("diagnose2", "2690", "(Diversen) Shock - Cardiogeen")
                ("diagnose2", "2700", "(Diversen) Shock - Hypovolemisch")
                ("diagnose2", "2710", "(Diversen) Shock - Septisch")
                ("diagnose2", "2685", "(Diversen) Shock (ongespecificeerd)")
                ("diagnose2", "2720", "(Diversen) SIRS")
                ("diagnose2", "2730", "(Diversen) Neoplasma Solide Orgaan - Maligne")
                ("diagnose2", "2740", "(Diversen) Neoplasma Solide Orgaan -Niet Maligne")
                ("diagnose2", "2750", "(Diversen) Syndroom of Malformatie")
                ("diagnose2", "2760", "(Diversen) Toxisch Shock Syndroom")
                ("diagnose2", "2770", "(Diversen) Beenmergtransplantatie")
                ("diagnose2", "2780", "(Diversen) Craniosynostose")
                ("diagnose2", "2790", "(Diversen) Hydrops Foetalis")
                ("diagnose2", "2800", "(Diversen) Neonaat - Kind van Diabetische Moeder")
                ("diagnose2", "2810", "(Diversen) Neonaat - Intrauteriene Groeivertraging")
                ("diagnose2", "2820", "(Diversen) Beenmergtransplantatie, Status na")
                ("diagnose2", "2830", "(Diversen) Scoliose")
                ("diagnose2", "2840", "(Diversen) Tumorlysis Syndroom")
                ("diagnose2", "2850", "(Diversen) Wondinfectie")
                ("diagnose2", "2860", "(Diversen) Hematologische Ziekte")
                ("diagnose2", "2870", "(Diversen) Orgaandonatie")
                ("diagnose2", "2871", "(Diversen) Graft vs Host disease")
                ("diagnose2", "2872", "(Diversen) Cystic Hygroma")
                ("diagnose2", "2873", "(Diversen) Ademstilstand (op de PICU)")
                ("diagnose2", "2878", "(Diversen) Hartstilstand op PICU")
                ("diagnose2", "2874", "(Diversen) Down syndroom")
                ("diagnose2", "2875", "(Diversen) Velo Cardio Faciaal Syndroom (22q11.2)")
                ("diagnose2", "2876", "(Diversen) Diabetes (comorbiditeit)")
                ("diagnose2", "2877", "(Diversen) SIADH")
                ("diagnose2", "6000", "(Diversen) Osteomyelitis")
                ("diagnose2", "6001", "(Diversen) Rhabdomyolysis")
                ("diagnose2", "2390", "(Diversen) Diversen - Overige")
                ("diagnose2", "3010", "(Chir.-Div/Anesth) Anesthetische Complicatie")
                ("diagnose2", "3020", "(Chir.-Div/Anesth) Hartkatheterisatie")
                ("diagnose2", "3030", "(Chir.-Div/Anesth) Ex-prematuur, na Algehele Anesthesie")
                ("diagnose2", "3040", "(Chir.-Div/Anesth) Invasieve Radiologie Procedure")
                ("diagnose2", "3050", "(Chir.-Div/Anesth) Massale Intraoperatieve Transfusie")
                ("diagnose2", "3060", "(Chir.-Div/Anesth) Hartkatheterisatie - Ballon Septostomie")
                ("diagnose2", "3070", "(Chir.-Div/Anesth) Hartkatheterisatie - Interventie")
                ("diagnose2", "3075", "(Chir.-Div/Anesth) Postoperatieve bloeding")
                ("diagnose2", "3000", "(Chir.-Div/Anesth) Na Procedure - Overige")
                ("diagnose2", "3450", "(Neurochirurgie) Craniotomie - Fossa Anterior")
                ("diagnose2", "3460", "(Neurochirurgie) Craniotomie - Fossa Posterior")
                ("diagnose2", "3470", "(Neurochirurgie) CZS Shunt - Insertie of revisie")
                ("diagnose2", "3480", "(Neurochirurgie) Decompressie - Craniaal")
                ("diagnose2", "3490", "(Neurochirurgie) Decompressie - Myelum")
                ("diagnose2", "3500", "(Neurochirurgie) Hemisferectomie of Lobectomie")
                ("diagnose2", "3510", "(Neurochirurgie) ICP monitor of Ventrikeldrain Insertie")
                ("diagnose2", "3520", "(Neurochirurgie) Intracraniele Bloeding, Evacuatie")
                ("diagnose2", "3522", "(Neurochirurgie) Myelomeningocoele reparatie")
                ("diagnose2", "3524", "(Neurochirurgie) Decompressie - Craniocervicaal")
                ("diagnose2", "3440", "(Neurochirurgie) Neurochirurgie - Overige")
                ("diagnose2", "3550", "(Thoraxchirurgie) Diafragma Pliceren")
                ("diagnose2", "3560", "(Thoraxchirurgie) Diafragma Herstel")
                ("diagnose2", "3570", "(Thoraxchirurgie) Longbiopsie")
                ("diagnose2", "3580", "(Thoraxchirurgie) Long Decorticatie")
                ("diagnose2", "3590", "(Thoraxchirurgie) Oesofagus Atresie Herstel")
                ("diagnose2", "3600", "(Thoraxchirurgie) Pneumectomie of Lobectomie")
                ("diagnose2", "3610", "(Thoraxchirurgie) Thoracale Tumor, Resectie")
                ("diagnose2", "3620", "(Thoraxchirurgie) Tracheo-oesofageale Fistel, Herstel")
                ("diagnose2", "3630", "(Thoraxchirurgie) Tracheopexie")
                ("diagnose2", "3632", "(Thoraxchirurgie) VATS procedure")
                ("diagnose2", "3540", "(Thoraxchirurgie) Thoraxchirurgie - Overige")
                ("diagnose2", "3660", "(KNO chirurgie) Adenotomie en/of Tonsillectomie")
                ("diagnose2", "3670", "(KNO chirurgie) Choanen Atresie, Herstel")
                ("diagnose2", "3680", "(KNO chirurgie) Cricoid Split")
                ("diagnose2", "3690", "(KNO chirurgie) Laryngeale Reconstructie")
                ("diagnose2", "3700", "(KNO chirurgie) Laryngobronchoscopie")
                ("diagnose2", "3710", "(KNO chirurgie) Tracheostomie")
                ("diagnose2", "3712", "(KNO chirurgie) Retopharyngeal abces drainage")
                ("diagnose2", "3714", "(KNO chirurgie) Laryngoplastie")
                ("diagnose2", "3715", "(KNO chirurgie) Trachea reconstructie/ Tracheaplastiek")
                ("diagnose2", "3650", "(KNO chirurgie) KNO - Overige")
                ("diagnose2", "3740", "(Alg. chirurgie) Abdominale Tumor, Resectie")
                ("diagnose2", "3750", "(Alg. chirurgie) Appendectomie")
                ("diagnose2", "3760", "(Alg. chirurgie) Extrophia Vesicae, Herstel")
                ("diagnose2", "3770", "(Alg. chirurgie) Brandwonden Chirurgie")
                ("diagnose2", "3780", "(Alg. chirurgie) Fundoplicatie")
                ("diagnose2", "3790", "(Alg. chirurgie) Gastroschizis of Exomphalos Herstel")
                ("diagnose2", "3800", "(Alg. chirurgie) GI endoscopie en/of Sclerotherapie")
                ("diagnose2", "3810", "(Alg. chirurgie) Intussusceptie Herstel")
                ("diagnose2", "3820", "(Alg. chirurgie) Kasai")
                ("diagnose2", "3830", "(Alg. chirurgie) Laparotomie - Overige")
                ("diagnose2", "3840", "(Alg. chirurgie) Transplantatie - Nier")
                ("diagnose2", "3850", "(Alg. chirurgie) Transplantatie - Lever")
                ("diagnose2", "3860", "(Alg. chirurgie) Transplantatie - Dunne Darm")
                ("diagnose2", "3870", "(Alg. chirurgie) Urogenitale Chirurgie - Overige")
                ("diagnose2", "3880", "(Alg. chirurgie) Laparotomie - Darmobstructie")
                ("diagnose2", "3890", "(Alg. chirurgie) Laparotomie - Darmperforatie")
                ("diagnose2", "3900", "(Alg. chirurgie) Laparotomie - GI Bloeding")
                ("diagnose2", "3910", "(Alg. chirurgie) Laparotomie - Necrotiserende Enterocolitis")
                ("diagnose2", "3920", "(Alg. chirurgie) Laparotomie - Peritonitis")
                ("diagnose2", "3930", "(Alg. chirurgie) Laparotomie - Trauma")
                ("diagnose2", "3931", "(Alg. chirurgie) Aorta chirurgie (Buik)")
                ("diagnose2", "3932", "(Alg. chirurgie) Wound Debridement")
                ("diagnose2", "3730", "(Alg. chirurgie) Algemene Chirurgie - Overige")
                ("diagnose2", "3960", "(Craniofaciale chir.) Schedel - Reconstructie")
                ("diagnose2", "3970", "(Craniofaciale chir.) Dentale Chirurgie")
                ("diagnose2", "3980", "(Craniofaciale chir.) Cheiloschizis herstel")
                ("diagnose2", "3990", "(Craniofaciale chir.) Mandibulaire Mobilisatie")
                ("diagnose2", "4000", "(Craniofaciale chir.) Midface Mobilisatie")
                ("diagnose2", "4010", "(Craniofaciale chir.) Palatoschizis Herstel")
                ("diagnose2", "3950", "(Craniofaciale chir.) Craniofaciale Chirurgie - Overige")
                ("diagnose2", "4040", "(Orthoped. chir.) Fractuur Fixatie")
                ("diagnose2", "4050", "(Orthoped. chir.) Spinale Instrumentatie")
                ("diagnose2", "4030", "(Orthoped. chir.) Orthopedische Chirurgie - Overige")
                ("diagnose2", "5040", "(Hartchirurgie) Annuloplasty")
                ("diagnose2", "5050", "(Hartchirurgie) Coronair arterie herstel met intrapulmonaire tunnel (Takeuchi)")
                ("diagnose2", "5060", "(Hartchirurgie) Coronair arterie herstel zonder intrapulmonaire tunnel")
                ("diagnose2", "5070", "(Hartchirurgie) Pulmonale arterie - reimplantatie")
                ("diagnose2", "5080", "(Hartchirurgie) Aorta klep vervanging")
                ("diagnose2", "5110", "(Hartchirurgie) Aortopexie")
                ("diagnose2", "5120", "(Hartchirurgie) Aortoplastie (not arch repair or graft)")
                ("diagnose2", "5130", "(Hartchirurgie) AP window repair")
                ("diagnose2", "5140", "(Hartchirurgie) Arterial switch operation")
                ("diagnose2", "5150", "(Hartchirurgie) Arterial switch operation with pulmonary artery band removal")
                ("diagnose2", "5160", "(Hartchirurgie) Arterial switch operation with repair of sub PS")
                ("diagnose2", "5170", "(Hartchirurgie) Arterial switch operation with VSD closure")
                ("diagnose2", "5180", "(Hartchirurgie) ASD and VSD repair")
                ("diagnose2", "5190", "(Hartchirurgie) ASD primum repair")
                ("diagnose2", "5200", "(Hartchirurgie) ASD surgery (including ASD secundum, sinus venosus ASD, patent forament ovale closure)")
                ("diagnose2", "5210", "(Hartchirurgie) Atrial septectomy")
                ("diagnose2", "5220", "(Hartchirurgie) Atrial switch operation")
                ("diagnose2", "5230", "(Hartchirurgie) Atrial switch operation with repair of sub PS")
                ("diagnose2", "5240", "(Hartchirurgie) Atrial switch operation with VSD closure")
                ("diagnose2", "5260", "(Hartchirurgie) Coarction repair > 30 dgn")
                ("diagnose2", "5270", "(Hartchirurgie) Coarction repair <= 30 dgn")
                ("diagnose2", "5290", "(Hartchirurgie) Common atrium Closure")
                ("diagnose2", "5310", "(Hartchirurgie) Cor triatriatum repair")
                ("diagnose2", "5320", "(Hartchirurgie) Coronary AV fistula repair")
                ("diagnose2", "5330", "(Hartchirurgie) Damus-Kaye-Stansel procedure")
                ("diagnose2", "5340", "(Hartchirurgie) Double switch")
                ("diagnose2", "5350", "(Hartchirurgie) Double-outlet right ventricle repair with or without repair of right ventricular obstruction")
                ("diagnose2", "5360", "(Hartchirurgie) Fontan procedure")
                ("diagnose2", "5370", "(Hartchirurgie) Glenn shunt")
                ("diagnose2", "5380", "(Hartchirurgie) Hypoplastic or interrupted arch repair with VSD closure")
                ("diagnose2", "5390", "(Hartchirurgie) Hypoplastic or interrupted arch repair without VSD closure")
                ("diagnose2", "5400", "(Hartchirurgie) Intracardiac tumour excision")
                ("diagnose2", "5410", "(Hartchirurgie) Konno procedure")
                ("diagnose2", "5420", "(Hartchirurgie) Left ventricular outflow tract patch")
                ("diagnose2", "5430", "(Hartchirurgie) Left ventricular to pulmonary artery conduit")
                ("diagnose2", "5440", "(Hartchirurgie) Left ventricular to right atrial shunt repair")
                ("diagnose2", "5450", "(Hartchirurgie) Mitral valve replacement")
                ("diagnose2", "5470", "(Hartchirurgie) Pacemaker insertion/replacement")
                ("diagnose2", "5480", "(Hartchirurgie) Partially anomalous pulmonary venous connection surgery")
                ("diagnose2", "5490", "(Hartchirurgie) PDA surgery >30d of age")
                ("diagnose2", "5500", "(Hartchirurgie) PDA surgery ≤30d of age")
                ("diagnose2", "5510", "(Hartchirurgie) Pulmonary artery banding")
                ("diagnose2", "5520", "(Hartchirurgie) Pulmonary artery stenosis repair")
                ("diagnose2", "5530", "(Hartchirurgie) Pulmonary outflow tract augmentation")
                ("diagnose2", "5540", "(Hartchirurgie) Pulmonary valve replacement")
                ("diagnose2", "5550", "(Hartchirurgie) Right ventricular infundibulectomy")
                ("diagnose2", "5560", "(Hartchirurgie) Right ventricular to pulmonary artery conduit")
                ("diagnose2", "5570", "(Hartchirurgie) Ross procedure")
                ("diagnose2", "5580", "(Hartchirurgie) Semilunar valve closure")
                ("diagnose2", "5590", "(Hartchirurgie) Septal defect repair (unspecified)")
                ("diagnose2", "5600", "(Hartchirurgie) Stage 1 repair of hypoplatsic left heart syndrome (Norwood)")
                ("diagnose2", "5610", "(Hartchirurgie) Stage 1 repair of nonhypoplastic left heart syndrome conditions")
                ("diagnose2", "5620", "(Hartchirurgie) Subaortic stenosis resection")
                ("diagnose2", "5630", "(Hartchirurgie) Systemic to pulmonary artery shunt")
                ("diagnose2", "5640", "(Hartchirurgie) Tetralogy of Fallot total repair")
                ("diagnose2", "5650", "(Hartchirurgie) Tetralogy of Fallot with pulmonary atresia repair")
                ("diagnose2", "5660", "(Hartchirurgie) Total repair of anamalous pulmonary veins >30d of age")
                ("diagnose2", "5670", "(Hartchirurgie) Total repair of anamalous pulmonary veins ≤30d of age")
                ("diagnose2", "5680", "(Hartchirurgie) Transection of pulmonary artery")
                ("diagnose2", "5690", "(Hartchirurgie) Transitional or complete atrioventricular canal repair with or without valve replacement")
                ("diagnose2", "5700", "(Hartchirurgie) Transplant – Heart")
                ("diagnose2", "5710", "(Hartchirurgie) Transplant – Heart Lung")
                ("diagnose2", "5720", "(Hartchirurgie) Transplant – Lung")
                ("diagnose2", "5730", "(Hartchirurgie) Transposition-VSD sub PS repair (Rastelli)")
                ("diagnose2", "5740", "(Hartchirurgie) Transverse arch graft")
                ("diagnose2", "5750", "(Hartchirurgie) Tricuspid valve replacement")
                ("diagnose2", "5760", "(Hartchirurgie) Tricuspid valve repositioning for Ebstein anomaly >30d of age")
                ("diagnose2", "5770", "(Hartchirurgie) Tricuspid valve repositioning for neonatal Ebstein ≤30d of age")
                ("diagnose2", "5780", "(Hartchirurgie) Truncus arteriosus and interrupted arch repair")
                ("diagnose2", "5790", "(Hartchirurgie) Truncus arteriosus repair")
                ("diagnose2", "5800", "(Hartchirurgie) Unifocalization for tetralogy of Fallot - pulmonary atresia")
                ("diagnose2", "5810", "(Hartchirurgie) Valvectomy of tricuspid valve")
                ("diagnose2", "5820", "(Hartchirurgie) Valvotomy - valvuloplasty (Aortic) ≤30d of age")
                ("diagnose2", "5830", "(Hartchirurgie) Valvotomy - valvuloplasty (Aortic) >30d of age")
                ("diagnose2", "5840", "(Hartchirurgie) Valvotomy - valvuloplasty (Mitral)")
                ("diagnose2", "5850", "(Hartchirurgie) Valvotomy - valvuloplasty (Pulmonary)")
                ("diagnose2", "5860", "(Hartchirurgie) Valvotomy - valvuloplasty (Tricuspid)")
                ("diagnose2", "5870", "(Hartchirurgie) Vascular ring surgery")
                ("diagnose2", "5880", "(Hartchirurgie) Ventriculomyotomy")
                ("diagnose2", "5900", "(Hartchirurgie) VSD closure and coarction repair")
                ("diagnose2", "5910", "(Hartchirurgie) VSD closure and pulmonary artery band removal")
                ("diagnose2", "5920", "(Hartchirurgie) VSD closure and pulmonary valvotomy or infundibular resection")
                ("diagnose2", "5930", "(Hartchirurgie) VSD enlargement for repair of complex anomaly (single ventricle)")
                ("diagnose2", "5940", "(Hartchirurgie) VSD repair")
                ("diagnose2", "5950", "(Hartchirurgie) Chest closure")
                ("diagnose2", "5960", "(Hartchirurgie) ECMO cannulation/exploration")
                ("diagnose2", "5970", "(Hartchirurgie) Emergency chest opening")
                ("diagnose2", "5980", "(Hartchirurgie) Pulmonary venous stenosis repair")
                ("diagnose2", "5020", "(Hartchirurgie) Hartoperatie Gesloten - Overige")
                ("diagnose2", "5030", "(Hartchirurgie) Hartoperatie Open - Overige")
                ("adm-desttypeid", "116", "Ander UMC")
                ("adm-desttypeid", "102", "Ander Ziekenhuis (Nederland)")
                ("adm-desttypeid", "103", "Ander Ziekenhuis (Buitenland)")
                ("adm-desttypeid", "104", "Huis")
                ("adm-desttypeid", "105", "Zorginstelling")
                ("adm-desttypeid", "127", "Hospice (zonder ic)")
                ("adm-desttypeid", "109", "Buitenland (overig)")
                ("adm-desttypeid", "128", "Mortuarium")
                ("adm-desttypeid", "77", "Overig")
                ("adm-desttypeid", "99", "Onbekend")
                ("adm-deceasedwhereid", "111", "niet overleden")
                ("adm-deceasedwhereid", "112", "in eigen IC")
                ("adm-deceasedwhereid", "101", "in eigen umc")
                ("adm-deceasedwhereid", "113", "in andere IC")
                ("adm-deceasedwhereid", "102", "in ander ziekenhuis")
                ("adm-deceasedwhereid", "104", "thuis")
                ("adm-deceasedwhereid", "77", "anders")
                ("adm-deceasedwhereid", "114", "op OK tijdens IC opname")
                ("adm-deathmodeid", "1", "hersendood")
                ("adm-deathmodeid", "2", "max met cpr")
                ("adm-deathmodeid", "3", "max zonder cpr")
                ("adm-deathmodeid", "4", "limiteren/staken")
                ("pupreaction3-12", "1", "normal")
                ("pupreaction3-12", "2", "one fixed & diluted")
                ("pupreaction3-12", "3", "both fixed & diluted")
                ("complicatie-soort", "1", "Accidentele extubatie")
                ("complicatie-soort", "2", "Lijn-gerelateerde infectie")
                ("complicatie-soort", "4", "Medicatie-gerelateerde complicatie")
                ("adm-sourceunitid", "129", "Directe opname van buiten eigen UMC")
                ("adm-sourceunitid", "103", "Volwassen-IC /CCU")
                ("adm-sourceunitid", "114", "Zorgafdeling via OK")
                ("adm-sourceunitid", "106", "(Zorg)Afdeling (zonder extra bewaking)")
                ("adm-sourceunitid", "115", "SEH via OK")
                ("adm-sourceunitid", "107", "SEH")
                ("adm-sourceunitid", "109", "Recovery")
                ("adm-sourceunitid", "105", "Afdeling met extra bewaking (HDU/HighCare)")
                ("adm-sourceunitid", "110", "Kraamafdeling")
                ("adm-sourceunitid", "77", "Overig")
                ("adm-sourceunitid", "108", "OK")
                ("adm-sourceunitid", "104", "Longstay-ic")
                ("adm-sourceunitid", "102", "NICU (IC-Neonatologie)")
                ("adm-sourceunitid", "99", "Onbekend")
                ("adm-readmtypeid", "12", "Andere problematiek dan voorgaande opname")
                ("adm-readmtypeid", "13", "Zelfde problematiek als voorgaande opname")
                ("adm-destunitid", "106", "(Zorg)Afdeling (zonder extra bewaking)")
                ("adm-destunitid", "102", "NICU (IC-Neonatologie)")
                ("adm-destunitid", "103", "Volwassen-IC /CCU")
                ("adm-destunitid", "105", "Afdeling met extra bewaking (HDU/HighCare)")
                ("adm-destunitid", "109", "Recovery")
                ("adm-destunitid", "77", "Overig")
                ("adm-destunitid", "110", "Kraamafdeling")
                ("adm-destunitid", "104", "Longstay-ic")
                ("adm-destunitid", "128", "Mortuarium")
                ("adm-destunitid", "129", "Direct aansluitend ontslag uit eigen UMC")
                ("adm-destunitid", "99", "Onbekend")
                ("adm-disreasonid", "1", "Klaar voor niet-ICU zorg")
                ("adm-disreasonid", "2", "Ontslag naar palliatieve zorg")
                ("adm-disreasonid", "3", "Ontslag ten gevolge van plaatsgebrek op PICU")
                ("adm-disreasonid", "5", "Huidige (PICU) zorg voortgezet op andere afdeling")
                ("adm-disreasonid", "6", "Gespecialiseerde zorg op andere afdeling")
                ("adm-disreasonid", "7", "Ontslag tegen medisch advies")
                ("adm-disreasonid", "8", "Ontslag wegens uitstel ingreep")
                ("adm-disreasonid", "100", "Overleden")
                ("bijkomende-diagnose", "40", "(Letsel) Anafylaxie")
                ("bijkomende-diagnose", "50", "(Letsel) Brandwonden")
                ("bijkomende-diagnose", "60", "(Letsel) Koolmonoxide vergiftiging")
                ("bijkomende-diagnose", "70", "(Letsel) Geneesmiddel toxiciteit-iatrogeen")
                ("bijkomende-diagnose", "80", "(Letsel) Electrocutie")
                ("bijkomende-diagnose", "90", "(Letsel) Vergiftiging")
                ("bijkomende-diagnose", "100", "(Letsel) Verhanging of verwurging")
                ("bijkomende-diagnose", "110", "(Letsel) Hyperthermie")
                ("bijkomende-diagnose", "120", "(Letsel) Hypothermie")
                ("bijkomende-diagnose", "130", "(Letsel) Verdrinking")
                ("bijkomende-diagnose", "150", "(Letsel) Rook inhalatie")
                ("bijkomende-diagnose", "170", "(Letsel) Trauma - Abdomen")
                ("bijkomende-diagnose", "180", "(Letsel) Trauma - Borstkas")
                ("bijkomende-diagnose", "190", "(Letsel) Trauma - Gelaat")
                ("bijkomende-diagnose", "200", "(Letsel) Trauma - Hoofd")
                ("bijkomende-diagnose", "210", "(Letsel) Trauma - Skelet")
                ("bijkomende-diagnose", "220", "(Letsel) Trauma - Wervelkolom")
                ("bijkomende-diagnose", "160", "(Letsel) Trauma - Overig")
                ("bijkomende-diagnose", "142", "(Letsel) Ingestie - drug")
                ("bijkomende-diagnose", "144", "(Letsel) Ingestie - non drug")
                ("bijkomende-diagnose", "30", "(Letsel) Letsel/vergiftiging/externe oorzaak-overig")
                ("bijkomende-diagnose", "250", "(Letsel) Fietser")
                ("bijkomende-diagnose", "260", "(Letsel) Val")
                ("bijkomende-diagnose", "270", "(Letsel) Boerderij Gereedschap")
                ("bijkomende-diagnose", "280", "(Letsel) Vuurwapen Verwonding")
                ("bijkomende-diagnose", "290", "(Letsel) Motorvoertuigongeluk - Passagier")
                ("bijkomende-diagnose", "300", "(Letsel) Motorvoertuigongeluk - Voetganger")
                ("bijkomende-diagnose", "310", "(Letsel) Niet Accidenteel Letsel")
                ("bijkomende-diagnose", "320", "(Letsel) Zelf Toegebracht Letsel")
                ("bijkomende-diagnose", "330", "(Letsel) Sport letsel")
                ("bijkomende-diagnose", "340", "(Letsel) Steekwond")
                ("bijkomende-diagnose", "350", "(Letsel) Motorfiets Bestuurder/Passagier")
                ("bijkomende-diagnose", "360", "(Letsel) Knel Verwonding")
                ("bijkomende-diagnose", "362", "(Letsel) Paard gerelateerde verwonding")
                ("bijkomende-diagnose", "240", "(Letsel) Letsel Mechanisme - Overig")
                ("bijkomende-diagnose", "400", "(Cardiovasculair) Afwezige Pulmonaal Klep")
                ("bijkomende-diagnose", "410", "(Cardiovasculair) Anomalie Coronair Arterie")
                ("bijkomende-diagnose", "420", "(Cardiovasculair) Aorta-insufficientie")
                ("bijkomende-diagnose", "430", "(Cardiovasculair) Aortastenose")
                ("bijkomende-diagnose", "440", "(Cardiovasculair) ASD")
                ("bijkomende-diagnose", "450", "(Cardiovasculair) AVSD")
                ("bijkomende-diagnose", "460", "(Cardiovasculair) Coarctatio aortae")
                ("bijkomende-diagnose", "470", "(Cardiovasculair) Cor triatriatum")
                ("bijkomende-diagnose", "480", "(Cardiovasculair) Ebstein anomalie")
                ("bijkomende-diagnose", "490", "(Cardiovasculair) Hypoplastisch Linker Hart Syndroom")
                ("bijkomende-diagnose", "500", "(Cardiovasculair) Interruptie of Hypoplastische Aortaboog")
                ("bijkomende-diagnose", "510", "(Cardiovasculair) Mitraal Insufficientie")
                ("bijkomende-diagnose", "520", "(Cardiovasculair) Mitraal Stenose")
                ("bijkomende-diagnose", "530", "(Cardiovasculair) Open Ductus Botalli")
                ("bijkomende-diagnose", "540", "(Cardiovasculair) Pulmonaal Atresie of Stenose")
                ("bijkomende-diagnose", "550", "(Cardiovasculair) Monoventrikel")
                ("bijkomende-diagnose", "560", "(Cardiovasculair) Totaal Abnornaal Pulmonaal Veneuze Return")
                ("bijkomende-diagnose", "570", "(Cardiovasculair) Tetralogie van Fallot")
                ("bijkomende-diagnose", "580", "(Cardiovasculair) Transpositie Grote Arterien")
                ("bijkomende-diagnose", "590", "(Cardiovasculair) Tricuspidaal Atresie of Stenose")
                ("bijkomende-diagnose", "600", "(Cardiovasculair) Tricuspidaal Insufficientie")
                ("bijkomende-diagnose", "610", "(Cardiovasculair) Truncus Arteriosus")
                ("bijkomende-diagnose", "620", "(Cardiovasculair) VSD")
                ("bijkomende-diagnose", "630", "(Cardiovasculair) Aortopulmonaal Venster")
                ("bijkomende-diagnose", "640", "(Cardiovasculair) Arterioveneuze Malformatie")
                ("bijkomende-diagnose", "650", "(Cardiovasculair) Double Outlet Rechter Ventrikel")
                ("bijkomende-diagnose", "660", "(Cardiovasculair) Linker Ventrikel Uitstroom Obstructie")
                ("bijkomende-diagnose", "670", "(Cardiovasculair) Pulmonaal Insufficientie")
                ("bijkomende-diagnose", "680", "(Cardiovasculair) Rechter Ventrikel Uitstroom Obstructie")
                ("bijkomende-diagnose", "682", "(Cardiovasculair) Hypoplastische Li ventrikel (géén HLHS)")
                ("bijkomende-diagnose", "684", "(Cardiovasculair) Hypoplastische Re ventrikel")
                ("bijkomende-diagnose", "686", "(Cardiovasculair) l-TGA (Levo Transpositie grote arterien)")
                ("bijkomende-diagnose", "390", "(Cardiovasculair) Cardiovasculair - Congenitaal - Overige")
                ("bijkomende-diagnose", "710", "(Cardiovasculair) Hartfalen")
                ("bijkomende-diagnose", "720", "(Cardiovasculair) Cardiale Tumor")
                ("bijkomende-diagnose", "730", "(Cardiovasculair) Cardiomyopathie/Myocarditis")
                ("bijkomende-diagnose", "740", "(Cardiovasculair) Dysritmie - Supraventriculair")
                ("bijkomende-diagnose", "750", "(Cardiovasculair) Dysritmie - Ventriculair")
                ("bijkomende-diagnose", "760", "(Cardiovasculair) Endocarditis")
                ("bijkomende-diagnose", "770", "(Cardiovasculair) Hypertensie - Pulmonaal")
                ("bijkomende-diagnose", "780", "(Cardiovasculair) Hypertensie - Systemisch")
                ("bijkomende-diagnose", "790", "(Cardiovasculair) Kawasaki, Ziekte van")
                ("bijkomende-diagnose", "800", "(Cardiovasculair) Pericardiale Effusie of Tamponade")
                ("bijkomende-diagnose", "810", "(Cardiovasculair) Vasculaire Trombose")
                ("bijkomende-diagnose", "820", "(Cardiovasculair) Vasculitis")
                ("bijkomende-diagnose", "830", "(Cardiovasculair) Hart-Long Transplantatie, Status na")
                ("bijkomende-diagnose", "840", "(Cardiovasculair) Harttransplantatie, Status na")
                ("bijkomende-diagnose", "842", "(Cardiovasculair) Rheumatic Heart Disease")
                ("bijkomende-diagnose", "844", "(Cardiovasculair) Hartoperatie in het verleden")
                ("bijkomende-diagnose", "846", "(Cardiovasculair) Longembolie")
                ("bijkomende-diagnose", "848", "(Cardiovasculair) Myocard (infarct/Ischemie)")
                ("bijkomende-diagnose", "700", "(Cardiovasculair) Cardiovasculair - Verworven- Overige")
                ("bijkomende-diagnose", "870", "(Neurologisch) Botulisme")
                ("bijkomende-diagnose", "880", "(Neurologisch) Hersenabces")
                ("bijkomende-diagnose", "890", "(Neurologisch) Hersen AV Malformatie")
                ("bijkomende-diagnose", "900", "(Neurologisch) Hersendood")
                ("bijkomende-diagnose", "910", "(Neurologisch) Herseninfarct of Beroerte")
                ("bijkomende-diagnose", "920", "(Neurologisch) Hersentumor")
                ("bijkomende-diagnose", "930", "(Neurologisch) CZS Shunt Dysfunctie of Infectie")
                ("bijkomende-diagnose", "940", "(Neurologisch) Encefalitis")
                ("bijkomende-diagnose", "950", "(Neurologisch) Encefalopathie, Acuut - Hypoxisch Ischaemisch")
                ("bijkomende-diagnose", "960", "(Neurologisch) Encefalopatie, Acuut - Overige")
                ("bijkomende-diagnose", "970", "(Neurologisch) Encefalopatie, Chronisch Degeneratief")
                ("bijkomende-diagnose", "980", "(Neurologisch) Encefalopatie, Chronisch Statisch")
                ("bijkomende-diagnose", "990", "(Neurologisch) Guillain Barre Syndroom")
                ("bijkomende-diagnose", "1000", "(Neurologisch) Hydrocefalus")
                ("bijkomende-diagnose", "1011", "(Neurologisch) Intracraniele Bloeding (spontaan)")
                ("bijkomende-diagnose", "1020", "(Neurologisch) Intracraniele Hypertensie")
                ("bijkomende-diagnose", "1030", "(Neurologisch) Meningitis")
                ("bijkomende-diagnose", "1040", "(Neurologisch) Meningomyelocele of Spina Bifida")
                ("bijkomende-diagnose", "1050", "(Neurologisch) Myopathie")
                ("bijkomende-diagnose", "1060", "(Neurologisch) Neuropathie")
                ("bijkomende-diagnose", "1070", "(Neurologisch) Convulsies")
                ("bijkomende-diagnose", "1080", "(Neurologisch) Myelum, Beschadiging")
                ("bijkomende-diagnose", "1090", "(Neurologisch) Veneuze Sinus Trombose")
                ("bijkomende-diagnose", "1100", "(Neurologisch) Cerebraal Aneurysma")
                ("bijkomende-diagnose", "1110", "(Neurologisch) Spierdystrofie")
                ("bijkomende-diagnose", "1120", "(Neurologisch) Myastenia Gravis")
                ("bijkomende-diagnose", "1130", "(Neurologisch) Tetanus")
                ("bijkomende-diagnose", "1135", "(Neurologisch) Arnold Chiari Malformation")
                ("bijkomende-diagnose", "1136", "(Neurologisch) Hersencyste")
                ("bijkomende-diagnose", "1137", "(Neurologisch) Epilepsie (comorbiditeit)")
                ("bijkomende-diagnose", "1138", "(Neurologisch) Hersenoedeem")
                ("bijkomende-diagnose", "1139", "(Neurologisch) Intracraniele bloeding (Traumatisch)")
                ("bijkomende-diagnose", "1131", "(Neurologisch) Acute disseminated encephalomyelitis (ADEM)")
                ("bijkomende-diagnose", "1132", "(Neurologisch) Congenitale hersenafwijking")
                ("bijkomende-diagnose", "1133", "(Neurologisch) Febrile convulsion")
                ("bijkomende-diagnose", "1134", "(Neurologisch) Transverse Myelitis")
                ("bijkomende-diagnose", "860", "(Neurologisch) Neurologisch - Overig")
                ("bijkomende-diagnose", "1170", "(Respiratoir) Choanen Atresie of Stenose")
                ("bijkomende-diagnose", "1180", "(Respiratoir) Epiglottitis")
                ("bijkomende-diagnose", "1190", "(Respiratoir) Vreemdlichaam - Inhalatie")
                ("bijkomende-diagnose", "1200", "(Respiratoir) Laryngotracheobronchitis")
                ("bijkomende-diagnose", "1210", "(Respiratoir) Obstructief Slaap Apnoe Syndroom")
                ("bijkomende-diagnose", "1220", "(Respiratoir) Pierre Robin Syndroom")
                ("bijkomende-diagnose", "1230", "(Respiratoir) Retrofaryngeaal Abces")
                ("bijkomende-diagnose", "1240", "(Respiratoir) Subglottische Stenose")
                ("bijkomende-diagnose", "1250", "(Respiratoir) Tracheitis")
                ("bijkomende-diagnose", "1260", "(Respiratoir) Bovenste Luchtwegobstructie - Overige")
                ("bijkomende-diagnose", "1270", "(Respiratoir) Bovenste Luchtweginfectie - Overige")
                ("bijkomende-diagnose", "1280", "(Respiratoir) Stembandparese")
                ("bijkomende-diagnose", "1290", "(Respiratoir) Subglottisch Hemangioom")
                ("bijkomende-diagnose", "1295", "(Respiratoir) Larygomalacia")
                ("bijkomende-diagnose", "1160", "(Respiratoir) Bovenste Luchtwegen - Overige")
                ("bijkomende-diagnose", "1320", "(Respiratoir) Astma")
                ("bijkomende-diagnose", "1330", "(Respiratoir) Bronchiolitis")
                ("bijkomende-diagnose", "1340", "(Respiratoir) Chronische Longziekte")
                ("bijkomende-diagnose", "1350", "(Respiratoir) Malacie - Trachea en/of Bronchus")
                ("bijkomende-diagnose", "1360", "(Respiratoir) Mediastinale Massa")
                ("bijkomende-diagnose", "1370", "(Respiratoir) Stenose - Trachea en/of Bronchus")
                ("bijkomende-diagnose", "1380", "(Respiratoir) Tracheo-oesofageale Fistel")
                ("bijkomende-diagnose", "1390", "(Respiratoir) Vaatring")
                ("bijkomende-diagnose", "1391", "(Respiratoir) Bronchiectasis")
                ("bijkomende-diagnose", "1310", "(Respiratoir) Lage luchtwegen - Overige")
                ("bijkomende-diagnose", "1420", "(Respiratoir) Luchtlek Syndroom")
                ("bijkomende-diagnose", "1430", "(Respiratoir) Apnoe - Centraal")
                ("bijkomende-diagnose", "1440", "(Respiratoir) ARDS")
                ("bijkomende-diagnose", "1450", "(Respiratoir) Aspiratie")
                ("bijkomende-diagnose", "1460", "(Respiratoir) Chylothorax")
                ("bijkomende-diagnose", "1470", "(Respiratoir) Congenitale Hernia Diafragmatic")
                ("bijkomende-diagnose", "1480", "(Respiratoir) Congenitale Longafwijking")
                ("bijkomende-diagnose", "1490", "(Respiratoir) Cystic Fibrosis")
                ("bijkomende-diagnose", "1500", "(Respiratoir) Empyeem")
                ("bijkomende-diagnose", "1510", "(Respiratoir) Hyaliene Membraanziekte")
                ("bijkomende-diagnose", "1520", "(Respiratoir) Hypoventilatie - Centraal")
                ("bijkomende-diagnose", "1530", "(Respiratoir) Longabces")
                ("bijkomende-diagnose", "1540", "(Respiratoir) Meconium Aspiratie Syndroom")
                ("bijkomende-diagnose", "1550", "(Respiratoir) Pleura Effusie")
                ("bijkomende-diagnose", "1560", "(Respiratoir) Pneumonie of Pneumonitis")
                ("bijkomende-diagnose", "1570", "(Respiratoir) Long Hypoplasie")
                ("bijkomende-diagnose", "1580", "(Respiratoir) Pulmonaal Oedeem")
                ("bijkomende-diagnose", "1590", "(Respiratoir) Respiratoir Falen")
                ("bijkomende-diagnose", "1600", "(Respiratoir) Lage Luchtweginfectie - Overige")
                ("bijkomende-diagnose", "1610", "(Respiratoir) Kinkhoest")
                ("bijkomende-diagnose", "1620", "(Respiratoir) Longtransplantatie, Status na")
                ("bijkomende-diagnose", "1630", "(Respiratoir) Voorbijgaande Tachypnoe van de Pasgeborene")
                ("bijkomende-diagnose", "1632", "(Respiratoir) Atelectase")
                ("bijkomende-diagnose", "1410", "(Respiratoir) Ademhaling - Overige")
                ("bijkomende-diagnose", "1660", "(Renaal) Hemolytisch Uremisch Syndroom")
                ("bijkomende-diagnose", "1670", "(Renaal) Nefrotisch en/of Nefritisch Syndroom")
                ("bijkomende-diagnose", "1680", "(Renaal) Nierfalen - Acuut")
                ("bijkomende-diagnose", "1690", "(Renaal) Nierfalen - Chronisch")
                ("bijkomende-diagnose", "1700", "(Renaal) Niertransplantatie, Status na")
                ("bijkomende-diagnose", "1710", "(Renaal) Urineweg Infectie")
                ("bijkomende-diagnose", "1712", "(Renaal) Hydronefrose")
                ("bijkomende-diagnose", "1650", "(Renaal) Renaal - Overige")
                ("bijkomende-diagnose", "1740", "(MDL) Darmobstructie")
                ("bijkomende-diagnose", "1750", "(MDL) Colitis")
                ("bijkomende-diagnose", "1760", "(MDL) Gastroenteritis")
                ("bijkomende-diagnose", "1770", "(MDL) Gastrointestinale Bloeding")
                ("bijkomende-diagnose", "1780", "(MDL) Gastroschizis of Exomphalus")
                ("bijkomende-diagnose", "1790", "(MDL) Hepatitis")
                ("bijkomende-diagnose", "1800", "(MDL) Invaginatie")
                ("bijkomende-diagnose", "1820", "(MDL) Leverfalen - Acuut")
                ("bijkomende-diagnose", "1830", "(MDL) Leverfalen - Chronisch")
                ("bijkomende-diagnose", "1810", "(MDL) Leverziekte - Overige")
                ("bijkomende-diagnose", "1840", "(MDL) Necrotiserende Enterocolitis")
                ("bijkomende-diagnose", "1850", "(MDL) Oesofagus Atresie")
                ("bijkomende-diagnose", "1860", "(MDL) Pancreatitis")
                ("bijkomende-diagnose", "1870", "(MDL) Peritonitis")
                ("bijkomende-diagnose", "1880", "(MDL) Pylorus Stenose")
                ("bijkomende-diagnose", "1890", "(MDL) Short Bowel Syndroom")
                ("bijkomende-diagnose", "1900", "(MDL) Ulcus - Duodenum")
                ("bijkomende-diagnose", "1910", "(MDL) Ulcus - Maag of Gastritis")
                ("bijkomende-diagnose", "1920", "(MDL) Varices - Oesofagus of Maag")
                ("bijkomende-diagnose", "1930", "(MDL) Galgang Atresie")
                ("bijkomende-diagnose", "1940", "(MDL) Darmperforatie")
                ("bijkomende-diagnose", "1950", "(MDL) Hirschsprung, Ziekte van")
                ("bijkomende-diagnose", "1960", "(MDL) Neonatale Icterus")
                ("bijkomende-diagnose", "1970", "(MDL) Oesofageaal Vreemdlichaam")
                ("bijkomende-diagnose", "1980", "(MDL) Portale Hyperternsie")
                ("bijkomende-diagnose", "1990", "(MDL) Levertransplantatie, Status na")
                ("bijkomende-diagnose", "2000", "(MDL) Volvulus")
                ("bijkomende-diagnose", "2002", "(MDL) Gastro-oesophagiale reflux")
                ("bijkomende-diagnose", "2004", "(MDL) Veno Occlusive Disease (VOD)")
                ("bijkomende-diagnose", "2006", "(MDL) Chylus Effusie")
                ("bijkomende-diagnose", "1730", "(MDL) Gastrointestinaal, Overige")
                ("bijkomende-diagnose", "2030", "(Infectie) Adenovirus")
                ("bijkomende-diagnose", "2040", "(Infectie) Bacterie - Overige")
                ("bijkomende-diagnose", "2050", "(Infectie) Bacterie - Gramnegatief - Overige")
                ("bijkomende-diagnose", "2060", "(Infectie) Bacterie - Grampositief - Overige")
                ("bijkomende-diagnose", "2070", "(Infectie) Candida")
                ("bijkomende-diagnose", "2080", "(Infectie) Clostridium")
                ("bijkomende-diagnose", "2090", "(Infectie) CMV")
                ("bijkomende-diagnose", "2100", "(Infectie) EBV")
                ("bijkomende-diagnose", "2110", "(Infectie) Enterovirus")
                ("bijkomende-diagnose", "2120", "(Infectie) Schimmel - Overige")
                ("bijkomende-diagnose", "2130", "(Infectie) Haemophilus Influenzae Type b")
                ("bijkomende-diagnose", "2140", "(Infectie) Hepatitis - Viraal")
                ("bijkomende-diagnose", "2150", "(Infectie) Herpes Simplex Virus")
                ("bijkomende-diagnose", "2160", "(Infectie) HIV")
                ("bijkomende-diagnose", "2170", "(Infectie) Influenza Virus")
                ("bijkomende-diagnose", "2180", "(Infectie) Legionella")
                ("bijkomende-diagnose", "2190", "(Infectie) Meningococ")
                ("bijkomende-diagnose", "2200", "(Infectie) Mycoplasma")
                ("bijkomende-diagnose", "2210", "(Infectie) Parainfluenzae Virus")
                ("bijkomende-diagnose", "2220", "(Infectie) Pertussis")
                ("bijkomende-diagnose", "2230", "(Infectie) Pneumococ")
                ("bijkomende-diagnose", "2240", "(Infectie) Pneumocystis Carinii")
                ("bijkomende-diagnose", "2250", "(Infectie) Rotavirus")
                ("bijkomende-diagnose", "2260", "(Infectie) RSV")
                ("bijkomende-diagnose", "2270", "(Infectie) Salmonella")
                ("bijkomende-diagnose", "2280", "(Infectie) Staphylococ")
                ("bijkomende-diagnose", "2290", "(Infectie) Streptococ - Other")
                ("bijkomende-diagnose", "2300", "(Infectie) Varicella")
                ("bijkomende-diagnose", "2310", "(Infectie) Virus - Overige")
                ("bijkomende-diagnose", "2320", "(Infectie) E Coli")
                ("bijkomende-diagnose", "2330", "(Infectie) Klebsiella")
                ("bijkomende-diagnose", "2340", "(Infectie) Malaria")
                ("bijkomende-diagnose", "2350", "(Infectie) Pseudomonas")
                ("bijkomende-diagnose", "2360", "(Infectie) Streptococ Group B")
                ("bijkomende-diagnose", "2370", "(Infectie) Mazelen Virus")
                ("bijkomende-diagnose", "2372", "(Infectie) Staphylococ (MRSA)")
                ("bijkomende-diagnose", "2374", "(Infectie) Staphylococ (géén MRSA)")
                ("bijkomende-diagnose", "2376", "(Infectie) Tuberculose")
                ("bijkomende-diagnose", "2378", "(Infectie) Enterobacter")
                ("bijkomende-diagnose", "2379", "(Infectie) Moraxella")
                ("bijkomende-diagnose", "2020", "(Infectie) Infectie - Overige")
                ("bijkomende-diagnose", "2400", "(Diversen) ALTE")
                ("bijkomende-diagnose", "2410", "(Diversen) Hartstilstand - In Ziekenhuis")
                ("bijkomende-diagnose", "2420", "(Diversen) Hartstilstand - Buiten Ziekenhuis")
                ("bijkomende-diagnose", "2430", "(Diversen) Chromosoom Afwijking")
                ("bijkomende-diagnose", "2440", "(Diversen) Stollingsstoornis")
                ("bijkomende-diagnose", "2450", "(Diversen) Dehydratie")
                ("bijkomende-diagnose", "2460", "(Diversen) Dermatologische Afwijking")
                ("bijkomende-diagnose", "2470", "(Diversen) Diabetes Insipidus")
                ("bijkomende-diagnose", "2480", "(Diversen) Diabetes Mellitus met Ketoacidose")
                ("bijkomende-diagnose", "2490", "(Diversen) Diabetes Mellitus zonder Ketoacidose")
                ("bijkomende-diagnose", "2500", "(Diversen) Electroliet verstoring")
                ("bijkomende-diagnose", "2510", "(Diversen) Endocriene Afwijking")
                ("bijkomende-diagnose", "2520", "(Diversen) Gasgangreen")
                ("bijkomende-diagnose", "2530", "(Diversen) Thuisbeademing Patient")
                ("bijkomende-diagnose", "2540", "(Diversen) Hypoglycemie")
                ("bijkomende-diagnose", "2550", "(Diversen) IC Diagnostische Monitoring - Electief")
                ("bijkomende-diagnose", "2560", "(Diversen) IC Procedure")
                ("bijkomende-diagnose", "2570", "(Diversen) Immunodeficientie - Congenitaal")
                ("bijkomende-diagnose", "2580", "(Diversen) Immunosuppressie - Verworven")
                ("bijkomende-diagnose", "2590", "(Diversen) Stofwisselingsziekte, Aangeboren")
                ("bijkomende-diagnose", "2600", "(Diversen) Leukemie of Lymfoom")
                ("bijkomende-diagnose", "2610", "(Diversen) Necrotiserende Fasciitis")
                ("bijkomende-diagnose", "2620", "(Diversen) Neutropenie")
                ("bijkomende-diagnose", "2630", "(Diversen) Pancytopenie")
                ("bijkomende-diagnose", "2640", "(Diversen) Feochromocytoom")
                ("bijkomende-diagnose", "2650", "(Diversen) Prematuriteit (< 37/4 & < 12 mnd)")
                ("bijkomende-diagnose", "2660", "(Diversen) Ademstilstand - In Ziekenhuis")
                ("bijkomende-diagnose", "2670", "(Diversen) Ademstilstand - Buiten Ziekenhuis")
                ("bijkomende-diagnose", "2680", "(Diversen) Sepsis")
                ("bijkomende-diagnose", "2690", "(Diversen) Shock - Cardiogeen")
                ("bijkomende-diagnose", "2700", "(Diversen) Shock - Hypovolemisch")
                ("bijkomende-diagnose", "2710", "(Diversen) Shock - Septisch")
                ("bijkomende-diagnose", "2685", "(Diversen) Shock (ongespecificeerd)")
                ("bijkomende-diagnose", "2720", "(Diversen) SIRS")
                ("bijkomende-diagnose", "2730", "(Diversen) Neoplasma Solide Orgaan - Maligne")
                ("bijkomende-diagnose", "2740", "(Diversen) Neoplasma Solide Orgaan -Niet Maligne")
                ("bijkomende-diagnose", "2750", "(Diversen) Syndroom of Malformatie")
                ("bijkomende-diagnose", "2760", "(Diversen) Toxisch Shock Syndroom")
                ("bijkomende-diagnose", "2770", "(Diversen) Beenmergtransplantatie")
                ("bijkomende-diagnose", "2780", "(Diversen) Craniosynostose")
                ("bijkomende-diagnose", "2790", "(Diversen) Hydrops Foetalis")
                ("bijkomende-diagnose", "2800", "(Diversen) Neonaat - Kind van Diabetische Moeder")
                ("bijkomende-diagnose", "2810", "(Diversen) Neonaat - Intrauteriene Groeivertraging")
                ("bijkomende-diagnose", "2820", "(Diversen) Beenmergtransplantatie, Status na")
                ("bijkomende-diagnose", "2830", "(Diversen) Scoliose")
                ("bijkomende-diagnose", "2840", "(Diversen) Tumorlysis Syndroom")
                ("bijkomende-diagnose", "2850", "(Diversen) Wondinfectie")
                ("bijkomende-diagnose", "2860", "(Diversen) Hematologische Ziekte")
                ("bijkomende-diagnose", "2870", "(Diversen) Orgaandonatie")
                ("bijkomende-diagnose", "2871", "(Diversen) Graft vs Host disease")
                ("bijkomende-diagnose", "2872", "(Diversen) Cystic Hygroma")
                ("bijkomende-diagnose", "2873", "(Diversen) Ademstilstand (op de PICU)")
                ("bijkomende-diagnose", "2878", "(Diversen) Hartstilstand op PICU")
                ("bijkomende-diagnose", "2874", "(Diversen) Down syndroom")
                ("bijkomende-diagnose", "2875", "(Diversen) Velo Cardio Faciaal Syndroom (22q11.2)")
                ("bijkomende-diagnose", "2876", "(Diversen) Diabetes (comorbiditeit)")
                ("bijkomende-diagnose", "2877", "(Diversen) SIADH")
                ("bijkomende-diagnose", "6000", "(Diversen) Osteomyelitis")
                ("bijkomende-diagnose", "6001", "(Diversen) Rhabdomyolysis")
                ("bijkomende-diagnose", "2390", "(Diversen) Diversen - Overige")
                ("bijkomende-diagnose", "2890", "(IC procedures) Cardioversie / defibrillatie")
                ("bijkomende-diagnose", "2900", "(IC procedures) Hemodialyse (intermitterende)")
                ("bijkomende-diagnose", "2902", "(IC procedures) MARS (leverdialyse bij leverfalen)")
                ("bijkomende-diagnose", "2910", "(IC procedures) Peritoneaaldialyse")
                ("bijkomende-diagnose", "2920", "(IC procedures) ECMO")
                ("bijkomende-diagnose", "2930", "(IC procedures) Hemofiltratie (bv CVVH, CVVHD)")
                ("bijkomende-diagnose", "2940", "(IC procedures) HFO")
                ("bijkomende-diagnose", "2950", "(IC procedures) Plasmafiltratie")
                ("bijkomende-diagnose", "2960", "(IC procedures) Ventricular Assist Device")
                ("bijkomende-diagnose", "2970", "(IC procedures) NO-inhalatie")
                ("bijkomende-diagnose", "3010", "(Chir.-Div/Anesth) Anesthetische Complicatie")
                ("bijkomende-diagnose", "3020", "(Chir.-Div/Anesth) Hartkatheterisatie")
                ("bijkomende-diagnose", "3030", "(Chir.-Div/Anesth) Ex-prematuur, na Algehele Anesthesie")
                ("bijkomende-diagnose", "3040", "(Chir.-Div/Anesth) Invasieve Radiologie Procedure")
                ("bijkomende-diagnose", "3050", "(Chir.-Div/Anesth) Massale Intraoperatieve Transfusie")
                ("bijkomende-diagnose", "3060", "(Chir.-Div/Anesth) Hartkatheterisatie - Ballon Septostomie")
                ("bijkomende-diagnose", "3070", "(Chir.-Div/Anesth) Hartkatheterisatie - Interventie")
                ("bijkomende-diagnose", "3075", "(Chir.-Div/Anesth) Postoperatieve bloeding")
                ("bijkomende-diagnose", "3000", "(Chir.-Div/Anesth) Na Procedure - Overige")
                ("bijkomende-diagnose", "3450", "(Neurochirurgie) Craniotomie - Fossa Anterior")
                ("bijkomende-diagnose", "3460", "(Neurochirurgie) Craniotomie - Fossa Posterior")
                ("bijkomende-diagnose", "3470", "(Neurochirurgie) CZS Shunt - Insertie of revisie")
                ("bijkomende-diagnose", "3480", "(Neurochirurgie) Decompressie - Craniaal")
                ("bijkomende-diagnose", "3490", "(Neurochirurgie) Decompressie - Myelum")
                ("bijkomende-diagnose", "3500", "(Neurochirurgie) Hemisferectomie of Lobectomie")
                ("bijkomende-diagnose", "3510", "(Neurochirurgie) ICP monitor of Ventrikeldrain Insertie")
                ("bijkomende-diagnose", "3520", "(Neurochirurgie) Intracraniele Bloeding, Evacuatie")
                ("bijkomende-diagnose", "3522", "(Neurochirurgie) Myelomeningocoele reparatie")
                ("bijkomende-diagnose", "3524", "(Neurochirurgie) Decompressie - Craniocervicaal")
                ("bijkomende-diagnose", "3440", "(Neurochirurgie) Neurochirurgie - Overige")
                ("bijkomende-diagnose", "3550", "(Thoraxchirurgie) Diafragma Pliceren")
                ("bijkomende-diagnose", "3560", "(Thoraxchirurgie) Diafragma Herstel")
                ("bijkomende-diagnose", "3570", "(Thoraxchirurgie) Longbiopsie")
                ("bijkomende-diagnose", "3580", "(Thoraxchirurgie) Long Decorticatie")
                ("bijkomende-diagnose", "3590", "(Thoraxchirurgie) Oesofagus Atresie Herstel")
                ("bijkomende-diagnose", "3600", "(Thoraxchirurgie) Pneumectomie of Lobectomie")
                ("bijkomende-diagnose", "3610", "(Thoraxchirurgie) Thoracale Tumor, Resectie")
                ("bijkomende-diagnose", "3620", "(Thoraxchirurgie) Tracheo-oesofageale Fistel, Herstel")
                ("bijkomende-diagnose", "3630", "(Thoraxchirurgie) Tracheopexie")
                ("bijkomende-diagnose", "3632", "(Thoraxchirurgie) VATS procedure")
                ("bijkomende-diagnose", "3540", "(Thoraxchirurgie) Thoraxchirurgie - Overige")
                ("bijkomende-diagnose", "3660", "(KNO chirurgie) Adenotomie en/of Tonsillectomie")
                ("bijkomende-diagnose", "3670", "(KNO chirurgie) Choanen Atresie, Herstel")
                ("bijkomende-diagnose", "3680", "(KNO chirurgie) Cricoid Split")
                ("bijkomende-diagnose", "3690", "(KNO chirurgie) Laryngeale Reconstructie")
                ("bijkomende-diagnose", "3700", "(KNO chirurgie) Laryngobronchoscopie")
                ("bijkomende-diagnose", "3710", "(KNO chirurgie) Tracheostomie")
                ("bijkomende-diagnose", "3712", "(KNO chirurgie) Retopharyngeal abces drainage")
                ("bijkomende-diagnose", "3714", "(KNO chirurgie) Laryngoplastie")
                ("bijkomende-diagnose", "3715", "(KNO chirurgie) Trachea reconstructie/ Tracheaplastiek")
                ("bijkomende-diagnose", "3650", "(KNO chirurgie) KNO - Overige")
                ("bijkomende-diagnose", "3740", "(Alg. chirurgie) Abdominale Tumor, Resectie")
                ("bijkomende-diagnose", "3750", "(Alg. chirurgie) Appendectomie")
                ("bijkomende-diagnose", "3760", "(Alg. chirurgie) Extrophia Vesicae, Herstel")
                ("bijkomende-diagnose", "3770", "(Alg. chirurgie) Brandwonden Chirurgie")
                ("bijkomende-diagnose", "3780", "(Alg. chirurgie) Fundoplicatie")
                ("bijkomende-diagnose", "3790", "(Alg. chirurgie) Gastroschizis of Exomphalos Herstel")
                ("bijkomende-diagnose", "3800", "(Alg. chirurgie) GI endoscopie en/of Sclerotherapie")
                ("bijkomende-diagnose", "3810", "(Alg. chirurgie) Intussusceptie Herstel")
                ("bijkomende-diagnose", "3820", "(Alg. chirurgie) Kasai")
                ("bijkomende-diagnose", "3830", "(Alg. chirurgie) Laparotomie - Overige")
                ("bijkomende-diagnose", "3840", "(Alg. chirurgie) Transplantatie - Nier")
                ("bijkomende-diagnose", "3850", "(Alg. chirurgie) Transplantatie - Lever")
                ("bijkomende-diagnose", "3860", "(Alg. chirurgie) Transplantatie - Dunne Darm")
                ("bijkomende-diagnose", "3870", "(Alg. chirurgie) Urogenitale Chirurgie - Overige")
                ("bijkomende-diagnose", "3880", "(Alg. chirurgie) Laparotomie - Darmobstructie")
                ("bijkomende-diagnose", "3890", "(Alg. chirurgie) Laparotomie - Darmperforatie")
                ("bijkomende-diagnose", "3900", "(Alg. chirurgie) Laparotomie - GI Bloeding")
                ("bijkomende-diagnose", "3910", "(Alg. chirurgie) Laparotomie - Necrotiserende Enterocolitis")
                ("bijkomende-diagnose", "3920", "(Alg. chirurgie) Laparotomie - Peritonitis")
                ("bijkomende-diagnose", "3930", "(Alg. chirurgie) Laparotomie - Trauma")
                ("bijkomende-diagnose", "3931", "(Alg. chirurgie) Aorta chirurgie (Buik)")
                ("bijkomende-diagnose", "3932", "(Alg. chirurgie) Wound Debridement")
                ("bijkomende-diagnose", "3730", "(Alg. chirurgie) Algemene Chirurgie - Overige")
                ("bijkomende-diagnose", "3960", "(Craniofaciale chir.) Schedel - Reconstructie")
                ("bijkomende-diagnose", "3970", "(Craniofaciale chir.) Dentale Chirurgie")
                ("bijkomende-diagnose", "3980", "(Craniofaciale chir.) Cheiloschizis herstel")
                ("bijkomende-diagnose", "3990", "(Craniofaciale chir.) Mandibulaire Mobilisatie")
                ("bijkomende-diagnose", "4000", "(Craniofaciale chir.) Midface Mobilisatie")
                ("bijkomende-diagnose", "4010", "(Craniofaciale chir.) Palatoschizis Herstel")
                ("bijkomende-diagnose", "3950", "(Craniofaciale chir.) Craniofaciale Chirurgie - Overige")
                ("bijkomende-diagnose", "4040", "(Orthoped. chir.) Fractuur Fixatie")
                ("bijkomende-diagnose", "4050", "(Orthoped. chir.) Spinale Instrumentatie")
                ("bijkomende-diagnose", "4030", "(Orthoped. chir.) Orthopedische Chirurgie - Overige")
                ("bijkomende-diagnose", "5040", "(Hartchirurgie) Annuloplasty")
                ("bijkomende-diagnose", "5050", "(Hartchirurgie) Coronair arterie herstel met intrapulmonaire tunnel (Takeuchi)")
                ("bijkomende-diagnose", "5060", "(Hartchirurgie) Coronair arterie herstel zonder intrapulmonaire tunnel")
                ("bijkomende-diagnose", "5070", "(Hartchirurgie) Pulmonale arterie - reimplantatie")
                ("bijkomende-diagnose", "5080", "(Hartchirurgie) Aorta klep vervanging")
                ("bijkomende-diagnose", "5110", "(Hartchirurgie) Aortopexie")
                ("bijkomende-diagnose", "5120", "(Hartchirurgie) Aortoplastie (not arch repair or graft)")
                ("bijkomende-diagnose", "5130", "(Hartchirurgie) AP window repair")
                ("bijkomende-diagnose", "5140", "(Hartchirurgie) Arterial switch operation")
                ("bijkomende-diagnose", "5150", "(Hartchirurgie) Arterial switch operation with pulmonary artery band removal")
                ("bijkomende-diagnose", "5160", "(Hartchirurgie) Arterial switch operation with repair of sub PS")
                ("bijkomende-diagnose", "5170", "(Hartchirurgie) Arterial switch operation with VSD closure")
                ("bijkomende-diagnose", "5180", "(Hartchirurgie) ASD and VSD repair")
                ("bijkomende-diagnose", "5190", "(Hartchirurgie) ASD primum repair")
                ("bijkomende-diagnose", "5200", "(Hartchirurgie) ASD surgery (including ASD secundum, sinus venosus ASD, patent forament ovale closure)")
                ("bijkomende-diagnose", "5210", "(Hartchirurgie) Atrial septectomy")
                ("bijkomende-diagnose", "5220", "(Hartchirurgie) Atrial switch operation")
                ("bijkomende-diagnose", "5230", "(Hartchirurgie) Atrial switch operation with repair of sub PS")
                ("bijkomende-diagnose", "5240", "(Hartchirurgie) Atrial switch operation with VSD closure")
                ("bijkomende-diagnose", "5260", "(Hartchirurgie) Coarction repair > 30 dgn")
                ("bijkomende-diagnose", "5270", "(Hartchirurgie) Coarction repair <= 30 dgn")
                ("bijkomende-diagnose", "5290", "(Hartchirurgie) Common atrium Closure")
                ("bijkomende-diagnose", "5310", "(Hartchirurgie) Cor triatriatum repair")
                ("bijkomende-diagnose", "5320", "(Hartchirurgie) Coronary AV fistula repair")
                ("bijkomende-diagnose", "5330", "(Hartchirurgie) Damus-Kaye-Stansel procedure")
                ("bijkomende-diagnose", "5340", "(Hartchirurgie) Double switch")
                ("bijkomende-diagnose", "5350", "(Hartchirurgie) Double-outlet right ventricle repair with or without repair of right ventricular obstruction")
                ("bijkomende-diagnose", "5360", "(Hartchirurgie) Fontan procedure")
                ("bijkomende-diagnose", "5370", "(Hartchirurgie) Glenn shunt")
                ("bijkomende-diagnose", "5380", "(Hartchirurgie) Hypoplastic or interrupted arch repair with VSD closure")
                ("bijkomende-diagnose", "5390", "(Hartchirurgie) Hypoplastic or interrupted arch repair without VSD closure")
                ("bijkomende-diagnose", "5400", "(Hartchirurgie) Intracardiac tumour excision")
                ("bijkomende-diagnose", "5410", "(Hartchirurgie) Konno procedure")
                ("bijkomende-diagnose", "5420", "(Hartchirurgie) Left ventricular outflow tract patch")
                ("bijkomende-diagnose", "5430", "(Hartchirurgie) Left ventricular to pulmonary artery conduit")
                ("bijkomende-diagnose", "5440", "(Hartchirurgie) Left ventricular to right atrial shunt repair")
                ("bijkomende-diagnose", "5450", "(Hartchirurgie) Mitral valve replacement")
                ("bijkomende-diagnose", "5470", "(Hartchirurgie) Pacemaker insertion/replacement")
                ("bijkomende-diagnose", "5480", "(Hartchirurgie) Partially anomalous pulmonary venous connection surgery")
                ("bijkomende-diagnose", "5490", "(Hartchirurgie) PDA surgery >30d of age")
                ("bijkomende-diagnose", "5500", "(Hartchirurgie) PDA surgery ≤30d of age")
                ("bijkomende-diagnose", "5510", "(Hartchirurgie) Pulmonary artery banding")
                ("bijkomende-diagnose", "5520", "(Hartchirurgie) Pulmonary artery stenosis repair")
                ("bijkomende-diagnose", "5530", "(Hartchirurgie) Pulmonary outflow tract augmentation")
                ("bijkomende-diagnose", "5540", "(Hartchirurgie) Pulmonary valve replacement")
                ("bijkomende-diagnose", "5550", "(Hartchirurgie) Right ventricular infundibulectomy")
                ("bijkomende-diagnose", "5560", "(Hartchirurgie) Right ventricular to pulmonary artery conduit")
                ("bijkomende-diagnose", "5570", "(Hartchirurgie) Ross procedure")
                ("bijkomende-diagnose", "5580", "(Hartchirurgie) Semilunar valve closure")
                ("bijkomende-diagnose", "5590", "(Hartchirurgie) Septal defect repair (unspecified)")
                ("bijkomende-diagnose", "5600", "(Hartchirurgie) Stage 1 repair of hypoplatsic left heart syndrome (Norwood)")
                ("bijkomende-diagnose", "5610", "(Hartchirurgie) Stage 1 repair of nonhypoplastic left heart syndrome conditions")
                ("bijkomende-diagnose", "5620", "(Hartchirurgie) Subaortic stenosis resection")
                ("bijkomende-diagnose", "5630", "(Hartchirurgie) Systemic to pulmonary artery shunt")
                ("bijkomende-diagnose", "5640", "(Hartchirurgie) Tetralogy of Fallot total repair")
                ("bijkomende-diagnose", "5650", "(Hartchirurgie) Tetralogy of Fallot with pulmonary atresia repair")
                ("bijkomende-diagnose", "5660", "(Hartchirurgie) Total repair of anamalous pulmonary veins >30d of age")
                ("bijkomende-diagnose", "5670", "(Hartchirurgie) Total repair of anamalous pulmonary veins ≤30d of age")
                ("bijkomende-diagnose", "5680", "(Hartchirurgie) Transection of pulmonary artery")
                ("bijkomende-diagnose", "5690", "(Hartchirurgie) Transitional or complete atrioventricular canal repair with or without valve replacement")
                ("bijkomende-diagnose", "5700", "(Hartchirurgie) Transplant – Heart")
                ("bijkomende-diagnose", "5710", "(Hartchirurgie) Transplant – Heart Lung")
                ("bijkomende-diagnose", "5720", "(Hartchirurgie) Transplant – Lung")
                ("bijkomende-diagnose", "5730", "(Hartchirurgie) Transposition-VSD sub PS repair (Rastelli)")
                ("bijkomende-diagnose", "5740", "(Hartchirurgie) Transverse arch graft")
                ("bijkomende-diagnose", "5750", "(Hartchirurgie) Tricuspid valve replacement")
                ("bijkomende-diagnose", "5760", "(Hartchirurgie) Tricuspid valve repositioning for Ebstein anomaly >30d of age")
                ("bijkomende-diagnose", "5770", "(Hartchirurgie) Tricuspid valve repositioning for neonatal Ebstein ≤30d of age")
                ("bijkomende-diagnose", "5780", "(Hartchirurgie) Truncus arteriosus and interrupted arch repair")
                ("bijkomende-diagnose", "5790", "(Hartchirurgie) Truncus arteriosus repair")
                ("bijkomende-diagnose", "5800", "(Hartchirurgie) Unifocalization for tetralogy of Fallot - pulmonary atresia")
                ("bijkomende-diagnose", "5810", "(Hartchirurgie) Valvectomy of tricuspid valve")
                ("bijkomende-diagnose", "5820", "(Hartchirurgie) Valvotomy - valvuloplasty (Aortic) ≤30d of age")
                ("bijkomende-diagnose", "5830", "(Hartchirurgie) Valvotomy - valvuloplasty (Aortic) >30d of age")
                ("bijkomende-diagnose", "5840", "(Hartchirurgie) Valvotomy - valvuloplasty (Mitral)")
                ("bijkomende-diagnose", "5850", "(Hartchirurgie) Valvotomy - valvuloplasty (Pulmonary)")
                ("bijkomende-diagnose", "5860", "(Hartchirurgie) Valvotomy - valvuloplasty (Tricuspid)")
                ("bijkomende-diagnose", "5870", "(Hartchirurgie) Vascular ring surgery")
                ("bijkomende-diagnose", "5880", "(Hartchirurgie) Ventriculomyotomy")
                ("bijkomende-diagnose", "5900", "(Hartchirurgie) VSD closure and coarction repair")
                ("bijkomende-diagnose", "5910", "(Hartchirurgie) VSD closure and pulmonary artery band removal")
                ("bijkomende-diagnose", "5920", "(Hartchirurgie) VSD closure and pulmonary valvotomy or infundibular resection")
                ("bijkomende-diagnose", "5930", "(Hartchirurgie) VSD enlargement for repair of complex anomaly (single ventricle)")
                ("bijkomende-diagnose", "5940", "(Hartchirurgie) VSD repair")
                ("bijkomende-diagnose", "5950", "(Hartchirurgie) Chest closure")
                ("bijkomende-diagnose", "5960", "(Hartchirurgie) ECMO cannulation/exploration")
                ("bijkomende-diagnose", "5970", "(Hartchirurgie) Emergency chest opening")
                ("bijkomende-diagnose", "5980", "(Hartchirurgie) Pulmonary venous stenosis repair")
                ("bijkomende-diagnose", "5020", "(Hartchirurgie) Hartoperatie Gesloten - Overige")
                ("bijkomende-diagnose", "5030", "(Hartchirurgie) Hartoperatie Open - Overige")
                ("compl-verrichting-tube", "1", "Endotracheale tube in situ")
                ("compl-verrichting-tube", "2", "Tracheacanule in situ")
                ("compl-verrichting-bloedkweek", "3", "Sepsis (criterium 1 geen huidflora)")
                ("compl-verrichting-bloedkweek", "4", "Sepsis (criterium 2A met huidflora)")
                ("compl-verrichting-bloedkweek", "5", "Sepsis (criterium 2B met huidflora)")
                ("compl-verrichting-medicatiefout", "6", "geen medicatiefout")
                ("compl-verrichting-medicatiefout", "7", "een onder- of overdosering")
                ("compl-verrichting-medicatiefout", "8", "een verkeerde medicamentkeuze")
                ("compl-verrichting-medicatiefout", "9", "een verkeerde bereiding")
                ("compl-verrichting-medicatiefout", "10", "een toedieningsfout")
                ("beademingtype", "1", "Beademing (invasief)")
                ("beademingtype", "2", "Beademing (non-invasief)")
                ("beademingtype", "3", "Overige ademhalingsondersteuning")
                ("beademingsinvoer", "2", "Geen beademing")
                ("beademingsinvoer", "3", "Registratie totale beademingsduur")
                ("beademingsinvoer", "4", "Registratie beademingsepisodes (met start- en stopdata)")
                ("beademingsinvoer", "5", "Zowel registratie Totale duur als Episodes")
                ("e-emv", "1", "Geen reactie")
                ("e-emv", "2", "op pijnprikkel")
                ("e-emv", "3", "op aanspreken")
                ("e-emv", "4", "Spontaan")
                ("e-emv12", "1", "Geen reactie")
                ("e-emv12", "2", "op pijnprikkel")
                ("e-emv12", "3", "op aanspreken")
                ("e-emv12", "4", "Spontaan")
                ("m-emv", "1", "Geen reactie")
                ("m-emv", "2", "abnormaal strekken op pijnprikkel")
                ("m-emv", "3", "abnormaal buigen op pijnprikkel")
                ("m-emv", "4", "trekt terug op pijnprikkel")
                ("m-emv", "5", "lokaliseert pijnprikkel")
                ("m-emv", "6", "Spontaan/gehoorzaamt opdrachten")
                ("m-emv12", "1", "Geen reactie")
                ("m-emv12", "2", "abnormaal strekken op pijnprikkel")
                ("m-emv12", "3", "abnormaal buigen op pijnprikkel")
                ("m-emv12", "4", "trekt terug op pijnprikkel")
                ("m-emv12", "5", "lokaliseert pijnprikkel")
                ("m-emv12", "6", "Spontaan/gehoorzaamt opdrachten")
                ("v-emv", "1", "geen reactie")
                ("v-emv", "2", "kreunt op pijnprikkel/onverstaanbare geluiden")
                ("v-emv", "3", "huilt alleen op pijnprikkel/onsamenhangende woorden")
                ("v-emv", "4", "minder dan gebruikelijke woorden, huilt geïrriteerd/gedesoriënteerd praten")
                ("v-emv", "5", "Alert, brabbelen, gebruikelijke woorden")
                ("v-emv12", "1", "geen reactie")
                ("v-emv12", "2", "kreunt op pijnprikkel/onverstaanbare geluiden")
                ("v-emv12", "3", "huilt alleen op pijnprikkel/onsamenhangende woorden")
                ("v-emv12", "4", "minder dan gebruikelijke woorden, huilt geïrriteerd/gedesoriënteerd praten")
                ("v-emv12", "5", "Alert, brabbelen, gebruikelijke woorden")
                ("adm-desthospitalid-bu", "1001", "Hamburg-Eppendorf, Universitätsklinikum")
                ("adm-desthospitalid-bu", "1002", "Meppen, Krankenhaus Ludmillenstift")
                ("adm-desthospitalid-bu", "1003", "Leer, Borromäus-Hospital")
                ("adm-desthospitalid-bu", "1004", "Münster, Universitätsklinikum")
                ("adm-desthospitalid-bu", "1005", "Bremen, Klinikum Bremen")
                ("adm-desthospitalid-bu", "1006", "Aken, Aachen (Klinikum)")
                ("adm-desthospitalid-bu", "1999", "Ziekenhuis Duitsland (ongespecificeerd)")
                ("adm-desthospitalid-bu", "2001", "Edegem, Universitair Ziekenhuis Antwerpen")
                ("adm-desthospitalid-bu", "2002", "Leuven, Ziekenhuis Gasthuisberg (KUL)")
                ("adm-desthospitalid-bu", "2003", "Brussel, (België)")
                ("adm-desthospitalid-bu", "2004", "Gent, (België) UZ Gent")
                ("adm-desthospitalid-bu", "2999", "Ziekenhuis België (ongespecificeerd)")
                ("adm-desthospitalid-bu", "3001", "Lille, Centre Hospitalier Régional et Universitaire (CHRU)")
                ("adm-desthospitalid-bu", "3999", "Ziekenhuis Frankrijk (ongespecificeerd)")
                ("adm-desthospitalid-bu", "4999", "Ziekenhuis VK (ongespecificeerd)")
                ("adm-desthospitalid-bu", "5999", "Ziekenhuis Oostenrijk (ongespecificeerd)")
                ("adm-desthospitalid-bu", "10000", "Ziekenhuis Overig Buitenland")
                ("adm-sourcehospitalid-bu", "1001", "Hamburg-Eppendorf, Universitätsklinikum")
                ("adm-sourcehospitalid-bu", "1002", "Meppen, Krankenhaus Ludmillenstift")
                ("adm-sourcehospitalid-bu", "1003", "Leer, Borromäus-Hospital")
                ("adm-sourcehospitalid-bu", "1004", "Münster, Universitätsklinikum")
                ("adm-sourcehospitalid-bu", "1005", "Bremen, Klinikum Bremen")
                ("adm-sourcehospitalid-bu", "1006", "Aken, Aachen (Klinikum)")
                ("adm-sourcehospitalid-bu", "1999", "Ziekenhuis Duitsland (ongespecificeerd)")
                ("adm-sourcehospitalid-bu", "2001", "Edegem, Universitair Ziekenhuis Antwerpen")
                ("adm-sourcehospitalid-bu", "2002", "Leuven, Ziekenhuis Gasthuisberg (KUL)")
                ("adm-sourcehospitalid-bu", "2003", "Brussel, (België)")
                ("adm-sourcehospitalid-bu", "2004", "Gent, (België) UZ Gent")
                ("adm-sourcehospitalid-bu", "2999", "Ziekenhuis België (ongespecificeerd)")
                ("adm-sourcehospitalid-bu", "3001", "Lille, Centre Hospitalier Régional et Universitaire (CHRU)")
                ("adm-sourcehospitalid-bu", "3999", "Ziekenhuis Frankrijk (ongespecificeerd)")
                ("adm-sourcehospitalid-bu", "4999", "Ziekenhuis VK (ongespecificeerd)")
                ("adm-sourcehospitalid-bu", "5999", "Ziekenhuis Oostenrijk (ongespecificeerd)")
                ("adm-sourcehospitalid-bu", "10000", "Ziekenhuis Overig Buitenland")
                ("adm-admhospunitid", "101", "PICU (IC-Kinderen)")
                ("adm-admhospunitid", "102", "NICU (IC-Neonatologie)")
                ("adm-admhospunitid", "103", "Volwassen-IC /CCU")
                ("adm-admhospunitid", "106", "(Zorg)Afdeling (zonder extra bewaking)")
                ("adm-admhospunitid", "107", "SEH")
                ("adm-admhospunitid", "109", "Recovery")
                ("adm-admhospunitid", "105", "Afdeling met extra bewaking (HDU/HighCare)")
                ("adm-admhospunitid", "110", "Kraamafdeling")
                ("adm-admhospunitid", "77", "Overig")
                ("adm-admhospunitid", "108", "OK")
                ("adm-admhospunitid", "104", "Longstay-ic")
                ("adm-admhospunitid", "99", "Onbekend")
                ("bestm-tran-door", "101", "Eigen UMC")
                ("bestm-tran-door", "116", "Ander UMC")
                ("bestm-tran-door", "102", "Niet UMC")
                ("bestm-tran-door", "103", "Zkh Buitenland")
                ("bestm-tran-door", "99", "Onbekend")
                ("herk-tran-door", "101", "Eigen UMC")
                ("herk-tran-door", "116", "Ander UMC")
                ("herk-tran-door", "102", "Niet UMC")
                ("herk-tran-door", "103", "Zkh Buitenland")
                ("herk-tran-door", "99", "Onbekend")
                ("adm-transport-adm", "11", "PICU team")
                ("adm-transport-adm", "12", "NICU team")
                ("adm-transport-adm", "14", "MICU team")
                ("adm-transport-adm", "6", "PICU/NICU-team (vervallen optie per 15-5-2017)")
                ("adm-transport-adm", "15", "MMT/Traumateam")
                ("adm-transport-adm", "16", "Niet PICU/NICU specialist")
                ("adm-transport-adm", "18", "GGD-Transport (zonder arts)")
                ("adm-transport-adm", "19", "Niet (para-)medisch begeleid (bv: Ouders)")
                ("adm-transport-adm", "99", "Onbekend")
                ("adm-transport-dis", "11", "PICU team")
                ("adm-transport-dis", "12", "NICU team")
                ("adm-transport-dis", "14", "MICU team")
                ("adm-transport-dis", "6", "PICU/NICU-team (vervallen optie per 15-5-2017)")
                ("adm-transport-dis", "15", "MMT/Traumateam")
                ("adm-transport-dis", "16", "Niet PICU/NICU specialist")
                ("adm-transport-dis", "18", "GGD-Transport (zonder arts)")
                ("adm-transport-dis", "19", "Niet (para-)medisch begeleid (bv: Ouders)")
                ("adm-transport-dis", "99", "Onbekend")
                ("adm-desthospunitid", "106", "(Zorg)Afdeling (zonder extra bewaking)")
                ("adm-desthospunitid", "101", "PICU (IC-Kinderen)")
                ("adm-desthospunitid", "102", "NICU (IC-Neonatologie)")
                ("adm-desthospunitid", "103", "Volwassen-IC /CCU")
                ("adm-desthospunitid", "105", "Afdeling met extra bewaking (HDU/HighCare)")
                ("adm-desthospunitid", "109", "Recovery")
                ("adm-desthospunitid", "77", "Overig")
                ("adm-desthospunitid", "110", "Kraamafdeling")
                ("adm-desthospunitid", "104", "Longstay-ic")
                ("adm-desthospunitid", "128", "Mortuarium")
                ("adm-desthospunitid", "99", "Onbekend")
                ("adm-desthospitalid", "100", "Alkmaar, MC Alkmaar")
                ("adm-desthospitalid", "18", "Almelo, Ziekenhuisgroep Twente (Twenteborg Ziekenhuis)")
                ("adm-desthospitalid", "284", "Almere, Flevoziekenhuis")
                ("adm-desthospitalid", "436", "Alphen a/d Rijn, Rijnland Ziekenhuis")
                ("adm-desthospitalid", "449", "Amersfoort, Meander MC")
                ("adm-desthospitalid", "98", "Amstelveen, Ziekenhuis Amstelland")
                ("adm-desthospitalid", "383", "Amsterdam, AMC")
                ("adm-desthospitalid", "409", "Amsterdam, Antoni van Leeuwenhoek Ziekenhuis")
                ("adm-desthospitalid", "261", "Amsterdam, BovenIJ Ziekenhuis")
                ("adm-desthospitalid", "203", "Amsterdam, OLVG")
                ("adm-desthospitalid", "423", "Amsterdam, OLVG - Oosterpark")
                ("adm-desthospitalid", "190", "Amsterdam, Sint Lucas Andreas Ziekenhuis")
                ("adm-desthospitalid", "200", "Amsterdam, Slotervaart Ziekenhuis")
                ("adm-desthospitalid", "382", "Amsterdam, VU MC")
                ("adm-desthospitalid", "433", "Apeldoorn, Gelre ziekenhuizen (voorheen lokatie Lukas)")
                ("adm-desthospitalid", "268", "Arnhem, Rijnstate Arnhem")
                ("adm-desthospitalid", "9", "Assen, Wilhelmina ziekenhuis")
                ("adm-desthospitalid", "274", "Baarn, Meander MC - Baarn")
                ("adm-desthospitalid", "97", "Bergen op Zoom, Bravis ziekenhuis (voorheen Lievensberg)")
                ("adm-desthospitalid", "270", "Beverwijk, Rode Kruis Ziekenhuis")
                ("adm-desthospitalid", "271", "Blaricum, Tergooi Blaricum")
                ("adm-desthospitalid", "139", "Boxmeer, Maasziekenhuis")
                ("adm-desthospitalid", "262", "Breda, Amphia Ziekenhuis - Langendijk")
                ("adm-desthospitalid", "102", "Breda, Amphia Ziekenhuis - Molengracht")
                ("adm-desthospitalid", "442", "Brunssum, Atrium MC - Brunssum")
                ("adm-desthospitalid", "388", "Capelle aan den Ijssel, IJsselland Ziekenhuis")
                ("adm-desthospitalid", "243", "Delft, Reinier de Graaf")
                ("adm-desthospitalid", "113", "Delfzijl, Ommelander Ziekenhuis Groep, locatie Delfzicht")
                ("adm-desthospitalid", "450", "Den Bosch, Jeroen Bosch Ziekenhuis")
                ("adm-desthospitalid", "22", "Den Haag, Bronovo Ziekenhuis")
                ("adm-desthospitalid", "329", "Den Haag, Juliana Kinderziekenhuis (Leyweg, voorheen Sportlaan)")
                ("adm-desthospitalid", "180", "Den Haag, Leyweg - Haga Ziekenhuis")
                ("adm-desthospitalid", "105", "Den Haag, MCH - Westeinde")
                ("adm-desthospitalid", "428", "Den Haag, Sportlaan (voorheen Rode Kruis Ziekenhuis) - Haga Ziekenhuis")
                ("adm-desthospitalid", "140", "Den Helder, Gemini Ziekenhuis")
                ("adm-desthospitalid", "429", "Deurne, Elkerliek Ziekenhuis - Deurne")
                ("adm-desthospitalid", "3", "Deventer ziekenhuis")
                ("adm-desthospitalid", "179", "Dirksland, Ziekenhuis Van Weel-Bethesda")
                ("adm-desthospitalid", "34", "Doetinchem, Slingeland Ziekenhuis")
                ("adm-desthospitalid", "77", "Dokkum, Talma Sionsberg")
                ("adm-desthospitalid", "278", "Dordrecht, Albert Schweitzer - Amstelwijck")
                ("adm-desthospitalid", "258", "Dordrecht, Albert Schweitzer - Dordwijk")
                ("adm-desthospitalid", "129", "Drachten, Ziekenhuis Nij Smellinghe")
                ("adm-desthospitalid", "265", "Ede, Ziekenhuis Gelderse Vallei")
                ("adm-desthospitalid", "35", "Eindhoven, Catharina Ziekenhuis")
                ("adm-desthospitalid", "27", "Eindhoven, Maxima MC - Eindhoven")
                ("adm-desthospitalid", "425", "Emmeloord, Antonius ziekenhuis")
                ("adm-desthospitalid", "178", "Emmen, Scheper Ziekenhuis")
                ("adm-desthospitalid", "269", "Enschede, Medisch Spectrum Twente")
                ("adm-desthospitalid", "231", "Geldrop, Sint Anna Ziekenhuis")
                ("adm-desthospitalid", "242", "Goes, Admiraal de Ruyter ziekenhuis")
                ("adm-desthospitalid", "263", "Gorinchem, Beatrixziekenhuis (Rivas)")
                ("adm-desthospitalid", "287", "Gouda, Groene Hart Ziekenhuis")
                ("adm-desthospitalid", "282", "Groningen, Martini Ziekenhuis")
                ("adm-desthospitalid", "376", "Groningen, UMCG")
                ("adm-desthospitalid", "445", "Haaksbergen, Medisch Spectrum Twente (polikliniek)")
                ("adm-desthospitalid", "286", "Haarlem, Spaarne (Kennemer) Gasthuis (ongespecificeerd)")
                ("adm-desthospitalid", "451", "Haarlem, Spaarne Gasthuis-Noord")
                ("adm-desthospitalid", "441", "Haarlem, Spaarne Gasthuis-Zuid (voorheen EG)")
                ("adm-desthospitalid", "239", "Hardenberg, Röpcke-Zweers ziekenhuis")
                ("adm-desthospitalid", "39", "Harderwijk, Ziekenhuis St. Jansdal")
                ("adm-desthospitalid", "206", "Harlingen, MCL")
                ("adm-desthospitalid", "438", "Heemstede, Spaarne Ziekenhuis - Heemstede")
                ("adm-desthospitalid", "59", "Heerenveen, Ziekenhuis De Tjongerschans")
                ("adm-desthospitalid", "279", "Heerlen, Zuyderland MC - Heerlen")
                ("adm-desthospitalid", "248", "Helmond, Elkerliek Ziekenhuis - Helmond")
                ("adm-desthospitalid", "121", "Hengelo, Ziekenhuisgroep Twente")
                ("adm-desthospitalid", "245", "Hilversum, Tergooi Hilversum")
                ("adm-desthospitalid", "448", "Hoofddorp, Spaarne ziekenhuis")
                ("adm-desthospitalid", "54", "Hoogeveen, Ziekenhuis Bethesda")
                ("adm-desthospitalid", "247", "Hoorn, Westfries Gasthuis")
                ("adm-desthospitalid", "405", "Hulst, Zorgsaam Zeeuws-Vlaanderen - Liduina")
                ("adm-desthospitalid", "148", "Kerkrade, Atrium MC - Kerkrade")
                ("adm-desthospitalid", "407", "Leerdam, Rivas Zorggroep - Lingepolikliniek")
                ("adm-desthospitalid", "400", "Leeuwarden, MCL (voorheen MCL-Zuid)")
                ("adm-desthospitalid", "29", "Leiden, Diaconessenhuis (Alrijne)")
                ("adm-desthospitalid", "384", "Leiden, LUMC")
                ("adm-desthospitalid", "73", "Leiderdorp, Rijnland Ziekenhuis (Alrijne)")
                ("adm-desthospitalid", "155", "Leidschendam, MC Haaglanden - Sint Antoniushove")
                ("adm-desthospitalid", "277", "Lelystad, MC Zuiderzee")
                ("adm-desthospitalid", "414", "Losser, Medisch Spectrum Twente - Losser (buitenpoli)")
                ("adm-desthospitalid", "86", "Maastricht, MUMC")
                ("adm-desthospitalid", "167", "Meppel, Isala Diaconessenhuis (Meppel)")
                ("adm-desthospitalid", "435", "Naaldwijk, Behandelcentrum Westland (BCW)")
                ("adm-desthospitalid", "6", "Nieuwegein, Sint Antonius Ziekenhuis")
                ("adm-desthospitalid", "57", "Nijmegen, Canisius-Wilhelmina")
                ("adm-desthospitalid", "377", "Nijmegen, Radboudumc")
                ("adm-desthospitalid", "416", "Nijmegen, Sint Maartenskliniek")
                ("adm-desthospitalid", "418", "Oldenzaal, Medisch Spectrum Twente - Oldenzaal")
                ("adm-desthospitalid", "419", "Oostburg, Zorgsaam Zeeuws-Vlaanderen - Antonius")
                ("adm-desthospitalid", "281", "Oosterhout, Amphia Ziekenhuis - Pasteurlaan")
                ("adm-desthospitalid", "106", "Oss, Bernhoven (sinds 2015 Polikliniek)")
                ("adm-desthospitalid", "244", "Purmerend, Waterlandziekenhuis")
                ("adm-desthospitalid", "63", "Roermond, Laurentius Ziekenhuis")
                ("adm-desthospitalid", "160", "Roosendaal, Bravis Ziekenhuis (voorheen Franciscus)")
                ("adm-desthospitalid", "422", "Rotterdam, Erasmus MC - centrumlocatie")
                ("adm-desthospitalid", "421", "Rotterdam, Erasmus MC - Daniel den Hoed")
                ("adm-desthospitalid", "386", "Rotterdam, Erasmus MC - Sophia")
                ("adm-desthospitalid", "205", "Rotterdam, Havenziekenhuis")
                ("adm-desthospitalid", "128", "Rotterdam, Ikazia Ziekenhuis")
                ("adm-desthospitalid", "453", "Rotterdam, Maasstad")
                ("adm-desthospitalid", "415", "Rotterdam, Oogziekenhuis Rotterdam")
                ("adm-desthospitalid", "51", "Rotterdam, Sint Franciscus Gasthuis")
                ("adm-desthospitalid", "452", "Rotterdam - Capelle a/d IJssel, Erasmus MC-Pallieterburght (longstay ICK)")
                ("adm-desthospitalid", "67", "Schiedam, Vlietland Ziekenhuis")
                ("adm-desthospitalid", "257", "Sittard, Zuyderland MC Sittard-Geleen")
                ("adm-desthospitalid", "412", "Sliedrecht, Albert Schweitzer - Sliedrecht")
                ("adm-desthospitalid", "43", "Sneek, Antonius Ziekenhuis")
                ("adm-desthospitalid", "276", "Spijkenisse, MC (voorheen: Ruwaard van Putten)")
                ("adm-desthospitalid", "112", "Stadskanaal, Refaja Ziekenhuis")
                ("adm-desthospitalid", "273", "Terneuzen, Zorgsaam Zeeuws-Vlaanderen - De Honte")
                ("adm-desthospitalid", "237", "Tiel, Ziekenhuis Rivierenland")
                ("adm-desthospitalid", "143", "Tilburg, Sint Elisabeth Ziekenhuis")
                ("adm-desthospitalid", "72", "Tilburg, TweeSteden ziekenhuis - Tilburg")
                ("adm-desthospitalid", "454", "Uden, Bernhoven")
                ("adm-desthospitalid", "403", "Utrecht, Centraal Militair Hospitaal")
                ("adm-desthospitalid", "61", "Utrecht, Diakonessenhuis - Utrecht")
                ("adm-desthospitalid", "455", "Utrecht, Leidsche Rijn (st Antonius)")
                ("adm-desthospitalid", "456", "Utrecht, Prinses Maxima Centrum voor kinderoncologie")
                ("adm-desthospitalid", "379", "Utrecht, UMCU")
                ("adm-desthospitalid", "381", "Utrecht, Wilhelmina Kinderziekenhuis (UMCU)")
                ("adm-desthospitalid", "182", "Veldhoven, Maxima Medisch Centrum")
                ("adm-desthospitalid", "280", "Venlo, Vie Curi Medisch Centrum - Venlo")
                ("adm-desthospitalid", "443", "Venray, VieCuri - MC Noord-Limburg - Venray")
                ("adm-desthospitalid", "241", "Vlissingen, ADRZ Vlissingen")
                ("adm-desthospitalid", "118", "Voorburg, Reinier de Graaf (voorheen: Diaconessenhuis)")
                ("adm-desthospitalid", "183", "Waalwijk, TweeSteden ziekenhuis - Waalwijk")
                ("adm-desthospitalid", "127", "Weert, Sint Jans Gasthuis")
                ("adm-desthospitalid", "173", "Winschoten, Ommelander Ziekenhuis Groep locatie Lucas")
                ("adm-desthospitalid", "238", "Winterswijk, Streekziekenhuis Koningin Beatrix")
                ("adm-desthospitalid", "228", "Woerden, Hofpoort Ziekenhuis")
                ("adm-desthospitalid", "65", "Zaandam, Zaans Medisch Centrum")
                ("adm-desthospitalid", "184", "Zeist, Diakonessenhuis - Zeist")
                ("adm-desthospitalid", "186", "Zevenaar, Rijnstate locatie Zevenaar")
                ("adm-desthospitalid", "264", "Zoetermeer, Lange Land Ziekenhuis")
                ("adm-desthospitalid", "101", "Zutphen, Gelre ziekenhuizen - Het Spittaal")
                ("adm-desthospitalid", "427", "Zwijndrecht, Albert Schweitzer - Zwijndrecht")
                ("adm-desthospitalid", "1", "Zwolle, Isala (voorheen isala klinieken - sophia)")
                ("adm-sourcehospitalid", "100", "Alkmaar, MC Alkmaar")
                ("adm-sourcehospitalid", "18", "Almelo, Ziekenhuisgroep Twente (Twenteborg Ziekenhuis)")
                ("adm-sourcehospitalid", "284", "Almere, Flevoziekenhuis")
                ("adm-sourcehospitalid", "436", "Alphen a/d Rijn, Rijnland Ziekenhuis")
                ("adm-sourcehospitalid", "449", "Amersfoort, Meander MC")
                ("adm-sourcehospitalid", "98", "Amstelveen, Ziekenhuis Amstelland")
                ("adm-sourcehospitalid", "383", "Amsterdam, AMC")
                ("adm-sourcehospitalid", "409", "Amsterdam, Antoni van Leeuwenhoek Ziekenhuis")
                ("adm-sourcehospitalid", "261", "Amsterdam, BovenIJ Ziekenhuis")
                ("adm-sourcehospitalid", "203", "Amsterdam, OLVG")
                ("adm-sourcehospitalid", "423", "Amsterdam, OLVG - Oosterpark")
                ("adm-sourcehospitalid", "190", "Amsterdam, Sint Lucas Andreas Ziekenhuis")
                ("adm-sourcehospitalid", "200", "Amsterdam, Slotervaart Ziekenhuis")
                ("adm-sourcehospitalid", "382", "Amsterdam, VU MC")
                ("adm-sourcehospitalid", "433", "Apeldoorn, Gelre ziekenhuizen (voorheen lokatie Lukas)")
                ("adm-sourcehospitalid", "268", "Arnhem, Rijnstate Arnhem")
                ("adm-sourcehospitalid", "9", "Assen, Wilhelmina ziekenhuis")
                ("adm-sourcehospitalid", "274", "Baarn, Meander MC - Baarn")
                ("adm-sourcehospitalid", "97", "Bergen op Zoom, Bravis ziekenhuis (voorheen Lievensberg)")
                ("adm-sourcehospitalid", "270", "Beverwijk, Rode Kruis Ziekenhuis")
                ("adm-sourcehospitalid", "271", "Blaricum, Tergooi Blaricum")
                ("adm-sourcehospitalid", "139", "Boxmeer, Maasziekenhuis")
                ("adm-sourcehospitalid", "262", "Breda, Amphia Ziekenhuis - Langendijk")
                ("adm-sourcehospitalid", "102", "Breda, Amphia Ziekenhuis - Molengracht")
                ("adm-sourcehospitalid", "442", "Brunssum, Atrium MC - Brunssum")
                ("adm-sourcehospitalid", "388", "Capelle aan den Ijssel, IJsselland Ziekenhuis")
                ("adm-sourcehospitalid", "243", "Delft, Reinier de Graaf")
                ("adm-sourcehospitalid", "113", "Delfzijl, Ommelander Ziekenhuis Groep, locatie Delfzicht")
                ("adm-sourcehospitalid", "450", "Den Bosch, Jeroen Bosch Ziekenhuis")
                ("adm-sourcehospitalid", "22", "Den Haag, Bronovo Ziekenhuis")
                ("adm-sourcehospitalid", "329", "Den Haag, Juliana Kinderziekenhuis (Leyweg, voorheen Sportlaan)")
                ("adm-sourcehospitalid", "180", "Den Haag, Leyweg - Haga Ziekenhuis")
                ("adm-sourcehospitalid", "105", "Den Haag, MCH - Westeinde")
                ("adm-sourcehospitalid", "428", "Den Haag, Sportlaan (voorheen Rode Kruis Ziekenhuis) - Haga Ziekenhuis")
                ("adm-sourcehospitalid", "140", "Den Helder, Gemini Ziekenhuis")
                ("adm-sourcehospitalid", "429", "Deurne, Elkerliek Ziekenhuis - Deurne")
                ("adm-sourcehospitalid", "3", "Deventer ziekenhuis")
                ("adm-sourcehospitalid", "179", "Dirksland, Ziekenhuis Van Weel-Bethesda")
                ("adm-sourcehospitalid", "34", "Doetinchem, Slingeland Ziekenhuis")
                ("adm-sourcehospitalid", "77", "Dokkum, Talma Sionsberg")
                ("adm-sourcehospitalid", "278", "Dordrecht, Albert Schweitzer - Amstelwijck")
                ("adm-sourcehospitalid", "258", "Dordrecht, Albert Schweitzer - Dordwijk")
                ("adm-sourcehospitalid", "129", "Drachten, Ziekenhuis Nij Smellinghe")
                ("adm-sourcehospitalid", "265", "Ede, Ziekenhuis Gelderse Vallei")
                ("adm-sourcehospitalid", "35", "Eindhoven, Catharina Ziekenhuis")
                ("adm-sourcehospitalid", "27", "Eindhoven, Maxima MC - Eindhoven")
                ("adm-sourcehospitalid", "425", "Emmeloord, Antonius ziekenhuis")
                ("adm-sourcehospitalid", "178", "Emmen, Scheper Ziekenhuis")
                ("adm-sourcehospitalid", "269", "Enschede, Medisch Spectrum Twente")
                ("adm-sourcehospitalid", "231", "Geldrop, Sint Anna Ziekenhuis")
                ("adm-sourcehospitalid", "242", "Goes, Admiraal de Ruyter ziekenhuis")
                ("adm-sourcehospitalid", "263", "Gorinchem, Beatrixziekenhuis (Rivas)")
                ("adm-sourcehospitalid", "287", "Gouda, Groene Hart Ziekenhuis")
                ("adm-sourcehospitalid", "282", "Groningen, Martini Ziekenhuis")
                ("adm-sourcehospitalid", "376", "Groningen, UMCG")
                ("adm-sourcehospitalid", "445", "Haaksbergen, Medisch Spectrum Twente (polikliniek)")
                ("adm-sourcehospitalid", "286", "Haarlem, Spaarne (Kennemer) Gasthuis (ongespecificeerd)")
                ("adm-sourcehospitalid", "451", "Haarlem, Spaarne Gasthuis-Noord")
                ("adm-sourcehospitalid", "441", "Haarlem, Spaarne Gasthuis-Zuid (voorheen EG)")
                ("adm-sourcehospitalid", "239", "Hardenberg, Röpcke-Zweers ziekenhuis")
                ("adm-sourcehospitalid", "39", "Harderwijk, Ziekenhuis St. Jansdal")
                ("adm-sourcehospitalid", "206", "Harlingen, MCL")
                ("adm-sourcehospitalid", "438", "Heemstede, Spaarne Ziekenhuis - Heemstede")
                ("adm-sourcehospitalid", "59", "Heerenveen, Ziekenhuis De Tjongerschans")
                ("adm-sourcehospitalid", "279", "Heerlen, Zuyderland MC - Heerlen")
                ("adm-sourcehospitalid", "248", "Helmond, Elkerliek Ziekenhuis - Helmond")
                ("adm-sourcehospitalid", "121", "Hengelo, Ziekenhuisgroep Twente")
                ("adm-sourcehospitalid", "245", "Hilversum, Tergooi Hilversum")
                ("adm-sourcehospitalid", "448", "Hoofddorp, Spaarne ziekenhuis")
                ("adm-sourcehospitalid", "54", "Hoogeveen, Ziekenhuis Bethesda")
                ("adm-sourcehospitalid", "247", "Hoorn, Westfries Gasthuis")
                ("adm-sourcehospitalid", "405", "Hulst, Zorgsaam Zeeuws-Vlaanderen - Liduina")
                ("adm-sourcehospitalid", "148", "Kerkrade, Atrium MC - Kerkrade")
                ("adm-sourcehospitalid", "407", "Leerdam, Rivas Zorggroep - Lingepolikliniek")
                ("adm-sourcehospitalid", "400", "Leeuwarden, MCL (voorheen MCL-Zuid)")
                ("adm-sourcehospitalid", "29", "Leiden, Diaconessenhuis (Alrijne)")
                ("adm-sourcehospitalid", "384", "Leiden, LUMC")
                ("adm-sourcehospitalid", "73", "Leiderdorp, Rijnland Ziekenhuis (Alrijne)")
                ("adm-sourcehospitalid", "155", "Leidschendam, MC Haaglanden - Sint Antoniushove")
                ("adm-sourcehospitalid", "277", "Lelystad, MC Zuiderzee")
                ("adm-sourcehospitalid", "414", "Losser, Medisch Spectrum Twente - Losser (buitenpoli)")
                ("adm-sourcehospitalid", "86", "Maastricht, MUMC")
                ("adm-sourcehospitalid", "167", "Meppel, Isala Diaconessenhuis (Meppel)")
                ("adm-sourcehospitalid", "435", "Naaldwijk, Behandelcentrum Westland (BCW)")
                ("adm-sourcehospitalid", "6", "Nieuwegein, Sint Antonius Ziekenhuis")
                ("adm-sourcehospitalid", "57", "Nijmegen, Canisius-Wilhelmina")
                ("adm-sourcehospitalid", "377", "Nijmegen, Radboudumc")
                ("adm-sourcehospitalid", "416", "Nijmegen, Sint Maartenskliniek")
                ("adm-sourcehospitalid", "418", "Oldenzaal, Medisch Spectrum Twente - Oldenzaal")
                ("adm-sourcehospitalid", "419", "Oostburg, Zorgsaam Zeeuws-Vlaanderen - Antonius")
                ("adm-sourcehospitalid", "281", "Oosterhout, Amphia Ziekenhuis - Pasteurlaan")
                ("adm-sourcehospitalid", "106", "Oss, Bernhoven (sinds 2015 Polikliniek)")
                ("adm-sourcehospitalid", "244", "Purmerend, Waterlandziekenhuis")
                ("adm-sourcehospitalid", "63", "Roermond, Laurentius Ziekenhuis")
                ("adm-sourcehospitalid", "160", "Roosendaal, Bravis Ziekenhuis (voorheen Franciscus)")
                ("adm-sourcehospitalid", "422", "Rotterdam, Erasmus MC - centrumlocatie")
                ("adm-sourcehospitalid", "421", "Rotterdam, Erasmus MC - Daniel den Hoed")
                ("adm-sourcehospitalid", "386", "Rotterdam, Erasmus MC - Sophia")
                ("adm-sourcehospitalid", "205", "Rotterdam, Havenziekenhuis")
                ("adm-sourcehospitalid", "128", "Rotterdam, Ikazia Ziekenhuis")
                ("adm-sourcehospitalid", "453", "Rotterdam, Maasstad")
                ("adm-sourcehospitalid", "415", "Rotterdam, Oogziekenhuis Rotterdam")
                ("adm-sourcehospitalid", "51", "Rotterdam, Sint Franciscus Gasthuis")
                ("adm-sourcehospitalid", "452", "Rotterdam - Capelle a/d IJssel, Erasmus MC-Pallieterburght (longstay ICK)")
                ("adm-sourcehospitalid", "67", "Schiedam, Vlietland Ziekenhuis")
                ("adm-sourcehospitalid", "257", "Sittard, Zuyderland MC Sittard-Geleen")
                ("adm-sourcehospitalid", "412", "Sliedrecht, Albert Schweitzer - Sliedrecht")
                ("adm-sourcehospitalid", "43", "Sneek, Antonius Ziekenhuis")
                ("adm-sourcehospitalid", "276", "Spijkenisse, MC (voorheen: Ruwaard van Putten)")
                ("adm-sourcehospitalid", "112", "Stadskanaal, Refaja Ziekenhuis")
                ("adm-sourcehospitalid", "273", "Terneuzen, Zorgsaam Zeeuws-Vlaanderen - De Honte")
                ("adm-sourcehospitalid", "237", "Tiel, Ziekenhuis Rivierenland")
                ("adm-sourcehospitalid", "143", "Tilburg, Sint Elisabeth Ziekenhuis")
                ("adm-sourcehospitalid", "72", "Tilburg, TweeSteden ziekenhuis - Tilburg")
                ("adm-sourcehospitalid", "454", "Uden, Bernhoven")
                ("adm-sourcehospitalid", "403", "Utrecht, Centraal Militair Hospitaal")
                ("adm-sourcehospitalid", "61", "Utrecht, Diakonessenhuis - Utrecht")
                ("adm-sourcehospitalid", "455", "Utrecht, Leidsche Rijn (st Antonius)")
                ("adm-sourcehospitalid", "456", "Utrecht, Prinses Maxima Centrum voor kinderoncologie")
                ("adm-sourcehospitalid", "379", "Utrecht, UMCU")
                ("adm-sourcehospitalid", "381", "Utrecht, Wilhelmina Kinderziekenhuis (UMCU)")
                ("adm-sourcehospitalid", "182", "Veldhoven, Maxima Medisch Centrum")
                ("adm-sourcehospitalid", "280", "Venlo, Vie Curi Medisch Centrum - Venlo")
                ("adm-sourcehospitalid", "443", "Venray, VieCuri - MC Noord-Limburg - Venray")
                ("adm-sourcehospitalid", "241", "Vlissingen, ADRZ Vlissingen")
                ("adm-sourcehospitalid", "118", "Voorburg, Reinier de Graaf (voorheen: Diaconessenhuis)")
                ("adm-sourcehospitalid", "183", "Waalwijk, TweeSteden ziekenhuis - Waalwijk")
                ("adm-sourcehospitalid", "127", "Weert, Sint Jans Gasthuis")
                ("adm-sourcehospitalid", "173", "Winschoten, Ommelander Ziekenhuis Groep locatie Lucas")
                ("adm-sourcehospitalid", "238", "Winterswijk, Streekziekenhuis Koningin Beatrix")
                ("adm-sourcehospitalid", "228", "Woerden, Hofpoort Ziekenhuis")
                ("adm-sourcehospitalid", "65", "Zaandam, Zaans Medisch Centrum")
                ("adm-sourcehospitalid", "184", "Zeist, Diakonessenhuis - Zeist")
                ("adm-sourcehospitalid", "186", "Zevenaar, Rijnstate locatie Zevenaar")
                ("adm-sourcehospitalid", "264", "Zoetermeer, Lange Land Ziekenhuis")
                ("adm-sourcehospitalid", "101", "Zutphen, Gelre ziekenhuizen - Het Spittaal")
                ("adm-sourcehospitalid", "427", "Zwijndrecht, Albert Schweitzer - Zwijndrecht")
                ("adm-sourcehospitalid", "1", "Zwolle, Isala (voorheen isala klinieken - sophia)")
                ("compl-all-locatie", "a", "Centraal zenuwstelsel")
                ("compl-all-locatie", "b", "Perifeer zenuwstelsel")
                ("compl-all-locatie", "c", "Ademhaling / longen")
                ("compl-all-locatie", "d", "Circulatie")
                ("compl-all-locatie", "e", "Hematologie")
                ("compl-all-locatie", "f", "Bloedfunctie / metabool")
                ("compl-all-locatie", "g", "Lymfestelsel")
                ("compl-all-locatie", "h", "Lever / gal")
                ("compl-all-locatie", "i", "Nieren / urinewegen")
                ("compl-all-locatie", "j", "Overige buikorganen")
                ("compl-all-locatie", "k", "Huid, huidderivaten, slijmvliezen")
                ("compl-all-locatie", "l", "Bewegingstelsel")
                ("compl-all-locatie", "m", "Psychisch")
                ("compl-all-locatie", "n", "Endocrinologie")
                ("compl-all-locatie", "p", "Allergie / immunologie")
                ("compl-all-locatie", "q", "Symptomen")
                ("compl-all-aard-a", "00", "nvt / nno")
                ("compl-all-aard-a", "01", "gedaald bewustzijn")
                ("compl-all-aard-a", "02", "hersenoedeem")
                ("compl-all-aard-a", "03", "liquorlekkage")
                ("compl-all-aard-a", "04", "contusio")
                ("compl-all-aard-a", "05", "convulsie")
                ("compl-all-aard-a", "06", "bloeding (centraal zenuwstelsel)")
                ("compl-all-aard-a", "07", "infarct")
                ("compl-all-aard-a", "08", "agitatie")
                ("compl-all-aard-a", "09", "delier (centraal zenuwstelsel)")
                ("compl-all-aard-a", "99", "overig (omschrijf)")
                ("compl-all-aard-b", "00", "nvt / nno")
                ("compl-all-aard-b", "01", "zenuwletsel")
                ("compl-all-aard-b", "02", "zenuwfunctiestoornis")
                ("compl-all-aard-b", "99", "overig (omschrijf)")
                ("compl-all-aard-c", "00", "nvt / nno")
                ("compl-all-aard-c", "01", "hypoxie")
                ("compl-all-aard-c", "02", "resp. insufficiëntie")
                ("compl-all-aard-c", "03", "trachea/larynx letsel")
                ("compl-all-aard-c", "04", "bloeding luchtwegen")
                ("compl-all-aard-c", "05", "infectie luchtwegen")
                ("compl-all-aard-c", "06", "pneumothorax")
                ("compl-all-aard-c", "07", "atelectase")
                ("compl-all-aard-c", "08", "pleuravocht")
                ("compl-all-aard-c", "99", "overig (omschrijf)")
                ("compl-all-aard-d", "00", "nvt / nno")
                ("compl-all-aard-d", "01", "bloeding (circulatie)")
                ("compl-all-aard-d", "02", "veneuze trombose")
                ("compl-all-aard-d", "03", "arteriële trombose")
                ("compl-all-aard-d", "04", "infarcering")
                ("compl-all-aard-d", "05", "veneuze embolie")
                ("compl-all-aard-d", "06", "arteriële embolie")
                ("compl-all-aard-d", "07", "flebitis")
                ("compl-all-aard-d", "08", "bacteriemie")
                ("compl-all-aard-d", "09", "sepsis")
                ("compl-all-aard-d", "90", "Sepsis (criterium 1 geen huidflora)")
                ("compl-all-aard-d", "91", "Sepsis (criterium 2A met huidflora)")
                ("compl-all-aard-d", "92", "sepsis (criterium 2B met huidflora)")
                ("compl-all-aard-d", "10", "overvulling")
                ("compl-all-aard-d", "11", "ondervulling")
                ("compl-all-aard-d", "12", "hypertensie")
                ("compl-all-aard-d", "13", "hypotensie")
                ("compl-all-aard-d", "14", "pericard vocht")
                ("compl-all-aard-d", "99", "overig (omschrijf)")
                ("compl-all-aard-e", "00", "nvt / nno")
                ("compl-all-aard-e", "01", "anemie")
                ("compl-all-aard-e", "02", "leucopenie")
                ("compl-all-aard-e", "03", "trombopenie")
                ("compl-all-aard-e", "04", "leucocytose")
                ("compl-all-aard-e", "05", "trombocytose")
                ("compl-all-aard-e", "06", "hyperviscositeit")
                ("compl-all-aard-e", "99", "overig (omschrijf)")
                ("compl-all-aard-f", "00", "nvt / nno")
                ("compl-all-aard-f", "01", "hyperglycemie")
                ("compl-all-aard-f", "02", "hypernatriemie")
                ("compl-all-aard-f", "03", "hyperkaliemie")
                ("compl-all-aard-f", "04", "hypercalciemie")
                ("compl-all-aard-f", "05", "hyperfosfatemie")
                ("compl-all-aard-f", "06", "hypermagnesiemie")
                ("compl-all-aard-f", "07", "hyperchloremie")
                ("compl-all-aard-f", "08", "hypertriglyceridemie")
                ("compl-all-aard-f", "09", "hypoglycemie")
                ("compl-all-aard-f", "10", "hyponatriemie")
                ("compl-all-aard-f", "11", "hypokaliemie")
                ("compl-all-aard-f", "12", "hypocalciemie")
                ("compl-all-aard-f", "13", "hypofosfatemie")
                ("compl-all-aard-f", "14", "hypomagnesiemie")
                ("compl-all-aard-f", "15", "hypochloremie")
                ("compl-all-aard-f", "16", "hypotriglyceridemie")
                ("compl-all-aard-f", "17", "acidose metabool")
                ("compl-all-aard-f", "18", "acidose respiratoir")
                ("compl-all-aard-f", "19", "alkalose metabool")
                ("compl-all-aard-f", "20", "alkalose respiratoir")
                ("compl-all-aard-f", "21", "hypoxie")
                ("compl-all-aard-f", "99", "overig (omschrijf)")
                ("compl-all-aard-g", "00", "nvt / nno")
                ("compl-all-aard-g", "01", "lymfe lekkage")
                ("compl-all-aard-g", "02", "lymfoedeem")
                ("compl-all-aard-g", "99", "overig (omschrijf)")
                ("compl-all-aard-h", "00", "nvt / nno")
                ("compl-all-aard-h", "01", "leverfunctiestoornis")
                ("compl-all-aard-h", "02", "gallekkage")
                ("compl-all-aard-h", "03", "cholangitis")
                ("compl-all-aard-h", "04", "pancreatitis")
                ("compl-all-aard-h", "99", "overig (omschrijf)")
                ("compl-all-aard-i", "00", "nvt / nno")
                ("compl-all-aard-i", "01", "blaasperforatie")
                ("compl-all-aard-i", "02", "nierruptuur")
                ("compl-all-aard-i", "03", "cystitis")
                ("compl-all-aard-i", "04", "nefritis")
                ("compl-all-aard-i", "05", "nierinsufficientie")
                ("compl-all-aard-i", "99", "overig (omschrijf)")
                ("compl-all-aard-j", "00", "nvt / nno")
                ("compl-all-aard-j", "01", "braken")
                ("compl-all-aard-j", "02", "diarree")
                ("compl-all-aard-j", "03", "obstipatie")
                ("compl-all-aard-j", "04", "bloeding (overige buikorganen)")
                ("compl-all-aard-j", "99", "overig (omschrijf)")
                ("compl-all-aard-k", "00", "nvt / nno")
                ("compl-all-aard-k", "01", "brandwond")
                ("compl-all-aard-k", "02", "huidinfarct")
                ("compl-all-aard-k", "03", "decubitus")
                ("compl-all-aard-k", "04", "mucositis")
                ("compl-all-aard-k", "99", "overig (omschrijf)")
                ("compl-all-aard-l", "00", "nvt / nno")
                ("compl-all-aard-l", "01", "contractuur")
                ("compl-all-aard-l", "02", "artritis")
                ("compl-all-aard-l", "03", "fractuur")
                ("compl-all-aard-l", "99", "overig (omschrijf)")
                ("compl-all-aard-m", "00", "nvt / nno")
                ("compl-all-aard-m", "01", "delier (psychisch)")
                ("compl-all-aard-m", "02", "depressie")
                ("compl-all-aard-m", "03", "ontwenning")
                ("compl-all-aard-m", "04", "psychose")
                ("compl-all-aard-m", "99", "overig (omschrijf)")
                ("compl-all-aard-n", "00", "nvt / nno")
                ("compl-all-aard-n", "01", "hypofyse uitval")
                ("compl-all-aard-n", "02", "schildklier hypofunctie")
                ("compl-all-aard-n", "03", "schildklier hyperfunctie")
                ("compl-all-aard-n", "04", "bijnierschors insuff.")
                ("compl-all-aard-n", "99", "overig (omschrijf)")
                ("compl-all-aard-p", "00", "nvt / nno")
                ("compl-all-aard-p", "01", "allergische reactie huid")
                ("compl-all-aard-p", "02", "allergische reactie systemisch")
                ("compl-all-aard-p", "99", "overig (omschrijf)")
                ("compl-all-aard-q", "00", "nvt / nno")
                ("compl-all-aard-q", "01", "hypothermie")
                ("compl-all-aard-q", "02", "hyperthermie/koorts")
                ("compl-all-aard-q", "03", "pijn")
                ("compl-all-aard-q", "04", "falen procedure")
                ("compl-all-aard-q", "05", "valincident")
                ("compl-all-aard-q", "99", "overig (omschrijf)")
                ("compl-all-verrichting", "00", "geen verrichting")
                ("compl-all-verrichting", "01", "verrichting nno")
                ("compl-all-verrichting", "02", "punctie nno")
                ("compl-all-verrichting", "03", "biopt nno")
                ("compl-all-verrichting", "04", "endoscopie nno")
                ("compl-all-verrichting", "05", "katheterisatie nno")
                ("compl-all-verrichting", "06", "drain nno")
                ("compl-all-verrichting", "07", "perifeer infuus")
                ("compl-all-verrichting", "08", "perifeer infuus bijplaatsen")
                ("compl-all-verrichting", "09", "perifeer infuus in situ")
                ("compl-all-verrichting", "10", "centrale lijn (incl. navelcath.) bijplaatsen")
                ("compl-all-verrichting", "11", "centrale lijn (incl. navelcath.)")
                ("compl-all-verrichting", "12", "arteriele lijn bijplaatsen")
                ("compl-all-verrichting", "13", "arteriele lijn in situ")
                ("compl-all-verrichting", "48", "CVVH bijplaatsen")
                ("compl-all-verrichting", "49", "CVVH in situ")
                ("compl-all-verrichting", "50", "ballon pomp bijplaatsen")
                ("compl-all-verrichting", "51", "ballon pomp in situ")
                ("compl-all-verrichting", "52", "LVAD bijplaatsen")
                ("compl-all-verrichting", "53", "LVAD in situ")
                ("compl-all-verrichting", "54", "RVAD bijplaatsen")
                ("compl-all-verrichting", "55", "RVAD in situ")
                ("compl-all-verrichting", "56", "ECMO bijplaatsen")
                ("compl-all-verrichting", "57", "ECMO in situ")
                ("compl-all-verrichting", "14", "thoraxdrain bijplaatsen")
                ("compl-all-verrichting", "15", "thoraxdrain in situ")
                ("compl-all-verrichting", "16", "pleurapunctie")
                ("compl-all-verrichting", "17", "maagsonde bijplaatsen")
                ("compl-all-verrichting", "18", "maagsonde in situ")
                ("compl-all-verrichting", "19", "duodenumsonde bijplaatsen")
                ("compl-all-verrichting", "20", "duodenumsonde in situ")
                ("compl-all-verrichting", "21", "PEG sonde bijplaatsen")
                ("compl-all-verrichting", "22", "PEG sonde in situ")
                ("compl-all-verrichting", "23", "blaaspunctie")
                ("compl-all-verrichting", "24", "blaaskatheter bijplaatsen")
                ("compl-all-verrichting", "25", "blaaskatheter in situ")
                ("compl-all-verrichting", "26", "ascites punctie")
                ("compl-all-verrichting", "27", "gewrichtspunctie")
                ("compl-all-verrichting", "28", "nierbiopt")
                ("compl-all-verrichting", "29", "leverbiopt")
                ("compl-all-verrichting", "30", "endoscopisch biopt")
                ("compl-all-verrichting", "31", "lumbaal punctie")
                ("compl-all-verrichting", "32", "endotracheale tube")
                ("compl-all-verrichting", "33", "endotracheale tube bijplaatsen")
                ("compl-all-verrichting", "34", "endotracheale tube in situ")
                ("compl-all-verrichting", "47", "tracheacanule bijplaatsen")
                ("compl-all-verrichting", "35", "tracheacanule in situ")
                ("compl-all-verrichting", "36", "mechanische ventilatie")
                ("compl-all-verrichting", "37", "ecg plakker")
                ("compl-all-verrichting", "38", "eeg plakker")
                ("compl-all-verrichting", "39", "transducer transcutane meter")
                ("compl-all-verrichting", "40", "warmtebron extern")
                ("compl-all-verrichting", "41", "aanwezigheid implantaat")
                ("compl-all-verrichting", "42", "geen medicatiefout")
                ("compl-all-verrichting", "43", "een onder- of overdosering")
                ("compl-all-verrichting", "60", "een verkeerde medicamentkeuze")
                ("compl-all-verrichting", "61", "een verkeerde bereiding")
                ("compl-all-verrichting", "62", "een toedieningsfout")
                ("compl-all-verrichting", "45", "beeldvormende diagnostiek")
                ("compl-all-verrichting", "46", "laboratorium diagnostiek")
                ("compl-all-verrichting", "99", "overig (omschrijf)")
                ("compl-all-gevolg", "11", "geen gevolgen")
                ("compl-all-gevolg", "01", "aanvullende behandeling noodzakelijk")
                ("compl-all-gevolg", "02", "verlenging opname")
                ("compl-all-gevolg", "03", "leidend tot operatie")
                ("compl-all-gevolg", "04", "blijvende schade of invaliditeit")
                ("compl-all-gevolg", "05", "overlijden")
                ("compl-all-gevolg", "99", "geen van bovenstaande (omschrijf)")
                ("compl-gevolg", "11", "geen gevolgen")
                ("compl-gevolg", "01", "aanvullende behandeling noodzakelijk")
                ("compl-gevolg", "02", "verlenging opname")
                ("compl-gevolg", "03", "leidend tot operatie")
                ("compl-gevolg", "04", "blijvende schade of invaliditeit")
                ("compl-gevolg", "05", "overlijden")
                ("compl-gevolg", "99", "geen van bovenstaande (omschrijf)")
                ("compl-voorgedef-of-alles", "1", "Voorgedefinieerde complicatie")
                ("compl-voorgedef-of-alles", "2", "Volledige lijst")
                ("compl-locatie-aard-medicatie", "F17", "acidose metabool")
                ("compl-locatie-aard-medicatie", "A08", "agitatie")
                ("compl-locatie-aard-medicatie", "F19", "alkalose metabool")
                ("compl-locatie-aard-medicatie", "P01", "allergische reactie huid")
                ("compl-locatie-aard-medicatie", "P02", "allergische reactie systemisch")
                ("compl-locatie-aard-medicatie", "N04", "bijnierschors insufficiëntie")
                ("compl-locatie-aard-medicatie", "A06", "bloeding (centraal zenuwstelsel)")
                ("compl-locatie-aard-medicatie", "D01", "bloeding (circulatie)")
                ("compl-locatie-aard-medicatie", "J04", "bloeding (overige buikorganen)")
                ("compl-locatie-aard-medicatie", "J01", "braken")
                ("compl-locatie-aard-medicatie", "A05", "convulsie")
                ("compl-locatie-aard-medicatie", "A09", "delier (centraal zenuwstelsel)")
                ("compl-locatie-aard-medicatie", "M01", "delier (psychisch)")
                ("compl-locatie-aard-medicatie", "M02", "depressie")
                ("compl-locatie-aard-medicatie", "J02", "diarree")
                ("compl-locatie-aard-medicatie", "Q04", "falen procedure")
                ("compl-locatie-aard-medicatie", "A01", "gedaald bewustzijn")
                ("compl-locatie-aard-medicatie", "F04", "hypercalciemie")
                ("compl-locatie-aard-medicatie", "F07", "hyperchloremie")
                ("compl-locatie-aard-medicatie", "F05", "hyperfosfatemie")
                ("compl-locatie-aard-medicatie", "F01", "hyperglycemie")
                ("compl-locatie-aard-medicatie", "F03", "hyperkaliemie")
                ("compl-locatie-aard-medicatie", "F06", "hypermagnesiemie")
                ("compl-locatie-aard-medicatie", "F02", "hypernatriemie")
                ("compl-locatie-aard-medicatie", "D12", "hypertensie")
                ("compl-locatie-aard-medicatie", "Q02", "hyperthermie/koorts")
                ("compl-locatie-aard-medicatie", "F08", "hypertriglyceridemie")
                ("compl-locatie-aard-medicatie", "F12", "hypocalciemie")
                ("compl-locatie-aard-medicatie", "F15", "hypochloremie")
                ("compl-locatie-aard-medicatie", "F13", "hypofosfatemie")
                ("compl-locatie-aard-medicatie", "N01", "hypofyse uitval")
                ("compl-locatie-aard-medicatie", "F09", "hypoglycemie")
                ("compl-locatie-aard-medicatie", "F11", "hypokaliemie")
                ("compl-locatie-aard-medicatie", "F14", "hypomagnesiemie")
                ("compl-locatie-aard-medicatie", "F10", "hyponatriemie")
                ("compl-locatie-aard-medicatie", "D13", "hypotensie")
                ("compl-locatie-aard-medicatie", "Q01", "hypothermie")
                ("compl-locatie-aard-medicatie", "F16", "hypotriglyceridemie")
                ("compl-locatie-aard-medicatie", "H01", "leverfunctiestoornis")
                ("compl-locatie-aard-medicatie", "J03", "obstipatie")
                ("compl-locatie-aard-medicatie", "D11", "ondervulling")
                ("compl-locatie-aard-medicatie", "M03", "ontwenning")
                ("compl-locatie-aard-medicatie", "D10", "overvulling")
                ("compl-locatie-aard-medicatie", "Q03", "pijn")
                ("compl-locatie-aard-medicatie", "M04", "psychose")
                ("compl-locatie-aard-medicatie", "C02", "resp. insufficiëntie")
                ("compl-locatie-aard-medicatie", "N03", "schildklier hyperfunctie")
                ("compl-locatie-aard-medicatie", "N02", "schildklier hypofunctie")
                ("pcode", "999999", "Onbekend")
                ("geslacht", "1", "Man")
                ("geslacht", "2", "Vrouw")
                ("geslacht", "9", "Onbekend")
                ("status", "0", "In leven")
                ("status", "1", "Overleden")
                ("status", "9", "Onbekend")
                ("adm-destdirectpicu", "0", "Nee")
                ("adm-destdirectpicu", "1", "Ja")
                ("adm-disdelayed", "0", "Nee")
                ("adm-disdelayed", "1", "Ja")
                ("adm-ppvalidator", "0", "Nee")
                ("adm-ppvalidator", "1", "Ja")
                ("adm-pupilemvppvalidator", "0", "Nee")
                ("adm-pupilemvppvalidator", "1", "Ja")
                ("adm-sourcedirectpicu", "0", "Nee")
                ("adm-sourcedirectpicu", "1", "Ja")
                ("admpupils", "0", "Nee")
                ("admpupils", "1", "Ja")
                ("bmtrecipient-riskpim", "0", "Nee")
                ("bmtrecipient-riskpim", "1", "Ja")
                ("bypass", "0", "Nee")
                ("bypass", "1", "Ja")
                ("cancer", "0", "Nee")
                ("cancer", "1", "Ja")
                ("canule", "0", "Nee")
                ("canule", "1", "Ja")
                ("cardmyopath-riskpim", "0", "Nee")
                ("cardmyopath-riskpim", "1", "Ja")
                ("chromosomal", "0", "Nee")
                ("chromosomal", "1", "Ja")
                ("contrean12", "0", "Nee")
                ("contrean12", "1", "Ja")
                ("cprprehosp-riskpim", "0", "Nee")
                ("cprprehosp-riskpim", "1", "Ja")
                ("cprprepicu-riskpim", "0", "Nee")
                ("cprprepicu-riskpim", "1", "Ja")
                ("hiv-riskpim", "0", "Nee")
                ("hiv-riskpim", "1", "Ja")
                ("hypoplast-riskpim", "0", "Nee")
                ("hypoplast-riskpim", "1", "Ja")
                ("leukemie-riskpim", "0", "Nee")
                ("leukemie-riskpim", "1", "Ja")
                ("neurodegen-riskpim", "0", "Nee")
                ("neurodegen-riskpim", "1", "Ja")
                ("recovery", "0", "Nee")
                ("recovery", "1", "Ja")
                ("scid-riskpim", "0", "Nee")
                ("scid-riskpim", "1", "Ja")
                ("septische-shock", "0", "Nee")
                ("septische-shock", "1", "Ja")
                ("sponthersenbl-riskpim", "0", "Nee")
                ("sponthersenbl-riskpim", "1", "Ja")
                ("ventilated", "0", "Nee")
                ("ventilated", "1", "Ja")
                ("land", "nl", "Nederland")
                ("land", "de", "Duitsland")
                ("land", "be", "België")
                ("land", "se", "Zweden")
                ("land", "unknown", "Onbekend")
            |]

        let find name code =
            if String.IsNullOrWhiteSpace(name) ||
               String.IsNullOrWhiteSpace (code) then None
            else
                codes
                |> Array.tryFind (fun (n, c, _) ->
                    name |> eqs n &&
                    code |> eqs c
                )
                |> function
                | Some (_, _, v) -> Some v
                | None -> None



module Patient =

    open Types
    

    let kiloPascalToMmHg n = n * 7.50061683


    module PIM =
        


        // PIM2
        //High Risk Diagnoses include:
        //Cardiac Arrest
        //Cardiomyopathy or Myocarditis
        //Severe Combined Immunodeficiency (SCID)
        //Hypoplastic Left Heart Syndrome (HLHS)
        //Leukemia or Lymphoma after first induction of chemotherapy
        //Liver Failure as primary ICU admission reason
        //HIV Positive
        //Spontaneous Cerebral Hemorrhage (from any cause, except intracranial bleeding related to head trauma)
        //Neurodegenerative Disorder
        let pim2HighRisk =
            [ CardiacArrest
              CardiomyopathyOrMyocarditis
              SevereCombinedImmuneDeficiency
              HypoplasticLeftHeartSyndrome
              LeukemiaorLymphoma
              LiverFailure
              HIVPositive
              CerebralHemorrhage
              NeurodegenerativeDisorder ]
        //Low Risk Diagnoses include:
        //Asthma
        //Bronchiolitis
        //Croup
        //Obstructive Sleep Apnea (OSA)
        //Diabetic Ketoacidosis (DKA)
        let pim2LowRisk =
            [ Asthma
              Bronchiolitis
              Croup
              ObstructiveSleepApnea
              DiabeticKetoacidosis ]
        // PIM3
        // Low-risk diagnosis:
        //  asthma,
        //  bronchiolitis,
        //  croup,
        //  obstructive sleep apnea,
        //  diabetic ketoacidosis,
        //  seizure disorder.
        let pim3LowRisk =
            [ Asthma
              Bronchiolitis
              Croup
              ObstructiveSleepApnea
              DiabeticKetoacidosis
              SeizureDisorder ]
        // High-risk diagnosis:
        //  spontaneous cerebral hemorrhage,
        //  cardiomyopathy or myocarditis,
        //  hypoplastic left heart syndrome,
        //  neurodegenerative disorder,
        //  necrotizing enterocolitis.
        let pim3HighRisk =
            [ CerebralHemorrhage
              CardiomyopathyOrMyocarditis
              HypoplasticLeftHeartSyndrome
              NeurodegenerativeDisorder
              NecrotizingEnterocolitis ]
        // Very high-risk diagnosis:
        //  cardiac arrest preceding ICU admission,
        //  severe combined immune deficiency,
        //  leukemia or lymphoma after first induction,
        //  bone marrow transplant recipient,
        //  liver failure.
        let pim3VeryHighRisk =
            [ CardiacArrest
              SevereCombinedImmuneDeficiency
              LeukemiaorLymphoma
              BoneMarrowTransplant
              LiverFailure ]


        let calcRiskFromScore score = Math.Exp(score) / (1. + Math.Exp(score))


        // PIM2 score =
        //    -0.9282(Elective) +
        //    -1.0244(Recovery) +
        //    0.7507(Bypass) +
        //    1.6829(High-Risk) +
        //    -1.577(Low-Risk) +
        //    3.0791(Pupils) +
        //    1.3352(Ventilator) +
        //    0.01395(absolute value of SBP-120) +
        //    0.1040(absolute value of base excess) +
        //    0.2888(100 x FiO2 / PaO2) +
        //    -4.8841
        let calculatePIM2 (pim: PIM) =
            let mapHighRisk rd =
                pim2HighRisk
                |> List.exists (fun d -> rd |> List.exists ((=) d))
                |> fun b -> if b then 1.6829 |> Some else None

            let mapLowRisk rd =
                pim2LowRisk
                |> List.exists (fun d -> rd |> List.exists ((=) d))
                |> fun b -> if b then -1.577 |> Some else None

            let paO2 =
                pim.PaO2
                |> Option.defaultValue 0.
                |> kiloPascalToMmHg

            let score =
                [
                    "elective",
                    if pim.Urgency = Elective then -0.9282 else 0.
                    "recovery",
                    if pim.Recovery then -1.0244 else 0.
                    "bypass",
                    if pim.CardiacByPass then 0.7507 else 0.

                    "risk diagnosis",
                    [
                        pim.RiskDiagnosis |> mapHighRisk
                        // lowRisc
                        pim.RiskDiagnosis |> mapLowRisk
                    ]
                    |> List.filter Option.isSome
                    |> List.map Option.get
                    |> function
                    | [] -> 0.
                    | xs -> xs |> List.max

                    "pupils",
                    pim.AdmissionPupils
                    |> function
                    | FixedDilated -> 3.0791
                    | _ -> 0.
                    "ventilator",
                    if pim.Ventilated then 1.3352 else 0.
                    "SBP",
                    (((pim.SystolicBloodPressure
                     |> Option.defaultValue 120.)
                    - 120.)
                    |> Math.Abs)
                    * 0.01395
                    "base excess",
                    (pim.BaseExcess
                    |> Option.defaultValue 0.
                    |> Math.Abs)
                    * 0.1040
                    "fiO2",
                    if paO2 > 0. then
                      (((pim.FiO2 |> Option.defaultValue 0.) * 100.)
                       / paO2)
                      * 0.2888
                    else
                      0.
                    "baseline",
                    -4.8841
                ]
                |> List.mapi (fun i (l, v) ->
                    //printfn "%i. %s: %f" (i + 1) l v
                    v)
                |> List.reduce (+)

            let mort =
                calcRiskFromScore score

            { pim with
                PIM2Score = Some score
                PIM2Mortality = Some mort
            }


        // PIM3 score =
        //  (3.8233 × pupillary reaction) +
        //  (−0.5378 × elective admission) +
        //  (0.9763 × mechanical ventilation) +
        //  (0.0671 × [absolute {base excess}]) +
        //  (−0.0431 × SBP) + (0.1716 × [SBP^2/1,000]) +
        //  (0.4214 × [{FiO2 × 100}/PaO2]) +
        //  (-1.2246 × bypass cardiac procedure) +
        //  (-0.8762 × non-bypass cardiac procedure) +
        //  (-1.5164 × noncardiac procedure) +
        //  (1.6225 × very high-risk diagnosis) +
        //  (1.0725 × high-risk diagnosis)
        //  (-2.1766 × low-risk diagnosis) +
        //  −1.7928
        let calculatePIM3 (pim: PIM) =
            let mapVeryHighRisk rd =
                pim3VeryHighRisk
                |> List.exists (fun d -> rd |> List.exists ((=) d))
                |> fun b -> if b then 1.6225 |> Some else None

            let mapHighRisk rd =
                pim3HighRisk
                |> List.exists (fun d -> rd |> List.exists ((=) d))
                |> fun b -> if b then 1.0725 |> Some else None

            let mapLowRisk rd =
                pim3LowRisk
                |> List.exists (fun d -> rd |> List.exists ((=) d))
                |> fun b -> if b then -2.1766 |> Some else None

            let paO2 =
                pim.PaO2
                |> Option.defaultValue 0.
                |> kiloPascalToMmHg

            let sbp =
                pim.SystolicBloodPressure
                |> Option.defaultValue 120.

            let score =
                [
                    "urgency"
                    , if pim.Urgency = Elective then -0.5378 else 0.
                    "bypass"
                    , if pim.Recovery && pim.CardiacByPass then -1.2246 else 0.
                    "no bypass"
                    , if pim.Recovery && pim.CardiacNonByPass then -0.8762 else 0.
                    "non cardiac"
                    , if pim.Recovery && pim.NonCardiacProcedure ||
                         pim.Recovery && not (pim.CardiacByPass || pim.NonCardiacProcedure) then -1.5164 else 0.

                    "risk diagnosis",
                    [
                        pim.RiskDiagnosis |> mapVeryHighRisk
                        // highRisc
                        pim.RiskDiagnosis |> mapHighRisk
                        // lowRisc
                        pim.RiskDiagnosis |> mapLowRisk
                    ]
                    |> List.filter Option.isSome
                    |> List.map Option.get
                    |> function
                    | [] -> 0.
                    | xs -> xs |> List.max

                    "pupils",
                    pim.AdmissionPupils
                    |> function
                    | FixedDilated -> 3.8233
                    | _ -> 0.
                    "vent"
                    , if pim.Ventilated then 0.9763 else 0.
                    "SBP"
                    , (-0.0431 * sbp) + (0.1716 * ((sbp ** 2.) / 1000.))
                    "base excess",
                    (pim.BaseExcess
                    |> Option.defaultValue 0.
                    |> Math.Abs)
                    * 0.0671
                    "fiO2",
                    if paO2 > 0. then
                      (((pim.FiO2 |> Option.defaultValue 0.) * 100.)
                       / paO2)
                      * 0.4214
                    else
                      0.
                    "baseline",
                    -1.7928
                ]
                |> List.mapi (fun i (l, v) ->
//                    printfn "%i. %s: %f" (i + 1) l v
                    v)
                |> List.reduce (+)

            { pim with
                PIM3Score = Some score
                PIM3Mortality = score |> calcRiskFromScore |> Some
            }



    module PRISM =

        open System

        type Mapping =
            {
                Item : Item
                AgeType : AgePRISM3
                LowPoints : int
                MidPoints : int
                HighPoints : int
                LowRange : float
                MidRangeLow : float option
                MidRangeHigh : float Option
                HighRange : float
            }

        type Score = NonNeuro of int | Neuro of int

        type ItemCalculator = AgePRISM3 -> Value -> Score

        let ageMap = function
        | TwoWeeks  -> 1.311
        | OneMonth  -> 0.968
        | OneYear   -> 0.357
        | EightTeen
        | UnknownAge -> 0.


        let admissionMap = function
        | EmergencyUnit -> 0.693
        | AnotherHospital -> 1.012
        | InHospital ->  1.626
        | Recovery
        | UnknownAdmissionSource  -> 0.


        //maps
        let mappings =
            [
                // blood pressures
                { Item = BloodPressure; AgeType = Neonate;       LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 40.;  MidRangeLow = Some 40.;  MidRangeHigh = Some 55.;  HighRange = 55. }
                { Item = BloodPressure; AgeType = Infant;        LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 45.;  MidRangeLow = Some 45.;  MidRangeHigh = Some 65.;  HighRange = 65. }
                { Item = BloodPressure; AgeType = Child;         LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 55.;  MidRangeLow = Some 55.;  MidRangeHigh = Some 75.;  HighRange = 75. }
                { Item = BloodPressure; AgeType = Adolescent;    LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 65.;  MidRangeLow = Some 65.;  MidRangeHigh = Some 85.;  HighRange = 85. }
                // temparature
                { Item = Temperature;   AgeType = Neonate;       LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. }
                { Item = Temperature;   AgeType = Infant;        LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. }
                { Item = Temperature;   AgeType = Child;         LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. }
                { Item = Temperature;   AgeType = Adolescent;    LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. }
                // mental status
                { Item = MentalStatus;  AgeType = Neonate;       LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. }
                { Item = MentalStatus;  AgeType = Infant;        LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. }
                { Item = MentalStatus;  AgeType = Child;         LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. }
                { Item = MentalStatus;  AgeType = Adolescent;    LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. }
                // heart rate
                { Item = HeartRate;     AgeType = Neonate;       LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 215.; MidRangeLow = Some 215.; MidRangeHigh = Some 225.; HighRange = 225. }
                { Item = HeartRate;     AgeType = Infant;        LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 215.; MidRangeLow = Some 215.; MidRangeHigh = Some 225.; HighRange = 225. }
                { Item = HeartRate;     AgeType = Child;         LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 185.; MidRangeLow = Some 185.; MidRangeHigh = Some 205.; HighRange = 205. }
                { Item = HeartRate;     AgeType = Adolescent;    LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 145.; MidRangeLow = Some 145.; MidRangeHigh = Some 155.; HighRange = 155. }
                // creatinine
                { Item = Creatinine;    AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 75. }
                { Item = Creatinine;    AgeType = Infant;        LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 80. }
                { Item = Creatinine;    AgeType = Child;         LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 80. }
                { Item = Creatinine;    AgeType = Adolescent;    LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 115. }
                // urea
                { Item = Urea;          AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 4.3 }
                { Item = Urea;          AgeType = AllMinNeonate; LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 5.4 }
                // prothPT
                { Item = ProthPT;       AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 22.0 }
                { Item = ProthPT;       AgeType = AllMinNeonate; LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 22.0 }
                // prothPTT.
                { Item = ProthPTT;      AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 85.0 }
                { Item = ProthPTT;      AgeType = AllMinNeonate; LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 57.0 }
                // pupils
                { Item = Pupils;        AgeType = AnyAge;        LowPoints = 0; MidPoints = 7; HighPoints = 11; LowRange = 0.;   MidRangeLow = Some 1.;   MidRangeHigh = Some 1.;   HighRange = 2.}
                // pHl
                { Item = Ph;            AgeType = AnyAge;        LowPoints = 0; MidPoints = 2; HighPoints = 3; LowRange = 7.47;  MidRangeLow = Some 7.48; MidRangeHigh = Some 7.55; HighRange = 7.55 }
                // tCO2
                { Item = TotalCO2;      AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 4; LowRange = 34.0;  MidRangeLow = Some 34.0; MidRangeHigh = Some 34.0; HighRange = 34.0 }
                // pCO2
                { Item = PCO2;          AgeType = AnyAge;        LowPoints = 0; MidPoints = 1; HighPoints = 3; LowRange = 50.0;  MidRangeLow = Some 50.0; MidRangeHigh = Some 75.0; HighRange = 75.0 }
                // pAO2
                { Item = PaO2;          AgeType = AnyAge;        LowPoints = 6; MidPoints = 3; HighPoints = 0; LowRange = 42.0;  MidRangeLow = Some 42.0; MidRangeHigh = Some 49.9; HighRange = 49.9 }
                // glu
                { Item = Glucose;       AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 2; LowRange = 0.;    MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 11.0 }
                // pot
                { Item = Potassium;     AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 3; LowRange = 0.;    MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 6.9 }
                // wbc
                { Item = WBC;           AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 4; LowRange = 0.;    MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 3000. }
                // wbc
                { Item = Platelets;     AgeType = AnyAge;        LowPoints = 2; MidPoints = 4; HighPoints = 5; LowRange = 50.;   MidRangeLow = Some 100.; MidRangeHigh = None;      HighRange = 200. }
            ]

        let input =
            {
                Age = None
                SystolicBloodPressure = NoValue
                Temperature = NoValue
                MentalStatus = NoValue
                HeartRate = NoValue
                PupilsFixed = NoValue
                PH = NoValue
                TotalCO2 = NoValue
                PCO2 = NoValue
                PaO2 = NoValue
                Glucose = NoValue
                Potassium = NoValue
                Creatinine = NoValue
                Urea = NoValue
                WhiteBloodCount = NoValue
                PT = NoValue
                PTT = NoValue
                Platelets = NoValue
                AdmissionSource = UnknownAdmissionSource
                CPR24HourBefore = false
                Cancer = false
                LowRiskPrimary = true
                PRISM3Score = None
                PRISM3Neuro = None
                PRISM4Mortality = None
            }

        let mapPRISM3Age a =
            let diff (a : DateTime) = (DateTime.Now - a).TotalDays |> int
            match a with
            | None   -> AnyAge
            | Some a when a |> diff <= 30 -> Neonate
            | Some a when a |> diff <= 2 * 365 -> Infant
            | Some a when a |> diff <= 12 * 365 -> Child
            | Some _ -> Adolescent


        let mapPRISM4Age a =
            let diff (a : DateTime) = (DateTime.Now - a).TotalDays |> int
            match a with
            | a when a |> diff <= 14 -> TwoWeeks
            | a when a |> diff <= 2 * 365 -> OneMonth
            | a when a |> diff <= 12 * 365 -> OneYear
            | _ -> EightTeen


        let eqsAge a2 a1 =
            match a2 with
            | AnyAge -> true
            | AllMinNeonate -> a1 <> Neonate
            | _ -> a1 = a2


        let calculators : (Item * ItemCalculator) list =
            // calculate the value for
            // item i, age a, value v
            // using function f
            let inline calc i a v f =
                mappings
                |> List.filter (fun x -> x.Item = i && a |> eqsAge x.AgeType)
                |> function
                | [m] ->
                    f v m
                | _   ->
                    sprintf "no mapping for %A %A" i a
                    |> failwith
                |> fun s ->
                    if i = MentalStatus || i = Pupils then s |> Neuro
                    else s |> NonNeuro
            // calculator for one value
            let calcOne i a v f =
                match v with
                | NoValue    -> 0 |> NonNeuro
                | OneValue v -> calc i a v f
                | TwoValues _ ->
                    sprintf "expected one value but got %A" v
                    |> failwith
            // calculator for two values
            let calcTwo i a v f =
                match v with
                | NoValue    -> 0 |> NonNeuro
                | OneValue v ->
                    sprintf "expected two values but got %A" v
                    |> failwith
                | TwoValues (v1, v2) -> calc i a (v1, v2) f
            // generic calculator
            let genCalc v m =
                if v > m.HighRange then m.HighPoints;
                else 0

            // list of item and calculators
            [
                BloodPressure,
                fun a v ->
                    fun v m ->
                        match m.MidRangeLow, m.MidRangeHigh with
                        | _ when (v < m.LowRange)                -> m.LowPoints
                        | Some l, Some h when (v >= l && v <= h) -> m.MidPoints
                        | _ when (v > m.HighRange)               -> m.HighPoints
                        | _ -> 0
                    |> calcOne BloodPressure a v

                Temperature,
                fun a v ->
                    fun (v1, v2) m ->
                        match m.MidRangeLow, m.MidRangeHigh with
                        | _ when (v1 < m.LowRange || v2 > m.HighRange) -> m.LowPoints
                        | _ -> m.MidPoints
                    |> calcTwo Temperature a v

                MentalStatus,
                fun a v ->
                    fun v m ->
                        if v >= m.LowRange && v <= m.HighRange then m.MidPoints
                        else 0
                    |> calcOne MentalStatus a v

                HeartRate,
                fun a v ->
                    fun v m ->
                        match m.MidRangeLow, m.MidRangeHigh with
                        | _ when v < m.LowRange                -> m.LowPoints
                        | Some l, Some h when v >= l && v <= h -> m.MidPoints
                        | _ when v > m.HighRange               -> m.HighPoints
                        | _ -> 0
                    |> calcOne HeartRate a v

                Pupils,
                fun a v ->
                    fun v m ->
                        match v with
                        | _ when v = 1. -> m.MidPoints
                        | _ when v = 2. -> m.HighPoints
                        | _ -> 0
                    |> calcOne Pupils a v

                TotalCO2,
                fun a v ->
                    fun (_, v2) m ->
                        match m.MidRangeLow with
                        | Some l when v2 > l -> m.HighPoints
                        | _ -> 0
                    |> calcTwo TotalCO2 a v

                Ph,
                fun a v ->
                    fun (_, v2) m ->
                        match m.MidRangeLow, m.MidRangeHigh with
                        | Some l, Some h when v2 >= l && v2 <= h -> m.MidPoints
                        | _ when v2 > m.HighRange -> m.HighPoints
                        | _ -> 0
                    |> calcTwo Ph a v

                PCO2,
                fun a v ->
                    fun v m ->
                        let v = v |> kiloPascalToMmHg
                        match v with
                        | _ when v >= m.LowRange && v <= m.HighRange -> m.MidPoints
                        | _ when v > m.HighRange                     -> m.HighPoints
                        | _ -> 0
                    |> calcOne PCO2 a v

                PaO2,
                fun a v ->
                    fun v m ->
                        let v = v |> kiloPascalToMmHg
                        match m.MidRangeLow, m.MidRangeHigh with
                        | _ when v < m.LowRange                -> m.LowPoints
                        | Some l, Some h when v >= l && v <= h -> m.MidPoints
                        | _ -> 0
                    |> calcOne PaO2 a v

                Glucose,
                fun a v ->
                    genCalc
                    |> calcOne Glucose a v

                Potassium,
                fun a v ->
                    genCalc
                    |> calcOne Potassium a v

                Creatinine,
                fun a v ->
                    genCalc
                    |> calcOne Creatinine a v

                Urea,
                fun a v ->
                    genCalc
                    |> calcOne Urea a v

                WBC,
                fun a v ->
                    fun v m ->
                        if v < m.HighRange then m.HighPoints
                        else 0
                    |> calcOne WBC a v

                ProthPT,
                fun a v ->
                    genCalc
                    |> calcOne ProthPT a v

                ProthPTT,
                fun a v ->
                    genCalc
                    |> calcOne ProthPTT a v

                Platelets,
                fun a v ->
                    fun v m ->
                        match m.MidRangeLow with
                        | _ when v < m.LowRange   -> m.HighPoints
                        | Some l when v < l       -> m.MidPoints
                        | _ when v <= m.HighRange -> m.LowPoints
                        | _ -> 0
                    |> calcOne Platelets a v

            ]


        //if(rspt === rsptt){
        //	setSubScore(clpt.lblId, rspt);
        //	setSubScore(clptt.lblId, 0);
        //	return;
        //}
        //if(rspt > rsptt) {
        //	setSubScore(clpt.lblId, rspt);
        //	setSubScore(clptt.lblId, 0);
        //	return;
        //}
        //if(rspt < rsptt) {
        //	setSubScore(clptt.lblId, rsptt);
        //	setSubScore(clpt.lblId, 0);
        //	return;
        let calculateCoagulation sPT sPTT =
        //    printfn "calculating coagulation %A and %A" sPT sPTT
            match sPT, sPTT with
            | NonNeuro rspt, NonNeuro rsptt when rspt > rsptt -> sPT
            | NonNeuro rspt, NonNeuro rsptt when rspt < rsptt -> sPTT
            | _ -> sPT

        //function calAcidosisPh(lowestpH, lowestTotalcO2){
        //    if(lowestpH < 7 || lowestTotalcO2 < 5) return 6;
        //    if(lowestpH <= 7.28 || lowestTotalcO2 <= 16.9) return 2;
        //    return 0;
        //}
        let calculateAcidosisPh pH tCO2 =
            match pH, tCO2 with
            | TwoValues(pHl, _), TwoValues(tCO2l, _) ->
                if pHl < 7. || tCO2l < 5. then 6
                elif pHl <= 7.28 || tCO2l <= 16.9 then 2
                else 0
            | _ -> 0
            |> NonNeuro


        let calcItem a item v =
            calculators
            |> List.tryFind (fst >> ((=) item))
            |> function
            | Some c ->
                c
                |> snd
                |> fun f ->
                    f a v
            | None ->
                sprintf "no calculator for %A" item
                |> failwith


        let calcScore (input : PRISM) =
            let calc = input.Age |> mapPRISM3Age |> calcItem
            [
                "SBP", input.SystolicBloodPressure |> calc BloodPressure
                "Creat", input.Creatinine |> calc Creatinine
                "Glucose", input.Glucose |> calc Glucose
                "HR", input.HeartRate |> calc HeartRate
                "Mental", input.MentalStatus |> calc MentalStatus
                "PaO2", input.PaO2 |> calc PaO2
                "PCO2", input.PCO2 |> calc PCO2
                "pH", input.PH |> calc Ph
                "Platelets", input.Platelets |> calc Platelets
                "Potassium", input.Potassium |> calc Potassium
                "Pupils", input.PupilsFixed |> calc Pupils
                "Temp", input.Temperature |> calc Temperature
                "Bicarbonate", input.TotalCO2 |> calc TotalCO2
                "Urea", input.Urea |> calc Urea
                "WBC", input.WhiteBloodCount |> calc WBC
                "Acidosis", calculateAcidosisPh input.PH input.TotalCO2
                "Coagulation", calculateCoagulation (input.PT |> calc ProthPT) (input.PTT |> calc ProthPTT)
            ]
            |> List.mapi (fun i (l, s) ->
                    s
                )
            |> List.fold (fun acc s ->
                match acc with
                | Neuro n1, NonNeuro n2 ->
                    match s with
                    | Neuro n    -> Neuro (n + n1), NonNeuro n2
                    | NonNeuro n -> Neuro n1,       NonNeuro (n + n2)
                | _ ->
                    sprintf "invalid acc %A" acc
                    |> failwith
            ) (Neuro 0, NonNeuro 0)


        let calcProbability input s =
            match input.Age with
            | None ->
                printfn "Cannot calculate probability without age for score: %A" s
                None
            | Some a ->
                let a = a |> mapPRISM4Age
                match s with
                | Neuro n1, NonNeuro n2 ->
                    [
                        a |> ageMap
                        input.AdmissionSource |> admissionMap
                        if input.CPR24HourBefore then 1.082 else 0.
                        if input.Cancer then 0.766 else 0.
                        if input.LowRiskPrimary then -1.697 else 0.
                        (n1 |> float) * 0.197
                        (n2 |> float) * 0.163
                        -5.776
                    ]
                    |> List.reduce (+)
                    |> fun x ->
                        Math.Exp(x) / (1. + Math.Exp(x))
                    |> Some
                | _ ->
                    printfn "not a valid score: %A" s
                    None


        let calculate input =
            match input.Age with
            | None -> input
            | _ ->
                let neuro, nonneuro =
                    match input |> calcScore with
                    | Neuro v1, NonNeuro v2 -> Some v1, Some v2
                    | _ -> None, None

                { input with
                    PRISM3Neuro = neuro
                    PRISM3Score = nonneuro
                    PRISM4Mortality =
                        input
                        |> calcScore
                        |> calcProbability input
                }



    let create hn bd ps dd dm =
        {
            HospitalNumber = hn
            BirthDate = bd
            PatientState = ps
            DateOfDeath = dd
            DeathMode = dm
            HospitalAdmissions = []
        }

    let createHospitalAdmission hn adt ddt =
        {
            HospitalNumber = hn
            AdmissionDate = adt
            DischargeDate = ddt
            PICUAdmissions = []
        }

    let createPICUAdmission
        hospitalNumber
        admissionDate
        dischargeDate
        dischargeReason
        urgency
        admissionType
        recovery
        admissionIndication
        referingSpecialism
        admissionWeight
        admissionLength
        tempMin12
        tempMax12
        riskDiagnosis
        paO2
        fiO2
        be
        systolicBP
        ventilated
        pupils
        cardiacByPass
        lowSystolicBP
        admissionSource
        mentalStatus
        fixedPupils
        cpr
        cancer
        lowRiskDiagnosis
        heartRate
        pHLow
        pHHigh
        bicarbonateLow
        bicarbonateHigh
        pCO2
        glucose
        potassium
        creatinin
        urea
        whiteBloodCount
        PT
        aPTT
        platelets
        =
        let mapOptToOneValue o =
            match o with
            | Some v -> OneValue v
            | None -> NoValue

        let mapOptToTwoValue o1 o2 =
            match o1, o2 with
            | Some v1, Some v2 -> TwoValues (v1, v2)
            | _ -> NoValue
        {
            HospitalNumber = hospitalNumber
            AdmissionDate = admissionDate
            DischargeDate = dischargeDate
            DischargeReason = dischargeReason
            Urgency = urgency
            AdmissionType = admissionType    
            Recovery = recovery
            AdmissionIndication = admissionIndication
            ReferingSpecialism = referingSpecialism
            AdmissionWeight = admissionWeight
            AdmissionLength = admissionLength
            TempMin12 = tempMin12
            TempMax12 = tempMax12
            RiskDiagnosis = riskDiagnosis
            PIM =
                {
                    Urgency = urgency
                    Recovery = recovery
                    RiskDiagnosis = riskDiagnosis
                    CardiacByPass = cardiacByPass
                    CardiacNonByPass = false //cardiacNonByPass
                    NonCardiacProcedure = false //nonCardiacProcedure
                    Ventilated = ventilated
                    AdmissionPupils = pupils
                    PaO2 = paO2
                    FiO2 = fiO2
                    BaseExcess = be
                    SystolicBloodPressure = systolicBP
                    PIM2Score = None
                    PIM2Mortality = None
                    PIM3Score = None
                    PIM3Mortality = None
                }
                |> PIM.calculatePIM2
                |> PIM.calculatePIM3
            PRISM =
                {
                    Age = None
                    SystolicBloodPressure = lowSystolicBP |> mapOptToOneValue
                    Temperature = mapOptToTwoValue tempMin12 tempMax12
                    MentalStatus = mentalStatus |> mapOptToOneValue
                    HeartRate = heartRate |> mapOptToOneValue
                    PupilsFixed = fixedPupils |> mapOptToOneValue
                    PH = mapOptToTwoValue pHLow pHHigh    
                    TotalCO2 = mapOptToTwoValue bicarbonateLow bicarbonateHigh
                    PCO2 = pCO2 |> mapOptToOneValue
                    PaO2 = paO2 |> mapOptToOneValue
                    Glucose = glucose |> mapOptToOneValue
                    Potassium = potassium |> mapOptToOneValue
                    Creatinine = creatinin |> mapOptToOneValue
                    Urea = urea |> mapOptToOneValue
                    WhiteBloodCount = whiteBloodCount |> mapOptToOneValue
                    PT = PT |> mapOptToOneValue
                    PTT = aPTT |> mapOptToOneValue
                    Platelets = platelets |> mapOptToOneValue
                    AdmissionSource = admissionSource
                    CPR24HourBefore = cpr
                    Cancer = cancer
                    LowRiskPrimary = lowRiskDiagnosis
                    PRISM3Score = None
                    PRISM3Neuro = None
                    PRISM4Mortality = None
                }
        }

    let picuAdmissionToString (a : PICUAdmission) =
        sprintf "%A %s %s"
            (a.AdmissionDate)
            (a.ReferingSpecialism)
            (a.AdmissionIndication)


    let hospitalAdmissionToString (a : HospitalAdmission) =
        sprintf "Ziekenhuis opname: %A - %A"
            a.AdmissionDate
            a.DischargeDate
        |> fun s ->
            a.PICUAdmissions
            |> List.map (picuAdmissionToString >> (sprintf "%s %s") s)


    let piceAdmissionToString (a : Patient) =
        sprintf "%s: %A"
            a.HospitalNumber
            a.PatientState
        |> fun s ->
            a.HospitalAdmissions
            |> List.collect (fun ha ->
                ha
                |> hospitalAdmissionToString
                |> List.map (sprintf "%s %s" s))


            

module Validation =

    open Types

    let validatePat (p : Patient) =
        let fromLaterThanUntil from until =
            match from, until with
            | Some from, Some until -> from > until
            | _ -> false
            
        [
            match p.BirthDate, p.DateOfDeath with
            | None, _ -> NotValid (p, "Geen geboortedatum") 
            | Some _, None -> IsValid
            | Some bd, Some dd ->
                if bd > dd then
                    NotValid (p, "Geboortedatum na datum van overlijden")

            match p.DateOfDeath, p.PatientState with
            | Some _, Alive -> NotValid (p, "Patient status is in leven maar datum van overlijden bekend")
            | _ -> IsValid

            let xs =
                p.HospitalAdmissions
                |> List.filter (fun ha -> fromLaterThanUntil ha.AdmissionDate ha.DischargeDate)

            if xs |> List.length > 0 then
                NotValid (p, "Ziekenhuis opname datum na ontslag datum")

            let xs =
                p.HospitalAdmissions
                |> List.filter (fun ha ->
                    ha.PICUAdmissions
                    |> List.exists (fun pa -> fromLaterThanUntil pa.AdmissionDate pa.DischargeDate)
                )

            if xs |> List.length > 0 then
                NotValid(p, "PICU Opname datum na ontslag datum")

            if String.IsNullOrWhiteSpace(p.HospitalNumber) then
                NotValid(p, "Geen ziekenhuis nummer")
        ]
        |> List.filter ((<>) IsValid)



module Parsing =

    open System.Diagnostics
    open System.Globalization

    open Types
    open MRDM
    open Patient

    open Result.Operators

    let isNullOrWhiteSpace (s : String) = s |> String.IsNullOrWhiteSpace
    let notNullOrWhiteSpace = isNullOrWhiteSpace >> not


    module Parsers =

        let parseInt s =
            match Int32.TryParse(s) with
            | true, x  -> Some x
            | false, _ -> None


        let parseFloat s =
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


        let mapRiscDiagnosis d leukemia bmt cva card scid hiv neuro hlhs =
            match d with
            | s when s = "0" -> []
            | s when s = "1" -> [ Croup ]
            | s when s = "2" -> [ ObstructiveSleepApnea ]
            | s when s = "3" -> [ Bronchiolitis ]
            | s when s = "4" -> [ Asthma ]
            | s when s = "5" -> [ LiverFailure ]
            | s when s = "6" -> [ SeizureDisorder ]
            | s when s = "7" -> [ DiabeticKetoacidosis ]
            | s when s = "8" -> [ LiverFailure ]
            | s when s = "9" -> [ NecrotizingEnterocolitis ]
            | s when s = "10" -> [ DiabeticKetoacidosis ]
            | s when s = "11" -> [ CardiomyopathyOrMyocarditis ]
            | _ -> []
            |> List.append [
                if leukemia = "1" then LeukemiaorLymphoma
                if bmt = "1" then BoneMarrowTransplant
                if cva = "1" then CerebralHemorrhage
                if card = "1" then CardiomyopathyOrMyocarditis
                if scid = "1" then SevereCombinedImmuneDeficiency
                if hiv = "1" then HIVPositive
                if neuro = "1" then NeurodegenerativeDisorder
                if hlhs = "1" then HypoplasticLeftHeartSyndrome
            ]


        let mapUrgency = function
            | s when s = "10" -> NotElective
            | s when s = "11" -> Elective
            | _ -> UnknownUrgency


        let mapPupils = function
            | s when s = "1" -> FixedDilated
            | s when s = "0" -> NormalPupils
            | _ -> UnknownPupils


        let mapPatientState = function
            | s when s = "0" -> Alive
            | s when s = "1" -> Dead
            | _ -> UnknownPatientState


        let mapAdmissionType = function
            | s when s = "1" -> Medical
            | s when s = "2" -> Surgery
            | s when s = "3" -> DOA
            | _ -> UnknownAdmissionType

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
            | s when s = "109"  -> Recovery
            | s when s = "129"  -> AnotherHospital
            | s when s = "115" || s = "107" -> EmergencyUnit
            | s when s = "99"  || s = "77" -> UnknownAdmissionSource
            | _ -> InHospital


        let mapLowRiskPRISM s =
            [
                "7"
                "10"
                "11"
            ]
            |> List.exists ((=) s)



    let parsePatient (hospData : MRDMHospital.Row[]) (d : MRDMPatient.Row) =
        let getHospNum (data : MRDMHospital.Row[]) =
            let errs, hn =
                data
                |> Array.filter (fun hd -> hd.idcode = d.idcode)
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

        let parseDateOpt s =
            if s |> isNullOrWhiteSpace then None |> Result.ok
            else s |> Result.tryWithOk Parsers.parseDate
        let mapPatientState = Parsers.mapPatientState >> Result.ok

        Patient.create
        <!> getHospNum hospData
        <*> parseDateOpt d.gebdat
        <*> mapPatientState d.status
        <*> parseDateOpt d.datovl
        <*> Result.ok d.``adm-deathmodeid``


    let parseHospAdm (hospData: MRDMHospital.Row[]) =
        let parseDateOpt s =
            if s |> isNullOrWhiteSpace then None |> Result.ok
            else s |> Result.tryWithOk Parsers.parseDate

        let fErr msgs = msgs |> Result.Error
        let fOk ((p : Patient), msgs1) =
            hospData
            |> Array.filter (fun d -> d.``ziekenhuis-episode-upn`` = p.HospitalNumber)
            |> Array.map (fun d ->
                Patient.createHospitalAdmission
                <!> Result.ok d.``ziekenhuis-episode-upn``
                <*> parseDateOpt d.``adm-hosp-admdate``
                <*> parseDateOpt d.``adm-hosp-disdate`` 
            )
            |> Result.foldOk
            |> function
            | Result.Ok (adms, msgs2) ->
                msgs1 |> Array.append msgs2 |> Result.okMsg ({ p with HospitalAdmissions = adms |> Array.toList })
            | Result.Error msgs2  -> msgs1 |> Array.append msgs2 |> Result.error 

        Result.either fOk fErr


    let addPICUAdmissions (admissions : Result<PICUAdmission[] * string[], _>) =
        let inPeriod dt1 dt2 dt3 dt4 =
            match dt1, dt2, dt3, dt4 with
            | Some d1, Some d2, Some d3, Some d4 -> d1 <= d3 && d2 >= d4
            | Some d1, _      , Some d3, None
            | Some d1, None   , Some d3, _  -> d1 <= d3
            | _ -> false

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
                                                pa.HospitalNumber = ha.HospitalNumber &&
                                                inPeriod ha.AdmissionDate ha.DischargeDate pa.AdmissionDate pa.DischargeDate  
                                            )
                                            |> Array.map (fun pa ->
                                                { pa with
                                                    PRISM =
                                                        {
                                                            pa.PRISM with
                                                                Age = p.BirthDate
                                                        }
                                                        |> PRISM.calculate
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
        | Result.Error msgs -> msgs |> Result.error
        | Result.Ok (pats, msgs1) ->
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
                    |> Array.mapi (sprintf "%i. %A\n")

                distPats, msgs

            Result.okMsg pats (msgs1 |> Array.append msgs2)


    let parsePICUAdmissions (picuData : MRDMPicu.Row[]) =
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
        let mapAdmType = Parsers.mapAdmissionType >> Result.ok
        let mapUrgency = Parsers.mapUrgency >> Result.ok
        let mapRisk d leukemia bmt cva card scid hiv neuro hlhs =
            Parsers.mapRiscDiagnosis d leukemia bmt cva card scid hiv neuro hlhs
            |> Result.ok
        let mapPupils  = Parsers.mapPupils >> Result.ok
        let mapAdmissionSource = Parsers.mapAdmissionSource >> Result.ok
        let mapLowRiskPRISM = Parsers.mapLowRiskPRISM >> Result.ok

        picuData
        |> Array.map (fun d ->
            let find n c =
                if c |> isNullOrWhiteSpace then Result.ok ""
                else
                    match MRDM.Codes.find n c with
                    | Some v -> Result.ok v
                    | None ->
                        [| sprintf "couldn't find code %s with name %s" c n |]
                        |> Result.error

            Patient.createPICUAdmission
            <!> Result.ok d.``ziekenhuis-episode-upn``
            <*> parseDateOpt d.``adm-ic-admdate``
            <*> parseDateOpt d.``adm-ic-disdate``
            <*> find "adm-disreasonid" d.``adm-disreasonid``
            <*> mapUrgency d.``adm-typeid-urgentie``
            <*> mapAdmType d.``adm-typeid-soort``
            <*> parseBool d.recovery
            <*> find "adm-indication" d.``adm-indication``
            <*> find "adm-refspecialism" d.``adm-refspecialism``
            <*> parseFloat d.gewicht
            <*> parseInt d.``adm-length``
            <*> parseFloat d.``t-min12``
            <*> parseFloat d.``t-max12``
            <*> (mapRisk d.``risicodiag-hoofd``
                         d.``leukemie-riskpim``
                         d.``bmtrecipient-riskpim``
                         d.``sponthersenbl-riskpim``
                         d.``cardmyopath-riskpim``
                         d.``scid-riskpim``
                         d.``hiv-riskpim``
                         d.``neurodegen-riskpim``
                         d.``hypoplast-riskpim``)
            <*> parseFloat d.``pao2-0``
            <*> parseFloat d.``fio2-0``
            <*> parseFloat d.``be-0``
            <*> parseFloat d.``sbp-0``
            <*> parseBool d.ventilated
            <*> mapPupils d.admpupils
            <*> parseBool d.bypass
            <*> parseFloat d.``sbp-min12``
            <*> mapAdmissionSource d.``adm-sourceunitid``
            <*> parseFloat d.``adm-emv``
            <*> parseFloat d.admpupils
            <*> parseBool d.contrean12
            <*> parseBool d.cancer
            <*> mapLowRiskPRISM d.``risicodiag-hoofd``
            <*> parseFloat d.``hr-max12``
            <*> parseFloat d.``ph-min12``
            <*> parseFloat d.``ph-max12``
            <*> parseFloat d.``bicarbonate-min12``
            <*> parseFloat d.``bicarbonate-max12``
            <*> parseFloat d.``paco2-max12``
            <*> parseFloat d.``glucose-max12``
            <*> parseFloat d.``k-max12``
            <*> parseFloat d.``creatinine-max12``
            <*> parseFloat d.``ureum-max12``
            <*> parseFloat d.``leuco-min12``
            <*> parseFloat d.``pt-max12``
            <*> parseFloat d.``ptt-max12``
            <*> parseFloat d.``thrombo-min12``
        )
        |> Array.toList
        |> Result.foldOk


    let parseMRDM () : Result<Patient[] * string[], string[]> =
        printfn "Start parsing, this can take a while ..."
        let timer = new Stopwatch ()
        timer.Start ()

        let hospData = mrdmHospital.Data |> Seq.toArray
        let picuData = mrdmPicu.Data |> Seq.toArray
        let picuAdms =
            printfn "parsing picu admissions"
            parsePICUAdmissions picuData

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
            addPICUAdmissions picuAdms

        mrdmPatient.Data
        |> Seq.toArray
        |> Array.mapi parsePat
        |> Array.mapi parseHosp
        |> Array.mapi addPICU
        |> filterDuplicateOrMore



module StringBuilder =

    open System.Text

    let builder (s : string) = StringBuilder(s)

    let append (s : string) (sb : StringBuilder) = sb.Append(s)

    let appendLine (s : string) (sb : StringBuilder) = sb.AppendLine(s)

    let newLine = appendLine ""

    let appendFormat (fs : string) vs (sb : StringBuilder) = sb.AppendFormat(fs, (vs |> List.toArray))

    let appendLineFormat (fs : string) vs (sb : StringBuilder) = sb.AppendFormat(fs + "\n", (vs |> List.toArray))

    let replace (s1 : string) s2 (sb : StringBuilder) = sb.Replace(s1, s2)
    
    let toString (sb : StringBuilder) = sb.ToString()



module Statistics =

    open System.Text

    open Types
    open Validation
    open Parsing


    type Totals () =
        member val Patients = 0 with get, set
        member val Admissions = 0 with get, set
        member val Admitted = 0 with get, set
        member val Discharged = 0 with get, set
        member val PICUDays = 0 with get, set
        member val Deaths = 0 with get, set
        member val PICUDeaths = 0 with get, set
        member val PIM2Mortality = 0. with get, set
        member val PIM3Mortality = 0. with get, set
        member val PRISM4Mortality = 0. with get, set
        member val DischargeReasons : (string * int) list = [] with get, set


    type MonthTotals () =
        member val Month = 0 with get, set
        member val Totals = Totals ()


    type YearTotals () =
        member val Year = 0 with get, set
        member val Totals = Totals ()
        member val MonthTotals : MonthTotals list = [] with get, set


    type Statistics () =
        member val Totals : Totals = Totals () with get, set
        member val YearTotals : YearTotals list = [] with get, set
        member val InvalidPatients : (string * int) list = [] with get, set


    let periodInYear yr (from : DateTime option) (until : DateTime option) =
        if from > until then
            sprintf "from: %A cannot be later than until %A" from until
            |> failwith
        else
            match from, until with
            | None,     _    -> false
            | Some fdt, None -> fdt.Year <= yr
            | Some fdt, Some udt ->
                fdt.Year <= yr && udt.Year >= yr


    let dateInYear yr (date: DateTime option) =
        match date with
        | None     -> false
        | Some dt -> dt.Year = yr


    let periodInYearMonth yr mo from until  =
        if periodInYear  yr from until |> not then false
        else
            match from, until with
            | None,     _    -> false
            | Some adt, None -> adt.Month <= mo
            | Some adt, Some ddt ->
                adt.Month <= mo && ddt.Month >= mo
       

    let dateInYearMonth yr mo date =
        if dateInYear yr date |> not then false
        else
            match date with
            | None     -> false
            | Some dt -> dt.Month = mo


    let periodFilter yr mo adt ddt =
        match yr, mo with
        | None, None -> true
        | Some y, None   -> periodInYear  y adt ddt
        | Some y, Some m -> periodInYearMonth y m adt ddt
        | _ -> "not a valid filter" |> failwith


    let dateFilter yr mo adt =
        match yr, mo with
        | None, None -> true
        | Some y, None   -> dateInYear y adt
        | Some y, Some m -> dateInYearMonth y m adt
        | _ -> "not a valid filter" |> failwith


    let calculate (pats: Patient list) =
        let stats = Statistics ()

        let disReasons =
            pats
            |> List.collect (fun p ->
                p.HospitalAdmissions
                |> List.collect (fun ha  ->
                    ha.PICUAdmissions
                    |> List.map (fun pa -> pa.DischargeReason)
                )
            )
            |> List.distinct

        let pats =
            let notValid =
                pats
                |> List.map validatePat
                |> List.filter (fun errs -> errs |> List.length > 0)
                |> fun errs ->
                    stats.InvalidPatients <-
                        errs
                        |> List.collect (fun errs ->
                            errs
                            |> List.collect (fun err -> match err with | IsValid -> [] | NotValid(_, s) -> [s])
                        )
                        |> List.countBy id

                    errs
                |> List.collect (fun errs ->
                    errs
                    |> List.collect (fun err -> match err with | IsValid -> [] | NotValid(p, _) -> [p])
                )
                |> List.distinct

            pats
            |> List.filter (fun p ->
                notValid
                |> List.exists ((=) p)
                |> not
            )
            |> List.distinctBy (fun p -> p.HospitalNumber)
            |> List.collect (fun p ->
                p.HospitalAdmissions
                |> List.collect (fun ha ->
                    ha.PICUAdmissions
                    |> List.map (fun pa ->
                        {|
                            patient = p
                            hospitalAdmission = ha
                            picuAdmission = pa
                        |}
                    )
                )
            )

        stats.Totals.Patients <-
            pats
            |> List.map (fun p -> p.patient)
            |> List.distinctBy (fun p -> p.HospitalNumber)
            |> List.length

        stats.Totals.Admissions <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.distinct
            |> List.length

        stats.Totals.Discharged <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa -> pa.DischargeDate |>  Option.isSome)
            |> List.length

        stats.Totals.Deaths <-
            pats
            |> List.map (fun p -> p.patient)
            |> List.filter (fun p -> p.PatientState = Dead)
            |> List.length

        stats.Totals.PIM2Mortality <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa ->
                pa.DischargeDate |>  Option.isSome &&
                pa.PIM.PIM2Mortality |> Option.isSome
            )
            |> List.map (fun pa -> pa.PIM.PIM2Mortality |> Option.get)
            |> List.filter (Double.IsNaN >> not)
            |> List.sum

        stats.Totals.PIM3Mortality <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa ->
                pa.DischargeDate |>  Option.isSome &&
                pa.PIM.PIM3Mortality |> Option.isSome
            )
            |> List.map (fun pa -> pa.PIM.PIM3Mortality |> Option.get)
            |> List.filter (Double.IsNaN >> not)
            |> List.sum

        stats.Totals.PRISM4Mortality <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa ->
                pa.DischargeDate |>  Option.isSome &&
                pa.PRISM.PRISM4Mortality |> Option.isSome
            )
            |> List.map (fun pa -> pa.PRISM.PRISM4Mortality |> Option.get)
            |> List.filter (Double.IsNaN >> not)
            |> List.sum

        stats.Totals.DischargeReasons <-
            pats
            |> List.countBy (fun p -> p.picuAdmission.DischargeReason)
            |> List.map (fun (k, v) -> if k = "" then "Onbekend", v else k, v)
            
        stats.Totals.PICUDays <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.map (fun p ->
                match p.AdmissionDate, p.DischargeDate with
                | Some adt, Some ddt -> (ddt - adt).TotalDays
                | _ -> 0.
            )
            |> List.sum
            |> int

        let yrTots =
            [ 2003..DateTime.Now.Year ]
            |> List.map (fun yr ->
                let tot = new YearTotals()
                tot.Year <- yr
                tot.MonthTotals <-
                    [1..12]
                    |> List.map (fun m ->
                        let stat = new MonthTotals ()
                        stat.Month <- m
                        stat
                    )
                tot
            )

        let inline getAdmitted f m = 
            pats
            |> List.filter (fun d -> d.picuAdmission.DischargeDate |> Option.isSome)
            |> List.filter (fun d ->
                f d.picuAdmission.AdmissionDate d.picuAdmission.DischargeDate
            )
            |> List.map m
            |> List.distinct

        let inline getAdmissions f m = 
            pats
            |> List.filter (fun p ->
                f p.picuAdmission.AdmissionDate
            )
            |> List.map m
            |> List.distinct

        let inline getDischarged f m =
            pats
            |> List.filter (fun p ->
                f p.picuAdmission.DischargeDate
            )
            |> List.map m
            |> List.distinct
        // Patient statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let admitted = getAdmitted (periodFilter yr None) (fun d -> d.patient)

            tot.Totals.Patients <-
                admitted
                |> List.length

            tot.Totals.Deaths <-
                admitted
                |> List.filter (fun p ->
                    match p.DateOfDeath with
                    | None    -> false
                    | Some dt -> dt.Year = tot.Year
                )
                |> List.length


            tot.Totals.PIM2Mortality <-
                pats
                |> List.map (fun p -> p.picuAdmission)
                |> List.filter (fun pa ->
                    pa.DischargeDate |>  Option.isSome &&
                    pa.PIM.PIM2Mortality |> Option.isSome
                )
                |> List.filter (fun pa -> pa.DischargeDate.Value.Year = yr.Value)
                |> List.map (fun pa -> pa.PIM.PIM2Mortality |> Option.get)
                |> List.filter (Double.IsNaN >> not)
                |> List.sum

            tot.Totals.PIM3Mortality <-
                pats
                |> List.map (fun p -> p.picuAdmission)
                |> List.filter (fun pa ->
                    pa.DischargeDate |>  Option.isSome &&
                    pa.PIM.PIM3Mortality |> Option.isSome
                )
                |> List.filter (fun pa -> pa.DischargeDate.Value.Year = yr.Value)
                |> List.map (fun pa -> pa.PIM.PIM3Mortality |> Option.get)
                |> List.filter (Double.IsNaN >> not)
                |> List.sum

            tot.Totals.PRISM4Mortality <-
                pats
                |> List.map (fun p -> p.picuAdmission)
                |> List.filter (fun pa ->
                    pa.DischargeDate |>  Option.isSome &&
                    pa.PRISM.PRISM4Mortality |> Option.isSome
                )
                |> List.filter (fun pa -> pa.DischargeDate.Value.Year = yr.Value)
                |> List.map (fun pa -> pa.PRISM.PRISM4Mortality |> Option.get)
                |> List.filter (Double.IsNaN >> not)
                |> List.sum

        )
        // PICU admission statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let admissions = getAdmissions (dateFilter yr None) (fun d -> d.picuAdmission)

            tot.Totals.Admissions <-
                admissions
                |> List.length
        )
        // PICU discharge statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let discharged = getDischarged (dateFilter yr None) (fun d -> d.picuAdmission)

            tot.Totals.Discharged <-
                discharged
                |> List.length
        )
        // PICU admitted statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let admitted = getAdmitted (periodFilter yr None) (fun d -> d.picuAdmission)

            tot.Totals.Admitted <-
                admitted
                |> List.length

            tot.Totals.PICUDays <-
                admitted
                |> List.map (fun pa ->
                    let start = DateTime(tot.Year, 1, 1)
                    let stop = DateTime(tot.Year, 12, 31)

                    let adt =
                        pa.AdmissionDate |> Option.get
                        |> fun dt -> if dt.Year < tot.Year then start else dt
                    let ddt =
                        pa.DischargeDate |> Option.get
                        |> fun dt -> if dt.Year > tot.Year then stop else dt

                    (ddt - adt).TotalDays 
                )
                |> List.sum
                |> int

            tot.Totals.DischargeReasons <-
                admitted
                |> List.groupBy (fun a -> a.DischargeReason )
                |> List.map (fun (r, xs) -> r, xs |> List.length)
        )

        yrTots
        |> List.iter (fun yrTot ->
            yrTot.MonthTotals <-
                [1..12]
                |> List.filter (fun mo ->
                    DateTime(yrTot.Year, mo, 1) > DateTime.Now |> not
                )
                |> List.map (fun mo ->
                    let moTot = MonthTotals()

                    moTot.Month <- mo

                    let yr = Some yrTot.Year
                    let mo = Some mo

                    moTot.Totals.Patients <-
                        getAdmitted (periodFilter yr mo) (fun d -> d.patient)
                        |> List.length

                    moTot.Totals.Deaths <-
                        getAdmitted (periodFilter yr mo) (fun d -> d.patient)
                        |> List.filter (fun p -> p.DateOfDeath |> Option.isSome)
                        |> List.length

                    moTot.Totals.Admissions <-
                        getAdmissions (dateFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.length

                    moTot.Totals.Admitted <-
                        getAdmitted (periodFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.length

                    moTot.Totals.Discharged <-
                        getDischarged (dateFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.length

                    moTot.Totals.PICUDays <-
                        getAdmitted (periodFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.map (fun pa ->
                            let start = DateTime(yrTot.Year, moTot.Month, 1)
                            let stop = start.AddMonths(1)

                            let adt =
                                pa.AdmissionDate |> Option.get
                                |> fun dt ->
                                    if dt.Month < moTot.Month then start else dt
                            let ddt =
                                pa.DischargeDate |> Option.get
                                |> fun dt -> if dt.Month > moTot.Month then stop else dt

                            (ddt - adt).TotalDays 
                        )
                        |> List.sum
                        |> int

                    moTot
                )
        )

        stats.YearTotals <- yrTots
        stats
    
    module Literals =

        [<Literal>]
        let line = "---"
        [<Literal>]
        let headers5 = "|---|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns5 = "|{0}|{1}|{2}|{3}|{4}|"
        [<Literal>]
        let headers6 = "|---|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns6 = "|{0}|{1}|{2}|{3}|{4}|{5}|"
        [<Literal>]
        let columns6link = "|[{0}]()|{1}|{2}|{3}|{4}|{5}|"
        [<Literal>]
        let columns6tick = "|{0}|`{1}`|`{2}`|`{3}`|`{4}`|`{5}`|"
        [<Literal>]
        let headers8 = "|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns8 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|"
        [<Literal>]
        let headers9 = "|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns9 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|"
        [<Literal>]
        let patTot = "* Totaal aantal patienten: {0}"
        [<Literal>]
        let adsTot = "* Totaal aantal opnames: {0}"
        [<Literal>]
        let disTot = "* Totaal aantal ontslagen: {0}"
        [<Literal>]
        let adtTot = "* Totaal aantal opgenomen: {0}"
        [<Literal>]
        let dthTot = "* Totaal aantal overleden: {0}"
        [<Literal>]
        let dayTot = "* Totaal aantal verpleegdagen: {0}"
        [<Literal>]
        let estPIM2 = "* Geschatte PIM2 mortaliteit: {0:F0}"
        [<Literal>]
        let estPIM3 = "* Geschatte PIM3 mortaliteit: {0:F0}"
        [<Literal>]
        let estPRISM = "* Geschatte PRISM4 mortaliteit: {0:F0}"
        [<Literal>]
        let yearTitle = "### Rapportage van {0}"
        [<Literal>]
        let monthTitle = "{0:MMMM}"
        [<Literal>]
        let disReason = "* {0}: {1}"

    let toString (stats : Statistics) =
        let yrs =
            stats.YearTotals
            |> List.fold (fun acc stat ->
                acc
                
                |> StringBuilder.appendLineFormat Literals.yearTitle [ stat.Year |> box ]
                |> StringBuilder.appendLineFormat Literals.patTot [ stat.Totals.Patients |> box ]
                |> StringBuilder.appendLineFormat Literals.adsTot [ stat.Totals.Admissions |> box ]
                |> StringBuilder.appendLineFormat Literals.disTot [ stat.Totals.Discharged |> box ]
                |> StringBuilder.appendLineFormat Literals.dthTot [ stat.Totals.Deaths |> box ]
                |> StringBuilder.appendLineFormat Literals.estPIM2 [ stat.Totals.PIM2Mortality |> box ]
                |> StringBuilder.appendLineFormat Literals.estPIM3 [ stat.Totals.PIM3Mortality |> box ]
                |> StringBuilder.appendLineFormat Literals.dayTot [ stat.Totals.PICUDays |> box ]
                |> StringBuilder.appendLine "#### Ontslag redenen"
                |> fun sb ->
                    stat.Totals.DischargeReasons
                    |> List.fold (fun acc (s, c) ->
                        acc
                        |> StringBuilder.appendLineFormat Literals.disReason [ s |> box; c |> box ]
                    ) sb

                |> fun sb ->
                    let caps =
                        [
                            "Maand"
                            "Patienten"
                            "Opnames"
                            "Ontslagen"
                            "Overleden"
                            "Ligdagen"
                        ]
                        |> List.map (fun s -> "== " + s + " ==")
                        |> List.map box

                    let sb =
                        sb
                        |> StringBuilder.appendLine "#### Per maand"
                        |> StringBuilder.newLine
                        |> StringBuilder.appendLineFormat Literals.columns6 caps
                        |> StringBuilder.appendLine Literals.headers6

                    stat.MonthTotals
                    |> List.fold (fun acc stat ->
                        let mo =
                            StringBuilder.builder ""
                            |> StringBuilder.appendFormat Literals.monthTitle [ DateTime(2000, stat.Month, 1) |> box ]
                        let vals =
                            [
                                mo.ToString ()         |> box
                                stat.Totals.Patients   |> box
                                stat.Totals.Admissions |> box
                                stat.Totals.Discharged |> box
                                stat.Totals.Deaths     |> box
                                stat.Totals.PICUDays   |> box
                            ]
                            
                        acc
                        |> StringBuilder.appendLineFormat Literals.columns6 vals
                    ) sb

                |> fun sb ->
                    sb
                    |> StringBuilder.newLine
                    |> StringBuilder.appendLine Literals.line
                    |> StringBuilder.newLine

            ) ("" |> StringBuilder.builder)
            |> StringBuilder.toString

        let caps =
            [
                "Jaar"
                "Patienten"
                "Opnames"
                "Ontslagen"
                "Ligdagen"
                "Overleden"
                "PIM2 Mortaliteit"
                "PIM3 Mortaliteit"
                "PRISM4 Mortaliteit"
            ]
            |> List.map (fun s -> "== " + s + " ==")
            |> List.map box

        "# PICE Rapport"
        |> StringBuilder.builder
        |> StringBuilder.newLine
        |> StringBuilder.appendLine "## Validatie"
        |> fun sb ->
            stats.InvalidPatients
            |> List.fold (fun acc (s, c) ->
                acc
                |> StringBuilder.appendLineFormat Literals.disReason [ s |> box; c |> box ]
            ) sb
        |> StringBuilder.appendLine "## Totalen over de hele periode"
        |> StringBuilder.appendLineFormat Literals.patTot [ stats.Totals.Patients |> box ]
        |> StringBuilder.appendLineFormat Literals.adsTot [ stats.Totals.Admissions |> box ]
        |> StringBuilder.appendLineFormat Literals.disTot [ stats.Totals.Discharged |> box ]
        |> StringBuilder.appendLineFormat Literals.dthTot [ stats.Totals.Deaths |> box ]
        |> StringBuilder.appendLineFormat Literals.dayTot [ stats.Totals.PICUDays |> box ]
        |> StringBuilder.appendLineFormat Literals.estPIM2 [ stats.Totals.PIM2Mortality |> box ]
        |> StringBuilder.appendLineFormat Literals.estPIM3 [ stats.Totals.PIM3Mortality |> box ]
        |> StringBuilder.appendLineFormat Literals.estPRISM [ stats.Totals.PRISM4Mortality |> box ]
        |> StringBuilder.appendLine "#### Ontslag redenen"
        |> fun sb ->
            stats.Totals.DischargeReasons
            |> List.fold (fun acc (s, c) ->
                acc
                |> StringBuilder.appendLineFormat Literals.disReason [ s |> box; c |> box ]
            ) sb
        |> StringBuilder.appendLine "#### Per jaar"
        |> fun sb ->
            let sb =
                sb
                |> StringBuilder.newLine
                |> StringBuilder.appendLineFormat Literals.columns9 caps
                |> StringBuilder.appendLine Literals.headers9

            stats.YearTotals
            |> List.fold (fun acc stat ->
                let vals =
                    [
                        stat.Year              |> box
                        stat.Totals.Patients   |> box
                        stat.Totals.Admissions |> box
                        stat.Totals.Discharged |> box
                        stat.Totals.PICUDays   |> box
                        stat.Totals.Deaths     |> box
                        stat.Totals.PIM2Mortality |> box
                        stat.Totals.PIM3Mortality |> box
                        stat.Totals.PRISM4Mortality |> box
                    ]
        
                acc
                |> StringBuilder.appendLineFormat Literals.columns9 vals
            ) sb
        |> StringBuilder.newLine
        |> StringBuilder.appendLine Literals.line
        |> StringBuilder.appendLine yrs
        |> StringBuilder.toString




let pats = Parsing.parseMRDM ()


pats
|> Result.valueOrDefault (fun _ -> [||])
|> Array.toList
|> List.collect (fun p -> p.HospitalAdmissions)
|> List.collect (fun ha -> ha.PICUAdmissions)
|> List.map (fun pa -> pa.PIM.PIM2Mortality)
|> List.map (Option.get)
|> List.filter (Double.IsNaN)
|> List.length



pats
|> Result.valueOrDefault (fun _ -> [||]) 
|> Array.toList
|> Statistics.calculate
|> Statistics.toString
//|> Markdown.toHtml
|> Markdown.toBrowser
|> ignore


pats
|> Result.valueOrDefault (fun _ -> [||]) 
|> Array.map Validation.validatePat
|> Array.filter ((<>) [])
|> Array.toList
|> List.collect id
|> List.map (fun pv ->
    match pv with
    | Types.NotValid(p, s) -> sprintf "%s: %s" p.HospitalNumber s
    | Types.IsValid -> ""
)
|> List.filter ((<>) "")
|> List.iter (printfn "%s")



"""
---
### Rapportage van 2019
* Totaal aantal patienten: 961
* Totaal aantal opnames 1269
* Totaal aantal ontslagen 1265
* Totaal aantal overleden 42
* Totaal aantal verpleegdagen 4531
#### Ontslag redenen
* Klaar voor niet-ICU zorg: 1222
* Overleden: 32
* Huidige (PICU) zorg voortgezet op andere afdeling: 13
* Ontslag naar palliatieve zorg: 4
* Ontslag ten gevolge van plaatsgebrek op PICU: 3
* Ontslag wegens uitstel ingreep: 1
#### Rapportage per maand

|Maand|Patienten  |Opnames  |Ontslagen  |Overleden  |Ligdagen  |
|-----|:---:|-----|-----|-----|-----|
|januari|111|125|0|1|294|
|februari|106|111|0|5|357|
|maart|100|96|0|5|385|
|april|97|107|0|10|314|
|mei|106|108|0|5|343|
|juni|107|107|0|5|357|
|juli|112|109|0|4|405|
|augustus|107|97|0|5|388|
|september|108|104|0|3|360|
|oktober|101|97|0|8|364|
|november|111|107|0|7|416|
|december|93|101|0|4|320|


---
"""
|> Markdown.toBrowser



pats
|> Result.valueOrDefault (fun _ -> [||]) 
|> Array.toList
|> List.take 100
|> List.iter (fun p ->
    printfn "%A" p
)

"1,5"
|> Parsing.Parsers.parseFloat

MRDM.mrdmPicu.Data
|> Seq.take 100
|> Seq.iter (fun r->
    r.``ph-min12``
    |> printfn "%A"
)

open Types

{ Urgency =
           NotElective
  Recovery = true
  RiskDiagnosis = []
  CardiacByPass =
                 false
  CardiacNonByPass =
                    false
  NonCardiacProcedure =
                       false
  Ventilated = false
  AdmissionPupils =
                   NormalPupils
  PaO2 = None
  FiO2 = None
  BaseExcess =
              Some 1.4
  SystolicBloodPressure =
                         None
  PIM2Score =
             Some
               -5.7629
  PIM2Mortality =
                 Some
                   0.003132145453
  PIM3Score =
             Some
               -5.27602
  PIM3Mortality =
                 Some
                   0.005086731921 }
|> Patient.PIM.calculatePIM3
