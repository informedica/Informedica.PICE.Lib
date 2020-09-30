namespace Informedica.PICE.Lib



module Patient =

    open System
    open Types
    

    let kiloPascalToMmHg n = n * 7.50061683
    
    
    let calcRiskFromScore score = Math.Exp(score) / (1. + Math.Exp(score))
    

    module PIM =
        
        open PIM


        let pupilsToString = function
            | PIM.FixedDilated  -> "1"
            | PIM.NormalPupils  -> "0"
            | PIM.UnknownPupils -> ""


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

            let recovScore = function
                | PIM.NoRecovery        -> 0.
                | PIM.PostCardiacByPass -> 0.7507
                | _                     -> -1.0244

            let score =
                [
                    "elective",
                    if pim.Urgency = Elective then -0.9282 else 0.
                    "recovery",
                    pim.Recovery |> recovScore

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

            let recovScore = function
                | PIM.NoRecovery              -> 0.
                | PIM.PostCardiacByPass       -> -1.2246
                | PIM.PostCariacNonByPass     -> -0.8762
                | PIM.PostNonCardiacProcedure -> -1.5164

            let score =
                [
                    "urgency"
                    , if pim.Urgency = Elective then -0.5378 else 0.

                    "recovery"
                    , pim.Recovery |> recovScore

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

        open PRISM

        type Value =
            | NoValue
            | OneValue of float
            | TwoValues of float * float

        type Input =
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
                AdmissionSource : PRISM.AdmissionSource
                CPR24HourBefore : bool
                Cancer : bool
                LowRiskPrimary : bool
                PRISM3Score : int option
                PRISM3Neuro : int option
                PRISM4Mortality : float option
            }


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

        let prism = 
            {
                Age = None
                SystolicBloodPressureMin = None
                TemperatureMin = None
                TemperatureMax = None
                MentalStatus = None
                HeartRateMax = None
                PupilsFixed = None
                PHMin = None
                PHMax = None
                BicarbonateMin = None
                BicarbonateMax = None
                PCO2Max = None
                PaO2Min = None
                GlucoseMax = None
                PotassiumMax = None
                CreatinineMax = None
                UreaMax = None
                WhiteBloodCountMin = None
                PTMax = None
                PTTMax = None
                PlateletsMin = None
                AdmissionSource = PRISM.AdmissionSource.UnknownAdmissionSource
                CPR24HourBefore = false
                Cancer = false
                LowRiskPrimary = false
                PRISM3Score = None
                PRISM3Neuro = None
                PRISM4Mortality = None
            }

        let input =
            {
                Age = None
                SystolicBloodPressure = NoValue
                Temperature = NoValue
                MentalStatus = 15. |> OneValue
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

        let mapPRISM3Age date a =
            let diff (a : DateTime) = (date - a).TotalDays |> int
            match a with
            | None   -> AnyAge
            | Some a when a |> diff <= 30 -> Neonate
            | Some a when a |> diff <= 2 * 365 -> Infant
            | Some a when a |> diff <= 12 * 365 -> Child
            | Some _ -> Adolescent


        let mapPRISM4Age date a =
            let diff (a : DateTime) = (date - a).TotalDays |> int
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


        let calcScore date (input : Input) =
            let calc = input.Age |> mapPRISM3Age date |> calcItem
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


        let calcProbability date input s =
            match input.Age with
            | None ->
                printfn "Cannot calculate probability without age for score: %A" s
                None
            | Some a ->
                let a = a |> mapPRISM4Age date
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
                    |> calcRiskFromScore
                    |> Some
                | _ ->
                    printfn "not a valid score: %A" s
                    None


        let mapPRISMtoInput (prism: PRISM) : Input =
            let mapOptToOneValue o =
                match o with
                | Some v -> OneValue v
                | None   -> NoValue

            let mapOptToTwoValue o1 o2 =
                match o1, o2 with
                | Some v1, Some v2 -> TwoValues (v1, v2)
                | _                -> NoValue

            {
                input with
                    Age = prism.Age
                    SystolicBloodPressure = prism.SystolicBloodPressureMin |> mapOptToOneValue
                    Temperature = mapOptToTwoValue prism.TemperatureMin prism.TemperatureMax
                    MentalStatus = prism.MentalStatus |> Option.map float |> mapOptToOneValue
                    HeartRate = prism.HeartRateMax |> Option.map float |> mapOptToOneValue
                    PupilsFixed = prism.PupilsFixed |> Option.map float |> mapOptToOneValue
                    PH = mapOptToTwoValue prism.PHMin prism.PHMax
                    TotalCO2 = mapOptToTwoValue prism.BicarbonateMin prism.BicarbonateMax
                    PCO2 = prism.PCO2Max |> mapOptToOneValue
                    PaO2 = prism.PaO2Min |> mapOptToOneValue
                    Glucose = prism.GlucoseMax |> mapOptToOneValue
                    Potassium = prism.PotassiumMax |> mapOptToOneValue
                    Creatinine = prism.CreatinineMax |> mapOptToOneValue
                    Urea = prism.UreaMax |> mapOptToOneValue
                    WhiteBloodCount = prism.WhiteBloodCountMin |> mapOptToOneValue
                    PT = prism.PTMax |> mapOptToOneValue
                    PTT = prism.PTTMax |> mapOptToOneValue
                    Platelets = prism.PlateletsMin |> mapOptToOneValue
                    AdmissionSource = prism.AdmissionSource
                    CPR24HourBefore = prism.CPR24HourBefore
                    Cancer = prism.Cancer
                    LowRiskPrimary = prism.LowRiskPrimary
            }

        let mapInputToPRISM (prism: PRISM) (input : Input) : PRISM =
            { prism with
                PRISM3Neuro = input.PRISM3Neuro
                PRISM3Score = input.PRISM3Score
                PRISM4Mortality = input.PRISM4Mortality
            }

        let calculate date input =
            match input.Age with
            | None -> input
            | _ ->
                let neuro, nonneuro =
                    match input |> calcScore date with
                    | Neuro v1, NonNeuro v2 -> Some v1, Some v2
                    | _ -> None, None

                { input with
                    PRISM3Neuro = neuro
                    PRISM3Score = nonneuro
                    PRISM4Mortality =
                        input
                        |> calcScore date
                        |> calcProbability date input
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

    let createPIM
        urgency
        recovery
        bypass
        cardiac
        riskDiagnosis
        ventilated
        pupils
        paO2
        fiO2
        be
        sbp
        =
        let recovMapping =
            match recovery, bypass, cardiac with
            | false, false, true  
            | false, false, false -> PIM.NoRecovery
            | false, true,  false // asume that post cardiac bypass is always recovery
            | false, true,  true  // asume that post cardiac bypass is always recovery
            | true,  true,  true  // asume that post cardiac bypass has precedence above cardiac
            | true,  true,  false -> PIM.PostCardiacByPass
            | true,  false, true  -> PIM.PostCariacNonByPass
            | true,  false, false -> PIM.PostNonCardiacProcedure
                   
        {
            Urgency = urgency
            Recovery = recovMapping
            RiskDiagnosis = riskDiagnosis
            Ventilated = ventilated
            AdmissionPupils = pupils
            PaO2 = paO2
            FiO2 = fiO2
            BaseExcess = be
            SystolicBloodPressure = sbp
            PIM2Score = None
            PIM2Mortality = None
            PIM3Score = None
            PIM3Mortality = None
        }
        |> PIM.calculatePIM2
        |> PIM.calculatePIM3


    let createPRISM
        sbpMin
        tempMin
        tempMax
        emv
        hrMax
        pupils
        phMin
        phMax
        bicMin
        bicMax
        pCO2Max
        paO2Min
        glucMax
        potassiumMax
        creatMax
        ureaMax
        wbcMin
        ptMax
        pttMax
        platMin
        admSource
        cpr
        cancer
        lowRisk =
        {
            Age = None
            SystolicBloodPressureMin = sbpMin
            TemperatureMin = tempMin
            TemperatureMax = tempMax
            MentalStatus = emv
            HeartRateMax = hrMax
            PupilsFixed = pupils
            PHMin = phMin
            PHMax = phMax
            BicarbonateMin = bicMin
            BicarbonateMax = bicMax
            PCO2Max = pCO2Max
            PaO2Min = paO2Min
            GlucoseMax = glucMax
            PotassiumMax = potassiumMax
            CreatinineMax = creatMax
            UreaMax = ureaMax
            WhiteBloodCountMin = wbcMin
            PTMax = ptMax
            PTTMax = pttMax
            PlateletsMin = platMin
            AdmissionSource = admSource
            CPR24HourBefore = cpr
            Cancer = cancer
            LowRiskPrimary = lowRisk
            PRISM3Score = None
            PRISM3Neuro = None
            PRISM4Mortality = None
        }
        |> Some


    let createPICUAdmission
        hospitalNumber
        clickId
        admissionDate
        dischargeDate
        dischargeReason
        admissionType
        admissionIndication
        referingSpecialism
        primaryDiagn
        secondaryDiagn
        admissionWeight
        admissionLength
        pim
        prism24
        prism12
        prism4
        =
        {
            ClickId = clickId
            HospitalNumber = hospitalNumber
            AdmissionDate = admissionDate
            DischargeDate = dischargeDate
            DischargeReason = dischargeReason
            AdmissionType = admissionType
            AdmissionIndication = admissionIndication
            ReferingSpecialism = referingSpecialism
            PrimaryDiagnosis = primaryDiagn
            SecondaryDiagnosis = secondaryDiagn
            Diagnoses = []
            AdmissionWeight = admissionWeight
            AdmissionLength = admissionLength
            PIM = pim
            PRISM24 = prism24
            PRISM12 = prism12
            PRISM4 = prism4
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

