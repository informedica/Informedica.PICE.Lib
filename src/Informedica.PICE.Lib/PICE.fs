namespace Informedica.PICE.Lib

module PICE =

    open System

    type Patient =
        {
            // ziekenhuis-episode-upn
            HospitalNumber : string
            // gebdat
            BirthDate : DateTime
            // geslacht
            Gender : Gender
            // pat-zwduur
            GestAge : Weeks option
            // status
            State : PatientState
            // datovl
            DateOfDeath : DateTime option
            // adm-deathmodeid
            DeathMode : DeathMode option
            HospitalAdmissions : HospitalAdmission list
        }
    and DeathMode = BrainDeath | CPRPerformed | NoCPRPerformed | Abstained
    and Weeks = int
    and Gender = Male | Female | UnknownGender
    and PatientState = Alive | Dead | UnknownPatientState
    and HospitalAdmission =
        {
            // adm-hosp-admdate
            HospitalAdmissionDate : DateTime
            // adm-hosp-disdate
            HospitalDischargeDate : DateTime option
            // adm-sourcedirectpicu
            DirectPICUAdmission : bool
            // adm-sourcetypeid
            AdmissionSourceType : string
            // adm-admhospunitid
            AdmissionSourceUnit : string
            // adm-transport-adm
            AdmissionTransportSource : string
            // herk-tran-door
            AdmissionTransportTeam : string
            // adm-sourcehospitalid-umc
            // adm-sourcehospitalid
            // adm-sourcehospitalid-bu
            AdmissionHospitalSource : Hospital option
            // adm-destdirectpicu
            DischargeFromPICU : bool
            // adm-desttypeid
            DischargeDestination : string
            // adm-desthospunitid (afdeling buiten eigen UMC?)
            DischargeUnit : string
            // adm-transport-dis
            DischargeTransportSource : string
            // bestm-tran-door
            DischargeTransportTeam : string
            // adm-desthospitalid-umc
            // adm-desthospitalid
            // adm-desthospitalid-bu
            DischargeHospitalDestination : Hospital option
            PICUAdmissions : PICUAdmission list
        }
    and Hospital =
        | Academic of string
        | NonAcademic of string
        | NonDutch of string
    and PICUAdmission =
        {
            // adm-ic-admdate
            AdmissionDate : DateTime
            // adm-ic-disdate
            DischargeDate : DateTime option
            // adm-disreasonid
            DischargeReason : DischargeReason
            // adm-sourceunitid
            HospitalSourceUnit : string
            // adm-typeid-urgentie
            Urgency : AdmissionUrgency
            // adm-typeid-soort
            AdmissionType : AdmissionType
            // bypass and option 11
            AdmissionProcedure : AdmissionProcedure
            // adm-readmtypeid
            Readmission : bool
            // recovery
            Recovery : bool
            // canule
            TracheoStomy : bool
            // adm-indication
            AdmissionIndication : string
            // adm-refspecialism
            ReferingSpecialism : string
            // risicodiag-hoofd
            RiskDiagnosis : RiskDiagnosis list
            // diagnose1
            MainDiagnosis : string
            // diangose2
            SecundaryDiagnosis : string
            // bijkomendediagnoses
            OtherDiagnoses : string list
            // gewicht kg
            AdmissionWeight : float option
            // adm-length cm
            AdmissionLength : int option
            // adm-pimloc
            PIMScoreLocation : string
            // t-min12
            TempMin12 : float option
            // t-max12
            TempMax12 : float option
            // pao2-0 kPa
            PaO2_1 : float option
            // fio2-0 %
            FiO2_PaO2_1 : float option
            // spo2-0 %
            Saturation_1: float option
            // fio2sat-0 %
            FiO2_Sat_1 : float option
            // be-0 mmol/L
            BE_1 : float option
            // sbp-0 mmHg
            SBP_1 : int option
            // sbp-min12 mmHg
            SBP_Min12 : int option
            // hr-max12
            HeartRateMax12 : int option
            // lactaat0 mmol/L
            Lactate_1 : float option
            // ph-min12
            PhMin12 : float option
            // ph-max12
            PhMax12 : float option
            // paco2-max12 kPa
            PCO2_Max12 : float option
            // pao2p3-12 kPa
            PaO2_Min12 : float option
            // bicarbonate-min12 mmol/L
            BiCarbonMin12 : float option
            // bicarbonate-max12 mmol/L
            BiCarbonMax12 : float option
            // glucose-max12 mmol/L
            GlucoseMax12 : float option
            // k-max12 mmol/L
            PotassiumMax12 : float option
            // creatinine-max12 micromol/L
            CreatininMax12 : float option
            // ureum-max12 mmol/L
            UreaMax12 : float option
            // leuco-min12 10^9/L
            LeukocytesMin12 : float option
            // thrombo-min12 10^9/L
            TrombocytesMin12 : float option
            // pt-max12
            PT_Max12 : float option
            // ptt-max12
            APTT_Max12 : float option
            Ventilated : bool
            // admpupils
            AdmissionPupils : PupilResponse
            // e-emv
            EMV_E_1 : int option
            // m-emv
            EMV_M_1 : int option
            // v-emv
            EMV_V_1 : int option
            // emv
            EMV_1 : int option
            // pupreaction3-12
            Pupils_12 : PupilResponse
            // e-emv12
            EMV_E_12 : int option
            // m-emv12
            EMV_M_12 : int option
            // v-emv12
            EMV_V_12 : int option
            // emv12
            EMV_12 : int option
            // cprprehosp-riskpim
            CPRPreHosp : bool
            // cprprepicu-riskpim
            CPRPrePICU : bool
            // contrean12
            CPR_PICU_12 : bool
            // adm-catheterdays
            Duration_CVL : int option
            // adm-ventilationdaysin days
            Duration_MV_Invasive : int option
            // adm-ventilationdaysni days
            Duration_MV_NonInvasive : int option
            PIM2 : float option
            PIM3 : float option
            PRISM4 : float option
        }
    and AdmissionUrgency = | Elective | NotElective | UnknownUrgency
    and AdmissionType = Medical | Surgery | DOA | UnknownAdmissionType
    and AdmissionProcedure =
        | CardiacByPass | CardiacNonByPass | NonCardiac | NoProcedure
    and PupilResponse = FixedDilated | NormalPupils | UnknownPupils
    and RiskDiagnosis =
        | Asthma
        // bmtrecipient-riskpim
        | BoneMarrowTransplant
        | Bronchiolitis
        // cancer
        | Cancer
        // cprprehosp-riskpim
        // cprprepicu-riskpim
        | CardiacArrest
        // cardmyopath-riskpim
        | CardiomyopathyOrMyocarditis
        // sponthersenbl-riskpim
        | CerebralHemorrhage
        // chromosomal
        | ChromosomalAbnormality
        | Croup
        | DiabeticKetoacidosis
        // hiv-riskpim
        | HIVPositive
        // hypoplast-riskpim
        | HypoplasticLeftHeartSyndrome
        // leukemie-riskpim
        | LeukemiaOrLymphoma
        | LiverFailure
        | LiverTransplant
        | NecrotizingEnterocolitis
        // neurodegen-riskpim
        | NeurodegenerativeDisorder
        | ObstructiveSleepApnea
        | SeizureDisorder
        // septische-shock
        | SepticShock
        // scid-riskpim
        | SevereCombinedImmuneDeficiency
    and DischargeReason =
        | RegularDischarge
        | PalliativeCare
        | NoPICUBed
        | Transferred
        | SpecializedCare
        | AgainstMedicalAdvice
        | DelayedTreatment
        | Deceased
        | NoValidDischargeReason of string


    let mapDischargeReason = function
    | s when s = "1" -> RegularDischarge
    | s when s = "2" -> PalliativeCare
    | s when s = "3" -> NoPICUBed
    | s when s = "5" -> Transferred
    | s when s = "6" -> SpecializedCare
    | s when s = "7" -> AgainstMedicalAdvice
    | s when s = "8" -> DelayedTreatment
    | s when s = "100" -> Deceased
    | s -> s |> NoValidDischargeReason 



    let mapRiscDiagnosis = function
    | s when s = "0" -> []
    | s when s = "1" -> [ Croup ]
    | s when s = "2" -> [ ObstructiveSleepApnea ]
    | s when s = "3" -> [ Bronchiolitis ]
    | s when s = "4" -> [ Asthma ]
    | s when s = "5" -> [ LiverTransplant ]
    | s when s = "6" -> [ SeizureDisorder ] 
    | s when s = "7" -> [ DiabeticKetoacidosis ] 
    | s when s = "8" -> [ LiverFailure ] 
    | s when s = "9" -> [ NecrotizingEnterocolitis ] 
    | s when s = "10" -> [ DiabeticKetoacidosis ] 
    | _ -> []


    let mapAdmissionType = function 
    | s when s = "1" -> Medical 
    | s when s = "2" -> Surgery
    | s when s = "3" -> DOA
    | _ -> UnknownAdmissionType


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


    let mapGender = function
    | s when s = "1" -> Male
    | s when s = "2" -> Female
    | _ -> UnknownGender


    let objToString (o : obj) = o |> string


    let stringToIntOpt s =
        match Int32.TryParse(s) with
        | true, x -> x |> Some
        | _       -> None


    let stringToFloatOpt s =
        match Double.TryParse(s) with
        | true, x -> x |> Some
        | _       -> None


    let objToIntOpt x = x |> (box >> objToString >> stringToIntOpt)


    let objToFloatOpt (x : obj) : float option =
        try
            x :?> Double |> Some
        with
        | _ -> None


    let objToBool o =
        match o |> objToString with
        | "1" -> true
        | _   -> false


    let get () =
        MRDM.mrdm.Data
        |> Seq.toList
        |> List.groupBy (fun d -> d.ziekenhuis_episode_upn)
        |> List.map (fun (hn, data) ->
            let find rep n c =
                Mapping.find (hn |> objToString) rep n (c |> objToString)
            data
            |> List.tryHead
            |> function
            | Some d ->
                {
                    HospitalNumber = d.ziekenhuis_episode_upn |> string
                    BirthDate = d.gebdat
                    Gender = d.geslacht |> string |> mapGender
                    GestAge = d.pat_zwduur |> objToIntOpt
                    State = d.status |> objToString |> mapPatientState
                    DateOfDeath = None
                    DeathMode = None
                    HospitalAdmissions =
                        data
                        |> List.groupBy (fun d -> d.adm_hosp_admdate)
                        |> List.collect (fun (_, data) ->
                            data
                            |> List.tryHead
                            |> function
                            | Some d ->

                                let admh1 = d.adm_sourcehospitalid_umc |> find false "adm-sourcehospitalid-umc"
                                let admh2 = d.adm_sourcehospitalid |> find false "adm-sourcehospitalid"
                                let admh3 = d.adm_sourcehospitalid_bu |> find false  "adm-sourcehospitalid-bu"

                                let dish1 = d.adm_desthospitalid_umc |> find  false "adm-desthospitalid-umc"
                                let dish2 = d.adm_desthospitalid |> find false "adm-desthospitalid"
                                let dish3 = d.adm_desthospitalid_bu |> find false "adm-desthospitalid-bu"


                                {
                                    HospitalAdmissionDate = d.adm_hosp_admdate
                                    HospitalDischargeDate =
                                        d.adm_hosp_disdate |> Some
                                    DirectPICUAdmission = d.adm_sourcedirectpicu |> objToBool
                                    AdmissionSourceType =
                                        d.adm_sourcetypeid
                                        |> find true "adm-sourcetypeid"
                                    AdmissionSourceUnit =
                                        d.adm_admhospunitid
                                        |> find false "adm-admhospunitid"
                                    AdmissionTransportSource =
                                        d.adm_transport_adm
                                        |> find true "adm-transport-adm"
                                    AdmissionTransportTeam =
                                        d.herk_tran_door
                                        |> find false "herk-tran-door"
                                    AdmissionHospitalSource =
                                        match admh1, admh2, admh3 with
                                        | _ when  admh1   <> "" -> admh1 |> Academic |> Some
                                        | _ when admh2 <> ""    -> admh2 |> NonAcademic |> Some
                                        | _ when admh3 <> ""    -> admh3 |> NonDutch |> Some
                                        | _ -> None
                                    DischargeFromPICU = d.adm_destdirectpicu |> objToBool
                                    DischargeDestination =
                                        d.adm_desttypeid |> find true "adm-desttypeid"
                                    DischargeUnit =
                                        d.adm_desthospunitid |> find false "adm-desthospunitid"
                                    DischargeTransportSource =
                                        d.adm_transport_dis |> find true "adm-transport-dis"
                                    DischargeTransportTeam =
                                        d.bestm_tran_door |> find true "bestm-tran-door"
                                    DischargeHospitalDestination =
                                        match dish1, dish2, dish3 with
                                        | _ when  dish1   <> "" -> dish1 |> Academic |> Some
                                        | _ when dish2 <> ""    -> dish2 |> NonAcademic |> Some
                                        | _ when dish3 <> ""    -> dish3 |> NonDutch |> Some
                                        | _ -> None
                                    PICUAdmissions =
                                        data
                                        |> List.groupBy (fun d -> d.adm_ic_admdate)
                                        |> List.collect (fun (_, data) ->
                                            data
                                            |> List.tryHead
                                            |> function
                                            | Some d ->
                                                {
                                                    AdmissionDate = d.adm_ic_admdate
                                                    DischargeDate =
                                                        try
                                                            d.adm_ic_disdate
                                                            |> Some
                                                        with
                                                        | _ -> None
                                                    DischargeReason =
                                                        d.adm_disreasonid
                                                        |> objToString
                                                        |> mapDischargeReason
                                                    HospitalSourceUnit =
                                                        d.adm_sourceunitid
                                                        |> find true "adm-sourceunitid"
                                                    Urgency =
                                                        d.adm_typeid_urgentie
                                                        |> objToString
                                                        |> mapUrgency
                                                    AdmissionType =
                                                        d.adm_typeid_soort
                                                        |> objToString
                                                        |> mapAdmissionType
                                                    AdmissionProcedure =
                                                        if d.bypass |> objToBool then CardiacByPass
                                                        else NoProcedure
                                                    Readmission =
                                                        d.adm_readmtypeid |> objToBool
                                                    Recovery =
                                                        d.recovery |> objToBool
                                                    TracheoStomy =
                                                        d.canule |> objToBool
                                                    AdmissionIndication =
                                                        d.adm_indication |> find true "adm-indication"
                                                    ReferingSpecialism =
                                                        d.adm_refspecialism |> find true "adm-refspecialism"
                                                    RiskDiagnosis =
                                                        [
                                                            d.risicodiag_hoofd |> objToString |> mapRiscDiagnosis
                                                            if d.bmtrecipient_riskpim |> objToBool then [ BoneMarrowTransplant ]
                                                        ]
                                                        |> List.collect id
                                                    MainDiagnosis =
                                                        d.diagnose1 |> find true "diagnose1"
                                                    SecundaryDiagnosis =
                                                        d.diagnose2 |> find true "diagnose2"
                                                    OtherDiagnoses = []
                                                    AdmissionWeight = d.gewicht |> objToFloatOpt
                                                    AdmissionLength = d.adm_length |> objToIntOpt
                                                    PIMScoreLocation =
                                                        d.adm_pimloc |> find true "adm_pimloc"
                                                    TempMin12 = d.t_min12 |> objToFloatOpt
                                                    TempMax12 = d.t_max12 |> objToFloatOpt
                                                    PaO2_1 = d.pao2_0 |> objToFloatOpt
                                                    FiO2_PaO2_1 = d.fio2_0 |> objToFloatOpt
                                                    Saturation_1 = d.spo2_0 |> objToFloatOpt
                                                    FiO2_Sat_1 = d.fio2sat_0 |> objToFloatOpt
                                                    BE_1 = d.be_0 |> objToFloatOpt
                                                    SBP_1 = d.sbp_0 |> objToIntOpt
                                                    SBP_Min12 =  d.sbp_min12 |> objToIntOpt
                                                    HeartRateMax12 = d.hr_max12 |> objToIntOpt
                                                    Lactate_1 = d.lactaat0 |> objToFloatOpt
                                                    PhMin12 = d.ph_min12 |> objToFloatOpt
                                                    PhMax12 = d.ph_max12 |> objToFloatOpt
                                                    PCO2_Max12 = d.paco2_max12 |> objToFloatOpt
                                                    PaO2_Min12 = d.pao2p3_12 |> objToFloatOpt
                                                    BiCarbonMin12 = d.bicarbonate_min12 |> objToFloatOpt
                                                    BiCarbonMax12 = d.bicarbonate_max12 |> objToFloatOpt
                                                    GlucoseMax12 = d.glucose_max12 |> objToFloatOpt
                                                    PotassiumMax12 = d.k_max12 |> objToFloatOpt
                                                    CreatininMax12 = d.creatinine_max12 |> objToFloatOpt
                                                    UreaMax12 = d.ureum_max12 |> objToFloatOpt
                                                    LeukocytesMin12 = d.leuco_min12 |> objToFloatOpt
                                                    TrombocytesMin12 = d.thrombo_min12 |> objToFloatOpt
                                                    PT_Max12 = d.pt_max12 |> objToFloatOpt
                                                    APTT_Max12 = d.ptt_max12 |> objToFloatOpt
                                                    Ventilated = d.ventilated |> objToBool
                                                    AdmissionPupils =
                                                        d.admpupils |> objToString |> mapPupils
                                                    EMV_E_1 = d.e_emv |> objToIntOpt
                                                    EMV_M_1 = d.m_emv |> objToIntOpt
                                                    EMV_V_1 = d.v_emv |> objToIntOpt
                                                    EMV_1 = d.emv |> objToIntOpt
                                                    Pupils_12 =
                                                        d.pupreaction3_12 |> objToString |> mapPupils
                                                    EMV_E_12 = d.e_emv12 |> objToIntOpt
                                                    EMV_M_12 = d.m_emv12 |> objToIntOpt
                                                    EMV_V_12 = d.v_emv12 |> objToIntOpt
                                                    EMV_12 = d.emv12 |> objToIntOpt
                                                    CPRPreHosp = d.cprprehosp_riskpim |> objToBool
                                                    CPRPrePICU = d.cprprepicu_riskpim |> objToBool
                                                    CPR_PICU_12 = d.contrean12 |> objToBool
                                                    Duration_CVL = d.adm_catheterdays |> objToIntOpt
                                                    Duration_MV_Invasive = d.adm_ventilationdaysin |> objToIntOpt
                                                    Duration_MV_NonInvasive = d.adm_ventilationdaysni |> objToIntOpt
                                                    PIM2 = None
                                                    PIM3 = None
                                                    PRISM4 = None
                                                }
                                                |> List.singleton
                                            | None ->
                                                printfn "geen IC opnames voor %A" hn
                                                []
                                        )

                                }
                                |> List.singleton
                            | None ->
                                printf "no hospital admissions for %A" hn
                                []
                        )
                }
                |> List.singleton
            | None ->
                printfn "no data for %A" hn
                List.empty
        )
        |> List.collect id

