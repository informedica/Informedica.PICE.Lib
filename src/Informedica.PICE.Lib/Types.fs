namespace Informedica.PICE.Lib

module Types =

    open System
    open Informedica.PimPrism.Lib.Types

    type Patient =
        {
            Id : string
            HospitalNumber : string
            LastName : string
            FirstName : string
            BirthDate : DateTime option
            Gender : Gender
            BirthWeight : float option
            GestationalAge : int option
            PatientState : PatientState
            DateOfDeath : DateTime option
            DeathLocation : DataOption option
            DeathMode : DataOption option
            HospitalAdmissions : HospitalAdmission list
        }
    and PatientState = Alive | Dead | UnknownPatientState
    and HospitalAdmission =
        {
            Id : string
            HospitalNumber : string
            AdmissionDate : DateTime option
            DischargeDate : DateTime option
            TransportHospital : DataOption option
            TransportTeam : DataOption option
            DischargeDestination : DataOption option
            PICUAdmissions : PICUAdmission list
        }
    and PICUAdmission =
        {
            Id : string
            HospitalAdmissionId : string
            ClickId : string
            HospitalNumber : string
            Readmission : bool
            AdmissionDate : DateTime option
            DischargeDate : DateTime option
            DischargeReason : DataOption option
            AdmissionType : AdmissionType
            AdmissionIndication : DataOption option
            ReferingSpecialism : DataOption option
            PrimaryDiagnosis : Diagnose list
            SecondaryDiagnosis : Diagnose list
            Diagnoses : Diagnose list
            AdmissionWeight : float option
            AdmissionLength : int option
            ContinuousReanimation : bool
            Canule : bool
            Sepsis : bool
            PIM : PIM
            PRISM24 : PRISM option
            PRISM12 : PRISM option
            PRISM4 : PRISM option
        }
    and AdmissionType =
        | Medical
        | Surgery
        | DOA
        | UnknownAdmissionType
    and Diagnose =
        {
            Id: string
            Group : string
            Name : string
        }
    and Gender =
        | Male
        | Female
        | UnknownGender
    and DataOption = { Id : string; Label : string }


    type ParsingError =
        | ParseError of string
        | NoParseError

    type ValidationError =
        | NotValid of Patient * string
        | IsValid


    type Filter =
        | NoFilter
        | AgeFilter of AgeGroup
        | DiagnoseFilter of DiagnoseGroup
    and DiagnoseGroup =
        | Cardicac
        | Oncology
        | OtherDiagnoses
    and AgeGroup =
        | Neonate
        | Infant
        | Toddler
        | EarlyChildhood
        | MiddleChildhood
        | Adolescence

