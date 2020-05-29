namespace Informedica.PICE.Lib


module MRDM =

    open FSharp.Interop.Excel

    type MRDM = ExcelFile<"./../../mrdm/mrdm_export.xlsx">

    let mrdm = new MRDM ()

