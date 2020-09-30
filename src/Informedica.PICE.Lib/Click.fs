namespace Informedica.PICE.Lib


module Click =

    open FSharp.Interop.Excel

    [<Literal>]
    let path = "../../mrdm/pimprism_hist.xlsx"

    type PIMPRISMHist = ExcelFile<path, HasHeaders = true, ForceString = true>

    let pimprismHist = PIMPRISMHist ()
