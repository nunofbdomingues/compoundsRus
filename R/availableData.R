#' Retrieves organisms available on KEGG
#'
#' @description
#' Creates a `data.frame` with the KEGG identifiers and scientific names of all organisms
#' available in the database.
#' @returns A `data.frame` object.
#' @examples
#' df <- whichOrganisms(x = "organism")
#' @param x Parameter to specify what KEGGREST should look on KEGG.
#' @importFrom KEGGREST keggList
#' @export
whichOrganisms <- function(x = "organism") {
  tableWithOrg <- KEGGREST::keggList(x)[,c("organism", "species")] |>
    as.data.frame()
}


#' -- FOR FUTURE USE ON SHINY APP -- Displays the organism code corresponding to the scientific name
#'
#' @description Given a data.frame originated from [whichOrganisms()] and the name of an organism, returns
#' the KEGG identifier respective to that organism.
#' @returns
#' A single string with the KEDD ID.
#' @examples
#' #Retrieve available organisms from KEGG
#' df <- whichOrganisms(x = "organism")
#'
#' #Get KEGG ID for Vitis vinifera
#' getOrganismCode(orgTable = df, orgName = "Vitis vinifera (wine grape)")
#' @param orgTable Dataframe with a two columns: one with the organism KEGG codes, and the other with the organism's scientific name.
#' @param orgName Scientific name of the organism of interest.
#' @export
getOrganismCode <- function(orgTable, orgName) {

  orgTable[orgTable$species == orgName,][[1]]
}

