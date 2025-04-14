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
#' getOrganismCode(orgName = "Vitis vinifera (wine grape)")
#' @param orgName Scientific name of the organism of interest.
#' @export
getOrganismCode <- function(orgName) {

  organisms[organisms$species == orgName,][[1]]
}


#' Get all organisms' names available on KEGG
#'
#' @description
#' Simple function to provide all available organisms present in the KEGG database.
#' @returns
#' A data.frame vector containing all organisms' names available in KEGG.
#' @export
#'
getOrganismNamesCode <- function() {
  organisms
}

