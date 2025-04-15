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


#' -- FOR FUTURE USE ON SHINY APP -- Displays the organism name corresponding to a given KEGG code
#'
#' @description Given a KEGG code, displays the name of an organism
#' @returns
#' A single string with the organism name.
#' @examples
#' #Retrieve available organisms from KEGG
#' df <- whichOrganisms(x = "organism")
#'
#' #Get name from the KEGG ID for Vitis vinifera
#' getOrganismCode(orgName = "vvi")
#' @param orgName KEGG code of the organism of interest.
#' @export
getOrganismName <- function(orgName) {

  organisms[organisms$organism == orgName,][[2]]
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

