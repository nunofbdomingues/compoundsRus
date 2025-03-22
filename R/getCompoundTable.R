#' Creates the compound database for a specific organism, from PubChem or KEGG, as specified by the user
#'
#' @description
#' `getCompoundTable()` creates a dataframe object containing information regarding the compounds
#' participating in a user specified organism, as long as this information is available in Kyoto
#' Encyclopedia of Genes and Genomes data (KEGG) database.
#'
#' Although it primarily relies on KEGG, the function can also retrieve compound information from the PubChem
#' database provided that these compounds are cross referenced. Using PubChem has the advantage
#' of being faster and, in some cases, resulting in more standardised molecular formulas.
#'
#' @param orgCode KEGG ID for the organism of interest. See [whichOrganisms()] for a list of available organisms.
#' @param db Database from where the compounds should be extracted.
#'   * `db = "KEGG"`: Extract compounds from [KEGG](https://www.kegg.jp) database.
#'   * `"db = PubChem"`: Extract compounds from [PubChem](https://pubchem.ncbi.nlm.nih.gov) database.
#' @returns A `data.frame` containing, for each compound:
#'   * the KEGG identifier;
#'   * the PubChem identifier (when `db = "PubChem"`);
#'   * its name;
#'   * molecular formula;
#'   * exact mass;
#'   * and the pathways it participates in.
#' @examples
#' ## Get compounds availabe for Vitis vinifera from PubChem
#' vviData <- getCompoundTable("vvi", "PubChem")
#' @details
#' Compounds with no available information in regards to their name, molecular formula and/or exact
#' mass are promptly removed from the final `data.frame`.
#' @importFrom KEGGREST keggList
#' @importFrom purrr discard
#' @importFrom stats na.omit
#' @export
getCompoundTable <- function(orgCode, db) {

  ourPathwaysID <- KEGGREST::keggList("pathway", orgCode)

  pathwayInfo <- .getPathwayInfo(ourPathwaysID)

  compoundsPathways <- .getCompoundsPathways(pathwayInfo) |>
    purrr::discard(is.null) |>
    unlist(use.names = F) |>
    unique()

  if(db == "KEGG") {

    compoundInfo <- .getKeggCompounds(compoundsPathways)

    properties <- c("ENTRY", "NAME", "FORMULA", "EXACT_MASS")

    finalCompoundList <- as.data.frame(do.call(cbind,
                                          lapply(properties,
                                                 function(prop) .getKeggProperties(compoundInfo, prop)))) |>
      na.omit()

    colnames(finalCompoundList) <- properties

  } else if(db == "PubChem") {

    finalCompoundList <- .getPubChemCompounds(compoundsPathways)
  }


  CompoundsPathways.df <- .getCompoundPathways(pathwayInfo)

  finalCompoundDB <- base::merge(finalCompoundList, CompoundsPathways.df, by.x = "ENTRY", by.y = "Compound")

  return(finalCompoundDB)
}





# HELPER FUNCTIONS

#' Get informations regaring pathways of the chosen organism
#'
#' @param pathwaysID Character vector with valid KEGG IDs for pathways.
#' @importFrom KEGGREST keggGet
#' @importFrom stats na.omit
.getPathwayInfo <- function(pathwaysID) {
  counter <- 1
  pathwayInfo <- list()
  pathwaysNames <- names(pathwaysID)
  while(counter <= length(pathwaysID)) {

    if((length(pathwaysID) - counter) > 10) {

      pathToIterate <- pathwaysNames[counter:(counter + 9)]
      pathwayInfo <- base::append(pathwayInfo, KEGGREST::keggGet(pathToIterate))

    } else {

      pathToIterate <- pathwaysNames[counter:length(pathwaysID)]
      pathwayInfo <- base::append(pathwayInfo, KEGGREST::keggGet(pathToIterate)) |>
        setNames(pathwaysID)

    }

    counter <- counter + 10
  }

  return(pathwayInfo)
}


#' Extracts the pathways that contain information regarding the compounds participating on it
#'
#' @param pathwayInfo List containing data concerning the pathways
#' @importFrom purrr map
.getCompoundsPathways <- function(pathwayInfo) {

  purrr::map(pathwayInfo, function(thisPathway) {

    if("COMPOUND" %in% names(thisPathway)) {
      compounds <- names(thisPathway$COMPOUND)
    } else {
      compounds <- NULL

    }
    return(compounds)
  })
}


#' Extracts compounds data from KEGG
#'
#' @param compounds Character vector with valid KEGG IDs
#' @importFrom KEGGREST keggGet
.getKeggCompounds <- function(compounds) {

  counter = 1
  compoundInfo <- list()

  while(counter<=length(compounds)) {

    if((length(compounds) - counter) > 10) {
      compoundInfo <- append(compoundInfo, KEGGREST::keggGet(compounds[counter:(counter+9)]))

    } else {
      compoundInfo <- append(compoundInfo, KEGGREST::keggGet(compounds[counter:length(compounds)]))

    }
    counter <- counter + 10

  }
  return(compoundInfo)

}

#' Extracts compounds properties from KEGG
#'
#' @param listOfKegg List with data from compounds, extracted from KEGG.
#' @param property List of properties to extract.
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
.getKeggProperties <- function(listOfKegg, property) {

  purrr::map_dfr(listOfKegg, function(myBigList) {

    tibble::tibble(NAME = myBigList[[property]][1] %||% NA)

  })

}



#' Extracts compounds data from KEGG
#'
#' @param pathwayList List containing data concerning the pathways
#' @importFrom purrr map has_element discard
#' @importFrom tibble tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr nest
#' @importFrom dplyr group_by mutate
#' @importFrom stats setNames
.getCompoundPathways <- function(pathwayList) {
  # Checks which pathways contain information regarding the compounds participating on it
  pathwayswCompound <- purrr::map(pathwayList, function(myList) {

    listIterated <- myList

    contents <- names(myList)

    if(purrr::has_element(contents, "COMPOUND")) {
      return(myList$ENTRY)
    } else {
      return(NULL)
    }

  }) |>
    discard(is.null)

  # Merges the names of the pathways with the kegg identifiers, separating them with " | "
  pathwaysNameswCodes <- paste0(names(pathwayswCompound)," | ", pathwayswCompound)

  # Solely retains the names of the compounds in each pathway.
  compoundsPathways <- purrr::map(pathwayList, function(myList) {

    listIterated <- myList

    contents <- names(listIterated)

    if(purrr::has_element(contents, "COMPOUND")) {
      return(names(listIterated$COMPOUND))
    } else {
      return(NULL)
    }

  }) |>
    discard(is.null) |>
    setNames(pathwaysNameswCodes)

  # Transforms the list compoundsPathways into a dataframe
  compoundsPathways.df <- unlist(compoundsPathways) |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    setNames(c("Pathway", "Compound")) |>
    dplyr::group_by(Compound) |>
    tidyr::nest()


  # Unlists the pathways, resulting on a single string value with all the pathways to which the compound belongs to, where each pathwayName | keggID is separated by " & "
  finalCompactCompoundsPathways.df <- compoundsPathways.df %>%
    dplyr::mutate(., pathways = paste(as.vector(unlist(data)), collapse = " & "), .keep = "unused")

  return(finalCompactCompoundsPathways.df)
}
