#' Extracts compounds from PubChem, and KEGG when unavailable in the earlier
#'
#' @param compounds Character vector with valid KEGG IDs
#' @importFrom rvest read_html
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom stats na.omit setNames
#' @importFrom KEGGREST keggConv
.getPubChemCompounds <- function(compounds) {
  pubChemIDs <- KEGGREST::keggConv("pubchem", compounds, querySize = 100)

  pubChemIDsVect <- pubChemIDs %>%
    gsub("pubchem:", "", .) %>%
    unname()

  compoundList <- .getCIDs(pubChemIDsVect)

  compoundListwCID <- compoundList[-which(is.na(compoundList))]
  pubChemIDswCID <- pubChemIDs[-which(is.na(compoundList))]

  finalCompoundList <- .getCIDsProperties(compoundListwCID, pubChemIDswCID)

  keggNoPub <- gsub("cpd:", "", names(pubChemIDs)) %>%
    gsub("gl:", "", .)

  compoundsLeft <- compounds[-which(compounds %in% keggNoPub)]

  counter = 1
  compoundLeftInfo <- list()
  while(counter<=length(compoundsLeft)) {

    compoundLeftInfo <- append(compoundLeftInfo, KEGGREST::keggGet(compoundsLeft[counter:(counter+9)]))

    counter <- counter + 10
  }

  properties <- c("ENTRY", "NAME", "FORMULA", "EXACT_MASS")

  compoundLeftList <<- as.data.frame(do.call(cbind,
                                        lapply(properties,
                                               function(prop) .getKeggProperties(compoundLeftInfo, prop)))) |>
    na.omit() |>
    setNames(c("ENTRY", "NAME", "FORMULA", "EXACT_MASS"))


  compoundLeftList <- compoundLeftList |>
    dplyr::mutate(CID = NA, .after = ENTRY)


  finalRealCompoundList <- rbind(na.omit(finalCompoundList), compoundLeftList)

  return(finalRealCompoundList)
}


#' Extracts CIDs from SIDs
#'
#' @param SIDs Character vector with valid SIDs
#' @importFrom rvest read_html
.getCIDs <- function(SIDs) {

  counter <- 1
  compoundList <- c()
  while(counter <= length(SIDs)) {
    if((length(SIDs) - counter) >= 300) {
      elemsToSearch <- paste(SIDs[counter:(counter+299)], collapse = ",")
    } else {
      elemsToSearch <- paste(SIDs[counter:length(SIDs)], collapse = ",")
    }

    print(elemsToSearch)
    html <- rvest::read_html(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sid/",elemsToSearch,"/XML"))

    compoundsToAppend <- html |>
      .extractHTML("pc-substance", "pc-compoundtype_id_cid")

    compoundList <- base::append(compoundList, compoundsToAppend)

    counter <- counter + 300

    # In order to respect the time between queries:
    Sys.sleep(1.5)
  }
  View(compoundList)
  return(compoundList)
}


#' Extracts compounds properties from CIDs
#'
#' @param CIDs Character vector with valid CIDs
#' @importFrom rvest read_html
#' @importFrom magrittr %>%
#' @importFrom rvest read_html
.getCIDsProperties <- function(CIDs, pubChemIDswCID) {

  counter <- 1
  finalCompoundList <- data.frame()
  while(counter <= length(CIDs)) {

    if((length(CIDs) - counter) >= 300) {
      elemsToSearch <- paste(CIDs[counter:(counter+299)], collapse = ",")

      keggIDs <- gsub("cpd:", "", names(pubChemIDswCID[counter:(counter+299)])) %>%
        gsub("gl:", "", .)

    } else {
      elemsToSearch <- paste(CIDs[counter:length(CIDs)], collapse = ",")

      keggIDs <- gsub("cpd:", "", names(pubChemIDswCID[counter:length(pubChemIDswCID)])) %>%
        gsub("gl:", "", .)
    }

    html <- rvest::read_html(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",elemsToSearch,"/property/title,MolecularFormula,ExactMass/XML"))

    properties <- c("cid", "title", "molecularformula", "exactmass")

    thisCompoundList <- as.data.frame(do.call(cbind, lapply(properties,
                                                            function(prop) .extractHTML(html, "properties", prop))))

    thisCompoundList <- cbind(keggIDs, thisCompoundList)

    finalCompoundList <- rbind(finalCompoundList, thisCompoundList)

    counter <- counter + 300
    # In order to respect the time between queries:
    Sys.sleep(1.5)

  }
  names(finalCompoundList) <- c("ENTRY", "CID", "NAME", "FORMULA", "EXACT_MASS")
  return(finalCompoundList)
}

#' Extracts compounds from PubChem, and KEGG when unavailable in the earlier
#'
#' @param htmlFile Data extracted from read_html
#' @param node1 First node to search on the XML
#' @param node2 Second node to search on the XML
#' @importFrom rvest html_elements html_element html_text2
.extractHTML <- function(htmlFile, node1, node2) {

  compoundsToAppend <- htmlFile |>
    rvest::html_elements(node1) |>
    rvest::html_element(node2) |>
    rvest::html_text2()

  return(compoundsToAppend)
}
