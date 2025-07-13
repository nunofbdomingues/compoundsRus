#' Parses a a list of BRITE entries
#'
#' @param briteCol List with BRITE entries
#' @importFrom purrr map_chr has_element discard
#' @importFrom stringr str_extract_all
#' @keywords internal
.briteParser <- function(briteCol) {

  briteFilt <- stringr::str_extract_all(briteCol, "(?:[A-Z].*?\\[BR:br080[0-2]\\d\\].*?)(?=\\s*C\\d{5})")

  finalBrite <- purrr::map_chr(briteFilt, .parserElem)

  return(finalBrite)
}



#' Parses a single BRITE string
#'
#' @param elem element with BRITE information
.parserElem <- function(elem) {

  if(base::is.null(elem)) {

    return(NA)
  } else {

    elem <- base::gsub("C\\d{5}[^|]*\\|", "", base::paste(elem, collapse = " ")) |>
      base::gsub("\\s*\\|$", "", x = _) |>
      base::gsub("\\s+", " ", x = _)

  }

}

