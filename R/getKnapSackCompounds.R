#' Extracts compounds from KNApSAcK
#'
#' @param item Informs the type of information to be provided to the website.
#'   * `Organism`;
#'   * `Metabolite`;
#'   * `Molecular formula`;
#'   * ...
#' @param keyword Specifies what is to be searched
#' @param species Species' name
#' @importFrom httr2 request req_perform resp_body_html
#' @importFrom rvest html_element html_table
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate filter case_when arrange desc select
#' @importFrom stats setNames
.getKnapSackCompounds <- function(item, keyword, species) {

  URL <- paste0("https://www.knapsackfamily.com/knapsack_core/result.php?sname=", item,"&word=", keyword)

  tryCatch(
    expr = {
      knsckCompounds <<- request(URL) |>
        req_perform() |>
        resp_body_html() |>
        html_element("table") |>
        html_table() |>
        setNames(c("SNPSCKID", "casid", "NAME", "FORMULA", "EXACT_MASS", "ORGANISM")) |>
        dplyr::filter(str_detect(ORGANISM, paste0("^", keyword))) |>
        dplyr::mutate(BRITE = NA, .after = EXACT_MASS) |>
        dplyr::mutate(ORGANISM = case_when(str_detect(ORGANISM, species) ~ "Species",
                                           .default = "Genus"),
                      ENTRY = NA,
                      CID = NA,
                      pathways = NA) |>
        dplyr::arrange(desc(ORGANISM)) |>
        dplyr::filter(!duplicated(SNPSCKID)) |>
        dplyr::select(!casid)

      return(knsckCompounds)

    },
    error = function(error) {
      return(NULL)

    }
  )
}


#' Get genus of organism
#'
#' @param orgCode KEGG organism identifier
#'
.getKeyword <- function(orgCode) {
  organisms[organisms$organism == orgCode,][[2]]
}
