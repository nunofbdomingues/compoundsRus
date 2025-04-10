% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/availableData.R
\name{getOrganismCode}
\alias{getOrganismCode}
\title{-- FOR FUTURE USE ON SHINY APP -- Displays the organism code corresponding to the scientific name}
\usage{
getOrganismCode(orgTable, orgName)
}
\arguments{
\item{orgTable}{Dataframe with a two columns: one with the organism KEGG codes, and the other with the organism's scientific name.}

\item{orgName}{Scientific name of the organism of interest.}
}
\value{
A single string with the KEDD ID.
}
\description{
Given a data.frame originated from \code{\link[=whichOrganisms]{whichOrganisms()}} and the name of an organism, returns
the KEGG identifier respective to that organism.
}
\examples{
#Retrieve available organisms from KEGG
df <- whichOrganisms(x = "organism")

#Get KEGG ID for Vitis vinifera
getOrganismCode(orgTable = df, orgName = "Vitis vinifera (wine grape)")
}
