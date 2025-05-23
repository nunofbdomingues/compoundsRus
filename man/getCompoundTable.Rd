% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCompoundTable.R
\name{getCompoundTable}
\alias{getCompoundTable}
\title{Creates the compound database for a specific organism, from PubChem or KEGG, as specified by the user}
\usage{
getCompoundTable(orgCode, db, complete = FALSE)
}
\arguments{
\item{orgCode}{KEGG ID for the organism of interest. See \code{\link[=whichOrganisms]{whichOrganisms()}} for a list of available organisms.}

\item{db}{Database from where the compounds should be extracted.
\itemize{
\item \code{db = "KEGG"}: Extract compounds from \href{https://www.kegg.jp}{KEGG} database.
\item \code{db = "PubChem"}: Extract compounds from \href{https://pubchem.ncbi.nlm.nih.gov}{PubChem} database.
}}

\item{complete}{If \code{TRUE}, tries to include compound data
from \href{https://www.knapsackfamily.com/KNApSAcK/}{KNApSAcK}.}
}
\value{
A \code{data.frame} containing, for each compound:
\itemize{
\item the KEGG identifier;
\item KNApSAcK identifier (when \code{complete = TRUE});
\item the PubChem identifier (when \code{db = "PubChem"});
\item its name;
\item molecular formula;
\item exact mass;
\item and the pathways it participates in.
}
}
\description{
\code{getCompoundTable()} creates a dataframe object containing information regarding the compounds
participating in a user specified organism, as long as this information is available in Kyoto
Encyclopedia of Genes and Genomes data (KEGG) database.

Although it primarily relies on KEGG, the function can also retrieve compound information from the PubChem
database provided that these compounds are cross referenced. Using PubChem has the advantage
of being faster and, in some cases, resulting in more standardised molecular formulas.

The user also has the option to complement the database with compounds from SNApSAcK. In order to preserve
possible isomers, using SNApSAcK may add duplicate compounds.
}
\details{
Compounds with no available information in regards to their name, molecular formula and/or exact
mass are promptly removed from the final \code{data.frame}.
}
\examples{
## Get compounds availabe for Vitis vinifera from PubChem
vviData <- getCompoundTable("vvi", "PubChem", TRUE)
}
\references{
Afendi, F. M., Okada, T., Yamazaki, M., Hirai-Morita, A., Nakamura, Y., Nakamura, K., Ikeda, S.,
Takahashi, H., Altaf-Ul-Amin, Md., Darusman, L. K., Saito, K., & Kanaya, S. (2012). KNApSAcK
Family Databases: Integrated Metabolite–Plant Species Databases for Multifaceted Plant Research.
Plant and Cell Physiology, 53(2), e1. \url{https://doi.org/10.1093/pcp/pcr165}

Kanehisa, M., & Goto, S. (2000). KEGG: Kyoto encyclopedia of genes and genomes. Nucleic Acids
Research, 28(1), 27–30. \url{https://doi.org/10.1093/nar/28.1.27}

Kim, S., Chen, J., Cheng, T., Gindulyte, A., He, J., He, S., Li, Q., Shoemaker, B. A.,
Thiessen, P. A., Yu, B., Zaslavsky, L., Zhang, J., & Bolton, E. E. (2025). PubChem 2025 update.
Nucleic Acids Research, 53(D1), D1516–D1525. \url{https://doi.org/10.1093/nar/gkae1059}
}
