
#' Outputting drugs in clinical use which metabolism is potentially disrupted
#' by a critical SNP at the provided CYP isoform. drugs.rda must be loaded.
#'
#'
#'
#' @param CYP CYP isoform among CYP1A2, CYP2B6, CYP2C8, CYP2C9,
#'            CYP2C19, CYP2D6, CYP3A4
#'
#'
#' @returns List of drug which metabolism is potentially disrupted
#'          by a critical SNP at the provided CYP isoform
#'
#' @examples
#' \dontrun{
#' load(file = "./data/drugs.rda")
#' snpToDrug("CYP2D6")
#' }
#'
#' @references
#' Ministry of Health, Labour and Welfare (MHLW), Japan (2014).
#' Drug interaction guideline for drug development and labeling recommendations
#' European Medicines Agency (2013). Guideline on the Investigation of Drug
#' Interactions.
# 'University of Washington Metabolism and Transport Drug Interaction Database [Hachad et al. (2010), Hum Genomics, 5(1):61]
#'
#'
#' @export

snpToDrug <- function(CYP) {

  if (!is.string(CYP)){
    stop("CYP isoform must be provided as string")
  }

  isoforms <- c("CYP1A2", "CYP2B6", "CYP2C8", "CYP2C9", "CYP2C19", "CYP2D6", "CYP3A4")

  if (!CYP%in%isoforms){
    return ("No drug associated")
  }

  result <- drugs[CYP]

  return (result)

}

