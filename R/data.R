#' In vitro and in vivo current validated CYP - drug interactions
#'
#' @source FDA, drug interaction and labeling (2021).
#'
#' @format{
#' Named vector of CYP drug interaction:
#' \describe{
#' \item{name}{CYP isoform}
#' \item{drugs}{List of drugs interacting with CYP isoform.}
#' }
#' }
#'
#' @references
#' Ministry of Health, Labour and Welfare (MHLW), Japan (2014). Drug interaction guideline for drug development and labeling recommendations
#' European Medicines Agency (2013). Guideline on the Investigation of Drug Interactions.
# 'University of Washington Metabolism and Transport Drug Interaction Database [Hachad et al. (2010), Hum Genomics, 5(1):61]
#'
#' @examples
#' \dontrun{
#'  drugs
#' }
"drugs"



#' Wild type CYP isoform sequences
#'
#' @source UniprotKB (2021).
#'
#' @format{
#' Named vector of CYP wild sequences
#' \describe{
#' \item{name}{Name of CYP isoform.}
#' \item{baseSeq}{aminoacid sequence of wild type isoform}
#' }
#'
#' @references
#' The UniProt Consortium
#' UniProt: the universal protein knowledgebase in 2021
#' Nucleic Acids Res. 49:D1 (2021)
#'
#'
#'
#' @examples
#' \dontrun{
#'  baseSeqs
#' }
"baseSeqs"
