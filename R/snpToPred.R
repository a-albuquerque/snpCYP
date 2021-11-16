# Still in construction, Server currently offline

#' Generating a prediction of the potential phenotypical damage a snp may cause
#'
#' This function generates a probabilistic prediction of damage by probing
#' Polyphen-2 server engine and databases, which in turn perform analysis
#' on the sequence, phylogenetic and structural information related to the
#' provided substitution.
#'
#' The function also returns the false positive rate and true positive rate
#' associated with the prediction
#'
#'
#' @param prot UniProtKB identifier of the CYP to be analyzed, string
#'
#' @param pos Position of the nucleotide polymorphism, string
#'
#' @param aa1 Original (or wild type) aminoacid, single letter code, char
#'
#' @param aa2 Mutant aminoacid, single letter code, char
#'
#' @returns Probability of the provided mutation to cause phenotypical
#' enzymatic damage.
#'
#' @examples
#' \dontrun{
#' snpToPred("O95479", "453", "R", "Q")
#' }
#'
#' @references
#' Adzhubei, I., Jordan, D. M., & Sunyaev, S. R. (2013).
#' Predicting functional effect of human missense mutations using PolyPhen-2.
#' Current Protocols in Human Genetics, 76 (1), 7–20.
#'
#' @export

snpToPred <- function(prot, pos, aa1, aa2) {


  if (!is.character(aa1)){
    stop("Original aminoacid not a valid single-letter code")
  }

  if (!is.character(aa2)){
    stop("Mutated aminoacid not a valid single-letter code")
  }

  fileConn<-file("singleSNP.txt")
  writeLines(c(paste(prot, pos, aa1, aa2)), fileConn)
  close(fileConn)

  x <- HttpClient$new(url = "http://genetics.bwh.harvard.edu")

  res <- x$post(path="ggi/cgi-bin/ggi2.cgi",
                encode = "form",
                body = c(
                  `_ggi_project`='PPHWeb2',
                  `_ggi_origin`='query',
                  `_ggi_target_pipeline`='1',
                  MODELNAME='HumDiv',
                  UCSCDB='hg19',
                  SNPFUNC='m',
                  `ggi_batch_file`=upload_file("singleSNP.txt")))

  cat(rawToChar(res$content))

  return (TRUE)

  # Server currently offline
}

