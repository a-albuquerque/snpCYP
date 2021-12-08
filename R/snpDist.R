#' Generating a graphic output of the frequency of nsSNPs for each region
#' of the provided CYP isoform protein.
#'
#'
#' This function generates a graphic output of the frequency of nsSNPs for
#' each sequential group of 50 aminoacids at the CYP isoform provided.
#' baseSeqs.rda or a custom sequence of wild isoforms must be loaded.
#'
#'
#'
#'
#' @param CYP CYP isoform to be analyzed, string
#'
#' @param seq Aminoacid sequence (one-letter code) for the CYP isoform to be
#' analyzed (list of strings)
#'
#' @returns Visualization of the frequency of nsSNPs for each protein region
#'          Also return the SNPs positions for each region
#'
#' @examples
#' \dontrun{
#' load(file = "./data/baseSeqs.rda")
#' snpDist("CYP1A2",
#'        paste("AALSQSVPFSATELLLASAIFCLVFWVLKGLRPRVPKGLKSPPEPWGWPLLGHVLTLGKN",
#'              "LALALSRMSQRYGDVLQIRIGSTPVLVLSRLDTIRQALVRQGDDFKGRPDLYTSTLITDG",
#'              "TFSTFSTDSGPVWAARRRLAQNALNTFSIASDPASSSSCYLEEHVSKEAKALISRLQELM",
#'              "GHFGHFDPYNQVVVSVANVIGAMCFGQHFPESSDEMLSLVKNTHEFVETASSGNPLDFFP",
#'              "PNPALPNPALQRFKAFNQRFLWFLQKTVQEHYQKNSKNSVRDITGALFKHSKKGPRASGN",
#'              "PQPQEKIVNLVNDIFGAGFDTVTTAISWSLMYLVTKPEIQRKIQKELDTVIGRERRPRLS",
#'              "PQPQLPYLEAFILETFRHSSFLPFTIPHSTTRDTTLNGFYIPKKCCVFVNQWQVNHDPEL",
#'              "EEDPSEFRPERFLTADGTAINKPLSEKMMLFGMGKRRCIGEVLAKWEIFLFLAILLQQLE",
#'              "SSVPPGVKVDLTPIYGLTMKHARCEHVQARLRFSIN", sep=""))
#'  }
#'
#'
#' @export
#' @import stringr
#' @import assertthat
#' @import graphics

snpDist <- function(CYP, seq) {


  if (!is.string(CYP)){
    stop("CYP isoform must be provided as non empty string")
  }

  if (!is.string(seq)){
    stop("CYP sequences must be provided as a string")
  }

  # Marking number of SNPs
  counts <- vector()
  # Marking each section of 50 aan
  sections <- vector()

  # Processing mutated and wild-type sequence for the isoform to be analyzed
  seqVector <- unlist(strsplit(seq, ""))
  wildForm <- baseSeqs[CYP]
  wildFormVector <- unlist(strsplit(wildForm, ""))



  # Iterating through each section
  for (i in 1:(length(wildFormVector)/50)) {
    count <- 0
    for (j in 1:50) {

      # Guarantees that last section will be properly processed
      if (((50*(i-1))+j)>length(wildFormVector)) {break}

      # For each aa in the current section, compare it with corresponding
      # wild-type
      if (wildFormVector[[(50*(i-1))+j]] != seqVector[[(50*(i-1))+j]]) {
        count = count + 1
      }
    }
    counts <- append(counts, count)
    sections <- append(sections, toString((i-1)*50))
  }

  # Plotting distribution of SNPs across all sections of the CYP protein
  # analized, as compared to its wild type
  graphics::barplot(counts, xlab="Protein Section", ylab="Number of nsSNP",
          main="Distribution of nsSNP across sequence", names.arg=sections, col="red")



  names(counts) <- sections

  # Return number of SNPs at each 50-aa section of the mutated CYP analyzed
  return (counts)

}

# [END]
