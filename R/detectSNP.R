#' Generating a graphic output of the number of nsSNPs for each of the CYPs
#' supported
#'
#' This function generates a graphic output of the number of nsSNPs in each
#' of the following CYP isoforms (if provided): CYP1A2, CYP2B6, CYP2C8,
#' CYP2C9, CYP2C19, CYP2D6, CYP3A4. baseSeqs.rda or a custom sequence
#' of wild isoforms must be loaded.
#'
#'
#'
#' @param CYPs list of CYPs to be analyzed, list of strings
#'
#' @param seqs Aminoacid sequences in FASTA format, list of strings
#'
#' @returns Visualization of the amount of snSNPs for each CYP analyzed
#'          Also return the SNPs positions for each CYP
#'
#' @examples
#' \dontrun{
#' load(file = "./data/baseSeqs.rda")
#' detectSNP(list("CYP1A2", "CYP2B6"),
#'list(paste("ASASQSVPFSATELLLASAIFCLVFWVLKGLRPRVPKGLKSPPEPWGWPLLGHVLTLGKN",
#'           "PHLALSRMSQRYGDVLQIRIGSTPVLVLSRLDTIRQALVRQGDDFKGRPDLYTSTLITDG",
#'           "QSLTFSTDSGPVWAARRRLAQNALNTFSIASDPASSSSCYLEEHVSKEAKALISRLQELM",
#'           "AGPGHFDPYNQVVVSVANVIGAMCFGQHFPESSDEMLSLVKNTHEFVETASSGNPLDFFP",
#'           "ILRYLPNPALQRFKAFNQRFLWFLQKTVQEHYQDFDKNSVRDITGALFKHSKKGPRASGN",
#'           "LIPQEKIVNLVNDIFGAGFDTVTTAISWSLMYLVTKPEIQRKIQKELDTVIGRERRPRLS",
#'           "DRPQLPYLEAFILETFRHSSFLPFTIPHSTTRDTTLNGFYIPKKCCVFVNQWQVNHDPEL",
#'           "WEDPSEFRPERFLTADGTAINKPLSEKMMLFGMGKRRCIGEVLAKWEIFLFLAILLQQLE",
#'           "FSVPPGVKVDLTPIYGLTMKHARCEHVQARLRFSIN", sep=""), paste("MEL",
#'           "SVLLFLALLTGLLLLLVQRHPNTHDRLPPGPRPLPLLGNLLQMDRRGLLKSFLRFRE",
#'           "KYGDVFTVHLGPRPVVMLCGVEAIREALVDKAEAFSGRGKIAMVDPFFRGYGVIFANGNR",
#'            "WKVLRRFSVTTMRDFGMGKRSVEERIQEEAQCLIEELRKSKGALMDPTFLFQSITANIIC",
#'            "SIVFGKRFHYQDQEFLKMLNLFYQTFSLISSVFGQLFELFSGFLKYFPGAHRQVYKNLQE",
#'           "INAYIGHSVEKHRETLDPSAPKDLIDTYLLHMEKEKSNAHSENAHQNLNLNTLSLFFAGT",
#'           "ETTSTTLRYGFLLMLKYPHVAERVYREIEQVIGPHRPPELHDRAKMPYTEAVIYEIQRFS",
#'           "DLLPMGVPHIVTQHTSFRGYIIPKDTEVFLILSTALHDPHYFEKPDAFNPDHFLDANGAL",
#'           "KKTEAFIPFSLGKRICLGEGIARAELFLFFTTILQNFSMASPVAPEDIDLTPQECGVGKI",
#'           "PPTYQIRFLPR", sep="")))
#' }
#'
#'
#' @export
#' @import stringr
#' @import assertthat
#' @import graphics


detectSNP <- function(CYPs, seqs) {


  if (!is.string(CYPs[[1]])){
    stop("CYP isoform must be provided as non empty list of strings")
  }

  if (!is.string(seqs[[1]])){
    stop("CYP sequences must be provided as list of strings")
  }

  counts <- vector()
  positions <- vector()


  for (i in 1:length(CYPs)) {

    seqVector <- unlist(strsplit(seqs[[i]], ""))


    wildForm <- baseSeqs[CYPs[[i]]]
    wildFormVector <- unlist(strsplit(wildForm, ""))



    count <- 0

    for (j in 1:length(wildFormVector)) {

      if (wildFormVector[[j]] != seqVector[[j]]) {
        count = count + 1
        positions <- append(positions, c(CYPs[[i]], toString(j)))
      }
    }

    counts <- append(counts, count)


  }

  barplot(counts, xlab="CYP isoform", ylab="Number of nsSNP",
          main="Distribution of nsSNP across sample isoforms", names.arg=CYPs, col="green")

  return (positions)

}
