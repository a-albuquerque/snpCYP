#' Generating a graphic output of the number of nsSNPs for each of the CYPs
#' supported
#'
#' This function generates a graphic output of the number of nsSNPs in each
#' of the following CYP isoforms (if provided): CYP1A2, CYP2B6, CYP2C8,
#' CYP2C9, CYP2C19, CYP2D6, CYP3A4
#'
#'
#'
#' @param CYPs list of CYPs to be analyzed, list of strings
#'
#' @param seqs Aminoacid sequences in FASTA format, list of strings
#'
#' @returns Visualization of the amount of snSNPs for each CYP analyzed
#'
#' @examples
#' \dontrun{
#' detectSNP(list("CYP1A2", "CYP2B6"),
#'list(paste("MALSQSVPFSATELLLASAIFCLVFWVLKGLRPRVPKGLKSPPEPWGWPLLGHVLTLGKN",
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
#'           "WKVLRRFSVTTMRDFGMGKRSVEERIQEEAQCLIEELRKSKGALMDPTFLFQSITANIIC",
#'           "SIVFGKRFHYQDQEFLKMLNLFYQTFSLISSVFGQLFELFSGFLKYFPGAHRQVYKNLQE",
#'           "INAYIGHSVEKHRETLDPSAPKDLIDTYLLHMEKEKSNAHSEFSHQNLNLNTLSLFFAGT",
#'           "ETTSTTLRYGFLLMLKYPHVAERVYREIEQVIGPHRPPELHDRAKMPYTEAVIYEIQRFS",
#'           "DLLPMGVPHIVTQHTSFRGYIIPKDTEVFLILSTALHDPHYFEKPDAFNPDHFLDANGAL",
#'           "KKTEAFIPFSLGKRICLGEGIARAELFLFFTTILQNFSMASPVAPEDIDLTPQECGVGKI",
#'           "PPTYQIRFLPR", sep="")))
#' }
#'
#'
#' @export
#' @import stringr

detectSNP <- function(CYPs, seqs) {

  library(assertthat)

  if (!is.string(CYPs[[1]])){
    stop("CYP isoform must be provided as non empty list of strings")
  }

  if (!is.string(seqs[[1]])){
    stop("CYP sequences must be provided as list of strings")
  }

  load(file = "./data/baseSeqs.rda")

  counts <- vector()

  for (i in 1:length(CYPs)) {

    seqVector <- unlist(strsplit(seqs[[i]], ""))


    wildForm <- baseSeqs[CYPs[[i]]]
    wildFormVector <- unlist(strsplit(wildForm, ""))

    count <- 0

    for (j in 1:length(wildFormVector)) {
      #print(wildFormVector[[j]])
      #print(seqVector[j])
      if (wildFormVector[[j]] != seqVector[[j]]) {
        count = count + 1
      }
    }

    counts <- append(counts, count)


    #print(CYPs[[i]])
    #print(wildFormVector)
    #print(seqVector)

  }


  return (counts)

}
