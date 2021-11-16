
# snpCYP

## Description

The purpose of snpCYP is to streamline and automate the process of
reading and analyzing single nucleotide polymorphisms (SNP) on CYP genes
It is supposed to allow for easier exploratory analysis by graphically
outputting the nsSNP distribution and their potential disruptions on
drug metabolism by correlating the SNPs to known or predicted catalytic
activity from PolyPhen-2 and DIDB databases. The packet uniquely
improves current ability of researchers to automate their work flow by
adding non existing R language support for correlating sample’s
SNP’s/haplotypes to potential disruption on drug metabolism. This
project was developed using R 4.0.2 on MacOs 11.6.

## Installation

To download the package:

``` r
require("devtools")
devtools::install_github("a-albuquerque/snpCYP", build_vignettes = TRUE)
library("snpCYP")
```

To run the shinyApp: Under construction

## Overview

``` r
ls("package:snpCYP")
data(package="snpCYP")
```

Data sets available to the users are:

Functions available to the user are:

baseSeqs.rda drugs.rda

Please check this package’s vignette for detailed information on the
functions provided:

``` r
browseVignettes("snpCYP")
```

This package is structured according to the following tree:

``` r
- snpCYP
  |- snpCYP.Rproj
  |- DESCRIPTION
  |- NAMESPACE
  |- LICENSE
  |- README
  |- data
    |- baseSeqs.rda
    |- drugs.rda
  |- inst
    |- extdata
      |- README
  |- man
    |- figures
  |- R
    |- data.R
    |- detectSNP.R
    |- snpDist.R
    |- snpToDrug.R
    |- snpToPred.R
  |- vignettes
  |- tests
    |- testthat.R
    |- testthat
      |- test-test-detectSNP.R
      |- test-test-snpDist.R
      |- test-test-snpToDrug.R
      |- test-test-snpToPred.R
```

## Contributions

The author of this package is Alcides Albuquerque.

## References

Adzhubei, I., Jordan, D. M., & Sunyaev, S. R. (2013). Predicting
functional effect of human missense mutations using PolyPhen-2. Current
Protocols in Human Genetics, 76 (1), 7–20.

Banerjee, P., Dunkel, M., Kemmler, E., & Preissner, R. (2020).
SuperCYPsPred—a web server for the prediction of cytochrome activity.
Nucleic Acids Research, 48 (W1), W580–W585.

DIDB - the drug interaction database. (2021). In UW Drug Interaction
Solutions. Washinton University.
<https://www.druginteractionsolutions.org/solutions/drug-interaction-database/>

H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag
New York, 2016.

Rostkowski, M., Spjuth, O., & Rydberg, P. (2013). WhichCyp: Prediction
of cytochromes P450 inhibition. Bioinformatics, 29 (16), 2051–2052.

Wang, L.-L., Li, Y., & Zhou, S.-F. (2009). A bioinformatics approach for
the phenotype prediction of nonsynonymous single nucleotide
polymorphisms in human cytochromes P450. Drug Metabolism and
Disposition, 37 (5), 977–991.

Zhang, T., Zhou, Q., Pang, Y., Wang, Y., Jin, C., Huo, J., Liu, L. A., &
Wei, D. (2012). CYP-nsSNP: A specialized database focused on effect of
non-synonymous SNPs on function of CYPs. Interdisciplinary Sciences:
Computational Life Sciences, 4 (2), 83–89.

## Acknowledgements

This package was developed as part of an assessment for 2021 BCB410H:
Applied Bioinformatics, University of Toronto, Toronto, CANADA
