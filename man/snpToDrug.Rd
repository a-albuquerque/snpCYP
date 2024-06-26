% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snpToDrug.R
\name{snpToDrug}
\alias{snpToDrug}
\title{Outputting drugs in clinical use which metabolism is potentially disrupted
by a critical SNP at the provided CYP isoform. drugs.rda must be loaded.}
\usage{
snpToDrug(CYP)
}
\arguments{
\item{CYP}{CYP isoform among CYP1A2, CYP2B6, CYP2C8, CYP2C9,
CYP2C19, CYP2D6, CYP3A4}
}
\value{
List of drug which metabolism is potentially disrupted
         by a critical SNP at the provided CYP isoform
}
\description{
Outputting drugs in clinical use which metabolism is potentially disrupted
by a critical SNP at the provided CYP isoform. drugs.rda must be loaded.
}
\examples{
\dontrun{
load(file = "./data/drugs.rda")
snpToDrug("CYP2D6")
}

}
\references{
Ministry of Health, Labour and Welfare (MHLW), Japan (2014).
Drug interaction guideline for drug development and labeling recommendations
European Medicines Agency (2013). Guideline on the Investigation of Drug
Interactions.
}
