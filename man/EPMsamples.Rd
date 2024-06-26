% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyRPubMed_scr.R
\docType{data}
\name{EPMsamples}
\alias{EPMsamples}
\title{PubMed Records downloaded and analyzed via easyPubMed}
\format{
The dataset is formatted as a list including 4 elements: 

* `DF_papers_abs`: List of 4

* `DF_papers_std`: List of 4

* `NUBL_dw18`: List of 3

* `NUBL_1618`: List of 5
}
\usage{
data("EPMsamples")
}
\description{
This dataset includes a collection of 4 examples showing how to download and analyze records 
from PubMed by using easyPubMed. Each element in the EPMsamples list corresponds to a different query 
and/or analysis. Also, each element of EPMsamples is a list including intermediates and notes about the analysis.
}
\details{
The dataset was built as described in this vignette: \url{https://www.data-pulse.com/projects/Rlibs/vignettes/building_the_easyPubMed_EPMsamples_dataset.html}
}
\examples{
## Display some contents
data("EPMsamples")
# The following examples are focused on example query #4 (i.e., NUBL_1618)
# Display Query String used for collecting the data
print(EPMsamples$NUBL_1618$qry_st)
# show one PubMed record element from the IL vector
NU_records <- EPMsamples$NUBL_1618$rec_lst
cat(NU_records[[1]])
# cast PM recort to data.frame
BL_df <- article_to_df(NU_records[[6]], max_chars = 0)
print(BL_df)
}
\keyword{datasets}
