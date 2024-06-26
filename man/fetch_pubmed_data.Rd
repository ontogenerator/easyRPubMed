% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyRPubMed_scr.R
\name{fetch_pubmed_data}
\alias{fetch_pubmed_data}
\title{Retrieve PubMed Data in XML or TXT Format}
\usage{
fetch_pubmed_data(pubmed_id_list, 
                         retstart = 0, 
                         retmax = 500, 
                         format = "xml", 
                         encoding = "UTF8")
}
\arguments{
\item{pubmed_id_list}{List: the result of a get_pubmed_ids() call.}

\item{retstart}{Integer (>=0): index of the first UID in the retrieved PubMed Search Result set 
to be included in the output (default=0, corresponding to the first record of the entire set).}

\item{retmax}{Integer (>=1): size of the batch of PubMed records to be retrieved at one time.}

\item{format}{Character: element specifying the output format. The following values are allowed: 
c("asn.1", "xml", "medline", "uilist", "abstract").}

\item{encoding}{The encoding of an input/output connection can be specified by name 
(for example, "ASCII", or "UTF-8", in the same way as it would be given to the function base::iconv(). 
See iconv() help page for how to find out more about encodings that can be used on your platform. 
Here, we recommend using "UTF-8".}
}
\value{
An object (vector) of class "character". If format is set to "xml" (default), a single String including all 
PubMed records (with XML tags embedded) is returned. If a different format is selected, a vector of strings 
is returned, where each row corresponds to a line of the output document.
}
\description{
Retrieve PubMed records from Entrez following a search performed via the 
get_pubmed_ids() function. Data are downloaded in the XML or TXT format and are 
retrieved in batches of up to 5000 records.
}
\details{
Retrieve PubMed records based on the results of a get_pubmed_ids() query. 
Records are retrieved from Entrez via the PubMed API efetch function. The first entry to be retrieved 
may be adjusted via the retastart parameter (this allows the user to download large batches of PubMed 
data in multiple runs). The maximum number of entries to be retrieved can also be set adjusting the 
retmax parameter (1 < retmax < 5000). Data will be downloaded on the fly (no files are saved 
locally).
}
\examples{
try({ 
  ## Example 01: retrieve data in TXT format
  library("easyPubMed")
  dami_query_string <- "Damiano Fantini[AU] AND 2018[PDAT]"
  dami_on_pubmed <- get_pubmed_ids(dami_query_string)
  Sys.sleep(1) # avoid server timeout
  dami_papers <- fetch_pubmed_data(dami_on_pubmed, format = "abstract")
  dami_papers[dami_papers == ""] <- "\n"
  cat(paste(dami_papers[1:65], collapse = ""))
}, silent = TRUE)

\dontrun{
## Example 02: retrieve data in XML format
library("easyPubMed")
dami_query_string <- "Damiano Fantini[AU]"
dami_on_pubmed <- get_pubmed_ids(dami_query_string)
dami_papers <- fetch_pubmed_data(dami_on_pubmed)
titles <- custom_grep(dami_papers, "ArticleTitle", "char")
print(titles)
}

}
\references{
\url{https://www.data-pulse.com/dev_site/easypubmed/}
\url{https://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.T._valid_values_of__retmode_and/}
}
\author{
Damiano Fantini \email{damiano.fantini@gmail.com}
}
