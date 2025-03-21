% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/id_to_meta.R
\name{id_to_meta}
\alias{id_to_meta}
\title{Extract Data from a Vector of Records}
\usage{
id_to_meta(ids, max_chars = -1, get_authors = FALSE,
get_keywords = FALSE, api_key = NULL)
}
\arguments{
\item{ids}{Vector including one or more DOIs or PMIDs.}

\item{max_chars}{Numeric (integer). Maximum number of characters to be
extracted from the Article Abstract field. Set max_chars to -1 for extracting
the full-length abstract. Set max_chars to 0 to extract no abstract.}

\item{get_authors}{Logical. If FALSE, author information won't be extracted.
This will considerably speed up the operation and the output will have a
single row per record. Otherwise the output will have a single row per author
per record.}

\item{get_keywords}{Logical. If TRUE, an attempt to extract article Keywords
will be made.}

\item{api_key}{String (character vector of length 1): user-specific API key
to increase the limit of queries per second. You can obtain your key from
NCBI.}
}
\value{
Data frame including the extracted features. Each row corresponds to
a different entry from the input vector.
}
\description{
Extract publication-specific information from a vector of
DOIs or PMIDs. The input records are given as a character-class vector.
Data are returned as a data frame where each row corresponds 
to a single record or to a single author per record.
All records are assumed to be of the same type, either DOI or PMID.
}
\details{
For each Pubmed Article record, this function will automatically extract a
set of features. Extracted information include: PMID, DOI, article title,
article abstract, publication date (year, month, day), publication type,
journal name (title, abbreviation), and ISSN.
}
\examples{
id_to_meta("10.1001/jamaoncol.2020.0574")

pmids <- c("31200845", "30580006", "31100250")

id_to_meta(pmids)
}
\author{
Vladislav Nachev \email{vladislav.nachev@charite.de}
}
