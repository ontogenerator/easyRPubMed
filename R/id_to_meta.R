#' @title Extract Data from a Vector of Records
#'
#' @description Extract publication-specific information from a vector of
#' DOIs or PMIDs. The input records are given as a character-class vector.
#' Data are returned as a data frame where each row corresponds 
#' to a single record or to a single author per record.
#' All records are assumed to be of the same type, either DOI or PMID.
#'
#' @usage id_to_meta(ids, max_chars = -1, get_authors = FALSE,
#' get_keywords = FALSE, api_key = NULL)
#'                     
#' @param ids Vector including one or more DOIs or PMIDs.
#' @param max_chars Numeric (integer). Maximum number of characters to be
#' extracted from the Article Abstract field. Set max_chars to -1 for extracting
#' the full-length abstract. Set max_chars to 0 to extract no abstract.
#' @param get_keywords Logical. If TRUE, an attempt to extract article Keywords
#' will be made.
#' @param get_authors Logical. If FALSE, author information won't be extracted.
#' This will considerably speed up the operation and the output will have a
#' single row per record. Otherwise the output will have a single row per author
#' per record.
#' @param api_key String (character vector of length 1): user-specific API key
#' to increase the limit of queries per second. You can obtain your key from
#' NCBI.
#'
#' @details 
#' For each Pubmed Article record, this function will automatically extract a
#' set of features. Extracted information include: PMID, DOI, article title,
#' article abstract, publication date (year, month, day), publication type,
#' journal name (title, abbreviation), and ISSN.
#'
#' @return Data frame including the extracted features. Each row corresponds to
#' a different entry from the input vector.
#'
#' @author Vladislav Nachev \email{vladislav.nachev@@charite.de}
#'
#' @examples 
#' id_to_meta("10.1001/jamaoncol.2020.0574")
#' 
#' pmids <- c("31200845", "30580006", "31100250")
#' 
#' id_to_meta(pmids)
#' @importFrom stringr str_detect
#' @importFrom dplyr select mutate
#'
#' @export
id_to_meta <- function(ids, max_chars = -1, get_authors = FALSE,
                       get_keywords = FALSE, api_key = NULL) {

  keywords <- lastname <- firstname <- address <- email <-
    pmid <- year <- day <- NULL
  
  if (stringr::str_detect(ids[1], "^10\\.")) {
    id_type = "[DOI]"
  } else {
    id_type = "[UID]"
  }
  
  searchQuery <- ids |>
    paste0(id_type) |>
    purrr::reduce(paste, sep = " OR ")
  
  search_by_id <- get_pubmed_ids(searchQuery, api_key = api_key)
  
  results <- fetch_pubmed_data(search_by_id) |>
    articles_to_list() |>
    purrr::map(\(pubmedArticle) article_to_df(pubmedArticle,
                                              max_chars = max_chars,
                                              getKeywords = get_keywords,
                                              getAuthors = get_authors)) |>
    purrr::list_rbind() 
  
  if (get_keywords == FALSE) {
    results <- results |> 
      dplyr::select(-keywords)
  } 
  
  if (get_authors == FALSE) {
    results <- results |> 
      dplyr::select(-lastname, -firstname, -address, -email)
  } 
  
  results |>
    dplyr::mutate(pmid = as.numeric(pmid),
                  year = as.numeric(year),
                  day = as.numeric(day))
}

#' @title Fetch and Extract Data Batchwise from a Data Frame Containing DOIs or
#' PMIDs
#'
#' @description Extract publication-specific information from a vector of DOIs
#' or PMIDs. The input records are given as a character-class vector.
#' Data are returned as a data frame where each row corresponds 
#' to a single record or to a single author per record.
#' All records are assumed to be of the same type, either DOI or PMID.
#'
#' @usage get_metadata(tib, idcol, chunksize = 200, max_chars = -1,
#' get_keywords = FALSE, get_authors = FALSE, api_key = NULL)
#'                     
#' @param tib Data frame containing source data.
#' @param idcol Name of column inside data frame including one or more DOIs or
#' PMIDs.
#' @param chunksize Size of the chunks (number of articles) to be fetched in
#' each query. As the query string has an upper limit in its size, the currently
#' allowed maximum chunk size is set to 200. 
#' @param max_chars Numeric (integer). Maximum number of characters to be
#' extracted from the Article Abstract field. Set max_chars to -1 for extracting
#' the full-length abstract. Set max_chars to 0 to extract no abstract.
#' @param get_keywords Logical. If TRUE, an attempt to extract article Keywords
#' will be made.
#' @param get_authors Logical. If FALSE, author information won't be extracted.
#' This will considerably speed up the operation and the output will have a
#' single row per record. Otherwise the output will have a single row per author
#' per record.
#' @param api_key String (character vector of length 1): user-specific API key
#' to increase the limit of queries per second. You can obtain your key from
#' NCBI.
#' @details 
#' For each DOI or PMID in the source data frame, this function will
#' automatically fetch and extract a set of features. Extracted information
#' include: PMID, DOI, article title, article abstract,
#' publication date (year, month, day),  publication type, 
#' journal name (title, abbreviation), and ISSN.
#' @return A list of data frames including the extracted features.
#' Each element but the last has row numbers equal to the chunksize.
#' @author Vladislav Nachev \email{vladislav.nachev@@charite.de}
#' @examples
#' \dontrun{ 
#'   # load libraries
#'   library(tidyverse)
#'   data("EPMsamples")
#'   #Get records
#'   BL_list <- EPMsamples$NUBL_1618$rec_lst
#'   #remove metadata on purpose for the example
#'   BL_df <- BL_list |> 
#'     map(\(article) easyRPubMed::article_to_df(article,
#'      max_chars = 0, getAuthors = FALSE), .progress = TRUE) |> 
#'     list_rbind() |> 
#'     select(pmid, doi)
#'   # start batchwise process with a progressbar
#'     res <- BL_df |> 
#'     get_metadata(pmid, chunksize = 100, api_key = NULL)
#'
#' }
#' @importFrom assertthat assert_that
#' @import dplyr purrr
#' 
#' @export
get_metadata <- function(tib, idcol, chunksize = 200, max_chars = -1,
                         get_keywords = FALSE, get_authors = FALSE,
                         api_key = NULL) {
  
  assertthat::assert_that(chunksize <= 200,
                          msg = "Chunksize should not exeed 200!")
  
  # using pmid only and adding doi, etc.
  if (nrow(tib) > chunksize) {
    tib <- tib |> 
      dplyr::mutate(chunk = cumsum(dplyr::if_else(
        dplyr::row_number() %% chunksize == 0, 1, 0)))
    
    n_chunks <- ceiling(nrow(tib) / chunksize)
    
    ls_ids <- split(tib, f = tib$chunk) |> 
      purrr::map(\(ch) dplyr::pull(ch, {{ idcol }}))

    return(
      purrr::map(ls_ids, \(chunk) id_to_meta(chunk,
                                             max_chars = max_chars,
                                             api_key = api_key),
                 .progress = TRUE)
    )
  } else {
    return(id_to_meta(tib |> dplyr::pull({{ idcol }}),
                      max_chars = max_chars, get_authors = get_authors,
                      get_keywords = get_keywords, api_key = api_key))
    
  } 
  
}

#' @title Collapse metadata table with author information to a single row per
#' author
#'
#' @description Converts a data frame of publication records from a single row
#' per author to a single row per record. Author information is collapsed to a
#' single cell, with a given separator.
#'
#' @usage collapse_authors(tib, sep_author = "; ", sep_address = "|")
#'                     
#' @param tib Data frame. Publication records, typically the
#' output of `id_to_meta`.
#' @param sep_author Character. The separator to use between different authors.
#' @param sep_address Character. The separator to use between different author
#' addresses.
#'
#' @return Data frame where each row corresponds to a different record.
#'
#' @author Vladislav Nachev \email{vladislav.nachev@@charite.de}
#'
#' @examples
#' \dontrun{ 
#' collapse_authors(tib, sep_author = " and ", sep_address = "|")
#' }
#' @import dplyr tidyr
#' 
#' @export
collapse_authors <- function(tib, sep_author = "; ", sep_address = "|") {
  
  pmid <- doi <- lastname <- firstname <- author <- address <- NULL
  
  tib |> 
    dplyr::group_by(pmid, doi) |> 
    tidyr::unite("author", c(lastname, firstname), sep = ", ") |> 
    dplyr::mutate(author = paste(author, collapse = sep_author),
                  address = paste(address, collapse = sep_address)) |> 
    dplyr::ungroup() |> 
    dplyr::distinct(pmid, doi, .keep_all = TRUE)
}