#' @title Extract Data from a Vector of Records
#'
#' @description Extract publication-specific information from a vector of DOIs or PMIDs. 
#' The input records are given as a character-class vector.
#' Data are returned as a data frame where each row corresponds 
#' to a single record. All records are assumed to be of the same type, either DOI or PMID.
#'
#' @usage id_to_meta(ids, api_key = NULL)
#'                     
#' @param ids Vector including one or more DOIs or PMIDs.
#' @param api_key String (character vector of length 1): user-specific API key to increase 
#' the limit of queries per second. You can obtain your key from NCBI.

#'
#' @details 
#' For each Pubmed Article record, this function will automatically extract a set of features. 
#' Extracted information include: PMID, DOI, article title, article abstract, publication date (year, month, day), 
#' publication type, journal name (title, abbreviation), and ISSN.
#'
#' @return Data frame including the extracted features. Each row corresponds to a different entry from the input vector.
#'
#' @author Vladislav Nachev \email{vladislav.nachev@@charite.de}
#'
#' @examples 
#' id_to_meta("10.1001/jamaoncol.2020.0574")
#' 
#' pmids <- c("31200845", "30580006", "31100250")
#' 
#' id_to_meta(pmids)
#'
#' @export

id_to_meta <- function(ids, api_key = NULL) {
  
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
    purrr::map(\(pubmedArticle) article_to_df(pubmedArticle, max_chars = 50, getKeywords = FALSE,
                                           getAuthors = FALSE)) |>
    purrr::list_rbind() |> 
    dplyr::select(-keywords, -lastname, -firstname, -address, -email)
    
  
  results |>
    dplyr::mutate(pmid = as.numeric(pmid),
                  year = as.numeric(year),
                  day = as.numeric(day))
}

#' @title Fetch and Extract Data Batchwise from a Data Frame Containing DOIs or PMIDs
#'
#' @description Extract publication-specific information from a vector of DOIs or PMIDs. 
#' The input records are given as a character-class vector.
#' Data are returned as a data frame where each row corresponds 
#' to a single record. All records are assumed to be of the same type, either DOI or PMID.
#'
#' @usage get_metadata(tib, idcol, chunksize = 200, api_key = NULL)
#'                     
#' @param tib Data frame containing source data.
#' @param idcol Name of column inside data frame including one or more DOIs or PMIDs.
#' @param chunksize Size of the chunks (number of articles) to be fetched in each query. As the
#' query string has an upper limit in its size, the currently allowed maximum chunk size is set to 200.
#' @param api_key String (character vector of length 1): user-specific API key to increase 
#' the limit of queries per second. You can obtain your key from NCBI.

#'
#' @details 
#' For each DOI or PMID in the source data frame, this function will automatically fetch and extract a set of features. 
#' Extracted information include: PMID, DOI, article title, article abstract, publication date (year, month, day), 
#' publication type, journal name (title, abbreviation), and ISSN.
#'
#' @return A list of data frames including the extracted features. Each element but the last has row numbers equal
#' to the chunksize.
#'
#' @author Vladislav Nachev \email{vladislav.nachev@@charite.de}
#'
#' @examples
#' \dontrun{ 
#'   # load libraries
#'   library(tidyverse)
#'   library(progressr) # for progress bar
#'   data("EPMsamples")
#'   #Get records
#'   BL_list <- EPMsamples$NUBL_1618$rec_lst
#'   #remove metadata on purpose for the example
#'   BL_df <- BL_list |> 
#'     map(\(article) easyRPubMed::article_to_df(article, max_chars = 0, getAuthors = FALSE)) |> 
#'     list_rbind() |> 
#'     select(pmid, doi)
#'   # start batchwise process with a progressbar
#'   with_progress({
#'     res <- BL_df |> 
#'     get_metadata(pmid, chunksize = 100, api_key = NULL)
#'   })
#'
#' }
#' @export

get_metadata <- function(tib, idcol, chunksize = 200, api_key = NULL) {
  
  assertthat::assert_that(chunksize <= 200, msg = "Chunksize should not exeed 200!")
  
  # using pmid only and adding doi, etc.
  if (nrow(tib) > chunksize) {
    tib <- tib |> 
      dplyr::mutate(chunk = cumsum(dplyr::if_else(dplyr::row_number() %% chunksize == 0, 1, 0)))
    
    n_chunks <- ceiling(nrow(tib) / chunksize)
    
    ls_ids <- split(tib, f = tib$chunk) |> 
      purrr::map(\(ch) dplyr::pull(ch, {{ idcol }}))
    p <- progressr::progressor(steps = n_chunks)

    return(
      purrr::map(ls_ids, ~{
        p()
        # Sys.sleep(1)
        id_to_meta(., api_key = api_key)
      }, .progress = TRUE)
    )
  } else {
    return(id_to_meta(tib |> dplyr::pull({{ idcol }}), api_key = api_key))
    
  } 
  
}