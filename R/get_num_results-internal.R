#' get_num_results
#'
#' @description Find out how many results are available for the search
#'
#' @param query The URL specifying the query
#'
#' @return
#'
#' @import magrittr
#' @keywords internal
#' @export
#'
get_num_results <- function(query)
{
  ## The raw query
  raw_query <- xml2::read_html(query)

  ## Get the total result count and convert to numeric
  tot_results <- raw_query %>%
    rvest::html_nodes("span.totalcount") %>%
    extract(1) %>%
    rvest::html_text() %>%
    as.numeric()

  if(length(tot_results) == 0) tot_results = 0

  return(tot_results)
}