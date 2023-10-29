#' @title Call FDIC API
#' @description Call the FDIC API
#' @param api The API to call
#' @param filters A list of filters to apply to the query
#' @param search A list of search terms to apply to the query
#' @param fields A list of fields to return
#' @param sort_by A list of fields to sort by
#' @param sort_order A list of sort orders
#' @param limit The number of records to return
#' @param offset The number of records to skip
#' @param agg_by A list of fields to aggregate by
#' @param agg_term_fields A list of fields to aggregate terms by
#' @param format The format to return the data in
#' @param download Whether to download the data
#' @param filename The filename to download the data to
#' @import httr2 dplyr

#' @return A list of queries
#' @export

#' @examples
#' get_all_fdic_data(api = "financials", filters = "RSSDID: 37", fields = "RSSDID,REPDTE,ASSET,DEP", limit = 10)
get_all_fdic_data <- function(
    api = NULL,
    filters = NULL,
    search = NULL,
    fields = NULL,
    sort_by = NULL,
    sort_order = NULL,
    limit = NULL,
    offset = NULL,
    agg_by = NULL,
    agg_term_fields = NULL,
    format = NULL,
    download = NULL,
    filename = NULL) {
    # get a list of queries in the current call


    # generate query strings by mapping the query list onto create_query
    # (1:2) are subsetted out of the call list as 1 is the function name and
    # 2 is the first parameter (always API)


    base_url <- "https://banks.data.fdic.gov/api/"

    queries <- match.call()[-(1:2)] %>%
        as.list()

    req <- request("https://banks.data.fdic.gov/api/") %>%
        req_headers(Accept = "application/json") %>%
        req_url_path_append(api) %>%
        req_url_query(!!!queries)

    resp <- req %>%
        req_perform()

    return(resp)
}
