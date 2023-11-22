#' @title Call FDIC API
#'
#' @description Call the FDIC API
#'
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
#' @import httr2
#' @import dplyr
#' @importFrom purrr map pluck

#' @return A list of queries
#' @export

#' @examples
#' call_fdic_api(api = "financials", filters = "RSSDID: 37", fields = "RSSDID,REPDTE,ASSET,DEP", limit = 10)

call_fdic_api <- function(
    api = NULL,
    filters = NULL,
    search = NULL,
    fields = NULL,
    sort_by = NULL,
    sort_order = NULL,
    limit = 0,
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

    queries <- match.call()[-(1)] %>%
        as.list()

    if(limit == 0) {
        queries$limit <- 10000
    } else {
        if(limit > 10000) {
            queries$limit <- 10000
        } else {
            queries$limit <- limit
        }
    }


    req <- request("https://banks.data.fdic.gov/api/") %>%
        req_headers(Accept = "application/json") %>%
        req_url_path_append(api) %>%
        req_url_query(!!!queries)

    # perform the initial request and get the total records to be broken
    # into chunks
    resp_1 <- req %>%
        req_perform() %>%
        resp_body_json()


    if(limit == 0) {
        total_records <- resp_1$totals$count
    } else {
        if (limit > 10000) {
            total_records <- min(limit, resp_1$totals$count)
            } else {
                total_records <- limit
            }
    }



    # check to see if the total_records are over 10,000; if so then use the
    # iterate_with_offset function to create a list of queries to be performed
    # and then perform them
    if (total_records > 10000) {
        # create a list of offsets to be used in the query
        queries$limit <- 10000

        offsets <- 1:ceiling(total_records / 10000)

        # create a list of queries to be performed
        resp <-req_perform_iterative(
                 req,
                 next_req = iterate_with_offset("offset", start = 0,
                                                offset = 1),
                 max_reqs = ceiling(total_records / 10000)
                )

        # df <- resps %>%
        #     map(~.x %>% resp_body_json() %>%
        #             pluck("data")) %>%
        #     map(~.x %>% map(~.x$data) %>% bind_rows()) %>%
        #     bind_rows()

    } else {
        resp <- resp_1

        # df <- resp %>%
        #     pluck("data") %>%
        #     map(~.x$data) %>%
        #     bind_rows()
    }

    return(resp)
}

