#' Convert FDIC Response to Dataframe
#'
#' Function to convert response from call_fdic_api() to a dataframe
#'
#' @param fdic_resp response from call_fdic_api()
#' @import httr2
#' @importFrom tibble as_tibble_col
#' @importFrom tidyr unnest_wider hoist
#' @importFrom magrittr %>%
#' @importFrom purrr pluck
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' resp <- call_fdic_api(api = "financials", filters = "RSSDID: 37", fields = "RSSDID,REPDTE,ASSET,DEP", limit = 10)

fdic_process_response <- function(fdic_resp) {
  # define response function for processing single response
  resp_function <- function(resp) {
    resp_data <- resp %>%
      resp_body_json() %>%
      pluck("data") %>%
      as_tibble_col(column_name = "resp_data") %>%
      hoist(resp_data,
            data = "data") %>%
      unnest_wider(data)

    return(resp_data)
  }

  # process response with with resps_data
  resps_data <- fdic_resp %>%
    resps_data(\(resp) resp_function(resp))


}

