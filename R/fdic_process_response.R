#' Convert FDIC Response to Dataframe
#'
#' Function to convert response from call_fdic_api() to a dataframe
#'
#' @param resp response from call_fdic_api()
#' @param chunked boolean; if TRUE, then the response is chunked due to having
#' greater than 10,000 records
#' @import httr2
#' @import dplyr
#' @importFrom purrr map pluck bind_rows
#'
#' @return
#' @export
#'
#' @examples
fdic_process_response <- function(resp, chunked = TRUE) {
  if(chunked) {
    df <- resp %>%
        map(~.x %>% resp_body_json() %>%
                pluck("data")) %>%
        map(~.x %>% map(~.x$data) %>% bind_rows()) %>%
        bind_rows()
  } else {
    df <- resp %>%
        pluck("data") %>%
        map(~.x$data) %>%
        bind_rows()
  }
}





