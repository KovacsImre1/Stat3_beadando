#' Forint Package
#' @param x number
#' @return string
#' @export
#' @importFrom scales dollar
#' @examples
#'  forint(42)
#'

forint <- function(x) {
  dollar(x, prefix = "", suffix = " Ft", decimal.mark = ".", big.mark = " ")
}

#' Get UDS/HUF rate
#' @param retried number of times failed in the past
#' @return number
#' @export
#' @importFrom logger log_info
#' @importFrom httr GET content
#' @importFrom checkmate assert_number
#'
get_usdhuf <- function(retried = 0){
  tryCatch({
    response <- GET('https://api.exchangerate.host/latest?base=USD&symbols=HUF')
    usdhuf <- content(response)$rates$HUF
    assert_number(usdhuf, lower = 250, upper = 350)
    usdhuf
  }, error = function(e){
    log_error(e$message)
    Sys.sleep(1 + retried^2)
    get_usdhuf(retried = retried + 1)
  })
  log_info('1 USD currently costs {usdhuf} Hungarian Forints')
  return(usdhuf)
}


#'convert usd to huf
#' @param x number
#' @return number
#' @importFrom logger log_info
#' @export
convert_usd_to_huf <- function(x){
  y <- get_usdhuf()
  log_info('{x} USD currently equel to this amount of Hungarian Forints:')
  return(x*y)
}


#' Get HUF value from the forint() return string
#' @param x string
#' @return number
#' @export
#' @importFrom testthat expect_type
#' @importFrom formattable formattable
#' @examples
#' unhuf(-3,423.42 Ft)
#' -3423.42

unhuf <- function(x){
  if(grepl("[[:digit:]]",x)){
    formattable(as.numeric(gsub('[^-.0-9]*',"", x)), digits = 2, format = "f")
  }
}

