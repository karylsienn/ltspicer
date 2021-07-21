suffix_multiplier <- list(
  "p" = 1e-12,
  "n" = 1e-9,
  "u" = 1e-6,
  "m" = 1e-3,
  "K" = 1e3,
  "MEG" = 1e6,
  "G" = 1e9
)


#' Parse number with appropriate suffix
#'
#' @param num_with_suffix string
#'
#' @return double, appropriate number
#' @export
#'
#' @importFrom readr parse_number
#' @importFrom stringr str_trim str_extract str_to_upper str_to_lower
#'
#' @examples
#' parse_number_suffix('45     u')
#' parse_number_suffix('45Megkfdsaf')
#' parse_number_suffix('4.623M') == parse_number_suffix('4.623m')
parse_number_suffix <- function(num_with_suffix) {
  # Parse the numeric fragment
  number <- parse_number(num_with_suffix)
  suffixes <- "^(p|n|u|m|K|MEG|G)"
  # Check if there is any suffix
  # Match string preceded by a number
  suffix <- str_extract(num_with_suffix, '(?<=[0-9])(\\s*[A-Za-z]+)') # TODO: Check if there is no better way.
  if(!is.na(suffix)) {
    suffix <- str_trim(suffix) # Delete the white space
    large_suffix <- str_extract(str_to_upper(suffix), suffixes) # Try with a large suffix
    if(!is.na(large_suffix)) return(number * suffix_multiplier[[large_suffix]])

    small_suffix <- str_extract(str_to_lower(suffix), suffixes) # If not try with a small suffix
    if(!is.na(small_suffix)) return(number * suffix_multiplier[[small_suffix]])

    # No other options
    warning("Suffix was not valid. Returning a number without multiplication.")
  }
  return(number)
}



#' Parse LTspice netlist
#'
#' A one-liner to parse the netlist given the path to it.
#' @param netlist_path string, path to ltspice netlist
#'
#' @return string vector with parse lines of the netlist
#' @export
#'
#' @importFrom vroom vroom_lines locale
#'
#' @examples
#' netlist <- parse_netlist('spice_files/2m_cable_model.net')
#' print(netlist)
#'
parse_netlist <- function(netlist_path) {
  vroom_lines(netlist_path, locale = locale(encoding = 'utf-16le'), skip = 0)
}



#' Assert there is a value
#'
#' @param value
#'
#' @return
#' @export
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' assert_value("C1")
#' assert_value("45.4u")
#' assert_value("45)
#'
assert_value <- function(value) {
  value_pattern <- "^([0-9]+)(\\s*[A-Za-z]*)$"
  value_detected <- str_detect(value, value_pattern)
  if(!value_detected)
    stop("Make sure to put a value.")
  invisible(value_detected)
}







