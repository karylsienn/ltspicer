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
#' @examples
#' netlist <- parse_netlist('spice_files/2m_cable_model.net')
#' print(netlist)
#'
parse_netlist <- function(netlist_path) {
  readLines(netlist_path,  encoding = "UTF-16LE", skipNul = TRUE)
}



#' Write netlist to a file
#'
#' @param netlist_lines
#' @param netlist_path
#'
#' @return
#' @export
#'
#' @examples
write_netlist <- function(netlist_lines, netlist_path) {
  writeLines(netlist_lines, con = netlist_path, sep = "\n")
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
  value_pattern <- "^([0-9]+[\\.e\\-\\+[0-9]*]*)(\\s*[A-Za-z]*)$"
  value_detected <- str_detect(value, value_pattern)
  if(!value_detected)
    stop("Make sure to put a value.")
  invisible(value_detected)
}





#' Read raw LTspice file
#'
#' @param path path to the raw file
#' @param ascii boolean, are the values wrote in ascii?
#'
#' @return a tibble of data traces in raw file
#' @export
#'
#' @importFrom stringr str_which str_detect str_extract str_trim str_replace str_split str_to_upper
#' @importFrom readr read_tsv parse_number
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @importFrom dplyr vars mutate_all mutate_at
#' @importFrom purrr map
#'
#' @examples
read_raw_spice <- function(path, ascii=TRUE) {

  linesraw <- c()
  pos <- 1
  if(ascii) {
    linesraw <- readLines(path, encoding = "utf-16le", skipNul = TRUE)
    pos <- str_which(linesraw, "Values:") + 1
  } else {
    stop("Not implemented yet.")

    bflag <- FALSE
    while(!bflag) {
      line <- readLines(path, n = pos+1, encoding = "utf-16le", skipNul = TRUE)
      bflag <- any(str_detect(line, "Binary:"))
      if(!bflag) {
        linesraw <- line
      }
      pos <- pos + 1
    }

  }


  # What is the plotname
  plotname <- linesraw[str_which(linesraw, "Plotname:")]
  plotname <- str_extract(plotname,  "(?<=Plotname:)(\\s*\\w*)")
  plotname <- str_trim(plotname)

  # How many variables
  no_vars <- linesraw[str_which(linesraw, "No.\\s*Variables:")]
  no_vars <- str_extract(no_vars, "(?<=Variables:)(\\s*[0-9]+)")
  no_vars <- as.integer(str_trim(no_vars))

  # How many points?
  no_points <- linesraw[str_which(linesraw, "No.\\s*Points:")]
  no_points <- str_extract(no_points, "(?<=Points:)(\\s*[0-9]+)")
  no_points <- as.integer(str_trim(no_points))


  # Build a table of traces.
  pos_vars <- str_which(linesraw, "^Variables:")
  vars <- str_trim(linesraw[(pos_vars+1):(pos_vars + no_vars)])
  vars <- read_tsv(vars, col_names = c("trace_no", "trace_name", "description"))

  if(str_to_upper(plotname) == "AC") {
    if(ascii) {
      x <- linesraw[pos:length(linesraw)]
      x[str_detect(x, "^([0-9]+)")] <- str_replace(x[str_detect(x, "^([0-9]+)")], "^([0-9]+)", "")
      x <- str_trim(x)
      x <- matrix(x, nrow = no_points, ncol = no_vars, byrow = TRUE)
      colnames(x) <- vars$trace_name
      x <- as_tibble(x)
      x <- mutate_all(x, ~map(., function(val) {
        v1 <- parse_number(unlist(str_split(val, ",")))
        complex(real = v1[1], imaginary = v1[2])
      }))
      x <- unnest(x, cols = vars$trace_name)
      x <- mutate_at(x, vars(vars$trace_name[1]), Mod)
      colnames(x)[-1] <- str_to_upper(colnames(x)[-1])
    }
  }

  return(x)



}




