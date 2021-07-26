#' Impedance
#'
#' Provides impedance in the form of voltage over current.
#'
#' @param electrical_object the object inheriting from the class ElectricalObject
#'
#' @return string, ltspice command measuring the impedance
#' @export
#'
#' @importFrom stringr str_c
#'
#' @examples
part_impedance <- function(electrical_object) {
  voltage <- ifelse(electrical_object$minus == "0",
                    str_c("-", "`V(", electrical_object$plus, ")`"),
                    str_c("(`V(", electrical_object$minus, ")`", "-`V(", electrical_object$plus, ")`)"))
  current <- str_c("`I(", electrical_object$name, ")`")
  return(str_c(voltage, "/", current))
}


