#' ElectricalObject class
#' @title ElectricalObject
#' @docType class
#' @description A base class for the electrical objects used in LTspice netlist.
#' @field name Name of the object
#' @field plus name of the plus node
#' @field minus name of the minus node
ElectricalObject <- R6::R6Class("ElectricalObject",
    public = list(
      name = NULL,
      plus = NULL,
      minus = NULL
    )
)

#' Voltage class
#' @title Voltage Class
#' @docType class
#' @description Voltage class
#' @field name Name of the person
#' @field hair Hair colour
#'
#' @section Methods:
#' \describe{
#' \item{set_hair Set the hair color}
#' }
#'
#' @importFrom readr parse_number
#' @importFrom stringr str_trim str_split str_detect str_c
#'
#' @examples
#' @export
Voltage <- R6::R6Class('Voltage',
   inherit = ElectricalObject,
   public = list(
     unit = "volts",
     name = NULL,
     plus = NULL,
     minus = NULL,
     dc = NULL,
     ac = list(amplitude = NULL, phase = NULL),
     Rser = NULL,
     Cpar = NULL,

     initialize = function(voltage_txt) {

       voltage_parts <- str_trim(unlist(str_split(voltage_txt, "\\s")))

       # At least first for parts have to be filled-in.
       self$name <- voltage_parts[1]
       self$plus <- voltage_parts[2]
       self$minus <- voltage_parts[3]
       assert_value(voltage_parts[4])
       self$dc <- parse_number_suffix(voltage_parts[4])

       is_ac <- str_detect(voltage_parts, 'AC')
       if(sum(is_ac) == 1) {
         pos <- which(is_ac)
         assert_value(voltage_parts[pos+1])
         self$ac$amplitude <- parse_number_suffix(voltage_parts[pos+1])
         assert_value(voltage_parts[pos+2])
         self$ac$phase <- parse_number_suffix(voltage_parts[pos+2])
       }

       is_rser <- str_detect(voltage_parts, 'Rser')
       if(sum(is_rser) == 1) {
         rser <- str_extract(voltage_parts[is_rser], "(?<=Rser=)([0-9]+)([A-Za-z]*)$")
         if(!is.na(rser)) {
           rser <- parse_number_suffix(rser)
           self$Rser <- rser
         }
       }

       is_cpar <- str_detect(voltage_parts, 'Cpar')
       if(sum(is_cpar) == 1) {
         cpar <- str_extract(voltage_parts[is_cpar], "(?<=Cpar=)([0-9]+)([A-Za-z]*)$")
         if(!is.na(cpar)) {
           cpar <- parse_number_suffix(cpar)
           self$Cpar <- cpar
         }
       }
     }
   )
)





Capacitor <- R6::R6Class("Capacitor",
  inherit = ElectricalObject,
  public = list(
    units = "farads",
    name = NULL,
    plus = NULL,
    minus = NULL,
    capacitance = NULL,

    initialize = function(capacitor_txt) {

      parts <- str_trim(unlist(str_split(capacitor_txt, "\\s")))
      self$name <- parts[1]
      self$plus <- parts[2]
      self$minus <- parts[3]

      # Make sure there is value there
      assert_value(parts[4])
      self$capacitance <- parse_number_suffix(parts[4])

      # What about other parts?

    }
  )
)
