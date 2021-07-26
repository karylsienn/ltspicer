#' ElectricalObject class
#' @title ElectricalObject
#' @docType class
#' @description A base class for the electrical objects used in LTspice netlist.
#' @field name Name of the object
#' @field plus name of the plus node
#' @field minus name of the minus node
#' @field value value of the electrical object
#' @method spice_line parsed line to put to the netlist
ElectricalObject <- R6::R6Class("ElectricalObject",
    public = list(
      name = NULL,
      plus = NULL,
      minus = NULL,

      spice_line = function() {
        return(NULL)
      }

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
     },

     spice_line = function() {
       line <- str_c(
         self$name,
         self$plus,
         self$minus,
         self$dc,
         sep = " "
       )
       return(line)
     }
   )
)




#' Capacitor class
#' @title Capacitor Class
#' @docType class
#' @description Class for the capacitors.
#'
#' @section Methods:
#' \describe{
#' \item{num_impedance compute the impedance considering the value of the capacitance}
#' \item{sym_impedance give the symbolic impedance considering the name of the capacitor}
#' }
#'
#' @importFrom stringr str_trim str_split str_detect str_c str_interp
#'
#' @examples
#' @export
Capacitor <- R6::R6Class("Capacitor",
  inherit = ElectricalObject,
  public = list(
    units = "farads",
    name = NULL,
    plus = NULL,
    minus = NULL,
    capacitance = NULL,
    value = NULL,

    initialize = function(capacitor_txt) {

      parts <- str_trim(unlist(str_split(capacitor_txt, "\\s")))
      self$name <- parts[1]
      self$plus <- parts[2]
      self$minus <- parts[3]

      # Make sure there is value there
      assert_value(parts[4])
      self$capacitance <- parse_number_suffix(parts[4])
      self$value <- parse_number_suffix(parts[4])

      # What about other parts?
      if(length(parts) >= 4) {
        warning("The other parameters are not implemented yet.")
      }

    },

    num_impedance = function(f) {
      return(1i/(2*pi*f*self$capacitance))
    },

    sym_impedance = function(f) {
      return(str_interp("1i/(2*pi*${f}*${self$name})"))
    },

    set_value = function(new_cap) {
      self$capacitance <- new_cap
      self$value <- new_cap
      invisible(self)
    },

    spice_line = function() {
      line <- str_c(
        self$name,
        self$plus,
        self$minus,
        self$capacitance,
        sep = " "
      )
      return(line)
    }
  )
)



#' Inductor class
#' @title Inductor Class
#' @docType class
#' @description Class for the inductors
#'
#' @section Methods:
#' \describe{
#' \item{num_impedance compute the impedance considering the value of the inductance}
#' \item{sym_impedance give the symbolic impedance considering the name of the inductor}
#' \item{set_inductance set the inductance}
#' \item{spice_line produce the line for the netlist}
#' }
#'
#' @importFrom stringr str_trim str_split str_detect str_c str_interp
#'
#' @examples
#' @export
Inductor <- R6::R6Class("Inductor",
  public = list(
    units = "henries",
    name = NULL,
    plus = NULL,
    minus = NULL,
    inductance = NULL,
    value = NULL,

    initialize = function(inductor_txt) {
      parts <- str_trim(unlist(str_split(inductor_txt, "\\s")))
      self$name <- parts[1]
      self$plus <- parts[2]
      self$minus <- parts[3]

      # Make sure there is value there
      assert_value(parts[4])
      self$inductance <- parse_number_suffix(parts[4])
      self$value <- parse_number_suffix(parts[4])

      # What about other parts?
      if(length(parts) >= 4) {
        warning("The other parameters are not implemented yet.")
      }
    },

    set_value = function(new_ind) {
      self$inductance <- new_ind
      self$value <- new_ind
      invisible(self)
    },

    spice_line = function() {
      line <- str_c(
        self$name,
        self$plus,
        self$minus,
        self$inductance,
        sep = " "
      )
      return(line)
    },

    num_impedance = function(f) {
      return(1i*2*pi*f*self$inductance)
    },

    sym_impedance = function(f) {
      return(str_interp("1i*2*pi*${f}*${self$name}"))
    }


  ))





#' Resistor class
#' @title Resistor Class
#' @docType class
#' @description Class for the resistors
#'
#' @section Methods:
#' \describe{
#' \item{num_impedance compute the impedance considering the value of the resistance}
#' \item{sym_impedance give the symbolic impedance considering the name of the resistor}
#' \item{set_resistance set the resistance}
#' \item{spice_line produce the line for the netlist}
#' }
#'
#' @importFrom stringr str_trim str_split str_detect str_c str_interp
#'
#' @examples
#' @export
Resistor <- R6::R6Class("Resistor",
  public = list(
    units = "ohms",
    name = NULL,
    plus = NULL,
    minus = NULL,
    resistance = NULL,
    value = NULL,

    initialize = function(resistor_txt) {
      parts <- str_trim(unlist(str_split(resistor_txt, "\\s")))
      self$name <- parts[1]
      self$plus <- parts[2]
      self$minus <- parts[3]

      # Make sure there is value there
      assert_value(parts[4])
      self$resistance <- parse_number_suffix(parts[4])
      self$value <- parse_number_suffix(parts[4])

      # What about other parts?
      if(length(parts) >= 4) {
        warning("The other parameters are not implemented yet.")
      }
    },

    set_value = function(new_res) {
      self$resistance <- new_res
      self$value <- new_res
      invisible(self)
    },

    spice_line = function() {
      line <- str_c(
        self$name,
        self$plus,
        self$minus,
        self$resistance,
        sep = " "
      )
      return(line)
    },

    num_impedance = function(f) {
      return(self$resistance)
    },

    sym_impedance = function(f) {
      return(str_interp("${self$name}"))
    }
))




