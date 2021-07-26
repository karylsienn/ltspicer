Circuit <- R6::R6Class("Circuit",
    public = list(
      objects = list(),
      ltspice = NULL,
      netlist_lines = NULL,
      netlist_path = NULL,
      initialize = function(netlist_path) {
        self$netlist_path <- netlist_path
        netlist_lines <- parse_netlist(netlist_path)
        rexes <- c("^C([0-9]*)" = Capacitor, "^L([0-9]*)" = Inductor, "^R([0-9]*)" = Resistor, "^V([0-9]*)" = Voltage)
        # Read all the lines and create objects
        for(rex in names(rexes)) {
          # Detect the position in lines
          detect <- which(str_detect(netlist_lines, rex))
          if(length(detect) >= 1) {
            for(.detect in detect) {
              obj <- rexes[[rex]]$new(netlist_lines[.detect])
              name <- obj$name
              self$objects <- append(self$objects, lst(!!name := list(object = obj, line_number = .detect)))
            }
          }
        }
        self$netlist_lines <- netlist_lines
        if(str_detect(osVersion, "macOS"))
          self$ltspice <- "/Applications/LTspice.app/Contents/MacOS/LTspice"
      },

      # Run LTspice and get the impedance
      get_ltspice_impedance = function() {
        # First update the netlist with the current values of the parameters
        for(obj in self$objects) {
          spice_line <- obj$object$spice_line()
          self$netlist_lines[obj$line_number] <- spice_line
        }

        # Save the updated netlist
        write_netlist(self$netlist_lines, self$netlist_path)

        # Run LTspice
        cmd <- str_c(
          str_interp("cd ${dirname(self$netlist_path)}"),
          str_interp("${self$ltspice} -b ${basename(self$netlist_path)}"),
          sep = "\n"
        )
        system(cmd, intern = TRUE)

        # Read raw file.


      }
    )
)
