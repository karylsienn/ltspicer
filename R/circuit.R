Circuit <- R6::R6Class("Circuit",
    public = list(
      objects = list(),
      ltspice = NULL,
      netlist_lines = NULL,
      netlist_path = NULL,
      data = NULL,
      impedance_tbl = NULL,
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
      run_netlist = function(ascii=TRUE) {

        stopifnot(ascii)

        # First update the netlist with the current values of the parameters
        for(obj in self$objects) {
          spice_line <- obj$object$spice_line()
          self$netlist_lines[obj$line_number] <- spice_line
        }

        # Save the updated netlist
        write_netlist(self$netlist_lines, self$netlist_path)

        # Run LTspice
        if(ascii) {
          cmd <- str_c(
            str_interp("cd ${dirname(self$netlist_path)}"),
            str_interp("${self$ltspice} -ascii -b ${basename(self$netlist_path)}"),
            sep = "\n"
          )
          system(cmd, intern = TRUE)
        } else {
          warning("Non-ascii is not implemented yet")
        }

        # Update the data
        if(ascii) {
          self$data <- read_raw_spice(str_replace(self$netlist_path, ".net", ".raw"), ascii=TRUE)
        } else {
          warning("Non-ascii is not implemented yet.")
        }


      },

      # Set parameters
      # new_values are of the form (varname1 = value1, varname2 = value2,...)
      set_params = function(new_values) {
        nms <- names(new_values)
        for(.name in nms) {
          self$objects[[.name]]$object$set_value(new_values[[.name]])
        }
      },


      # Compute impedance
      compute_impedance = function() {

        # Get the voltage source (assume that there is one source)
        voltage <- self$objects[[str_which(names(self$objects), "^V")]]$object
        impedance <- part_impedance(voltage) # How to compute impedance?

        # Compute from the raw file stored in memory.
        if(is.null(self$data)) {
          raw_file <- str_replace(self$netlist_path, ".net", ".raw")
          if(file.exists(raw_file)) {
            self$data <- read_raw_spice(raw_file, ascii=TRUE)
          }
        }

        impedance <- with(self$data, eval(parse(text = impedance)))
        impedance_tbl <- tibble(self$data[1], impedance)
        colnames(impedance_tbl) <- c(colnames(self$data)[1], "impedance")
        self$impedance_tbl <- impedance_tbl
        return(impedance_tbl)

      },


      plot_impedance = function(what = "complex") {
        if(is.null(self$impedance_tbl)) {
          self$compute_impedance()
        }
        # We assume this is about frequency
        if (what == "complex") {
          x <- self$impedance_tbl %>%
            mutate(
              real = Re(impedance),
              imaginary = Im(impedance)) %>%
            select(-impedance)

          ggp <- x %>%
            ggplot(aes(x = frequency/1e6)) +
            geom_line(aes(y = real, linetype = "Re(Z)")) +
            geom_line(aes(y = scales::rescale(imaginary, to = range(real)), linetype = "Im(Z)")) +
            scale_y_continuous(sec.axis = sec_axis(~scales::rescale(., to = range(x$imaginary)), name = "Im(Z)"),
                               name = "Re(Z)") +
            scale_x_log10(name = "Frequency (MHz)") +
            scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
            theme_bw() +
            theme(legend.position = c(0.2,0.2))



        } else if (what == "polar") {
          x <- self$impedance_tbl %>%
            mutate(
              mod = Mod(impedance),
              mod = 10*log10(mod),
              arg = Arg(impedance),
              arg = 180*arg/pi) %>%
            select(-impedance)

          ggp <- x %>%
            ggplot(aes(x = frequency/1e6)) +
            geom_line(aes(y = mod, linetype = "|Z|")) +
            geom_line(aes(y = scales::rescale(arg, to = range(mod)), linetype = "Phase")) +
            scale_y_continuous(sec.axis = sec_axis(~scales::rescale(., to = range(x$arg)), name = "Phase (deg)"),
                               name = "|Z| (dB)") +
            scale_x_log10(name = "Frequency (MHz)") +
            scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
            theme_bw() +
            theme(legend.position = c(0.2,0.2))
        } else {

        }

        return(ggp)

      },

      plot_compared_impedance = function(compare_tbl, params) {
        if(missing(compare_tbl) && missing(params)) {
          stop("One of the two has to be specified")
        }

        if(missing(params)) {


        } else {

        }
      },

      # The assumption is that compare_tbl has two columns: frequency (dbl), impedance (cpl)
      optimize_params = function(compare_tbl, params_to_optimize, compare_fn, ...) {
          # get initial points. Then optimize by calling LTspice and comparing the result with compare_tbl
          # assume the loss function gets two complex vectors
        initial_points <- purrr::map_dbl(params_to_optimize, function(.name) {
          self$objects[[.name]]$object$value
        })

        loss_fn <- function(params) {
          names(params) <- params_to_optimize
          self$set_params(params)
          self$run_netlist(ascii = TRUE)
          self$compute_impedance()

          # Assume compare_tbl and data are properly arranged (but this should be checked)
          return(compare_fn(self$impedance_tbl$impedance, compare_tbl$impedance))
        }

        result <- optim(initial_points, loss_fn, ...)
        return(result)

      },

      # Print Circuit
      print = function() {
        # TODO: Implement print in each objects and use their `print` methods.
        purrr::walk(
          self$objects,
          function(x) cat(x$object$name, " ", x$object$value, "\n")
        )
      }
    )
)
