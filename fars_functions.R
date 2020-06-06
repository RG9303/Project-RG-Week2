library(roxygen2)

#' @title fars_read
#' @description The function \code{fars_read} read a csv file if it exists and forwards the argument a data frame.
#' @param filename to enter a database with format csv.
#' @return if file exists, this function read the file and return a database as a data frame. If the extension
#' is diferent to csv, it can not read the file.
#' @details you need install packages like dplyr and readr before this or it may result in an error
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @example fars_read(filename = x)
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' @title make_filename
#' @description The function \code{make_file} transform a variable as a integer and print a character vector
#' containing a formatted combination of text and variable value.
#' @param year as a variable to tranform it in an integer value.
#' @return a character vector containing a formatted combination of text and variable value.
#' @details you need enter a number or it return a NA.
#' @importFrom base as.integer sprintf
#' @examples make_filename(2014)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' @title fars_read_years
#' @description The function \code{fars_read_years} save the name of a specific data base according to a year,
#' read the ddatabase and transmute it drops existing variables as year.
#' @param years as a variable to tranform it in an integer value accross \code{make_filename}. It will be used
#' when it generate the name of a file with the function \code{fars_read}.
#' @return if the year is in the set: 2013, 2014 or 2015, it will transmute a database in a new data frame depending
#' on a specific month. Otherwise it will print a warning.
#' @details you need enter a number as a year contained in the set: 2013, 2014 or 2015 or it will return a warning
#' as a message.
#' @importFrom dplyr mutate select
#' @examples fars_read_years(2014)
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' @title fars_summarize_years
#' @description The function \code{fars_summarize_years} transmute the data frame by group, summarize and 
#' spread a key-value pair across the variables year and n.
#' @param years as a variable to read the other functions and transmute the data frame.
#' @return a data frame by group of year and month, and summarize by count. It will print the head of the database.
#' @details you need install the library tidyr and conserve the format of the variables.
#' @importFrom dplyr bind_rows group_by summarize 
#' @importFrom tidyr spread
#' @examples fars_summarize_years(2014)
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' @title fars_map_state
#' @description The function \code{fars_map_state} transform the principal database and add
#' conditionals for some variables. Plot a map with a specific lat and long.
#' @param state.num as a variable that represent a state.
#' @param year as a variable to tranform it in an integer value.
#' @return if number of a state is unique and it is contained in the variable STATE of the data
#' it will make a data frame with this filter, with conditionals to transform NAs in the 
#' variables LONGITUD AND LATITUDE and print a map with this location. Otherwise print a 
#' message "no accidents to plot" and return an invisible object.
#' @details you need to install the package "map" and specify a number of a state.
#' @importFrom dplyr filter
#' @importFrom base message invisible
#' @importFrom maps map
#' @importFrom graphics points
#' @examples fars_map_state(19, 2014)
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
