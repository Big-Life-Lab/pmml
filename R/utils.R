#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Margins as character vector.
get_margins <- function(chars) {
  trimmed_chars <- trimws(chars)
  is_singleton <- is_numeric(trimmed_chars)
  if (is_singleton) {
    return(c(trimmed_chars, trimmed_chars))
  }

  is_range <- grepl(pkg.env$margin_separator, trimmed_chars, fixed = TRUE)
  if (!is_range) {
    return(c(0, 0))
  }

  margins <- strsplit(substr(trimmed_chars, 2, nchar(trimmed_chars) - 1), pkg.env$margin_separator)[[1]]
  return(margins)
}

#' Get closure type for a margin.
#'
#' @param chars Character vector.
#'
#' @return Closure type.
#'
#' @examples
get_margin_closure <- function(chars) {
  if (is_left_open(chars)) {
    if (is_right_open(chars)) {
      return(pkg.env$node_attr.closure.open)
    }
    return(pkg.env$node_attr.closure.leftOpen)
  }
  if (is_right_open(chars)) {
    return(pkg.env$node_attr.closure.rightOpen)
  }
  return(pkg.env$node_attr.closure.closed)
}

#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Whether the left endpoint of an interval is open.
#'
#' @examples
is_left_open <- function(chars) {
  trimmed_chars <- trimws(chars)
  return(substr(trimmed_chars, 1, 1) == "(")
}

#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Whether the right endpoint of an interval is open.
#'
#' @examples
is_right_open <- function(chars) {
  trimmed_chars <- trimws(chars)
  return(substr(trimmed_chars, nchar(trimmed_chars), nchar(trimmed_chars) + 1) == ")")
}

#' Check if a character object can be converted to a number.
#'
#' @param chars Character object.
#'
#' @return Whether `chars` can be converted to a numeric value.
is_numeric <- function(chars) {
  return(suppressWarnings(!is.na(as.numeric(chars))))
}

#' Check if recFrom is a range for a variable details row.
#'
#' @param var_details_row Variable details sheet row.
#'
#' @return Whether recFrom is a range.
is_rec_from_range <- function(var_details_row) {
  margins <- get_margins(var_details_row[[pkg.env$columns.recFrom]])
  # only consider margins as a range if the endpoints are different
  return(margins[1] != margins[2])
}

#' Get all variable details rows for a variable and database combination.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param var_name Variable name.
#' @param db_name Database name.
#'
#' @return All variable details rows for the variable and database combination.
get_var_details_rows <- function(var_details_sheet, var_name, db_name) {
  var_name_indices <- get_var_details_row_indices(var_details_sheet, var_name)
  # Make sure that if a match is found, the word is the database name itself
  # rather than containing just a part of it
  db_name_regex <- paste("\\<", db_name, "\\>", sep = "")
  db_indices <- which(grepl(db_name_regex, var_details_sheet$databaseStart), arr.ind = TRUE)
  intersect_indices <- intersect(var_name_indices, db_indices)
  return(var_details_sheet[intersect_indices, ])
}

#' Get all variable details row indices for a variable.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param var_name Variable name.
#'
#' @return All variable details row indices for a variable.
get_var_details_row_indices <- function(var_details_sheet, var_name) {
  return(which(var_details_sheet$variable == var_name, arr.ind = TRUE))
}

#' Get variable row from variable sheet.
#'
#' @param var_name Variable name.
#' @param vars_sheet Variable sheet data frame.
#'
#' @return Variable row.
get_var_sheet_row <- function(var_name, vars_sheet) {
  return(vars_sheet[which(vars_sheet$variable == var_name, arr.ind = TRUE)[1], ])
}

#' Get variable name from variableStart using database name.
#'
#' @param var_details_row A variable details row.
#' @param db_name Name of database to extract from.
#'
#' @return character The name of the start variable.
get_start_var_name <- function(var_details_row, db_name) {
  # The value of the variableStart column for this variable details row
  start_variables <- var_details_row$variableStart

  # The regex that will be used to pluck the name of the start variable from a
  # start variable string. For example, if the db_name is cchs2001_p and the
  # list is cchs2001_p::RACA_6A,this regex will pluck out RACA_6A
  start_variable_with_db_regex <- paste0(db_name, "::(.+?)$")
  # Regex to pluck out the start variable from a default variable string
  # For example, if the string is [ADL_01], this regex will pull out ADL_01
  default_start_var_regex <- "\\[(.+?)\\]"
  # Split the start variable column into each start variable string
  # For example, db1::var1, db2::var2, [var3] would be split into
  # [db1::var1, db2::var2, [var3]]
  start_variables_split <- strsplit(start_variables, ",")
  # The name of the start variable for the passed db_name argument
  start_variable_for_db <- NA
  # The name of the default start variable for this start variable string
  default_start_var <- NA
  # Go through each of the start variable strings to find either one for this
  # db or a default one
  for (i in seq_len(length(start_variables_split[[1]]))) {
    current_start_variable_str <- start_variables_split[[1]][i]

    # Get regex match for variable start
    db_var_regex_matches <- regmatches(
      current_start_variable_str,
      regexec(start_variable_with_db_regex, current_start_variable_str)
    )
    possible_db_start_var <- db_var_regex_matches[[1]][2]
    # If we found a start variable for this db then assign it and break out
    # of the loop since we don't care about the default var
    if (!is.na(possible_db_start_var)) {
      start_variable_for_db <- possible_db_start_var
      break
    }

    # Find the matches for the default var regex in this start variable string
    default_var_regex_matches <- regmatches(
      current_start_variable_str,
      regexec(default_start_var_regex, current_start_variable_str)
    )
    # Either the name of the default var or NA if it does not match
    possible_default_var <- default_var_regex_matches[[1]][2]
    if (!is.na(possible_default_var)) {
      default_start_var <- possible_default_var
    }
  }

  if (!is.na(start_variable_for_db)) {
    return(start_variable_for_db)
  } else if (!is.na(default_start_var)) {
    return(default_start_var)
  }
  # Otherwise, throw an error saying we could not find a start variable
  else {
    stop(paste(
      "No start variable found for database ",
      db_name,
      "for column ",
      start_variables
    ))
  }
}

#' Get data type for variable type.
#'
#' @param var_details_rows All variable details rows for the variable.
#' @param var_type Variable type
#' @param is_start_var boolean if the passed variable is variable start
#'
#' @return `var_type` data type.
get_variable_type_data_type <- function(var_details_rows, var_type, is_start_var) {
  is_categorical <- var_type %in% c(pkg.env$var_details_cat, pkg.env$var_cat)
  if (is_categorical) {
    char_var_details_rows <- ifelse(is_start_var,
      var_details_rows[!is_numeric(var_details_rows[[pkg.env$columns.recFrom]]), ],
      var_details_rows[!is_numeric(var_details_rows[[pkg.env$columns.recTo]]), ]
    )
    if (length(char_var_details_rows) > 0) {
      return(pkg.env$node_attr.dataType.string)
    }
    return(pkg.env$node_attr.dataType.integer)
  }
  return(pkg.env$node_attr.dataType.float)
}
