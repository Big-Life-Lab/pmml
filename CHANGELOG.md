# Changelog

All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1.1.1] - 2026-02-25

### Fixed

* [Fix bug with converting logistic regression models due to target mismatch](https://github.com/Big-Life-Lab/pmml/commit/88a16c01e2883e4953010a9c48ff5674cc886fe6)

## [v1.1.0] - 2025-11-03

### Added

* Added support for logistic regression models in the
  `convert_model_export_to_pmml` function

### Changed

* Changed the `convert_model_export_to_pmml` function to validate the model
  export files and stop execution if validation fails
* Changed the minimum supported R version to 4.1. The validation mentioned in
  the previous point uses the `model.parameters` library which has a minimum
  supported version of 4.1.

### Fixed

* Fixed bug in `convert_model_export_to_pmml` with whitespaces around RCS
  variable names. For example, previously an RCS variable named ` Age_rcs1 `
  would be considered valid and put into the PMML file as is. This is now fixed
  by trimming the whitespace at the start and end of the string.
* Fixed bug in `convert_model_export_to_pmml` when converting data frame
  expressions in the return values. Previously, the function would throw an
  error with the following R code,

  ```{r}
  lookup_sodium_serving <- function(sodium_servings, food_group) {
    sodium_serving_row <-
        sodium_servings[sodium_servings$food_group == food_group, ]
    return(sodium_serving_row$sodium_servings)
  }
  ```
* Fixed bug in `convert_model_export_to_pmml` when converting data frame
  expressions. Previously, the function would incorrectly convert the following
  R code,

  ```{r}
  lookup_sodium_servings <- function(sodium_servings, food_group) {
    sodium_serving_row <-
        sodium_servings[sodium_servings$food_group == food_group, ]
    sodium_serving <- sodium_serving_row$sodium_servings
    return(sodium_servings)
  }
  ```
* Fixed bug in `convert_model_export_to_pmml` when converting data frame
  expressions. Previously, the function would incorrectly convert the following
  R code,

  ```
  lookup_sodium_servings <- function(sodium_servings, food_group_index) {
    sodium_serving_row <-
        sodium_servings[food_group_index, ]
    return(sodium_servings_row)
  }
  ```
* Fixed bug in `convert_model_export_to_pmml` when converting data frame
  expressions. Previously, the function would incorrectly convert the following
  R code,

  ```
  lookup_meat_sodium_servings <- function(sodium_servings) {
    sodium_servings <-
        sodium_servings[sodium_servings$food_group == "meat", "sodium_servings"]
    return(sodium_servings)
  }
  ```
* Fixed bug in `convert_model_export_to_pmml` when converting data frame
  expressions. Previously, the function would incorrectly convert the following
  R code,

  ```
  lookup_sodium_servings_indexes <- function(sodium_servings, food_group) {
    return(which(sodium_servings$food_group == food_group))
  }
  ```
* Fixed bug in `convert_model_export_to_pmml` when converting rows in a
  variable details sheet whose `recStart` column was `copy` and whose `recEnd`
  column was a non `else` value. Previously, the function would incorrectly
  assume that `recEnd` value is always `else`.

## [v1.0.2] - 2025-07-31

### Added

* `recode_to_pmml` now throws an error if the database the user wants to
   generate the PMML file for (the database_name parameter) is not present in
   the variables sheet
* `recode_to_pmml` now throws an error when a recoded variable and one of its
   start variables are named the same
* `recode_to_pmml` now throw an error if there's no entry for a model steps
   file in the model export file
* `recode_to_pmml` now throws an error if the entry for the model steps file
   cannot be found
* Added the `devtools` package to the DESCRIPTION file's `Suggests` field so
  `renv` can install it

