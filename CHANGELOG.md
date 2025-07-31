# Changelog

All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

