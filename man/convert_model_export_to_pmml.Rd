% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert-model-export-to-pmml.R
\name{convert_model_export_to_pmml}
\alias{convert_model_export_to_pmml}
\title{Converts the CSv files that are part of a model export into PMML}
\usage{
convert_model_export_to_pmml(
  model_parameters_folder_path,
  model_export_file_path,
  database_name,
  custom_function_files = NULL
)
}
\arguments{
\item{model_export_file_path}{string The absolute or relative path to the
model export file. If relative, the path should relative to the working
directory of the project}

\item{database_name}{string The database from which to take the starting variables
for the model}

\item{custom_function_files}{string vector An optional list of paths to R files 
that have functions referenced by the variable details sheet. Each of these
will be converted to PMML and added}
}
\value{
An object created by the XML::xmlNode function that represents the
PMML XML
}
\description{
Converts the CSv files that are part of a model export into PMML
}
