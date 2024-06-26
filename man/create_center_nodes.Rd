% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert-model-export-to-pmml.R
\name{create_center_nodes}
\alias{create_center_nodes}
\title{Create Center Nodes}
\usage{
create_center_nodes(
  orig_variable,
  center_value,
  centered_variable,
  centered_variable_type,
  PMML
)
}
\arguments{
\item{orig_variable}{the original variable name}

\item{center_value}{value to center on}

\item{centered_variable}{name of the centered variable}

\item{centered_variable_type}{type of the new centered variable}

\item{PMML}{the pmml that is appended to}
}
\value{
returns PMML with attached centered nodes
}
\description{
Creates center nodes according to passed params
}
