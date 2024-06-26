% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nodes.R
\name{attach_derived_field_child_nodes}
\alias{attach_derived_field_child_nodes}
\title{Attach child nodes to DerivedField node.}
\usage{
attach_derived_field_child_nodes(
  derived_field_node,
  var_details_sheet,
  var_name,
  db_name,
  custom_function_names,
  table_names
)
}
\arguments{
\item{derived_field_node}{DerivedField node to attach child nodes.}

\item{var_details_sheet}{Variable details sheet data frame.}

\item{var_name}{Variable name.}

\item{db_name}{Database name.}

\item{custom_function_names}{vector of strings. Holds the names of functions
parsed from custom function files.}

\item{table_names}{vector of strings. The list of tables part of this PMML}
}
\value{
Updated DerivedField node.
}
\description{
Attach child nodes to DerivedField node.
}
