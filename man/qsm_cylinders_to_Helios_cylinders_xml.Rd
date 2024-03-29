% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsm_cylinders_to_Helios_cylinders_xml.R
\name{qsm_cylinders_to_Helios_cylinders_xml}
\alias{qsm_cylinders_to_Helios_cylinders_xml}
\title{qsm_cylinders_to_Helios_cylinders_xml}
\usage{
qsm_cylinders_to_Helios_cylinders_xml(
  cylinder_file,
  output_file,
  subdiv = 5,
  data_fields,
  branch_file
)
}
\arguments{
\item{cylinder_file}{cylinder text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)}

\item{output_file}{name of the xml file to be written}

\item{subdiv}{integer number of subdivisions to be used in cylinder construction}

\item{data_fields}{vector of column names from input_file that should be written as cylinder object data in the xml file}

\item{branch_file}{optional branch text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)}
}
\description{
qsm_cylinders_to_Helios_cylinders_xml
}
\note{
if data_fields is not provided, all columns of the TreeQSM output (cylinder_file and branch_file if provided) are written as object data

if branch_file is provided "branch_" is appended to the beginning of each of that file's column names before joining to the cylinder file. If data_fields is provided, append "branch_" when referencing these column names.
}
