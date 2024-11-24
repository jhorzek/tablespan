% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_basic_table.R
\name{print_insert_header_entries}
\alias{print_insert_header_entries}
\title{print_insert_header_entries}
\usage{
print_insert_header_entries(
  header_partial,
  max_level,
  column_offset,
  header_table
)
}
\arguments{
\item{header_partial}{part of the header list}

\item{max_level}{depth of the header list}

\item{column_offset}{offset to write data in columns}

\item{header_table}{table in which the header entries should be inserted}
}
\value{
header_table with entries
}
\description{
Insert header entries into a matrix for printing.
}
\keyword{internal}
