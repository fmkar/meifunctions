% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_spindles.R
\name{plot_spindles}
\alias{plot_spindles}
\title{Graph meiotic time course spindle data}
\usage{
plot_spindles(file_dir, color = "")
}
\arguments{
\item{file_dir}{file directory to excel file with spindle data with the columns strain, time, two_poles, four_poles, and total.
total column denotes the total number of cells counted}

\item{color}{colors to be used for the graphs, you will need as many colors as strains, defaults to Okabe Ito color palette}
}
\value{
three graphs with \% of cells in MI + MII (two + four poles), \%cells in MI(two poles), and \% of cells in MII(four poles).
}
\description{
Graph meiotic time course spindle data
}
