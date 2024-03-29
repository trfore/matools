% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflows.R
\name{analysis_evoked_ap}
\alias{analysis_evoked_ap}
\title{Workflow: Analyze Evoked Action Potential Data}
\usage{
analysis_evoked_ap(
  data_folder_path,
  data_parameters,
  histo_bin_size = 1,
  histo_window_size = 50,
  histo_one_event_per_bin = FALSE
)
}
\arguments{
\item{data_folder_path}{path of data folder containing *.ASC files}

\item{data_parameters}{path of csv file with project parameters}

\item{histo_bin_size}{integer, histogram bin size. Wrapper for create_histogram_df(),
defaults to 1 ms.}

\item{histo_window_size}{integer, histogram window size. Wrapper for create_histogram_df(),
defaults to 50 ms.}

\item{histo_one_event_per_bin}{boolean, wrapper for create_histogram_df() arg one_event_per_bin,
defaults to FALSE.}
}
\value{
tibble of aggregated data
}
\description{
Working example of how to use the functions within this package to create an analysis pipeline.
}
\details{
This particular workflow is used to analyze the spike jitter, probability, and rate of action potentials
generated by externally stimulating synaptic inputs onto the recorded neuron.
}
\note{
This workflow has hard-coded values, yet is intended to serve as a reference point.
}
\examples{
\dontrun{
data_path <- paste(getwd(), "data-raw/evoked_ap", sep = "/")
data_folder <- paste(data_path, "data", sep = "/")
data_parameters <- paste(data_path, "evoked_ap_parameters.csv", sep = "/")

data_collection <- analysis_evoked_ap(
  data_folder_path = data_folder,
  data_parameters = data_parameters
)
}

}
