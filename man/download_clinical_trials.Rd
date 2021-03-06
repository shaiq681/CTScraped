% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_clinical_trials.R
\name{download_clinical_trials}
\alias{download_clinical_trials}
\title{Download CT.gov trials as a tibble}
\usage{
download_clinical_trials(search_query)
}
\arguments{
\item{search_query}{: character, query to be fetched using CT.gov api}
}
\value{
df : tibble
}
\description{
Download CT.gov trials as a tibble
}
\examples{

df_cancer <- download_clinical_trials(search_query = "cancer")
df_heart_attack <- download_clinical_trials(search_query = "heart attack")
df_complex_query <- download_clinical_trials(search_query = "covid19 AND (new york OR italy)")
}
