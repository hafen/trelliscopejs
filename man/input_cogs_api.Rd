% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_cogs.R
\name{input_cogs_api}
\alias{input_cogs_api}
\title{Specify a collection of input cognostics to be stored using an API}
\usage{
input_cogs_api(
  ...,
  set_url,
  get_url,
  get_request_options = list(mode = "cors", method = "GET", headers =
    list(`Content-Type` = "application/json", Accept = "application/json")),
  set_request_options = list(mode = "cors", method = "POST", headers =
    list(`Content-Type` = "application/json", Accept = "application/json"))
)
}
\arguments{
\item{set_url}{URL of the API endpoint for setting a single input}

\item{get_url}{URL of the API endpoint for getting all inputs for the display}

\item{get_request_options}{request options for the API call to set inputs}

\item{set_request_options}{request options for the API call to get inputs}

\item{\ldots}{objects created by any of \code{\link{input_radio}},
\code{\link{input_text}}}
}
\description{
Specify a collection of input cognostics to be stored using an API
}
\details{
See [here](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch#supplying_request_options for more information about request options.
}
