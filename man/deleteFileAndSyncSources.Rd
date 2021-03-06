% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleteFileAndSyncSources.R
\name{deleteFileAndSyncSources}
\alias{deleteFileAndSyncSources}
\title{delete a file from the dataset}
\usage{
deleteFileAndSyncSources(connection, dataset, name)
}
\arguments{
\item{connection}{the connection to data.world}

\item{dataset}{the "agentid/datasetid" for the dataset against which to execute the query}

\item{name}{the filename including the file extension. If a file by that name already
exists in the dataset, the file will be updated/overwritten.}
}
\description{
delete a file from the dataset
}
\examples{
connection <- data.world(token = "YOUR_API_TOKEN_HERE")
data.world::deleteFileAndSyncSources(connection, dataset = "ownerid/datasetid",
 name = "file.csv")
}
