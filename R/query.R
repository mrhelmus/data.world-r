'data.world-r
Copyright 2017 data.world, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.

You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.

This product includes software developed at data.world, Inc.(http://www.data.world/).'

#' execute a SQL or SPARQL query against a data.world client
#'
#' @param connection the connection to data.world
#' @param type       the type of the query - either "sql" or "sparql"
#' @param dataset    the "agentid/datasetid" for the dataset against which to execute the query
#' @param query      the SQL or SPARQL query to run .
#' @param queryParameters Optional comma-separated ?name=value pairs
#' @param ...  additional param
#' @return the query results as a data frame
#' @seealso \code{\link{data.world}}
#' @examples
#' connection <- data.world(token = "YOUR_API_TOKEN_HERE")
#' query(connection, dataset="user/dataset",
#'       query="SELECT *
#'                FROM TableName
#'               LIMIT 10")
#'
#' query(connection, dataset="user/dataset",
#'       query="SELECT *
#'                FROM TableName where `field1` = ? AND `field2` > ?
#'               LIMIT 10",
#'       queryParameters = list("value", 5.0))
#'
#' query(connection, dataset="user/dataset", type="sparql",
#'       query="SELECT *
#'              WHERE {
#'                ?s ?p ?o.
#'              } LIMIT 10")
#'
#' query(connection, dataset="user/dataset", type="sparql",
#'       query="SELECT *
#'              WHERE {
#'              [ :Year ?year ; :Region ?region ; :Indicator_Coverage_and_Disaggregation ?score ]
#'              FILTER(?score > $v1)
#'              } LIMIT 10",
#'              queryParameters = list("$v1"=5.5))
#'
#' @export
query <- function(connection, type, dataset, query, queryParameters = list(), ...) {
  UseMethod("query")
}

#' @export
query.default <- function(connection, type, dataset, query, queryParameters = list(), ...) {
  print("nope.")
}

#' @export
query.data.world <- function(connection, type = "sql", dataset, query, queryParameters = list(), ...) {
  url = sprintf("https://query.data.world/%s/%s", type, dataset)
  requestQuery = list(query = query)
  if (length(queryParameters) > 0) {
    if (type == "sparql") {
      requestQuery$parameters <- contructQueryParameterString(queryParameters)
    } else {
      namedQueryParmeters <- list()
       for (i in 0:(length(queryParameters)-1)) {
          namedQueryParmeters[[paste("$data_world_param",i, sep = "")]] <- queryParameters[[i+1]]
       }
      requestQuery$parameters <- contructQueryParameterString(namedQueryParmeters)
    }
  }
  response <- httr::GET( url,
                   query = requestQuery,
                   httr::add_headers(
                     Accept = "text/csv",
                     Authorization = sprintf("Bearer %s", connection$token)),
                   httr::user_agent(data.world::userAgent()))
  #print(response)
  ret <- httr::http_status(response)
  if (response$status_code == 200) {
    text <- httr::content(x=response, as='text')
    df <- readr::read_csv(text, ...)
    ret <- df
  }
  ret
}

contructQueryParameterString <- function (queryParameterNamedList) {
  queryParameterStrings = lapply(names(queryParameterNamedList), function (param) {
    sparqlLiteral <- convertToSparqlLiteral(queryParameterNamedList[[param]])
    paste(param, sparqlLiteral, sep = "=")
  })
  ret <- paste(queryParameterStrings, collapse = ",")
  ret
}

convertToSparqlLiteral <- function (v) {
  type <- class(v)
  iriTemplate <- switch(type,
         "logical"="\"%s\"^^<http://www.w3.org/2001/XMLSchema#boolean>",
         "numeric"="\"%s\"^^<http://www.w3.org/2001/XMLSchema#decimal>",
         "integer"="\"%s\"^^<http://www.w3.org/2001/XMLSchema#integer>",
         "character"="\"%s\""
         )
  if(is.null(iriTemplate)) {
    stop(sprintf("parameter value %s is of not supported type %s. Supported types are logical, numeric, integer, character.", v, type))
  }
  return(sprintf(iriTemplate, v))
}
