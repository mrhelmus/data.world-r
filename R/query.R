"data.world-r
Copyright 2017 data.world, Inc.

Licensed under the Apache License, Version 2.0 (the \"License\");
you may not use this file except in compliance with the License.

You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an \"AS IS\" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.

This product includes software developed at data.world, Inc.
https://data.world"


#' Execute a query on data.world.
#'
#' @param statement Query statement of type sql or sparql.
#' @param ... S3 method specific params.
#' @return Query results as a data frame.
#' @seealso \code{\link{sql}} \code{\link{sparql}}
#' @export
query <- function(statement, ...) {
  UseMethod("query")
}

#' @export
query.default <- function(statement, ...) {
  print("nope.")
}

#' @describeIn query Execute a SQL query on data.world.
#' @examples
#' \dontrun{
#'   sql_stmt <- data.world::sql("SELECT * FROM Tables")
#'   query_results_df <- data.world::query(
#'     sql_stmt, "jonloyens/an-intro-to-dataworld-dataset")
#' }
#' @export
query.sql <- function(statement, ...) {
  # Internal function to help unpack '...' param
  query_fn <- function(statement, dataset) {
    return(
      dwapi::sql(
        dataset = dataset,
        query = statement$query,
        query_params = statement$params
      )
    )
  }

  return(query_fn(statement, ...))
}

#' @describeIn query Execute a SPARQL query on data.world.
#' @export
query.sparql <- function(statement, ...) {
  # Internal function to help unpack '...' param
  query_fn <- function(statement, dataset) {
    return(
      dwapi::sparql(
        dataset = dataset,
        query = statement$query,
        query_params = statement$params
      )
    )
  }
  return(query_fn(statement, ...))
}

#' Constructor function for SQL query statements.
#'
#' @param query SQL query.
#' @param params Sequence of positional query parameters.
#' @return Object of type \code{sql}.
#' @export
sql <- function(query, params = NULL) {
  me <- list(query = query, params = params)
  class(me) <- "sql"
  return(me)
}

#' Constructor function for SPARQL query statements.
#'
#' @param query SPARQL query.
#' @param params Sequence of named query parameters.
#' @return Object of type \code{sparql}.
#' @export
sparql <- function(query, params = NULL) {
  me <- list(query = query, params = params)
  class(me) <- "sparql"
  return(me)
}
