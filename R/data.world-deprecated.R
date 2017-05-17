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

#' Deprecated function(s) in the data.world package
#'
#' These functions have been marked for removal from the data.world package.
#' @rdname data.world-deprecated
#' @name data.world-deprecated
#' @aliases query.data.world
#' @docType package
#' @section Details:
#' \tabular{ll}{
#'    \code{query.data.world} \tab is now \code{\link{query.sql}}\cr
#' }
NULL

#' Execute a SQL or SPARQL query against a data.world client.
#'
#' Params:
#' \enumerate{
#'  \item \code{connection} Object of type data.world.
#'  \item \code{type} Query type - either "sql" or "sparql".
#'  \item \code{dataset} Dataset URL or path.
#'  \item \code{query} SQL or SPARQL query to run.
#'  \item \code{queryParameters} Query parameters.
#' }
#'
#' @examples
#' connection <- data.world(token = "YOUR_API_TOKEN_HERE")
#' \dontrun{
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
#' }
#' @name query-deprecated
#' @rdname query-deprecated
#' @export
query.data.world <- function(connection, ...) {
  # Internal function to help unpack '...' param
  legacy_query_fun <- function(connection,
    type = "sql",
    dataset,
    query,
    queryParameters = list(),
    ...) {
    .Deprecated(
      package = "data.world",
      msg = "The connection argument, of type data.world, is no longer necessary.
      See ?data.world::query for new usage."
    )

    if (type == "sparql") {
      return(data.world::query(
        data.world::sparql(query = query, params = queryParameters),
        dataset
      ))
    } else {
      return(data.world::query(
        data.world::sql(query = query, params = queryParameters),
        dataset
      ))
    }
  }

  return(legacy_query_fun(connection, ...))
}
