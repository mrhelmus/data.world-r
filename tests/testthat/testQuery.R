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

library("data.world")
library("testthat")
source("testUtil.R")

test_that("sparql query making the correct HTTR request" , {
  sqlQuery = "SELECT * FROM TableName LIMIT 10"
  type = "sparql"
  dataset = "ownerid/datasetid"
  mockResponseLocalContentPath = "resources/file1.csv"
  queryParameters = list(key1="value1", "?key2"=1L, "?key3"=1, "?key4"=TRUE, "?key5"=1.5)
  response <- with_mock(
    `httr::GET` = function(url, query, header , userAgent)  {
      expect_equal(url,
                   sprintf("https://query.data.world/%s/%s", type, dataset))
      expect_equal(header$headers[["Authorization"]], "Bearer API_TOKEN")
      expect_equal(header$headers[["Accept"]], "text/csv")
      expect_equal(query[["query"]], sqlQuery)
      expect_equal(query[["parameters"]], "key1=\"value1\",?key2=\"1\"^^<http://www.w3.org/2001/XMLSchema#integer>,?key3=\"1\"^^<http://www.w3.org/2001/XMLSchema#decimal>,?key4=\"TRUE\"^^<http://www.w3.org/2001/XMLSchema#boolean>,?key5=\"1.5\"^^<http://www.w3.org/2001/XMLSchema#decimal>")
      expect_equal(userAgent$options$useragent, data.world::userAgent())
      return(successMessageResponseWithContent(mockResponseLocalContentPath, "application/csv"))
    },
    `mime::guess_type` = function(...)
      NULL,
    data.world::query(data.world(token = "API_TOKEN"), dataset = dataset , query = sqlQuery , type = type, queryParameters = queryParameters)
  )
  expect_equal(is.data.frame(response), TRUE)
  expected = read.csv(mockResponseLocalContentPath)
  expect_equal(all(expected == as.data.frame(response)), TRUE)
})


test_that("sql query making the correct HTTR request" , {
  sqlQuery = "SELECT * FROM TableName LIMIT 10"
  type = "sql"
  dataset = "ownerid/datasetid"
  mockResponseLocalContentPath = "resources/file1.csv"
  queryParameters = list("value1", 1L, 1, TRUE, 1.5)
  response <- with_mock(
    `httr::GET` = function(url, query, header , userAgent)  {
      expect_equal(url,
                   sprintf("https://query.data.world/%s/%s", type, dataset))
      expect_equal(header$headers[["Authorization"]], "Bearer API_TOKEN")
      expect_equal(header$headers[["Accept"]], "text/csv")
      expect_equal(query[["query"]], sqlQuery)
      expect_equal(query[["parameters"]], "$data_world_param0=\"value1\",$data_world_param1=\"1\"^^<http://www.w3.org/2001/XMLSchema#integer>,$data_world_param2=\"1\"^^<http://www.w3.org/2001/XMLSchema#decimal>,$data_world_param3=\"TRUE\"^^<http://www.w3.org/2001/XMLSchema#boolean>,$data_world_param4=\"1.5\"^^<http://www.w3.org/2001/XMLSchema#decimal>")
      expect_equal(userAgent$options$useragent, data.world::userAgent())
      return(successMessageResponseWithContent(mockResponseLocalContentPath, "application/csv"))
    },
    `mime::guess_type` = function(...)
      NULL,
    data.world::query(data.world(token = "API_TOKEN"), dataset = dataset , query = sqlQuery , type = type, queryParameters = queryParameters)
  )
  expect_equal(is.data.frame(response), TRUE)
  expected = read.csv(mockResponseLocalContentPath)
  expect_equal(all(expected == as.data.frame(response)), TRUE)
})
