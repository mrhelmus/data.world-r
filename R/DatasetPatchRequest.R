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

#' DatasetPatchRequest per https://api.data.world/v0/swagger.json
#' @param description Short description for dataset.
#' @param summary Dataset summary, markdown supported
#' @param tags Array of strings to tag datase
#' @param license Public Domain, PDDL, CC-0, CC-BY, ODC-BY, CC-BY-SA, ODC-ODbL, CC BY-NC-SA
#' @param visibility PRIVATE or OPEN
#' @param files list of \code{\link{FileCreateOrUpdateRequest}}
#' @examples
#' request <- data.world::DatasetPatchRequest(description = "description", summary = "summary",
#'    tags = list("sdk"), license = "Public Domain")
#' @export
DatasetPatchRequest <- function(description =  NULL, summary = NULL, tags = NULL, license = NULL, visibility = NULL, files = NULL) {
  is.nothing <- function(x) is.null(x) || is.na(x)
  me <- list()
  if (!is.nothing(visibility)) {
    me$visibility <- visibility
  }
  if (!is.nothing(description)) {
    me$description <- description
  }
  if (!is.nothing(summary)) {
    me$summary <- summary
  }
  if (!is.nothing(tags)) {
    me$tags <- tags
  }
  if (!is.nothing(license)) {
    me$license <- license
  }
  if (!is.null(files)) {
    me$files <- files
  }
  class(me) <- "DatasetPatchRequest"
  return(me)
}
