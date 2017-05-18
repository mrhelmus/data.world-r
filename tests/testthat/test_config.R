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

test_that("Config can be changed using runtime variables.", {
  runtime_cfg <- data.world::cfg(auth_token = "RUNTIME_TOKEN")
  data.world::set_config(runtime_cfg)
  testthat::expect_equal(getOption("dw.auth_token"), "RUNTIME_TOKEN")
})

test_that("Config can be changed using default environment variables.", {
  Sys.setenv(DW_AUTH_TOKEN = "DEFAULT_ENV_VAR_TOKEN")
  environment_cfg <- data.world::cfg_env()
  data.world::set_config(environment_cfg)
  testthat::expect_equal(getOption("dw.auth_token"), "DEFAULT_ENV_VAR_TOKEN")
})

test_that("Config can be changed using custom environment variables.", {
  Sys.setenv(MY_TEST_VAR = "CUSTOM_ENV_VAR_TOKEN")
  environment_cfg <- data.world::cfg_env(auth_token_var = "MY_TEST_VAR")
  data.world::set_config(environment_cfg)
  testthat::expect_equal(getOption("dw.auth_token"), "CUSTOM_ENV_VAR_TOKEN")
})

test_that("Config can be changed using configuration file.", {
  options(dw.config_path = "resources/single_profile_config")
  data.world::set_config(data.world::cfg_saved())
  testthat::expect_equal(getOption("dw.auth_token"), "DEFAULT_SAVED_TOKEN")
})

test_that("Config can be changed using configuration file and multiple profiles.", {

})

test_that("Config won't change using configuration file that does not exist.", {
  auth_token <- getOption("dw.auth_token")
  options(dw.config_path = "no__a_valid____file")
  testthat::expect_warning(data.world::set_config(data.world::cfg_saved()), regexp = NULL)
  testthat::expect_equal(getOption("dw.auth_token"), auth_token)
})

test_that("Config can be persisted to file.", {

})

test_that("Config profiles can be persisted to file.", {

})
