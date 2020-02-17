# test-env_utils.R

# set_pkg_environment ----
test_that("set_pkg_environment creates environment", {
  set_pkg_environment(force_new = TRUE)
  expect_type(matools_env, "environment")
  expect_length(matools_env, 0)
})

test_that("user can assign object to matools environment", {
  set_pkg_environment(force_new = TRUE)

  matools_env$TEST <- TRUE
  expect_equal(ls(matools_env), "TEST")
  expect_length(matools_env, 1)
})

test_that("set_pkg_environment resets environment", {
  expect_equal(ls(matools_env), "TEST")
  expect_length(matools_env, 1)

  set_pkg_environment(force_new = TRUE)
  expect_length(matools_env, 0)
})

# reset_pkg_environment ----
test_that("reset_pkg_environment test setup", {
  # note this test is only to setup the following test: "reset_pkg_environment resets environment"
  set_pkg_environment(force_new = TRUE)

  matools_env$TEST <- TRUE
  expect_equal(ls(matools_env), "TEST")
  expect_length(matools_env, 1)
})

test_that("reset_pkg_environment resets environment", {
  expect_equal(ls(matools_env), "TEST")
  expect_length(matools_env, 1)

  reset_pkg_environment()
  expect_length(matools_env, 0)
})
