
context("utils")
test_that("require_pkgs", {

  detach("package:scales")
  detach("package:sna")

  expect_false("package:scales" %in% search())
  expect_false("package:sna" %in% search())

  suppressMessages(require_pkgs(c("scales", "sna")))

  expect_true("package:scales" %in% search())
  expect_true("package:sna" %in% search())

  expect_error(suppressWarnings(suppressMessages(require_pkgs("DOES_NOT_EXIST_asdfasdfasfd"))))
})
