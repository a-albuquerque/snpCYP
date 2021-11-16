test_that("Unknown isoform", {
  expect_equal(snpToDrug("CYP1A7"), "No drug associated")
})

test_that("Valid isoform", {
  expect_true("omeprazole" %in% unlist(strsplit(snpToDrug("CYP1A2")[[1]], ",")))
})

test_that("Valid isoform", {
  expect_true(" cimetidine" %in% unlist(strsplit(snpToDrug("CYP1A2")[[1]], ",")))
})
