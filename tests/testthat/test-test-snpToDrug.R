test_that("Unknown isoform", {
  expect_equal(snpToDrug("CYP1A7"), "No drug associated")
})

test_that("Valid isoform", {
  expect_true("omeprazol" %in% snpToDrug("CYP1A2"))
})
