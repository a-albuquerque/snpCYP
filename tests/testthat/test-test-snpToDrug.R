# Test set for drug association


test_that("Unknown isoform", {
  expect_equal(snpToDrug("CYP1A7"), "No drug associated")
})

# This test should validate the presence of omeprazol as one of the drugs
# metabolized by CYP1A2
test_that("Valid isoform", {
  expect_true("omeprazole" %in% unlist(strsplit(snpToDrug("CYP1A2")[[1]], ",")))
})

# This test should validate the presence of cimetidine as one of the drugs
# metabolized by CYP1A2
test_that("Valid isoform", {
  expect_true(" cimetidine" %in% unlist(strsplit(snpToDrug("CYP1A2")[[1]], ",")))
})
