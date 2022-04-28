
library(dycdtools)

test_that('delete space works',{
  value <- "    test_string"
  extract_val <- unlist(strsplit(value, split = " "))
  extract_val <- delete_space(extract_val)

  expect_equal(extract_val, "test_string")
})
