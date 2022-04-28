
library(dycdtools)

test_that("change input file works", {

  fil <- tempfile(fileext = ".data")
  cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
      sep = "\n")

  change_input_file(input_file = fil,
                    row_no = 2,
                    new_value = 0.9)


  read.input<-function(input){
    par_data <- readLines(input)
    text<-unlist(strsplit(par_data[2], split = " "))[1]
    return(text)
  }

  expect_match(read.input(fil),
               "0.9")
})

