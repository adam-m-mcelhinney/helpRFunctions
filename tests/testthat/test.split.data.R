# Add the library statement here. For some reason this isnt working currently.
source('C:/Local_Files/helpRFunctions/R/split.data.R')

context('Test split.data')

df <- data.frame(matrix(rnorm(110), nrow = 11))

test_that('split.data handles seed correctly', {
  t <- split.data(df, seed = 100)
  u <- split.data(df, seed = 100)
  expect_identical(t, u)
}
) 


test_that('split.data handles invalid pcts correctly', {
          expect_error(split.data(df, pcts <- (.6)))
          expect_error(split.data(df, pcts <- c(.6, .40000001)))
          expect_error(split.data(df, pcts <- c(.6, .40000001, 10)))
          
}
) 

test_that('split.data handles invalid data.frames correctly', {
  expect_error(split.data(as.matrix(df)))
  }
)

test_that('split.data handles invalid names.', {
  expect_error(split.data(df, set.names = ('a')))
}
)

test_that('split.data splits rows properly.', {
  t = split.data(df)
  rows <- rep(NA, length(t))
  k = 1
  for (i in t){
    rows[k] <- nrow(i)
    k <- k+1
  }
  
  expect_equal(sum(rows), nrow(df))
}
)