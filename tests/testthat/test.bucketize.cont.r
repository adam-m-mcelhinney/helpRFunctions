# Add the library statement here. For some reason this isnt working currently.
library(helpRFunctions)

context('Test bucketize.cont')



test_that('bucketize.cont returns the correct amount of values.', { 
  vals = c(-1.5, 1000.1, .7, NA)
  expect_equal(bucketize.cont(vals, bucket.size = 1), c(-1, 1001, 1, NA))
  expect_equal(bucketize.cont(vals, bucket.size = 10000), c(0, 10000, 10000, NA))
  expect_equal(bucketize.cont(vals, bucket.size = .025), c(-1.5, 1000.1, .7, NA))
  }
  ) 
  
