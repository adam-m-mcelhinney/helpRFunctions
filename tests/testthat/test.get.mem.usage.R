# Add the library statement here. For some reason this isnt working currently.


context('Test get.mem.usage')



test_that('Ensure that get.mem.usage catches invalid inputs.', { 
  expect_error(get.mem.usage(sort = 'invalid value'))
  expect_error(get.mem.usage(limit = 'invalid value'))
}
) 

test_that('Ensure that get.mem.usage returns proper results.', {
  g <- get.mem.usage(limit = 5)
  expect_less_than(length(g), 5 + 1)
  h <- get.mem.usage(min.display.value = 10)
  expect_more_than(max(h), 10) 
}
) 