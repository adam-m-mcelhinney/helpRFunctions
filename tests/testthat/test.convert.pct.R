# Add the library statement here. For some reason this isnt working currently.

context('Test bucketize.cont')

test_that('bucketize.cont returns the correct amount of values.', { 
    # TODO: Figure out how to hide this warning that is created
    expect_equal(convert.pct(c("10%", "20.5%", "foo", "bar", NA))
                 , c(.1, .205, NA, NA, NA))
}
) 

