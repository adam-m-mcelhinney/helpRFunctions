# Right now I am pointing the source to my folder to make sure the unit test works.

source('K:\\Business Analytics\\Private\\Nan Xia\\2014-12-16 R helper function\\amortization function\\amortization function v2.R')
principal=c(1000,1500)
interest_rate=c(0.4,0.3)
funding_date=c(1/1/2014, 1/5/2014)
last_due_date=c(6/1/2014, 1/5/2015)
installment_due_dates=matrix(nrow=2, ncol=5)
installment_due_dates[1,]=c('1/1/2014','2/1/2014','3/15/2014','5/1/2014', '6/1/2014')
installment_due_dates[2,]=c('1/5/2014','3/12/2014','6/2/2014','9/1/2014', '1/5/2015')
test_that('amortization.function returns the correct payment amount.', { 
  
  expect_equal(amortize(principal, interest_rate, funding_date, last_due_date, installment_due_dates)$payment
               , c(276.1336, 431.9290))
}
) 
