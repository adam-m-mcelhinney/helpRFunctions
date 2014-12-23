library(roxygen2)
library(nleqslv)

#'Create the amortization schedule with arbitrary installment due dates.
#'
#'@param principal. The initial loan amount.
#'@param interest_rate. The gross fee percent of the loan. 
#'@param funding_date. The start date of an installment loan
#'@param last_due_date. The last payment date of the loan.
#'@param installment_due_dates.  A list of due dates for each installment.The first date in this list is the funding date.
#'@param num_installment.  Total number of installments.
#'@example
#'
#'amortize (1000,1.4,"2014/01/05","2014/06/30",c("2014/01/05","2014/01/31","2014/02/28","2014/03/31","2014/04/30","2014/05/31","2014/06/30"))

amortize <- function(principal
                     , interest_rate
                     , funding_date
                     , last_due_date
                     , installment_due_dates) {
  
  
  # total number of days in the installment loan   
  loan_length<-as.Date(as.character(last_due_date), format="%Y/%m/%d")-as.Date(as.character(funding_date), format="%Y/%m/%d")
  
  # daily interest rate
  daily_i= interest_rate/365 
  
  # number of installments
  num_installment=length(installment_due_dates)-1
  
  # duration of each installment
  num_days=NULL
  for(i in 1:num_installment+1){
    num_days[i-1]=c(as.Date(as.character(installment_due_dates[(i)]), format="%Y/%m/%d")-
                      as.Date(as.character(installment_due_dates[i-1]), format="%Y/%m/%d"))
  }
  
  # if the sum of durations of each installment does not equal to the loan length, stop and check your funding date and due date.
  if(loan_length!=sum(num_days)) stop ("Loan length differ. Check funding date and due date.")
  
  #interest rate in each installment = daily interest rate * number of days 
  interest_rate_per_inst=NULL
  for(i in 1:num_installment){
    interest_rate_per_inst[i]=num_days[i]*daily_i
  }
  
  
  #the solver function looks for the best payment amount which will make the remaining balance after the last payment as close to 0 as possible
  
  solver_for_payment=function(payment) {
    
    interest_payment=vector(mode='numeric',length=num_installment)
    principal_payment=vector(mode='numeric',length=num_installment)
    paid_principal=vector(mode='numeric',length=num_installment)
    paid_interest=vector(mode='numeric',length=num_installment)
    remaining_principal=vector(mode='numeric',length=num_installment)

    interest_payment=NULL
    principal_payment=NULL
    paid_principal=NULL
    paid_interest=NULL
    remaining_principal=NULL

    
    #the first interest payment is calculated seperately as a starting point for the remaining amortization schedule
    
    interest_payment[1]=principal*interest_rate_per_inst[1]
    principal_payment[1]=payment-interest_payment[1]
    paid_principal[1]=principal_payment[1]
    paid_interest[1]=interest_payment[1]
    remaining_principal[1]=principal-paid_principal[1]

    
    for(i in 2:num_installment){
      
      # interest paid during each installment= interest rate * remaining principal from last installment
      interest_payment[i]=(principal-paid_principal[(i-1)])*interest_rate_per_inst[i]
      
      # principal payment during each installment=payment-interest payment
      principal_payment[i]= payment-interest_payment[i]
      
      # paid principal is the cumulative principal payment up to and include the ith installment.
      # It is used as the intermediate step for calculating the interest generated for the next installment
      paid_principal[i]=  sum(principal_payment[1:i])
      
      #paid interest is the cumulative interest paid during the loan lifetime. 
      paid_interest[i]=sum(interest_payment[1:i])
      
      #remaining_principal
      remaining_principal[i]= principal-paid_principal[i]
    }
    
    remaining_principal[i]
  }
  #assign a starting value to the recursive function.
  payment_start=principal/num_installment
  remaining_principal_start=solver_for_payment(payment_start)
  
  #solve for the payment given the starting guess
  a=nleqslv(payment_start, solver_for_payment)
  payment=a$x
  
  
  # total expected payment 
  total_payment<-payment*num_installment
  
  
  #Compute interest payment and principal payment on each installment
  interest_payment=NULL
  principal_payment=NULL
  paid_principal=NULL
  paid_interest=NULL
  remaining_principal=NULL
  remaining_balance=NULL
  
  
  #the first interest payment is calculated seperately as a starting point for the remaining amortization schedule
  
  # define the length before the loop to improve efficiency
  
  interest_payment=vector(mode='numeric',length=num_installment)
  principal_payment=vector(mode='numeric',length=num_installment)
  paid_principal=vector(mode='numeric',length=num_installment)
  paid_interest=vector(mode='numeric',length=num_installment)
  remaining_principal=vector(mode='numeric',length=num_installment)
  remaining_balance=vector(mode='numeric',length=num_installment)
  
  #the first interest payment is calculated seperately as a starting point for the remaining amortization schedule
  interest_payment[1]=principal*interest_rate_per_inst[1]
  principal_payment[1]=payment-interest_payment[1]
  paid_principal[1]=principal_payment[1]
  paid_interest[1]=interest_payment[1]
  remaining_principal[1]=principal-paid_principal[1]
  remaining_balance[1]=payment*(num_installment-1)
  
  for(i in 2:num_installment){
    
    # interest paid during each installment= interest rate * remaining principal from last installment
    interest_payment[i]=(principal-paid_principal[(i-1)])*interest_rate_per_inst[i]
    
    # principal payment during each installment=payment-interest payment
    principal_payment[i]= payment-interest_payment[i]
    
    # paid principal is the cumulative principal payment up to and include the ith installment.
    # It is used as the intermediate step for calculating the interest generated for the next installment
    paid_principal[i]=  sum(principal_payment[1:i])
    
    #paid interest is the cumulative interest paid during the loan lifetime. 
    paid_interest[i]=sum(interest_payment[1:i])
    
    #remaining_principal
    remaining_principal[i]= principal-paid_principal[i]
    
    #remaining_balance included unpaid principal and interest
    remaining_balance[i]=total_payment-paid_principal[i]-paid_interest[i]
  }
  
  
  output <- list(
    
    "payment" = payment,
    "interest payment"=interest_payment,
    "principal payment"=principal_payment,
    "cumulative principal paid"=paid_principal,
    "cumulative interest paid"=paid_interest,
    "outstanding principal"=remaining_principal,
    "outstanding balance"=remaining_balance
    
  )
  
  
  output
}

