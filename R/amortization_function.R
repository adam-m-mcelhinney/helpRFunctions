library(roxygen2)
library(nleqslv)

#'Create the amortization schedule with arbitrary installment due dates.
#'
#'@param principal. The initial loan amount.
#'@param interest_rate. The gross fee percent of the loan. 
#'@param funding_date. The start date of an installment loan in the format of m/d/y.
#'@param last_due_date. The last payment date of the loan in the format of m/d/y.
#'@param installment_due_dates.  A list of due dates for each installment.The first date in this list is the funding date.
#'@example
#'principal=c(1000,1500)
#'interest_rate=c(0.4,0.3)
#'funding_date=c(1/1/2014, 1/5/2014)
#'last_due_date=c(6/1/2014, 1/5/2015)
#'installment_due_dates[1,]=c('1/1/2014','2/1/2014','3/15/2014','5/1/2014, '6/1/2014')
#'installment_due_dates[2,]=c('1/5/2014','3/12/2014','6/2/2014','9/1/2014, '1/5/2015')
#'amortize(principal, interest_rate, funding_date, last_due_date, installment_due_dates)


amortize <- function(principal
                     , interest_rate
                     , funding_date
                     , last_due_date
                     , installment_due_dates) {
  
  
  # total number of days in the installment loan   
  loan_length<-as.Date(as.character(last_due_date), format="%m/%d/%Y")-as.Date(as.character(funding_date), format="%m/%d/%Y")

  # daily interest rate

  daily_i= interest_rate/365 
  
  # number of installments

  num_installment=vector(mode='numeric', length=length(principal))
   
  for(i in 1:length(principal)){
    num_installment[i]=length(installment_due_dates[i,])-1
   }
  
  
  # duration of each installment

  num_days=matrix(nrow=length(principal), ncol=max(num_installment))
  
   for (i in 1:length(principal)) { 
     for (j in 2: max(num_installment+1)) 
       num_days[i,j-1]=c(as.Date(as.character(installment_due_dates[i,j]),format="%m/%d/%Y")-
                        as.Date(as.character(installment_due_dates[i,j-1]), format="%m/%d/%Y"))
   }
 
  #interest rate in each installment = daily interest rate * number of days 
  
  interest_rate_per_inst=matrix(nrow=length(principal), ncol=max(num_installment))
  
   for (i in 1:length(principal)) { 
    for (j in 1: max(num_installment)) 
     interest_rate_per_inst[i,j]=num_days[i,j]*daily_i[i]
   }
 
  
  #the solver function looks for the best payment amount which will make the remaining balance after the last payment as close to 0 as possible
  
  interest_payment=matrix(nrow=length(principal), ncol=max(num_installment))
  principal_payment=matrix(nrow=length(principal), ncol=max(num_installment))
  paid_principal=matrix(nrow=length(principal), ncol=max(num_installment))
  paid_interest=matrix(nrow=length(principal), ncol=max(num_installment))
  remaining_principal=matrix(nrow=length(principal), ncol=max(num_installment))
  payment=vector(mode='numeric', length=length(principal))
  
  
  solver_for_payment=function(payment) {
   
    
    #the first interest payment is calculated seperately as a starting point for the remaining amortization schedule
    
    interest_payment[,1]=principal[1]*interest_rate_per_inst[,1]
    principal_payment[,1]=payment-interest_payment[,1]
    paid_principal[,1]=principal_payment[,1]
    paid_interest[,1]=interest_payment[,1]
    remaining_principal[,1]=principal[1]-paid_principal[,1]
    
    
    for(j in 2:max(num_installment)){
      for (i in 1:length(principal)){
        
        # interest paid during each installment= interest rate * remaining principal from last installment
        interest_payment[i,j]=(principal[i]-paid_principal[i,(j-1)])*interest_rate_per_inst[i,j]
        
        # principal payment during each installment=payment-interest payment
        principal_payment[i,j]= payment[i]-interest_payment[i,j]
        
        # paid principal is the cumulative principal payment up to and include the ith installment.
        # It is used as the intermediate step for calculating the interest generated for the next installment
        paid_principal[i,j]=  sum(principal_payment[i,1:j])
        
        #paid interest is the cumulative interest paid during the loan lifetime. 
        paid_interest[i,j]=sum(interest_payment[i,1:j])
        
        #remaining_principal
        remaining_principal[i,j]= principal[i]-paid_principal[i,j]
      }
    }
    remaining_principal[,j]
  }
  
  payment_start=vector(mode='numeric', length=length(principal))
  x=vector(mode='numeric', length=length(principal))
 
  #assign a starting value to the recursive function.
  payment_start=principal/num_installment
  
  #calculate the payment amount for each loan
  payment=nleqslv(payment_start, solver_for_payment)$x
  
  #calculate the total payment (principal and interest combined)
  total_payment=payment*num_installment
  
  #Compute interest payment and principal payment on each installment
  interest_payment=matrix(nrow=length(principal), ncol=max(num_installment))
  principal_payment=matrix(nrow=length(principal), ncol=max(num_installment))
  paid_principal=matrix(nrow=length(principal), ncol=max(num_installment))
  paid_interest=matrix(nrow=length(principal), ncol=max(num_installment))
  remaining_principal=matrix(nrow=length(principal), ncol=max(num_installment))
  remaining_balance=matrix(nrow=length(principal), ncol=max(num_installment))
  
  #the first interest payment is calculated seperately as a starting point for the remaining amortization schedule  
  interest_payment[,1]=principal[1]*interest_rate_per_inst[,1]
  principal_payment[,1]=payment-interest_payment[,1]
  paid_principal[,1]=principal_payment[,1]
  paid_interest[,1]=interest_payment[,1]
  remaining_principal[,1]=principal[1]-paid_principal[,1]
  remaining_balance[,1]=total_payment[1]-paid_principal[,1]-paid_interest[,1]
  
  
   for(j in 2:max(num_installment)){
     for (i in 1:length(principal)){
      
      # interest paid during each installment= interest rate * remaining principal from last installment
      interest_payment[i,j]=(principal[i]-paid_principal[i,(j-1)])*interest_rate_per_inst[i,j]
      
      # principal payment during each installment=payment-interest payment
      principal_payment[i,j]= payment[i]-interest_payment[i,j]
      
      # paid principal is the cumulative principal payment up to and include the ith installment.
      # It is used as the intermediate step for calculating the interest generated for the next installment
      paid_principal[i,j]=  sum(principal_payment[i,1:j])
      
      #paid interest is the cumulative interest paid during the loan lifetime. 
      paid_interest[i,j]=sum(interest_payment[i,1:j])
      
      #remaining_principal
      remaining_principal[i,j]= principal[i]-paid_principal[i,j]
      
      #remaining_balance
      remaining_balance[i,j]=total_payment[i]-paid_principal[i,j]-paid_interest[i,j]
      
     }
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

