#How to use is included as comment at the end of the function, all you need to do is copy and paste. 
DollarWY<-function(){
  print('This model will help you find the Dollar weighted yield rate j') 
  print('Entries includes Open Balance Deposits Withdraws Closed Balance.') 
  print('Please you will need to pay absolute attention to the prompt.')   
  Y1 <-  as.numeric(readline(prompt=" Enter the YEAR of the Open Balance in format YYYY: "))   
  M1 <-  as.numeric(readline(prompt=" Enter the MONTH of the initial investment account balance MM: "))   
  A <- as.numeric(readline(prompt=" Enter the amount of OPEN BALANCE: "))   
  Y2 <-  as.numeric(readline(prompt=" Enter the YEAR of the Close balance YYYY: "))   
  M2 <-  as.numeric(readline(prompt=" Enter the MONTH of the Close balance MM: "))   
  B <- as.numeric(readline(prompt=" Enter the amount CLOSE BALANCE: "))   
  r = M2-M1   
  s = Y2-Y1      
  if (s==0){     
    p = 12     
    print(paste("The Investment period", p, "Months "))        
  } 
  else if (r!=0){     
    p = ((Y2-Y1)+1)*12 - (abs(M2-M1))     
    print(paste("The Investment period", p, "Months "))        
  } 
  else if (r==0){     
    p = ((Y2-Y1)+1)*12 - 12     
    print(paste("The Investment period ", p, "Months "))        
  } 
  c1 <-  as.numeric(readline(prompt=" HOW MANY DEPOSITS within this period?: "))   
  w1 <-  as.numeric(readline(prompt=" HOW MANY WITHDRAWS within this period?: "))
  
  contr = numeric()   
  witd = numeric()   
  mcontr = numeric()   
  mwitd = numeric()   
  xx = numeric()   
  yy = numeric()   
  i <- 1   
  while (i < c1+1) {     
    x1 <-  as.numeric(readline(prompt=": Enter the DEPOSIT Amount:"))     
    contr <- append(contr,x1)     
    x22 <-  as.numeric(readline(prompt=": Enter the YEAR of this Deposit YYYY:"))     
    x2 <-  as.numeric(readline(prompt=": Enter the MONTH of this Deposit mm:"))     
    mcontr <- append(mcontr,x2)          
    if (x22==Y1){       
      zz = x1*(1-((x2-1)/p))       
      xx <- append(xx,zz)            
    } 
    else if (x22>Y1){       
      pz1 <- ((x22-Y1)*12)+(y2-1)       
      pz1       
      zz = y1*(1-(pz1/p))       
      xx <- append(xx,zz)     
    }     
    #print(x1)     
    i = i+1   
  }   
  print(paste("All the Deposits are/is:"))   
  print(contr)   
  print(paste("And the corresponding months of these Deposits are/is:"))   
  print(mcontr)   
  #print(paste("These Deposits subject to their corresponding months are:"))   
  #print(xx)      
  j <- 1   
  while (j < w1+1) {     
    y1 <-  as.numeric(readline(prompt=": Enter the WITHDRAWALS without negative sign:"))
    witd <- append(witd,y1)     
    y22 <-  as.numeric(readline(prompt=": Enter the YEAR of this withdrawal YYYY:"))     
    y2 <-  as.numeric(readline(prompt=": Enter the MONTH of this withdrawal mm:"))     
    mwitd <- append(mwitd,y2)          
    if (y22==Y1){       
      zz1 = y1*(1-((y2-1)/p))       
      yy <- append(yy,zz1)            
    } 
    else if (y22>Y1){       
      pz <- ((y22-Y1)*12)+(y2-1)       
      pz       
      zz1 = y1*(1-(pz/p))       
      yy <- append(yy,zz1)     
    }          
    j = j+1   
  }   
  print(paste("All the withdrawals are/is:"))   
  print(witd)   
  print(paste("And the corresponding months of these withdrawals are/is:"))   
  print(mwitd)   
  #print(paste("These withdrawals subject to their corresponding months are:"))   
  #print(yy)      
  b1 <- sum(contr, na.rm = FALSE)   
  b2 <- sum(witd, na.rm = FALSE)   
  z = b1 - b2   
  I = B-A-z   
  bB1 <- sum(xx, na.rm = FALSE)   
  bB2 <- sum(yy, na.rm = FALSE)   
  F <- bB1 - bB2   
  j <- I/(A+F)   
  print(paste("The Approximate Annual Dollar-Weighted Yield Rate is, j = ", j)) 
  #### DollarWY() 
}  
