library(boot)
library(ggplot2)
library(quadprog)
library(xts)
library(zoo)
library(timeSeries)
library(fGarch)
library(ccgarch)

## new lines
out.mx.big.gradual <- matrix(0, 1300, 7)
colnames(out.mx.big.gradual) <- c("DJ", "oil", "NASDAQ", "gas", "REIT", "qr", "sim.no")
out.mx.big.gradual <- data.frame(out.mx.big.gradual)
out.mx.big.gradual$qr <- rep(1:13, 100)
out.mx.big.gradual$sim.no <- sort(rep(1:100, 13))
m <- 1

#For the result of using historical covariance matrix
# out.mx.hist.risk <- matrix(0, 13, 7)
# colnames(out.mx.hist.risk) <- c("DJ", "oil", "NASDAQ", "gas", "REIT", "qr", "sim.no")
# out.mx.hist.risk <- data.frame(out.mx.hist.risk)
# out.mx.hist.risk$qr <- 1:13
# out.mx.hist.risk$sim.no <- factor(rep(1, 13))
# m <- 1

out.mx.big.gradual <- read.csv("outMxBigGradual.csv", row.names = 1)
m <- min((1:1300)[out.mx.big.gradual[,2]==0])
first.bb <- 1 + (m - 1) %/% 13

for(bb in first.bb:100) {
# for (bb in 1:1) { # for historical risk

  
#Start to build up the dataset.
dates <- DJIA_10y$DATE
#Convert factor to date
dates <- as.Date(dates)
#Delete the last day because we cannot compute the return of last day
dates <- head(dates,-1)

values_DJIA <- DJIA_10y$VALUE
#Convert factor to numeric
values_DJIA <- as.numeric(levels(values_DJIA))[values_DJIA]
return_DJIA <- diff(log(values_DJIA))

#Construct time series xts object
data <- as.xts(return_DJIA,order.by = as.Date(dates,"%d/%m/%Y"))
names(data)[1] <- "DJIA"

#input the data of oil, nasdaq index, and natural gas
values_oil <- DCOILWTICO_10y$VALUE
values_nasdaq <- NASDAQ100_10y$VALUE
values_gas <- DHHNGSP_10y$VALUE
values_reits <- WILLREITIND_10y$VALUE
values_bond <- TBond$VALUE

values_oil <- as.numeric(levels(values_oil))[values_oil]
values_nasdaq <- as.numeric(levels(values_nasdaq))[values_nasdaq]
values_gas <- as.numeric(levels(values_gas))[values_gas]
values_reits <- as.numeric(levels(values_reits))[values_reits]
values_bond <- as.numeric(levels(values_bond))[values_bond]


return_oil <- diff(log(values_oil))
return_nasdaq <- diff(log(values_nasdaq))
return_gas <- diff(log(values_gas))
return_reits <- diff(log(values_reits))
return_bond <- (1+values_bond/100)^(1/365)-1

#construct time series
data <- transform(data,oil = return_oil)
data <- transform(data,nasdaq = return_nasdaq)
data <- transform(data,gas = return_gas)
data <- transform(data,reits = return_reits)
data <- transform(data,bond = return_bond)


#convert it to object for fPortfolio
data_ts <- as.timeSeries(data)

#Remove NAs
data_port <- na.omit(data_ts)

#data_port has 2409 daily observations
#Use 1200(5 years) as starting base, increment is 93(3 months), 13 intervals of integers
tangentpoints <- c()
moneypoints <- c()

DJIA_money <- c()
DJIA_share <- c()
DJIA_buy_price <- c()
DJIA_profit <- c()

DJIA_money[1] <- 0
DJIA_share[1] <- 0
DJIA_buy_price[1] <- 0
#############################################################
oil_money <- c()
oil_share <- c()
oil_buy_price <- c()
oil_profit <- c()

oil_money[1] <- 0
oil_share[1] <- 0
oil_buy_price[1] <- 0
#############################################################
nasdaq_money <- c()
nasdaq_share <- c()
nasdaq_buy_price <- c()
nasdaq_profit <- c()

nasdaq_money[1] <- 0
nasdaq_share[1] <- 0
nasdaq_buy_price[1] <- 0
#############################################################
reits_money <- c()
reits_share <- c()
reits_buy_price <- c()
reits_profit <- c()

reits_money[1] <- 0
reits_share[1] <- 0
reits_buy_price[1] <- 0
#############################################################
gas_money <- c()
gas_share <- c()
gas_buy_price <- c()
gas_profit <- c()

gas_money[1] <- 0
gas_share[1] <- 0
gas_buy_price[1] <- 0
#############################################################
# bond_money <- c()
# bond_profit <- c()
# 
# bond_money[1] <- 0
#############################################################


tax <- c()
tax[1] <- 0
DJIA_profit[1]=0
nasdaq_profit[1]=0
oil_profit[1]=0
gas_profit[1]=0
reits_profit[1]=0

#Initial oil price
initialOil <- 65.98
InitialOilshare <- 10000

basis_oil <- c()
basis_oil[1] <- 7
oil_money[1] <- 10000*81.34
oil_share[1] <- 10000
oil_buy_price[1] <- 81.34
## new line
out.mx.big.gradual[m, 1:5] <- c(0, oil_money[1], 0, 0, 0); m <- m+1

#For the result of using historical covariance matrix
# out.mx.hist.risk[m, 1:5] <- c(0, oil_money[1], 0, 0, 0); m <- m+1


basis_DJIA <- c()
basis_nasdaq <- c()
basis_gas <- c()
basis_reits <- c()


  

for(i in 2:13)
{
  

  j <- (1:nrow(DJIA_10y))[rownames(data_ts) == rownames(data_port[(1200 + (i-2)*93),0])]
  print(paste("Start of loop for i =", i))
  print(paste("Value to reallocate is ", DJIA_share[i-1]*values_DJIA[j] + oil_share[i-1]*values_oil[j] + nasdaq_share[i-1]*values_nasdaq[j] + gas_share[i-1]*values_gas[j] + reits_share[i-1]*values_reits[j]))
  money <- DJIA_share[i-1]*values_DJIA[j] + oil_share[i-1]*values_oil[j] + nasdaq_share[i-1]*values_nasdaq[j] + gas_share[i-1]*values_gas[j] + reits_share[i-1]*values_reits[j]
  
  ##Moving window
  all <- data_port[(1 + (i-2)*93):(1200 + (i-2)*93),]
  riskfree <- mean(all[,6])
  trainset <- data_port[(1 + (i-2)*93):(1200 + (i-2)*93),1:5]
#   Risk.mx <- cov(trainset)
  Return.v <- apply(trainset, 2, mean)
  
  ########################Multivariate Garch################################
  fitD <- garchFit(data = trainset$DJIA)
  fitO <- garchFit(data = trainset$oil)
  fitN <- garchFit(data = trainset$nasdaq)
  fitG <- garchFit(data = trainset$gas)
  fitR <- garchFit(data = trainset$reits)

  A <- diag(x = c(as.numeric(fitD@fit$coef["alpha1"]),as.numeric(fitO@fit$coef["alpha1"]),as.numeric(fitN@fit$coef["alpha1"]),as.numeric(fitG@fit$coef["alpha1"]),as.numeric(fitR@fit$coef["alpha1"])),
           nrow = 5, ncol = 5)

  B <- diag(x = c(as.numeric(fitD@fit$coef["beta1"]),as.numeric(fitO@fit$coef["beta1"]),as.numeric(fitN@fit$coef["beta1"]),as.numeric(fitG@fit$coef["beta1"]),as.numeric(fitR@fit$coef["beta1"])),
           nrow = 5, ncol = 5)
  R <- cov2cor(cov(trainset))
  a <- matrix(data = c(as.numeric(fitD@fit$coef["omega"]),as.numeric(fitO@fit$coef["omega"]),as.numeric(fitN@fit$coef["omega"]),as.numeric(fitG@fit$coef["omega"]),as.numeric(fitR@fit$coef["omega"])),
              nrow = 5, ncol = 1)

  estimation <- eccc.estimation(a,A,B,R,trainset,model = "diagonal")

  simulation <- eccc.sim(nobs = 100,estimation$para.mat$a,estimation$para.mat$A,estimation$para.mat$B,estimation$para.mat$R,model = "diagonal")
  simulated_DJIA <- fitD@fit$par[1] + simulation$eps[,1]
  simulated_oil <- fitO@fit$par[1] + simulation$eps[,2]
  simulated_nasdaq <- fitN@fit$par[1] + simulation$eps[,3]
  simulated_gas <- fitG@fit$par[1] + simulation$eps[,4]
  simulated_reits <- fitR@fit$par[1] + simulation$eps[,5]

  forecast <- cbind(simulated_DJIA,simulated_oil,simulated_nasdaq,simulated_gas,simulated_reits)

  Risk.mx <- cov(forecast)

  
#   h: a matrix of the simulated conditional variances (T \times N)
#   eps: a matrix of the simulated time series with (E)CCC-GARCH process (T \times N)
################################################################################################################  
  

  
  
####################################################################################
  A <- matrix(1,1,5)
  B <- matrix(data = c(0,1,0,0,0),1,5)
  D <- diag(5)
  Amat <- rbind(A,B,D,-D)

  f <- c(1,0.9 - (i-2)*0.1,rep(-1, 5),rep(-1, 5))
  #For immediate selloff we use the constraints
  # f <- c(1,rep(-1, 5),rep(-1, 5))
  
  sol <- solve.QP(Dmat=Risk.mx,dvec = rep(0,5), Amat=t(Amat), bvec=f, meq=1)
  
  
#   #Not allow short selling
#   D <- rbind(A,B,diag(5),-diag(5))
#   f <- c(1,0.9,rep(0,5),rep(-1,5))
  
return.at.min.risk <- sum(sol$solution*Return.v)


#Use linea programming to find the highest possible return, in selloff strategy remove the constraints of the selling limit.
a <- c(Return.v[1], -Return.v[1], Return.v[2], Return.v[3], -Return.v[3],
       Return.v[4], -Return.v[4], Return.v[5], -Return.v[5])

A1 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 1, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 1, -1, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 1, -1, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 1, -1), 5, 9, byrow=T)
b1 <- c(1, 1, 1, 1, 1)
A2 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 1, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 1, -1, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 1, -1, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 1, -1), 5, 9, byrow=T)
b2 <- c(-1, 0.9 - (i-2)*0.1, -1, -1, -1)
A3 <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), 1, 9, byrow=T)
b3 <- 1

bang<-simplex(a, A1, b1, A2, b2, A3, b3, maxi=T)

Return.seq <- seq(sum(sol$solution*Return.v), bang$value*0.9, length.out=250)
E <- matrix(data=Return.v, 1, 5)
Amat <- rbind(A,B,D,-D,E)
plot.mx <- matrix(0, length(Return.seq), 3)
colnames(plot.mx) <- c("Risk", "Return", "Sharpe.ratio")
plot.mx[1,] <- c(sqrt(sol$value), sum(sol$solution*Return.v),
                 (sum(sol$solution*Return.v)-riskfree)/sqrt(sol$value))

for (k in 1:length(Return.seq)) 
{
#  print(k)
  f <- c(1,0.9 - (i-2)*0.1,rep(-1, 5),rep(-1, 5), Return.seq[k])
  sol <- solve.QP(Dmat=Risk.mx,dvec = rep(0,5), Amat=t(Amat), bvec=f, meq=1)
  plot.mx[k,] <- c(sqrt(sol$value), sum(sol$solution*Return.v),
                   (sum(sol$solution*Return.v)-riskfree)/sqrt(sol$value))
}

plot.df <- data.frame(plot.mx)
Risk.free.df <- data.frame(Risk=0,Return=riskfree)

Sharpe.ratio.df <- data.frame(rbind(Risk.free.df,
                                    plot.df[(1:nrow(plot.df))[plot.df$Sharpe.ratio==max(plot.df$Sharpe.ratio)],][c("Risk","Return")]))

    

ggplot(plot.df, aes(x=Risk, y=Return)) + geom_line() + theme_bw() +
  geom_point(data=Risk.free.df, aes(x=Risk, y=Return)) +
  geom_line(data=Sharpe.ratio.df, aes(x=Risk, y=Return), colour="red")


find.best.sharpe.ratio <- function(Return.param) {
  f <- c(1,0.9 - (i-2)*0.1,rep(-1, 5),rep(-1, 5), Return.param/10000)
  sol <- solve.QP(Dmat=Risk.mx,dvec = rep(0,5), Amat=t(Amat), bvec=f, meq=1)
  return((sum(sol$solution*Return.v)-riskfree)/sqrt(sol$value))
}

low.bd <- 0.9*return.at.min.risk+0.1*bang$value
high.bd <- 0.1*return.at.min.risk+0.9*bang$value
thud <- optimize(f=find.best.sharpe.ratio, interval=c(low.bd*10000, high.bd*10000), maximum=T)

best.Return <- thud$maximum/10000
f <- c(1,0.9 - (i-2)*0.1,rep(-1, 5),rep(-1, 5), best.Return)
sol <- solve.QP(Dmat=Risk.mx,dvec = rep(0,5), Amat=t(Amat), bvec=f, meq=1)






#####################################################################################  
  DJIA_money[i]  <- money*sol$solution[1]
  oil_money[i]  <- money*sol$solution[2]
  nasdaq_money[i]  <- money*sol$solution[3]
  gas_money[i]  <- money*sol$solution[4]
  reits_money[i]  <- money*sol$solution[5]
  
  
  
  
  #Search assets prices
  
  j <- (1:nrow(DJIA_10y))[rownames(data_ts) == rownames(data_port[(1200 + (i-2)*93),0])]
  
  DJIA_buy_price[i] <- values_DJIA[j]
  oil_buy_price[i] <- values_oil[j]
  nasdaq_buy_price[i] <- values_nasdaq[j]
  gas_buy_price[i] <- values_gas[j]
  reits_buy_price[i] <- values_reits[j]
  
  DJIA_share[i] <- DJIA_money[i]/DJIA_buy_price[i]
  oil_share[i] <- oil_money[i]/oil_buy_price[i]
  nasdaq_share[i] <- nasdaq_money[i]/nasdaq_buy_price[i]
  gas_share[i] <- gas_money[i]/gas_buy_price[i]
  reits_share[i] <- reits_money[i]/reits_buy_price[i]
  
  
  ###########################################################
   
  
  
#Compute the profit of each asset for tax use.  
#################################################################################  
    profit.f <- function(s_price, s_shares, e_price, e_shares)
    {
      if(e_shares < s_shares && s_shares > 0)
      {
        profit = (s_shares - max(c(0, e_shares)))*(e_price-s_price)
      }
      
      else if(e_shares > s_shares && s_shares < 0)
      {
        profit = (min(c(0, e_shares)) - s_shares)*(s_price - e_price)
      }
      
      else
      {
        profit = 0
      }
      
      profit
      
    }
  ###################################################################################  
  
  
#Tax Adjustments
  if (i == 2)
  {
    basis_DJIA[i-1] <- DJIA_buy_price[i-1]
    basis_nasdaq[i-1] <- nasdaq_buy_price[i-1]
    basis_gas[i-1] <- gas_buy_price[i-1]
    basis_reits[i-1] <- reits_buy_price[i-1]
  }
 
   DJIA_profit[i] <- profit.f(basis_DJIA[i-1],DJIA_share[i-1],DJIA_buy_price[i],DJIA_share[i])
   oil_profit[i] <- profit.f(basis_oil[i-1],oil_share[i-1],oil_buy_price[i],oil_share[i]) ##basis of oil 10% of market price
   nasdaq_profit[i] <- profit.f(basis_nasdaq[i-1],nasdaq_share[i-1],nasdaq_buy_price[i],nasdaq_share[i])
   gas_profit[i] <- profit.f(basis_gas[i-1],gas_share[i-1],gas_buy_price[i],gas_share[i])
   reits_profit[i] <- profit.f(basis_reits[i-1],reits_share[i-1],reits_buy_price[i],reits_share[i])
   
   tax[i] <- (DJIA_profit[i] + nasdaq_profit[i] + gas_profit[i] + oil_profit[i] + reits_profit[i])*0.25
   
   taxA <- matrix(nrow = 6, ncol = 5)
   
   taxA[1,] = c(DJIA_buy_price[i]*(1-sol$solution[1]),-sol$solution[1]*oil_buy_price[i],
                -sol$solution[1]*nasdaq_buy_price[i], -sol$solution[1]*gas_buy_price[i],
                -sol$solution[1]*reits_buy_price[i])
   
   taxA[2,] = c(-sol$solution[2]*DJIA_buy_price[i],oil_buy_price[i]*(1-sol$solution[2]),
                -sol$solution[2]*nasdaq_buy_price[i], -sol$solution[2]*gas_buy_price[i],
                -sol$solution[2]*reits_buy_price[i])
   
   taxA[3,] = c(-sol$solution[3]*DJIA_buy_price[i],
                -sol$solution[3]*oil_buy_price[i],nasdaq_buy_price[i]*(1-sol$solution[3]),
                -sol$solution[3]*gas_buy_price[i],-sol$solution[3]*reits_buy_price[i])
   
   taxA[4,] = c(-sol$solution[4]*DJIA_buy_price[i],-sol$solution[4]*oil_buy_price[i],
                -sol$solution[4]*nasdaq_buy_price[i],gas_buy_price[i]*(1-sol$solution[4]),
                -sol$solution[4]*reits_buy_price[i])
   
   taxA[5,] = c(-sol$solution[5]*DJIA_buy_price[i],-sol$solution[5]*oil_buy_price[i],
                -sol$solution[5]*nasdaq_buy_price[i],-sol$solution[5]*gas_buy_price[i],
                reits_buy_price[i]*(1-sol$solution[5]))
   
   taxA[6,] = c(DJIA_buy_price[i],oil_buy_price[i],nasdaq_buy_price[i],gas_buy_price[i],reits_buy_price[i])
   
   afterTax <- DJIA_buy_price[i]*DJIA_share[i]+oil_buy_price[i]*oil_share[i]+nasdaq_buy_price[i]*nasdaq_share[i]+
     gas_buy_price[i]*gas_share[i]+reits_buy_price[i]*reits_share[i] - tax[i]
   
   
   taxb = matrix(c(0,0,0,0,0,afterTax),nrow = 1, ncol = 6)
   
   
  endshares <- qr.solve(taxA,t(taxb))
  
  #Updates share with its real shares
  DJIA_share[i] <- endshares[1]
  oil_share[i] <- endshares[2]
  nasdaq_share[i] <- endshares[3]
  gas_share[i] <- endshares[4]
  reits_share[i] <- endshares[5]
 #Calculating basis

  if(abs(DJIA_share[i]) > abs(DJIA_share[i-1]) && prod(DJIA_share[i],DJIA_share[i-1]) > 0)
  {
    basis_DJIA[i] <- (DJIA_share[i-1]*DJIA_buy_price[i-1]+(DJIA_share[i]-DJIA_share[i-1])*DJIA_buy_price[i])/(DJIA_share[i])
  }else if(prod(DJIA_share[i],DJIA_share[i-1]) <= 0)
  {
    basis_DJIA[i] <- DJIA_buy_price[i]
  }else
  {
    basis_DJIA[i] <- basis_DJIA[i-1]
  }
  
#################################  
  if(abs(oil_share[i]) > abs(oil_share[i-1]) && prod(oil_share[i],oil_share[i-1]) > 0)
  {
    basis_oil[i] <- (oil_share[i-1]*oil_buy_price[i-1]+(oil_share[i]-oil_share[i-1])*oil_buy_price[i])/(oil_share[i])
  }else if(prod(oil_share[i],oil_share[i-1]) <= 0)
  {
    basis_oil[i] <- oil_buy_price[i]
  }else
  {
    basis_oil[i] <- basis_oil[i-1]
  }
################################################################################
  if(abs(nasdaq_share[i]) > abs(nasdaq_share[i-1]) && prod(nasdaq_share[i],nasdaq_share[i-1]) > 0)
  {
    basis_nasdaq[i] <- (nasdaq_share[i-1]*nasdaq_buy_price[i-1]+(nasdaq_share[i]-nasdaq_share[i-1])*nasdaq_buy_price[i])/(nasdaq_share[i])
  }else if(prod(nasdaq_share[i],nasdaq_share[i-1]) <= 0)
  {
    basis_nasdaq[i] <- nasdaq_buy_price[i]
  }else
  {
    basis_nasdaq[i]<- basis_nasdaq[i-1]
  }
###############################################################################
  if(abs(gas_share[i]) > abs(gas_share[i-1]) && prod(gas_share[i],gas_share[i-1]) > 0)
  {
    basis_gas[i] <- (gas_share[i-1]*gas_buy_price[i-1]+(gas_share[i]-gas_share[i-1])*gas_buy_price[i])/(gas_share[i])
  }else if(prod(gas_share[i],gas_share[i-1]) <= 0)
  {
    basis_gas[i] <- gas_buy_price[i]
  }else
  {
    basis_gas[i] <- basis_gas[i-1]
  }
#################################################################################
  if(abs(reits_share[i]) > abs(reits_share[i-1]) && prod(reits_share[i],reits_share[i-1]) > 0)
  {
    basis_reits[i] <- (reits_share[i-1]*reits_buy_price[i-1]+(reits_share[i]-reits_share[i-1])*reits_buy_price[i])/(reits_share[i])
  }else if(prod(reits_share[i],reits_share[i-1]) <= 0)
  {
    basis_reits[i] <- reits_buy_price[i]
  }else
  {
    basis_reits[i] <- basis_reits[i-1]
  }
  
  
    
  
   ###############################################################################################################

  
  
  #########################################################
  #search the asset price after 3 months
  k <- (1:nrow(DJIA_10y))[rownames(data_ts) == rownames(data_port[(1200 + (i-1)*93),0])]
  
  DJIA_sell_price <- values_DJIA[k]
  gas_sell_price <- values_gas[k]
  nasdaq_sell_price <- values_nasdaq[k]
  oil_sell_price <- values_oil[k]
  reits_sell_price <- values_reits[k]
  
  moneypoints[i] = DJIA_sell_price*endshares[1]+oil_sell_price*endshares[2]+nasdaq_sell_price*endshares[3]+gas_sell_price*endshares[4]+reits_sell_price*endshares[5]
  
  money = moneypoints[i]
  
  col.names <- c("start.shares", "start.basis", "start.money", "start.weights",
                 "weights.this.qr", "end.shares", "profit", "end.money", "end.basis")
  debug.mx <- matrix(0,5,length(col.names))
  rownames(debug.mx) <- c("DJ", "oil", "NASDAQ", "gas", "reits")
  colnames(debug.mx) <- col.names
  debug.mx[,1] <- c(DJIA_share[i-1], oil_share[i-1], nasdaq_share[i-1], 
                    gas_share[i-1], reits_share[i-1])
  debug.mx[,2] <- c(basis_DJIA[i-1], basis_oil[i-1], basis_nasdaq[i-1],
                    basis_gas[i-1], basis_reits[i-1])
  debug.mx[,3] <- c(DJIA_money[i-1], oil_money[i-1], nasdaq_money[i-1],
                    gas_money[i-1], reits_money[i-1])
  debug.mx[,4] <- debug.mx[,3]/sum(debug.mx[,3])
  debug.mx[,5] <- sol$solution
  debug.mx[,6] <- endshares
  debug.mx[,7] <- c(DJIA_profit[i], oil_profit[i], nasdaq_profit[i],
                    gas_profit[i], reits_profit[i])
  debug.mx[,8] <- c(DJIA_buy_price[i]*endshares[1],oil_buy_price[i]*endshares[2],nasdaq_buy_price[i]*endshares[3],gas_buy_price[i]*endshares[4],reits_buy_price[i]*endshares[5])
  debug.mx[,9] <- c(basis_DJIA[i], basis_oil[i], basis_nasdaq[i],
                    basis_gas[i], basis_reits[i])
  print(paste("At end of rebalancing for i =", i))
  print(debug.mx)
  print(paste("Value after rebalancing and taxes:", sum(debug.mx[,8])))
  ## new line
  out.mx.big.gradual[m,1:5] <- debug.mx[,8]; m <- m + 1
  ## historical risk
  ## out.mx.hist.risk[i, 1:5] <- debug.mx[,8]
}
  
  print(paste("we have finished iteration ", bb))
  write.csv(out.mx.big.gradual, "outMxBigGradual.csv")

}

plot(moneypoints,type = "l")

#Plot the wealth of each simulation period.
# Calculating the hold strategy for each asset
hold.strategy <- matrix(0, 13, 6)
colnames(hold.strategy) <- c("DJ", "oil", "NASDAQ", "gas", "REIT", "qr")
hold.strategy <- as.data.frame(hold.strategy)
hold.strategy$qr <- 1:13
for (i in 1:13) {
  j <- (1:nrow(DJIA_10y))[rownames(data_ts) == rownames(data_port[(1200 + (i-1)*93),0])]
  hold.strategy[i, "DJ"] <- values_DJIA[j]
  hold.strategy[i, "oil"] <- values_oil[j]
  hold.strategy[i, "NASDAQ"] <- values_nasdaq[j]
  hold.strategy[i, "gas"] <- values_gas[j]
  hold.strategy[i, "REIT"] <- values_reits[j]
}
for (j in c(1,3,4,5)) {
  hold.strategy[,j] <- hold.strategy[1,2]*10000*hold.strategy[,j]/hold.strategy[1,j]
}
hold.strategy[,2] <- hold.strategy[,2]*10000
hold.strategy$sim.no <- factor(rep(1, 13))

out.mx.big.gradual$total <- apply(out.mx.big.gradual[,1:5],1,sum)
#For the result of using historical covariance
# out.mx.hist.risk$total <- apply(out.mx.hist.risk[,1:5],1,sum)


ggplot(data=out.mx.big.gradual, aes(x=qr, y=total, group=as.factor(sim.no))) + geom_line(colour="grey") + theme_bw() + geom_line(data=hold.strategy, aes(x=qr, y=oil, group=sim.no), colour="red", size=2) + geom_line(data=out.mx.hist.risk, aes(x=qr, y=total, group=sim.no), colour="blue", size=2) + annotate("text", label = "Hold strategy", x = 1.5, y = 1100000, size = 4, colour = "red") + annotate("text", label = "Historical risk", x = 2, y = 700000, size = 4, colour = "blue") + annotate("text", label = "GARCH simulations", x = 9, y = 1550000, size = 4, colour = "grey")
