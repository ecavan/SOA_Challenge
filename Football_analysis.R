#importing series data
library(readxl)
Revenue_cleaned <- read.csv("~/SOA Case Competition 2022/revenue_football.csv", sep = ';', dec = ',', header = TRUE)
Revenue_cleaned2 = as.matrix(Revenue_cleaned)
Total_revs = cbind(as.numeric(Revenue_cleaned2[,2]) , as.numeric(Revenue_cleaned2[,6]), as.numeric(Revenue_cleaned2[,10]), as.numeric(Revenue_cleaned2[,14]), as.numeric(Revenue_cleaned2[,18]))
as.numeric(Total_revs[,4]) -Revenue_cleaned$Per.Capita.Total.Revenue....2017
x = c(2016:2020)
plot(x = x, y = rev(colMeans(Total_revs)), type = 'l', xlab = "Year", ylab = 'Total Revenue per Capita (???)', main = "Plot of Average Total Revenue per Capita (???)", ylim = c(0, 330)) 
lines(x=x, y = rev((Total_revs[17,])), col = "red")


Match_revs = cbind(as.numeric(Revenue_cleaned2[,19]) , as.numeric(Revenue_cleaned2[,15]), as.numeric(Revenue_cleaned2[,11]), as.numeric(Revenue_cleaned2[,7]), as.numeric(Revenue_cleaned2[,3]))
as.numeric(Match_revs[,2]) -Revenue_cleaned$Per.Capita.Matchday....2017
plot(x = x, y = colMeans(Match_revs), type = 'l', xlab = "Year", ylab = 'Matchday Revenue per Capita (???)', main = "Plot of Average Matchday Revenue per Capita (???)", ylim = c(20, 55)) 
lines(x=x, y = Match_revs[17,] , col = "red")
plot(x = x, y = 100*(colMeans(Match_revs) / rev(colMeans(Total_revs))), type = 'l', xlab = "Year", ylab = '% of Total Revenue', main = "Plot of Matchday Revenue versus Total Revenue (%)") 
lines(x=x, y = 100*(Match_revs[17,] / rev(Total_revs[17,])), col = "red")

matplot(t(Match_revs), type = 'l')

Broad_revs = cbind(as.numeric(Revenue_cleaned2[,20]) , as.numeric(Revenue_cleaned2[,16]), as.numeric(Revenue_cleaned2[,12]), as.numeric(Revenue_cleaned2[,8]), as.numeric(Revenue_cleaned2[,4]))
as.numeric(Broad_revs[,2]) -Revenue_cleaned$Per.Capita.Broadcast....2017
plot(x = x, y = colMeans(Broad_revs), type = 'l', xlab = "Year", ylab = 'Broadcast Revenue per Capita (???)', main = "Plot of Average Broadcast Revenue per Capita (???)", ylim = c(50, 150)) 
lines(x=x, y = Broad_revs[17,] , col = "red")

plot(x = x, y = 100*(colMeans(Broad_revs) / rev(colMeans(Total_revs))), type = 'l', xlab = "Year", ylab = '% of Total Revenue', main = "Plot of Broadcast Revenue versus Total Revenue (%)", ylim = c(38, 46)) 
lines(x=x, y = 100*(Broad_revs[17,] / rev(Total_revs[17,])), col = "red")

matplot(t(Broad_revs), type = 'l')


Commercial_revs = cbind(as.numeric(Revenue_cleaned2[,21]) , as.numeric(Revenue_cleaned2[,17]), as.numeric(Revenue_cleaned2[,13]), as.numeric(Revenue_cleaned2[,9]), as.numeric(Revenue_cleaned2[,5]))
as.numeric(Commercial_revs[,2]) -Revenue_cleaned$Per.Capita.Commercial....2017
plot(x = x, y = colMeans(Commercial_revs), type = 'l', xlab = "Year", ylab = 'Commercial Revenue per Capita (???)', main = "Plot of Average Commercial Revenue per Capita (???)", ylim = c(50, 130)) 
lines(x=x, y = Commercial_revs[17,] , col = "red")
plot(x = x, y = 100*(colMeans(Commercial_revs) / rev(colMeans(Total_revs))), type = 'l', xlab = "Year", ylab = '% of Total Revenue', main = "Plot of Commercial Revenue versus Total Revenue (%)", ylim = c(38,46)) 
lines(x=x, y = 100*(Commercial_revs[17,] / rev(Total_revs[17,])), col = "red")




Expense_cleaned <- read.csv("~/SOA Case Competition 2022/Expense_cleaned.csv", sep = ';', dec = ',', header = TRUE)
Expense_cleaned2 = as.matrix(Expense_cleaned)
Total_exps = cbind(as.numeric(Expense_cleaned2[,2]) , as.numeric(Expense_cleaned2[,5]), as.numeric(Expense_cleaned2[,8]), as.numeric(Expense_cleaned2[,11]), as.numeric(Expense_cleaned2[,14]))
as.numeric(Total_exps[,4]) -Expense_cleaned$Per.Capita.Total.Expense....2017
x = c(2016:2020)
plot(x = x, y = rev(colMeans(Total_exps)), type = 'l', xlab = "Year", ylab = 'Total Expense per Capita (???)', main = "Plot of Average Total Expense per Capita (???)") 
plot(x = x, y =  rev(colMeans(Total_revs))- rev(colMeans(Total_exps)), type = 'l', xlab = "Year", ylab = 'Profit per Capita (???)', main = "Plot of Average Profit per Capita (???)", ylim = c(0,70)) 
lines(x=x, y = rev((Total_revs[17,]))- rev(Total_exps[16,]), col = "red")
#plot(x = x, y =  rev((Total_revs[17,]))- rev(Total_exps[16,]), type = 'l', xlab = "Year", ylab = 'Profit per Capita (???)', main = "Plot of Average Profit per Capita (???)") 
matplot(t(Total_revs[-6,]-Total_exps), type = 'l')

Staff_exps = cbind(as.numeric(Expense_cleaned2[,3]) , as.numeric(Expense_cleaned2[,6]), as.numeric(Expense_cleaned2[,9]), as.numeric(Expense_cleaned2[,12]), as.numeric(Expense_cleaned2[,15]))
as.numeric(Staff_exps[,4]) -Expense_cleaned$Per.Capita.Staff.Costs....2017
plot(x = x, y = rev(colMeans(Staff_exps)), type = 'l', xlab = "Year", ylab = 'Staff Expense per Capita (???)', main = "Plot of Average Staff Expense per Capita (???)", ylim = c(75, 180)) 
lines(x=x, y =  rev(Staff_exps[16,]), col = "red")

plot(x = x, y = 100* rev(colMeans(Staff_exps))/rev(colMeans(Total_exps)), type = 'l', xlab = "Year", ylab = 'Staff Expense %', main = "Plot of Staff Expense % per Capita (???)", ylim = c(60, 70)) 
lines(x=x, y =  100 *rev(Staff_exps[16,])/rev(Total_exps[16,]), col = "red")
#plot(x = x, y =  rev(colMeans(_revs))- rev(colMeans(Total_exps)), type = 'l', xlab = "Year", ylab = 'Profit per Capita (???)', main = "Plot of Average Profit per Capita (???)", ylim = c(0,70)) 
#lines(x=x, y = rev((Total_revs[17,]))- rev(Total_exps[16,]), col = "red")


### Monte Carlo DCF Model:
MC_DCF = function(n_iter){
  Terminal_values = seq(0, length = n_iter) # Stores the value given to our team after 5 years.
  Cash_savings = seq(0, length = n_iter) # Cash amount left after 5 years. 
  profits_mat  = matrix(0, nrow = n_iter, ncol = 5)
   for(j in (1:n_iter)){
  INIT_matchday_growth = rnorm(1, mean = 0.1, sd = 0.03) 
  matchday_revs =  c(24.63*(1+INIT_matchday_growth))
  INIT_Broadcast_growth = rnorm(1, mean = 0.25, sd =  0.08)
  Broadcast_revenue =c(63.44*(1+INIT_Broadcast_growth))
  INIT_commercial_growth = rnorm(1, mean = 0.2, sd = 0.035)
  commercial_revenue =c((75.06)*(1+INIT_Broadcast_growth))
  
  population = c(12569472*(1+rnorm(1, 0.005, sd = 0.0015)))
  Total_exps = c(148.69*(1+rnorm(1,0.13,sd = 0.01))) # 13% increase in overall spending + scholarship spending
  profits = seq(0, length = 5)
  profits[1] = population[1]*(matchday_revs[1] +commercial_revenue[1]+Broadcast_revenue[1] -  Total_exps[1] -20500000/ population[1])
  
  savings = 995 -150 - 500 # Saving in Millions (500 millions for the team and 150 for the stadium)
  for(i in (1:4)){ # Years 2022-2025 where we get an average team.
  population = c(population, population[i]*(1+rnorm(1, 0.005, sd = 0.0015)))
  if ( i == 3){
    # New Stadiums is open: High Increases in Matchday revenue this year
    matchday_revs =  c(matchday_revs, matchday_revs[i]*(1+rnorm(1, 1, 0.05)))

  }
  matchday_revs =  c(matchday_revs, matchday_revs[i]*(1+rnorm(1, 0.02, 0.10)))
  commercial_revenue =  c( commercial_revenue,  commercial_revenue[i]*(1+rnorm(1, 0.12, 0.035)))
  Broadcast_revenue = c(Broadcast_revenue, Broadcast_revenue[i]*(1+rnorm(1, 0.04,0.17)))
  
  Total_exps = c(Total_exps, Total_exps*(1+rnorm(1,0.13,sd = 0.01))) # 13% increase in overall spending + scholarship spending
  
  
  profits[i+1] = population[i+1]*(matchday_revs[i+1] +commercial_revenue[i+1]+Broadcast_revenue[i+1] -  Total_exps[i+1]-25000000/ population[1])- 100000000
  profits_mat[j,] = profits/1000000
  savings = savings+profits[i+1]/1000000
  }
  Discount_rate = max(rnorm(1, 0.15, 0.025),0.125)
  Terminal_values[j] = mean(profits)/(Discount_rate) 
  Cash_savings[j] = savings
  }
  return(list( Terminal_values = Terminal_values, cash = Cash_savings, profits_mat = profits_mat))
}

MC_DCF(100)$Terminal_values/1000000
sims = MC_DCF(100000)

mean(sims$Terminal_values/1000000)
hist(sims$Terminal_values/1000000)
hist(sims$cash)
sum(sims$Terminal_values/100000 <2000)/100000
sum(sims$Terminal_values/100000 <1000)/100000
sum(sims$Terminal_values/100000 <0)/100000
mean(sims$cash)
mean(sims$cash[which(sims$Terminal_values/100000 <0)])
mean(sims$cash[which(sims$Terminal_values/100000 <1000)])
mean(sims$cash[which(sims$Terminal_values/100000 <2000)])


colVar <- function(x) {
  colSums((x - colMeans(x))^2)/(dim(x)[1] - 1)
}
Profits_sd = sqrt(colVar(sims$profits_mat))
plot(colMeans(sims$profits_mat), type = 'l', ylim = c(-1000,3000))
lines(colMeans(sims$profits_mat)- 1.96*Profits_sd, col = "red")
lines(colMeans(sims$profits_mat)+ 1.96*Profits_sd, col = "red" )

cash_sd = sqrt(colVar(sims$cash))
plot(colMeans(sims$cash), type = 'l', ylim = c(-1000,3000))
lines(colMeans(sims$cash)- 1*cash_sd, col = "red")
lines(colMeans(sims$cash)+ 1*cash_sd, col = "red" )

