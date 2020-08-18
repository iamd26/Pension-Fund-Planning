########################################################################################################
############################################Part A######################################################
########################################################################################################
#Load the data
load("A.RData")
head(nominal)
nominal <- as.matrix(nominal)
nominal
asset <- nominal[,2:5]

#1. Calculate the mean and sd 
means <- colMeans(asset)
sd <- apply(asset,2,sd)
means
sd
#2. Calculate the correlation
cor <- cor(asset)

#3. VCV (variance-covariance matrix)
vcv <- cov(asset)
rf <- mean(nominal[,6])

#4. Maximal Sharpe Ratio (MSR) portfolio
means
excess_return <- means - rf
excess_return <- as.matrix(excess_return)
excess_return
one <- c(1,1,1,1)
weight_msr <- solve(vcv)%*%excess_return%*%solve(one%*%solve(vcv)%*%excess_return)

#4. Tangent return and variance
weight_msr
tan_return <- t(as.matrix(means))%*%weight_msr
tan_variance <- t(weight_msr)%*%vcv%*%weight_msr
tan_return
tan_variance

#4. Allocation for a given risk
aversion <- c(1/1.3,1/2.8,1/6.5,1/10.5,1/16.9)
allocation <- solve(vcv)%*%excess_return%*%t(aversion)
Tbill <- 1-colSums(allocation)
allocation <- rbind(allocation,Tbill)
allocation

#4. Allocation of the Global Minimum Variance (GMV) portfolio
weight_gmv <- solve(vcv)%*%one%*%solve(t(one)%*%solve(vcv)%*%one)
weight_gmv

return <- colMeans(nominal[,2:5])
gmv_return <- return%*%weight_gmv
gmv_return
gmv_variance <- t(weight_gmv[1:4,])%*%vcv%*%weight_gmv[1:4,]
gmv_variance

#5
weight_msr
Tbill <- c(0)
weight_msr <- rbind(weight_msr,Tbill)
colnames(weight_msr) <- c("MSR")
allocation
colnames(allocation) <- c("A=1.3","A=2.8","A=6.5","A=10.5","A=16.9")
weight_gmv <- rbind(weight_gmv,Tbill)
colnames(weight_gmv) <- c("GMV")
allocation_all <- cbind(weight_gmv,weight_msr,allocation)

#Sharpe Ratio
sr_msr <- (tan_return - rf)/(tan_variance)^0.5
sr_gmv <- (gmv_return - rf)/(gmv_variance)^0.5
sr_msr
sr_gmv

#Plot How to plot this picture???
allocation_all

########################################################################################################
######################################Case Study: OTPF##################################################
########################################################################################################
#Part B1
load("B1.RData")
mean <- B1$mean
sd <- B1$sd
cor <- B1$cor
rf <- B1$rf
#Data
mean
sd
cor
rf
#Efficient Frontier

#Part B2
#Load Data
load("B2.RData")
rf
s1 <- r$s1
s2 <- r$s2
s3 <- r$s3
payment
s1
s2
s3
#Understanding of the Prototype, further understanding is illustrated in excel
payment
#Understanding of three simulations
colMeans(s1)
colMeans(s2)
colMeans(s3)

#######################################################################################################
#Portfolio Value for Scenario1#########################################################################
#######################################################################################################
payment
s1
payments1 <- cbind(payment,s1)
colnames(payments1) <- c("Salary","Teacher","Employer","Benefits","Net.Payment","s1.4","s1.5","s1.6","s1.7")
payments1
#4%
payments1$port.s1.4 <- NA
payments1$port.s1.5 <- NA
payments1$port.s1.6 <- NA
payments1$port.s1.7 <- NA
payments1
year0 <- 0
payments1 <- rbind(year0,payments1)
payments1
for (i in 2:62) {
  payments1$port.s1.4[i] <- ifelse(payments1$port.s1.4[i-1]>0,
                                   payments1$port.s1.4[i-1]*(1+payments1$s1.4[i])+payments1$Net.Payment[i],
                                   payments1$port.s1.4[i-1]+payments1$Net.Payment[i])
}
payments1
#5%
for (i in 2:62) {
  payments1$port.s1.5[i] <- ifelse(payments1$port.s1.5[i-1]>0,
                                   payments1$port.s1.5[i-1]*(1+payments1$s1.5[i])+payments1$Net.Payment[i],
                                   payments1$port.s1.5[i-1]+payments1$Net.Payment[i])
}
payments1
#6%
for (i in 2:62) {
  payments1$port.s1.6[i] <- ifelse(payments1$port.s1.6[i-1]>0,
                                   payments1$port.s1.6[i-1]*(1+payments1$s1.6[i])+payments1$Net.Payment[i],
                                   payments1$port.s1.6[i-1]+payments1$Net.Payment[i])
}
payments1
#7%
for (i in 2:62) {
  payments1$port.s1.7[i] <- ifelse(payments1$port.s1.7[i-1]>0,
                                   payments1$port.s1.7[i-1]*(1+payments1$s1.7[i])+payments1$Net.Payment[i],
                                   payments1$port.s1.7[i-1]+payments1$Net.Payment[i])
}
payments1

#discount
for (i in 2:62) {
  payments1$discount[1] <- 0
  payments1$discount[i] <- 1.02^(-(i-1))
}
payments1

#Net Pension Assets (npa)
#4%
payments1$npa.s1.4[1] <- 0
payments1$npa.s1.4[2] <- payments1$port.s1.4[2] +
  crossprod(payments1$Net.Payment[3:62],payments1$discount[3:62])/payments1$discount[2]
payments1$npa.s1.4[3] <- payments1$port.s1.4[3] +
  crossprod(payments1$Net.Payment[4:62],payments1$discount[4:62])/payments1$discount[3]
for (i in 2:61) {
  payments1$npa.s1.4[i] <- payments1$port.s1.4[i] +
    crossprod(payments1$Net.Payment[(i+1):62],payments1$discount[(i+1):62])/payments1$discount[i]
}
payments1$npa.s1.4[62] <- payments1$port.s1.4[62]
payments1
#5%
payments1$npa.s1.5[1] <- 0
for (i in 2:61) {
  payments1$npa.s1.5[i] <- payments1$port.s1.5[i] +
    crossprod(payments1$Net.Payment[(i+1):62],payments1$discount[(i+1):62])/payments1$discount[i]
}
payments1$npa.s1.5[62] <- payments1$port.s1.5[62]
payments1
#6%
payments1$npa.s1.6[1] <- 0
for (i in 2:61) {
  payments1$npa.s1.6[i] <- payments1$port.s1.6[i] +
    crossprod(payments1$Net.Payment[(i+1):62],payments1$discount[(i+1):62])/payments1$discount[i]
}
payments1$npa.s1.6[62] <- payments1$port.s1.6[62]
payments1
#7%
payments1$npa.s1.7[1] <- 0
for (i in 2:61) {
  payments1$npa.s1.7[i] <- payments1$port.s1.7[i] +
    crossprod(payments1$Net.Payment[(i+1):62],payments1$discount[(i+1):62])/payments1$discount[i]
}
payments1$npa.s1.7[62] <- payments1$port.s1.7[62]
payments1
#Net Pension Asset Table
NPA_s1 <- payments1[,15:18]
NPA_s1

#######################################################################################################
#Portfolio Value for Scenario2#########################################################################
#######################################################################################################
payment
s2
payments2 <- cbind(payment,s2)
colnames(payments2) <- c("Salary","Teacher","Employer","Benefits","Net.Payment","s2.4","s2.5","s2.6","s2.7")
payments2
#4%
payments2$port.s2.4 <- NA
payments2$port.s2.5 <- NA
payments2$port.s2.6 <- NA
payments2$port.s2.7 <- NA
payments2
year0 <- 0
payments2 <- rbind(year0,payments2)
payments2
for (i in 2:62) {
  payments2$port.s2.4[i] <- ifelse(payments2$port.s2.4[i-1]>0,
                                   payments2$port.s2.4[i-1]*(1+payments2$s2.4[i])+payments2$Net.Payment[i],
                                   payments2$port.s2.4[i-1]+payments2$Net.Payment[i])
}
payments2
#5%
for (i in 2:62) {
  payments2$port.s2.5[i] <- ifelse(payments2$port.s2.5[i-1]>0,
                                   payments2$port.s2.5[i-1]*(1+payments2$s2.5[i])+payments2$Net.Payment[i],
                                   payments2$port.s2.5[i-1]+payments2$Net.Payment[i])
}
payments2
#6%
for (i in 2:62) {
  payments2$port.s2.6[i] <- ifelse(payments2$port.s2.6[i-1]>0,
                                   payments2$port.s2.6[i-1]*(1+payments2$s2.6[i])+payments2$Net.Payment[i],
                                   payments2$port.s2.6[i-1]+payments2$Net.Payment[i])
}
payments2
#7%
for (i in 2:62) {
  payments2$port.s2.7[i] <- ifelse(payments2$port.s2.7[i-1]>0,
                                   payments2$port.s2.7[i-1]*(1+payments2$s2.7[i])+payments2$Net.Payment[i],
                                   payments2$port.s2.7[i-1]+payments2$Net.Payment[i])
}
payments2

#discount
for (i in 2:62) {
  payments2$discount[1] <- 0
  payments2$discount[i] <- 1.02^(-(i-1))
}
payments2

#Net Pension Assets (npa)
#4%
payments2$npa.s2.4[1] <- 0
for (i in 2:61) {
  payments2$npa.s2.4[i] <- payments2$port.s2.4[i] +
    crossprod(payments2$Net.Payment[(i+1):62],payments2$discount[(i+1):62])/payments2$discount[i]
}
payments2$npa.s2.4[62] <- payments2$port.s2.4[62]
payments2
#5%
payments2$npa.s2.5[1] <- 0
for (i in 2:61) {
  payments2$npa.s2.5[i] <- payments2$port.s2.5[i] +
    crossprod(payments2$Net.Payment[(i+1):62],payments2$discount[(i+1):62])/payments2$discount[i]
}
payments2$npa.s2.5[62] <- payments2$port.s2.5[62]
payments2
#6%
payments2$npa.s2.6[1] <- 0
for (i in 2:61) {
  payments2$npa.s2.6[i] <- payments2$port.s2.6[i] +
    crossprod(payments2$Net.Payment[(i+1):62],payments2$discount[(i+1):62])/payments2$discount[i]
}
payments2$npa.s2.6[62] <- payments2$port.s2.6[62]
payments2
#7%
payments2$npa.s2.7[1] <- 0
for (i in 2:61) {
  payments2$npa.s2.7[i] <- payments2$port.s2.7[i] +
    crossprod(payments2$Net.Payment[(i+1):62],payments2$discount[(i+1):62])/payments2$discount[i]
}
payments2$npa.s2.7[62] <- payments2$port.s2.7[62]
payments2
#Net Pension Asset Table
NPA_s2 <- payments2[,15:18]
NPA_s2

#######################################################################################################
#Portfolio Value for Scenario3#########################################################################
######################################################################################################
payment
s3
payments3 <- cbind(payment,s3)
colnames(payments3) <- c("Salary","Teacher","Employer","Benefits","Net.Payment","s3.4","s3.5","s3.6","s3.7")
payments3
#4%
payments3$port.s3.4 <- NA
payments3$port.s3.5 <- NA
payments3$port.s3.6 <- NA
payments3$port.s3.7 <- NA
payments3
year0 <- 0
payments3 <- rbind(year0,payments3)
payments3
for (i in 2:62) {
  payments3$port.s3.4[i] <- ifelse(payments3$port.s3.4[i-1]>0,
                                   payments3$port.s3.4[i-1]*(1+payments3$s3.4[i])+payments3$Net.Payment[i],
                                   payments3$port.s3.4[i-1]+payments3$Net.Payment[i])
}
payments3
#5%
for (i in 2:62) {
  payments3$port.s3.5[i] <- ifelse(payments3$port.s3.5[i-1]>0,
                                   payments3$port.s3.5[i-1]*(1+payments3$s3.5[i])+payments3$Net.Payment[i],
                                   payments3$port.s3.5[i-1]+payments3$Net.Payment[i])
}
payments3
#6%
for (i in 2:62) {
  payments3$port.s3.6[i] <- ifelse(payments3$port.s3.6[i-1]>0,
                                   payments3$port.s3.6[i-1]*(1+payments3$s3.6[i])+payments3$Net.Payment[i],
                                   payments3$port.s3.6[i-1]+payments3$Net.Payment[i])
}
payments3
#7%
for (i in 2:62) {
  payments3$port.s3.7[i] <- ifelse(payments3$port.s3.7[i-1]>0,
                                   payments3$port.s3.7[i-1]*(1+payments3$s3.7[i])+payments3$Net.Payment[i],
                                   payments3$port.s3.7[i-1]+payments3$Net.Payment[i])
}
payments3

#discount
for (i in 2:62) {
  payments3$discount[1] <- 0
  payments3$discount[i] <- 1.02^(-(i-1))
}
payments3

#Net Pension Assets (npa)
#4%
payments3$npa.s3.4[1] <- 0
for (i in 2:61) {
  payments3$npa.s3.4[i] <- payments3$port.s3.4[i] +
    crossprod(payments3$Net.Payment[(i+1):62],payments3$discount[(i+1):62])/payments3$discount[i]
}
payments3$npa.s3.4[62] <- payments3$port.s3.4[62]
payments3
#5%
payments3$npa.s3.5[1] <- 0
for (i in 2:61) {
  payments3$npa.s3.5[i] <- payments3$port.s3.5[i] +
    crossprod(payments3$Net.Payment[(i+1):62],payments3$discount[(i+1):62])/payments3$discount[i]
}
payments3$npa.s3.5[62] <- payments3$port.s3.5[62]
payments3
#6%
payments3$npa.s3.6[1] <- 0
for (i in 2:61) {
  payments3$npa.s3.6[i] <- payments3$port.s3.6[i] +
    crossprod(payments3$Net.Payment[(i+1):62],payments3$discount[(i+1):62])/payments3$discount[i]
}
payments3$npa.s3.6[62] <- payments3$port.s3.6[62]
payments3
#7%
payments3$npa.s3.7[1] <- 0
for (i in 2:61) {
  payments3$npa.s3.7[i] <- payments3$port.s3.7[i] +
    crossprod(payments3$Net.Payment[(i+1):62],payments3$discount[(i+1):62])/payments3$discount[i]
}
payments3$npa.s3.7[62] <- payments3$port.s3.7[62]
payments3
#Net Pension Asset Table
NPA_s3 <- payments3[,15:18]
NPA_s3

#######################################################################################################
#NPA for 3 Scenario####################################################################################
#######################################################################################################
payments1
payments2
payments3
Port_s1 <- payments1[,10:13]
Port_s2 <- payments2[,10:13]
Port_s3 <- payments3[,10:13]
NPA_s1
NPA_s2
NPA_s3
Port_s1
Port_s2
Port_s3
#Create a table or set of tables summarizing these totals at ages 55, 64, 75 and 85 for each of the three scenarios

