
# BIC_GMM codes
print('Hi Rich.....welcome to R, Enjoy coding')

library('grid')
library('mixtools')
library('ggplot2')
library('MASS')

GMM_data = read.table("C:/Users/RSNasarebediako/Desktop/Final Unit one/Theoph.txt",header= TRUE)

sb = GMM_data$Subject # Subjects
wt = GMM_data$Wt      # Weight (Wt)
ds = GMM_data$Dose    #Dose
tm = GMM_data$Time    #Time
cn = GMM_data$conc    #Concentration(Conc)

df = data.frame(cn,ds,wt,tm,sb)
subframe = data.frame(cn,ds,wt,tm)
print(subframe)

AB = (wt-(cn+tm+ds)) # AB is absorption which measures the amount of drug left over time
print(AB)

#set.seed(40)

NORMfiting <- fitdistr(AB, densfun="normal") #Maximum-likelihood with normal density function
print(NORMfiting)
LNORMfiting <- fitdistr(AB, densfun="log-normal") #Maximum-likelihood with log-normal density function
print(LNORMfiting)
EXPfiting <- fitdistr(AB, densfun="exponential") #Maximum-likelihood with exponential density function
print(EXPfiting)
fitMy_GMM <- normalmixEM(AB) # defaults to GMM with 2 density functions
print(fitMy_GMM)

fitGMM_loglik <- fitMy_GMM$loglik
BIC_GMM <- -3*fitGMM_loglik+4*log(132) # includes 4 parameters with 132 sample
BICfit <- BIC(NORMfiting,LNORMfiting,EXPfiting)
print(BICfit)

print ("BIC for GMM")
print(BIC_GMM)
Plot1 <-ggplot(df, aes(x=AB)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dnorm, color="red", args=list(mean = NORMfiting$estimate[1], sd = NORMfiting$estimate[2])) 
Plot2 <-ggplot(df, aes(x=AB)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dlnorm, color="red", args=list(meanlog = LNORMfiting$estimate[1], sdlog = LNORMfiting$estimate[2])) 
Plot3 <-ggplot(df, aes(x=AB)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dexp, color="red", args=list(rate = EXPfiting$estimate[1])) 

#  GMM with two density functions
Plot4 <-ggplot(df, aes(x=AB)) + geom_histogram(aes(y=2*(..density..))) + geom_density(aes(y=2*(..density..))) + 
  stat_function(fun=dnorm, color="red", args=list(mean = fitMy_GMM$mu[1], sd = fitMy_GMM$sigma[1])) + 
  stat_function(fun=dnorm, color="red", args=list(mean = fitMy_GMM$mu[2], sd = fitMy_GMM$sigma[2])) 

# PushViewport plots
pushViewport(viewport(layout = grid.layout(2, 2)))
print(Plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(fitMy_GMM)
print(BICfit)
print(BIC_GMM)

print('Leaving R.....See you Rich')









