 
# ANOVA / Histogram

print('Hi Rich.....welcome to R, Enjoy coding')

sink("desc.txt",append = T)

set.seed(5000) #Set data to random numbers of 5000
data_set <- rnorm(5000,0,1) #Generate random 5000 no. with mean=0, SD=1
datamean <- mean(data_set)  #calculate data mean
datavariance <- var(data_set) #computes data variance
dataSD <- sd(data_set) #calculate data SD

print(datamean)
print(datavariance)
print(dataSD)

# Constructs histogram of data_set
hist(data_set,freq = FALSE,
     ylim = c(0,0.4),xlim = c(-3,3),
     breaks = 14)
  
lines(density(data_set),col = 'red',  #introduces density curve with red color and width = 3
           lwd = 2) 
                         

# Creates plot with ggplot2

library(ggplot2)

data_set2 = data.frame(data_set)
ggplot(data_set2, aes(data_set)) + 
  geom_histogram(fill="grey")+
  theme_classic()
sink()

  
# Exporting data(trees) from R to a text file

sink("desc.txt2",append = T)

head(trees)
write.table(trees,"C:/Users/RSNasarebediako/Desktop/Rcodes/trees.txt")

# Importing data(trees.txt) to R

ANOVAdata = read.table("C:/Users/RSNasarebediako/Desktop/Rcodes/trees.txt",header= TRUE)

Gt=ANOVAdata$Girth   #Trees Girth
Ht=ANOVAdata$Height  #Trees Height
Vl=ANOVAdata$Volume #Trees Volume


testData <- data.frame(Gt,Ht,Vl)
print(testData)

mean = mean(Vl)
S.D = sd(Vl)
S.E = sd(Vl)/sqrt(length(Vl))

Vl.fac = as.factor(Gt)


#Analysis of variance
myANOVAtest <- aov( Ht~Vl.fac,data=testData)
print(myANOVAtest)

#Pairwise t test, bonferroni and Benjamini-Hochberg

pairwise.t.test( Ht,Vl.fac, p.adj='bonferroni')
pairwise.t.test( Ht,Vl.fac, p.adj='BH')

library(ggplot2)

limits <- aes(ymax= mean-S.E,
              ymin= mean+S.E)


myplot <- ggplot(testData,aes(Vl.fac,Ht))+
  geom_bar(stat = "identity")+
  geom_errorbar(limits)+
  xlab('Girth of trees')+
  title('Girth of trees vs Volume')
  ylab('Volume of trees')+
  theme_classic()
print(myplot)

sink()

print('Leaving R.....See you Rich')



