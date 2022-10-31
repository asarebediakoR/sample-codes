
# PCA codes

print('Hi Rich.....welcome to R, Enjoy coding')

library(ggplot2)


#Reading iris_tab data for PCA analysis 
data3 = read.table('C:/Users/RSNasarebediako/Desktop/Rcodes/iris_R/iris_tab.txt',header= TRUE)

Sp = data3$species; # species
SL = data3$sepal_length; # sepal length
SW = data3$sepal_width; # sepal width
PL = data3$petal_length; # petal length
PW = data3$petal_width; # petal width

df2 = data.frame(SL, SW, PL, PW)
print (df2)

PCA <- princomp(df2,cor = TRUE) # Performs PCA on variance
PCAtest <- summary(PCA) 

Myload <- loadings(PCA) # Provides PC loading

Scores <- PCA$scores # the principal components

plot(PCA,type= 'lines') # scree plot command
biplot(PCA)
print(PCAtest)
print(Myload)
print(Scores)

print('Leaving R.....See you Rich')

