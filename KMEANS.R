
# KMEANS codes

print('Hi Rich.....welcome to R, Enjoy coding')

library(ggplot2)
library(grid)
My_data= read.table("C:/Users/RSNasarebediako/Desktop/Final Unit one/Theoph.txt",header= TRUE)

sb = My_data$Subject # Subjects
wt = My_data$Wt      # Weight (Wt)
ds = My_data$Dose    #Dose
tm = My_data$Time    #Time
cn = My_data$conc    #Concentration(Conc)

DF <- data.frame(sb,wt,ds,tm,cn) #Creats dataframe
print(DF)
subDF = data.frame(wt,ds,tm,cn) # Creates a sub data frame without categorical data

set.seed(3)

KMEANS_test <- kmeans(subDF,3,nstart = 3) #K means with 3 clusters
Plot1 <- ggplot(My_data,aes(cn,ds, color = sb)) +geom_point()
Plot3 <- ggplot(My_data, aes(wt,tm,color = sb)) + geom_point()
MyKMEANS <- summary (KMEANS_test)
print (MyKMEANS)
KMEANS_test$cluster <- as.factor(KMEANS_test$cluster) # Treats clusters in KMEANS test as factors
Plot2 <- ggplot(My_data, aes(cn,ds, color=KMEANS_test$cluster )) +geom_point()
Plot4 <- ggplot(My_data, aes(wt,tm, color=KMEANS_test$cluster )) +geom_point()

# Creates viewport with 2 grid
pushViewport(viewport(layout = grid.layout(2, 2)))
print(Plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))


print('Leaving R.....See you Rich')




