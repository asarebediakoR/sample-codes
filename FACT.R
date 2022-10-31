
# FACT codes
 print('Hi Rich.....welcome to R, Enjoy coding')

library('ggplot2')
library('psych')

# Exporting data(attitude) from R to a text file

head(attitude)
write.table(attitude,"C:/Users/RSNasarebediako/Desktop/Rcodes/Attitude_data.txt")

# Importing data(attitude) to R

FACT_data = read.table("C:/Users/RSNasarebediako/Desktop/Rcodes/Attitude_data.txt",header= TRUE)

rating = FACT_data$rating  #Rating
complaints = FACT_data$complaints # Complains
privileges = FACT_data$privileges # Privileges
learning = FACT_data$learning # Learning
raises = FACT_data$raises #Raises
critical = FACT_data$critical # Critical
advance = FACT_data$advance # Advance

Mydf = data.frame(rating,complaints,privileges,learning,raises,critical,advance)
print(Mydf)

MyFACT <- factanal(Mydf,3,rotation = 'varimax') # Performs factor analysis on data set with three(3) factors within seven(7) variables

print(MyFACT,digits=3, cutoff=.3, sort=TRUE)


# plot factor 1 by factor 3
FACTload <- MyFACT$loadings[,1:3]

plot(FACTload,type = 'b') # set up plot with points connected with lines
text(FACTload,labels=NULL) # add variable names

print('Leaving R.....See you Rich')
