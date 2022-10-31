
# MANOVA AND MR codes

print('Hi Rich.....welcome to R, Enjoy coding')

library('ggplot2')
library('grid')

# Exporting data(Theoph) from R to a text file

sink("desc.txt3",append = T)

head(Theoph)
write.table(Theoph,"C:/Users/RSNasarebediako/Desktop/Rcodes/Theoph.txt")

closeAllConnections() # Closes sink function to allow print out of subsequent print commands

# Importing data(Threoph) to R

Multiv_data = read.table("C:/Users/RSNasarebediako/Desktop/Rcodes/Theoph.txt",header= TRUE)

sb = Multiv_data$Subject # Subjects
wt = Multiv_data$Wt      # Weight (Wt)
ds = Multiv_data$Dose    #Dose
tm = Multiv_data$Time    #Time
cn = Multiv_data$conc    #Concentration(Conc)

testData2 <- data.frame(sb,wt,ds,tm,cn)
print(testData2)

#Box pot analysis

Myplot1 <- ggplot(testData2,aes(x = sb, y = wt,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Weight') + theme(axis.text.x=element_text(angle=0.5),panel.background = element_rect(fill = 'plum1'))
Myplot2 <- ggplot(testData2,aes(x = sb, y = ds,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Dose') + theme(axis.text.x=element_text(angle=0.5), panel.background = element_rect(fill = 'plum1'))
Myplot3 <- ggplot(testData2,aes(x = sb, y = tm,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Time') + theme(axis.text.x=element_text(angle=0.5), panel.background = element_rect(fill = 'plum1'))
Myplot4 <- ggplot(testData2,aes(x = sb, y = cn,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Concentration') + theme(axis.text.x=element_text(angle=0.5), panel.background = element_rect(fill = 'plum1'))

print(Myplot1)
print(Myplot2)
print(Myplot3)
print(Myplot4)

# Performs ANOVA within individual variables
Mytest1 <- oneway.test(wt~sb)
Mytest2 <- oneway.test(ds~sb)
Mytest3 <- oneway.test(tm~sb)
Mytest4 <- oneway.test(cn~sb)

B <- cbind(wt,ds,tm,cn) # Binds weight,dose,time and concentration columns together
print(B)

Mytest5 <- manova(B~sb, data= testData2) # Conducts MANOVA test on subjects and the remaining factors

print(Mytest1)
print(Mytest2)
print(Mytest3)
print(Mytest4)

MyMANOVA <- summary(Mytest5, test = 'Pillai')
print(MyMANOVA)

library('grid')
#Plots graph with pushViewport
pushViewport(viewport(layout = grid.layout(2, 2)))
print(Myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))



MyLM <- lm(wt~ds+tm+cn, data= testData2) # Linear model that weight influences dose,time and concentration
A <- cbind(wt,sb,tm,cn) # combines the individual data columns
LMplot <- pairs(A, col= as.factor(sb)) # Predicts how subjects are affected by weight,dose,time and concentration  
print(summary(MyLM)) # Print out MyLM test
print(LMplot)       #Print out LMplot

print('Leaving R.....See you Rich')

