
# Richmond Asare-Bediako
# MS Bioinformatics
# Unit_1.BIOL 672
# Windows O.S
# ANOVA compare codes

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

library(ggplot2)

Myplot1 <- ggplot(testData2,aes(x = sb, y = wt,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Weight') + theme(axis.text.x=element_text(angle=0.5),panel.background = element_rect(fill = 'plum1'))
Myplot2 <- ggplot(testData2,aes(x = sb, y = ds,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Dose') + theme(axis.text.x=element_text(angle=0.5), panel.background = element_rect(fill = 'plum1'))
Myplot3 <- ggplot(testData2,aes(x = sb, y = tm,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Time') + theme(axis.text.x=element_text(angle=0.5), panel.background = element_rect(fill = 'plum1'))
Myplot4 <- ggplot(testData2,aes(x = sb, y = cn,group=sb)) + geom_boxplot() + labs(x = 'Subjects', y = 'Concentration') + theme(axis.text.x=element_text(angle=0.5), panel.background = element_rect(fill = 'plum1'))

print(Myplot1)
print(Myplot2)
print(Myplot3)
print(Myplot4)

Mytest1 <- oneway.test(wt~sb)
Mytest2 <- oneway.test(ds~sb)
Mytest3 <- oneway.test(tm~sb)
Mytest4 <- oneway.test(cn~sb)

B <- cbind(wt,ds,tm,cn)
print(B)

Mytest5 <- manova(B~sb, data= testData2)

print(Mytest1)
print(Mytest2)
print(Mytest3)
print(Mytest4)

MyMANOVA <- summary(Mytest5, test = 'Pillai')
print(MyMANOVA)

library('grid')
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



#Reading text file after composite variable computation and creating dataframe with composite variable

ANCOVAdata = read.table("C:/Users/RSNasarebediako/Desktop/Rcodes/ANCOVA_data.txt",header = TRUE)

sb = ANCOVAdata$Subject # Subjects
wt = ANCOVAdata$Wt      # Weight (Wt)
ds = ANCOVAdata$Dose    #Dose
tm = ANCOVAdata$Time    #Time
cn = ANCOVAdata$Conc    #Concentration(Conc)
ab = ANCOVAdata$Absorption #Absorption (AB)

df <- data.frame(sb,wt,ds,tm,cn,ab, check.rows = TRUE, row.names = NULL)
print(df)

MyTest <- aov(formula = cn~ds*ab, data= df) # ANCOVA test that compares how concentration is affected by dose while controlling absorption
 D <- cbind(wt,ds,tm,cn)
 print(D)
 MyANCOVAplot <- pairs(D, col= as.factor(sb)) # plots subjects against factors combined in the previous step
 ANCOVA <- summary(MyTest)
 
 print(ANCOVA)
 print(MyANCOVAplot)
 


#Reading text file from the original iris_data sets
  data = read.table('C:/Users/RSNasarebediako/Desktop/Rcodes/iris_R/iris_tab.txt',header= TRUE)

  Sp = data$species; # species
  SL = data$sepal_length; # sepal length
  SW = data$sepal_width; # sepal width
  PL = data$petal_length; # petal length
  PW = data$petal_width; # petal width
  
  dataframe = data.frame(Sp,SL,SW,PL,PW) #combines data set into a data frame
   print(dataframe)
   
 #Conduct ANOVA on petals and sepals
   
  MY_ANOVA_1 <- aov(PL~Sp, data= dataframe)
  MY_ANOVA_2 <- aov(PW~Sp, data= dataframe)
  MY_ANOVA_3 <- aov(SL~Sp, data= dataframe)
  MY_ANOVA_4 <- aov(SW~Sp, data= dataframe)
   

 #Print ANOVA test and export to external file
  
  sink(file= 'C:/Users/RSNasarebediako/Desktop/ANOVA_compare1.txt')
  print(MY_ANOVA_1)
  print(MY_ANOVA_2)
  print(MY_ANOVA_3)
  print(MY_ANOVA_4)
  sink()
  
  #Reading text file from the corrupted iris_data sets
  data2 = read.table('C:/Users/RSNasarebediako/Desktop/Codes repo/course-code-repo-main/statistics_(BIOL470-672)/corrupted-extended_iris_data_sets/iris_tab.txt',header= TRUE)
 
  Sp = data2$species; # species
  SL = data2$sepal_length; # sepal length
  SW = data2$sepal_width; # sepal width
  PL = data2$petal_length; # petal length
  PW = data2$petal_width; # petal width
  
  dataframe2 = data.frame(Sp,SL,SW,PL,PW)
  print(dataframe2)

  #Conduct second ANOVA on petals and sepals
  
  MY_ANOVA_5 <- aov(PL~Sp, data= dataframe2)
  MY_ANOVA_6 <- aov(PW~Sp, data= dataframe2)
  MY_ANOVA_7 <- aov(SL~Sp, data= dataframe2)
  MY_ANOVA_8 <- aov(SW~Sp, data= dataframe2)

  
  #Print second ANOVA test and export to external file
  
  sink(file= 'C:/Users/RSNasarebediako/Desktop/ANOVA_compare2.txt')
  print(MY_ANOVA_5)
  print(MY_ANOVA_6)
  print(MY_ANOVA_7)
  print(MY_ANOVA_8)
  sink()

# After ANOVA comparison between iris_tab data from the original and corrupted file, no significant difference was noticed

 

  
  
  
  
  
  
  
  
  
  
  