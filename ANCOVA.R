
# ANCOVA codes
 print('Hi Rich.....welcome to R, Enjoy coding')

#Reading text file after composite variable computation and creating dataframe with composite variable

ANCOVAdata = read.table("C:/Users/RSNasarebediako/Desktop/Codes - Gethub/ANCOVA_data.txt",header = TRUE)

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
print('Leaving R.....See you Rich')