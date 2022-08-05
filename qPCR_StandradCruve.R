install.packages("lme4")
install.packages("broom")
install.packages("data.table")
install.packages("ggpmisc")
installed.packages("ggplot2")
install.packages("ggpubr")
install.packages("wesanderson")



library("data.table")
library("ggpubr")
library("dplyr")
library("pcr")
library("ggplot2")
library("cowplot")
library("lme4")
library("broom")
library("ggpmisc")
library("wesanderson")
setwd("/Users/dilant/Desktop")

## Create datafile , calculate mean sd and save as .csv

df<-read.csv("test5.csv")
str(df) #check strings of column
df$Cq <- as.numeric(as.character(df$Cq)) #make Cq column a numeric string instead of chr
str(df) #Check again 
df1<- df %>%
  group_by(across(where(is.character))) %>%
  summarise(Mean = mean(Cq), sd=sd(Cq))  #calculte mean and sd of Cq values grouped by character columns i.e. well.position and target
df1
df1_New<- df1[- 1, ]         # Remove first row
df1_New
write.csv(df1_New,"/Users/dilant/Desktop/df1_New.csv", row.names = FALSE) #save df as csv


## make a vector of RNA amounts
amount <- rep(c(-0.698970004, -1.397940009, -2.096910013, -2.795880017, -3.494850022, -4.193820026), each = 3)
amount
                         
df2 <- cbind(df1_New, amount = amount) #new column with vector RNA amounts
df2

# Plot the standard curve with Rsquare and y-intercept added 


P<- ggplot(df2, aes(x = amount, y = Mean, colour = Target))+
  geom_smooth(method="lm")+
  geom_point()+
  stat_regline_equation(label.x = -Inf, label.y = Inf, vjust = 1.5, hjust = -0.1, size = 3)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")),
           label.y= Inf, label.x = Inf, vjust = 1, hjust = 1.1, size = 3) +   #Got this from https://stackoverflow.com/questions/61266084/can-we-neatly-align-the-regression-equation-and-r2-and-p-value
  xlab("Log 10 (dilution factor)") + ylab("Treshold Cycle (Cq)") +
  facet_wrap(~Target, scales = "free") # it will work even with facetted graphs with different scales. 
P

## Change the color of plot to Wespalette 
P + scale_color_manual(values=wes_palette(n=3, name="Zissou1"))

## calculate rsquare 

rSquare<- df3[,summary(lm(Mean~amount))$r.squared,by=Target]

##calculate the regression coefficient r^2
df3<- data.table(df2)
yIntercept<-df3[,as.list(coef(lm(Mean ~ amount))), by=Target]

## Alternative way

#fits <- lmList(Mean ~ amount | Target, data=df2)  
#fits






#-------------------------------------


