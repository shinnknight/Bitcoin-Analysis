# Frameworks & Methods Midterm FALL 2017
#STUDENT NAME:


#You are provided with a file "BMIclass.csv" which contains data on
#age, weight, height, body mass index (BMI), and BMI classification (Class).
#You will be asked to load the dataset into R from this file, perform
#some data cleaning,and develop some basic diagnostics on this dataset. 

#This midterm is worth 60 points. Please submit an executable 
#R file with all the required libraries installed. 
#As much as you can, please document your code.

#BEGIN--------------------------------------------------------------#

#Section I:Reading and Loading data from .csv files

#Q1.
#Read the file "bmiclass.csv" 
#Load the contents in an dataset called "bmiclass"

bmiclass<-read.csv('bmiclass.csv')

#Q2.View the contents of bmiclass and examine the structure

View(bmiclass)

#Q3.Summarize the dataset bmiclass

summary(bmiclass)

#-----------------------------------------------------------------#

#SECTION II:Data Cleaning

#Q4.Identify data entry inconsistencies/errors in the variable "Class".
#for example, some entries maybe in upper case and some in lower case.

unique(bmiclass$Class)

#Q5.Clean the data entry errors to ensure consistency of text, 
#i.e. all either upper case or all lower case

bmiclass$Class[which(bmiclass$Class=='NORMAL')]<-'normal'

# bmiclass$Class = tolower(bmiclass$Class)
#Dealing with Missing values
#Q6.Identify location of missing values i.e. list the rows containing missing values

bmiclass[which(rowSums(is.na(bmiclass))!=0),]

#Q7. Compute the mean of BMI while retaining missing values

mean(bmiclass$BMI,na.rm = T)

#Q8.Impute by replacing all the missing values with a value = 21

bmiclass$BMI[is.na(bmiclass$BMI)]=21


#------------------------------------------------------------------#

#SECTION III: Data Vizualization

#Q9. Graph a scatterplot between the variables of bmiclass
# scatterplotMatrix(~age+ht_cm+wt_kg+BMI,data=bmiclass, main="ddfdf")
plot(bmiclass)
#Q10.Identifying any outliers in the dataset bmiclass

outlier_value <- boxplot(bmiclass)$out
boxplot(bmiclass,main='outlier',boxwex=0.1) 
points(outlier_value,col='blue')
mtext(paste("outliers:",paste(outlier_value,collapse = ",")),cex = 0.6)

#Q11.Impute the outlier value with the average value for the associated Class

a<-boxplot(bmiclass[,c(1:4)])
c<- bmiclass
for(i in c(1:length(a$group))){
  # (bmiclass[which(bmiclass[,a$group[i]] == a$out[i]),a$group[i]]) <-
  c[which(c[,a$group[i]] == a$out[i]),a$group[i]] <- mean(c[,a$group[i]])
}


#Q12.Create a bargraph of average BMI by Class
bmiclass$Class <- tolower(bmiclass$Class)
barplot(by(bmiclass$BMI,bmiclass$Class,mean),ylim = c(0,1.1*max(by(bmiclass$BMI,bmiclass$Class,mean))))

#Q13. Create a scatterplot of age and BMI, differentiated by Class
#Provide an interpretation of your the output.
library(ggplot2)
p <- ggplot(data = bmiclass)
p + geom_point(mapping = aes(x=bmiclass$age,y=bmiclass$BMI,col=bmiclass$Class))

# plot(x=bmiclass$age,y=bmiclass$BMI,col=bmiclass$Class)


#Q14. Add a smoothing line to the scatterplot created in Q14.
q<- ggplot(data = bmiclass, mapping = aes(x=age,y=BMI))
q + geom_point(mapping = aes(color = Class)) +
  geom_smooth(method = 'loess', span = 30)

# p + geom_point(mapping = aes(x=bmiclass$age,y=bmiclass$BMI,col=bmiclass$Class))
# + geom_line(( aes(x=bmiclass$age,y=bmiclass$BMI,col=bmiclass$Class)))

#Q15. Create a histogram of BMI, differentiated by Class.
#Provide an interpretation of your the output.
ggplot(data = bmiclass, mapping = aes(x=BMI)) +
  geom_histogram(mapping = aes(fill = Class),binwidth = 0.5)


#Q16. Create a boxplots of BMI by Class.
#Provide an interpretation of your the output.
ggplot(data = bmiclass, mapping = aes(x=Class,y=BMI)) +
geom_boxplot(mapping = aes(color = Class))

#END----------------------------------------------------------------#


