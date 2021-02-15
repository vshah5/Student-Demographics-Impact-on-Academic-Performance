#setwd to wherever the file is located first
setwd("C:/Users/valay/OneDrive/Desktop/Ivey/Fall Term 2020/Business Statistics/Team Assignment")

StudentsPerf <- read.csv("StudentsPerformance.csv", header=TRUE)
StudentsPerf

#Change numerical values to factors
StudentsPerf$gender <- factor(StudentsPerf$gender)
StudentsPerf$parental.level.of.education <- factor(StudentsPerf$parental.level.of.education)
StudentsPerf$race.ethnicity <- factor(StudentsPerf$race.ethnicity)
StudentsPerf$lunch <- factor(StudentsPerf$lunch)
StudentsPerf$test.preparation.course <- factor(StudentsPerf$test.preparation.course)

#median, summary stats, sd for all three numerical variables
median(StudentsPerf$math.score)
summary(StudentsPerf$math.score)
sd(StudentsPerf$math.score)

median(StudentsPerf$reading.score)
summary(StudentsPerf$reading.score)
sd(StudentsPerf$reading.score)

median(StudentsPerf$writing.score)
summary(StudentsPerf$writing.score)
sd(StudentsPerf$writing.score)

#summary statistics for all variables overall
summary(StudentsPerf)

#historgrams for the three courses
#pdf("plots.pdf")
hist(StudentsPerf$math.score, xlab="Math Score", main="Student's Math Scores Distribution", col="lightblue3")
hist(StudentsPerf$reading.score, xlab="Reading Score", main="Student's Reading Scores Distribution", col="orange")
hist(StudentsPerf$writing.score, , xlab="Writing Score", main="Student's Writing Scores Distribution", col="lightgreen")


#histogram with the norm distributions side by side
par(mfrow = c(1,3))
hist(StudentsPerf$math.score, xlab="Math Score", main="Student's Math Scores Distribution", col="lightblue3")
hist(StudentsPerf$reading.score, xlab="Reading Score", main="Student's Reading Scores Distribution", col="orange")
hist(StudentsPerf$writing.score, , xlab="Writing Score", main="Student's Writing Scores Distribution", col="lightgreen")

#mean and median comparisons across all variables
sapply(StudentsPerf, mean)
sapply(StudentsPerf, median)

#Scatterplots
plot(StudentsPerf$math.score ~ StudentsPerf$reading.score, ylab="Math Score", xlab="Reading Score",
     main="Scatterplot of Math Score versus Reading",
     pch=19, col="blue", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$reading.score),col="red",lwd=2)
plot(StudentsPerf$math.score ~ StudentsPerf$writing.score, ylab="Math Score", xlab="Writing Score",
     main="Scatterplot of Math Score versus Writing",
     pch=19, col="blue", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$writing.score),col="red",lwd=2)
plot(StudentsPerf$reading.score ~ StudentsPerf$writing.score, ylab="Reading Score", xlab="Writing Score",
     main="Scatterplot of Reading Score versus Writing",
     pch=19, col="blue", data=StudentsPerf)
lines(lowess(StudentsPerf$reading.score ~ StudentsPerf$writing.score),col="red",lwd=2)

par(mfrow = c(1,3))
plot(StudentsPerf$math.score ~ StudentsPerf$gender, ylab="Math Score", xlab="Gender",
     main="Boxplot of Math Score versus Gender",
     pch=19, col="lightblue3", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$gender),col="red",lwd=2)
plot(StudentsPerf$reading.score ~ StudentsPerf$gender, ylab="Reading Score", xlab="Gender",
     main="Boxplot of Reading Score versus Gender",
     pch=19, col="orange", data=StudentsPerf)
lines(lowess(StudentsPerf$reading.score ~ StudentsPerf$gender),col="red",lwd=2)
plot(StudentsPerf$writing.score ~ StudentsPerf$gender, ylab="Writing Score", xlab="Gender",
     main="Boxplot of Writing Score versus Gender",
     pch=19, col="lightgreen", data=StudentsPerf)
lines(lowess(StudentsPerf$writing.score ~ StudentsPerf$gender),col="red",lwd=2)

plot(StudentsPerf$math.score ~ StudentsPerf$race.ethnicity, ylab="Math Score", xlab="Ethnicity",
     main="Boxplot of Math Score versus Ethnicity",
     pch=19, col="lightblue3", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$race.ethnicity),col="red",lwd=2)
plot(StudentsPerf$reading.score ~ StudentsPerf$race.ethnicity, ylab="Reading Score", xlab="Ethnicity",
     main="Boxplot of Reading Score versus Ethnicity",
     pch=19, col="orange", data=StudentsPerf)
lines(lowess(StudentsPerf$reading.score ~ StudentsPerf$race.ethnicity),col="red",lwd=2)
plot(StudentsPerf$writing.score ~ StudentsPerf$race.ethnicity, ylab="Writing Score", xlab="Ethnicity",
     main="Boxplot of Writing Score versus Ethnicity",
     pch=19, col="lightgreen", data=StudentsPerf)
lines(lowess(StudentsPerf$writing.score ~ StudentsPerf$race.ethnicity),col="red",lwd=2)

par(mfrow = c(1,1))
plot(StudentsPerf$math.score ~ StudentsPerf$parental.level.of.education, ylab="Math Score", xlab="Parent's Education",
     main="Boxplot of Math Score versus Parent's Education",
     pch=19, col="lightblue3", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$parental.level.of.education),col="red",lwd=2)
plot(StudentsPerf$reading.score ~ StudentsPerf$parental.level.of.education, ylab="Reading Score", xlab="Parent's Education",
     main="Boxplot of Reading Score versus Parent's Education",
     pch=19, col="orange", data=StudentsPerf)
lines(lowess(StudentsPerf$reading.score ~ StudentsPerf$parental.level.of.education),col="red",lwd=2)
plot(StudentsPerf$writing.score ~ StudentsPerf$parental.level.of.education, ylab="Writing Score", xlab="Parent's Education",
     main="Boxplot of Writing Score versus Parent's Education",
     pch=19, col="lightgreen", data=StudentsPerf)
lines(lowess(StudentsPerf$writing.score ~ StudentsPerf$parental.level.of.education),col="red",lwd=2)

par(mfrow = c(1,3))
plot(StudentsPerf$math.score ~ StudentsPerf$lunch, ylab="Math Score", xlab="Lunch",
     main="Boxplot of Math Score versus Lunch",
     pch=19, col="lightblue3", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$lunch),col="red",lwd=2)
plot(StudentsPerf$reading.score ~ StudentsPerf$lunch, ylab="Reading Score", xlab="Lunch",
     main="Boxplot of Reading Score versus Lunch",
     pch=19, col="orange", data=StudentsPerf)
lines(lowess(StudentsPerf$reading.score ~ StudentsPerf$lunch),col="red",lwd=2)
plot(StudentsPerf$writing.score ~ StudentsPerf$lunch, ylab="Writing Score", xlab="Lunch",
     main="Boxplot of Writing Score versus Lunch",
     pch=19, col="lightgreen", data=StudentsPerf)
lines(lowess(StudentsPerf$writing.score ~ StudentsPerf$lunch),col="red",lwd=2)

plot(StudentsPerf$math.score ~ StudentsPerf$test.preparation.course, ylab="Math Score", xlab="Test Preparation Course Taken",
     main="Boxplot of Math Score versus If Test Preparation Course Taken",
     pch=19, col="lightblue3", data=StudentsPerf)
lines(lowess(StudentsPerf$math.score ~ StudentsPerf$test.preparation.course),col="red",lwd=2)
plot(StudentsPerf$reading.score ~ StudentsPerf$test.preparation.course, ylab="Reading Score", xlab="Test Preparation Course Taken",
     main="Boxplot of Reading Score versus If Test Preparation Course Taken",
     pch=19, col="orange", data=StudentsPerf)
lines(lowess(StudentsPerf$reading.score ~ StudentsPerf$test.preparation.course),col="red",lwd=2)
plot(StudentsPerf$writing.score ~ StudentsPerf$test.preparation.course, ylab="Writing Score", xlab="Test Preparation Course Taken",
     main="Boxplot of Writing Score versus If Test Preparation Course Taken",
     pch=19, col="lightgreen", data=StudentsPerf)
lines(lowess(StudentsPerf$writing.score ~ StudentsPerf$test.preparation.course),col="red",lwd=2)

plot(StudentsPerf$lunch ~ StudentsPerf$test.preparation.course, ylab="Math Score", xlab="Test Preparation Course Taken",
     main="Boxplot of Lunch versus If Test Preparation Course Taken",
     pch=19, col="lightblue", data=StudentsPerf)
lines(lowess(StudentsPerf$lunch ~ StudentsPerf$test.preparation.course),col="red",lwd=2)
plot(StudentsPerf$gender ~ StudentsPerf$test.preparation.course, ylab="Reading Score", xlab="Test Preparation Course Taken",
     main="Boxplot of Reading Score versus If Test Preparation Course Taken",
     pch=19, col="lightblue", data=StudentsPerf)
lines(lowess(StudentsPerf$gender ~ StudentsPerf$test.preparation.course),col="red",lwd=2)
plot(StudentsPerf$gender ~ StudentsPerf$lunch, ylab="Writing Score", xlab="Test Preparation Course Taken",
     main="Boxplot of Writing Score versus If Test Preparation Course Taken",
     pch=19, col="lightblue", data=StudentsPerf)
lines(lowess(StudentsPerf$gender ~ StudentsPerf$lunch),col="red",lwd=2)
plot(StudentsPerf$gender ~ StudentsPerf$race.ethnicity, ylab="Gender", xlab="Ethnicity",
     main="Boxplot of Gender versus Ethnicity",
     pch=19, col="lightblue", data=StudentsPerf)
lines(lowess(StudentsPerf$gender ~ StudentsPerf$race.ethnicity),col="red",lwd=2)
plot(StudentsPerf$race.ethnicity ~ StudentsPerf$parental.level.of.education, ylab="Ethnicity", xlab="Parent's Education",
     main="Boxplot of Ethnicity versus Parent's Education",
     pch=19, col="lightblue", data=StudentsPerf)
lines(lowess(StudentsPerf$race.ethnicity ~ StudentsPerf$parental.level.of.education),col="red",lwd=2)

#We can add more scatterplots based on what we want to analyze, scatterplots will only work once 
#dummy variables have been assigned

plot(StudentsPerf)

#Strip charts to show impact on scores based on factors
par(mfrow=c(1,2))

stripchart(math.score ~ gender, vertical=TRUE, data=StudentsPerf)
stripchart(math.score ~ race.ethnicity, vertical=TRUE, data=StudentsPerf)
stripchart(math.score ~ parental.level.of.education, vertical=TRUE, data=StudentsPerf)
stripchart(math.score ~ lunch, vertical=TRUE, data=StudentsPerf)
stripchart(math.score ~ test.preparation.course, vertical=TRUE, data=StudentsPerf)

stripchart(reading.score ~ gender, vertical=TRUE, data=StudentsPerf)
stripchart(reading.score ~ race.ethnicity, vertical=TRUE, data=StudentsPerf)
stripchart(reading.score ~ parental.level.of.education, vertical=TRUE, data=StudentsPerf)
stripchart(reading.score ~ lunch, vertical=TRUE, data=StudentsPerf)
stripchart(reading.score ~ test.preparation.course, vertical=TRUE, data=StudentsPerf)

stripchart(writing.score ~ gender, vertical=TRUE, data=StudentsPerf)
stripchart(writing.score ~ race.ethnicity, vertical=TRUE, data=StudentsPerf)
stripchart(writing.score ~ parental.level.of.education, vertical=TRUE, data=StudentsPerf)
stripchart(writing.score ~ lunch, vertical=TRUE, data=StudentsPerf)
stripchart(writing.score ~ test.preparation.course, vertical=TRUE, data=StudentsPerf)

#Impact of factors on median Math Score
aggregate(StudentsPerf$math.score, by=list(StudentsPerf$gender), FUN=median)
aggregate(StudentsPerf$math.score, by=list(StudentsPerf$race.ethnicity), FUN=median)
aggregate(StudentsPerf$math.score, by=list(StudentsPerf$parental.level.of.education), FUN=median)
aggregate(StudentsPerf$math.score, by=list(StudentsPerf$lunch), FUN=median)
aggregate(StudentsPerf$math.score, by=list(StudentsPerf$test.preparation.course), FUN=median)

#Impact of factors on median Reading Score
aggregate(StudentsPerf$reading.score, by=list(StudentsPerf$gender), FUN=median)
aggregate(StudentsPerf$reading.score, by=list(StudentsPerf$race.ethnicity), FUN=median)
aggregate(StudentsPerf$reading.score, by=list(StudentsPerf$parental.level.of.education), FUN=median)
aggregate(StudentsPerf$reading.score, by=list(StudentsPerf$lunch), FUN=median)
aggregate(StudentsPerf$reading.score, by=list(StudentsPerf$test.preparation.course), FUN=median)

#Impact of factors on median Writing Score
aggregate(StudentsPerf$writing.score, by=list(StudentsPerf$gender), FUN=median)
aggregate(StudentsPerf$writing.score, by=list(StudentsPerf$race.ethnicity), FUN=median)
aggregate(StudentsPerf$writing.score, by=list(StudentsPerf$parental.level.of.education), FUN=median)
aggregate(StudentsPerf$writing.score, by=list(StudentsPerf$lunch), FUN=median)
aggregate(StudentsPerf$writing.score, by=list(StudentsPerf$test.preparation.course), FUN=median)

#ggplot
library(ggplot2)
p1 <- ggplot(data=StudentsPerf, mapping=aes(x=test.preparation.course, y=math.score)) +
  geom_point()
p1 + labs(x="Test Prep Course Completed",y="Math score",
          title="Math score based on Test Prep Course Comepleted") +
  theme(plot.title=element_text(hjust=0.5))
p1 + facet_grid(~race.ethnicity)

#ggplot of gender, ethnicity and lunch option affecting math score
p1 + facet_grid(factor(parental.level.of.education)~factor(race.ethnicity))

#ggplot of gender, ethnicity and test prep course completion affecting math score
p1 + facet_grid(factor(gender)~factor(race.ethnicity))



p2 <- ggplot(data=StudentsPerf, mapping=aes(x=test.preparation.course, y=reading.score)) +
  geom_point()
p2 + labs(x="Test Prep Course Completed",y="Reading score",
          title="Reading score based on Test Prep Course Completed") +
  theme(plot.title=element_text(hjust=0.5))
p2 + facet_grid(~race.ethnicity)

#ggplot of gender, ethnicity and lunch option affecting reading score
p2 + facet_grid(factor(parental.level.of.education)~factor(race.ethnicity))

#ggplot of gender, ethnicity and test prep course completion affecting reading score
p2 + facet_grid(factor(gender)~factor(race.ethnicity))


p3 <- ggplot(data=StudentsPerf, mapping=aes(x=test.preparation.course, y=writing.score)) +
  geom_point()
p3 + labs(x="Test Prep Course Completed",y="Writing score",
          title="Writing score based on Test Prep Course Comepleted") +
  theme(plot.title=element_text(hjust=0.5))
p3 + facet_grid(~race.ethnicity)

#ggplot of gender, ethnicity and lunch option affecting reading score
p3 + facet_grid(factor(parental.level.of.education)~factor(race.ethnicity))

#ggplot of gender, ethnicity and test prep course completion affecting reading score
p3 + facet_grid(factor(gender)~factor(race.ethnicity))



#ggplot with categorical variables
#Math vs Reading Scores
p4 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, y=reading.score, colour=test.preparation.course,
                                       shape=factor(gender)))
p4 + geom_point(size=2)

p7 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, y=reading.score, colour=parental.level.of.education,
                                            shape=factor(test.preparation.course)))
p7 + geom_point(size=2)

p8 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, y=reading.score, colour=race.ethnicity,
                                            shape=factor(test.preparation.course)))
p8 + geom_point(size=2)


#Math vs Writing Scores
p5 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, y=writing.score, colour=test.preparation.course,
                                            shape=factor(gender)))
p5 + geom_point(size=2)

p9 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, y=writing.score, colour=parental.level.of.education,
                                            shape=factor(test.preparation.course)))
p9 + geom_point(size=2)

p10 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, y=reading.score, colour=race.ethnicity,
                                            shape=factor(test.preparation.course)))
p10 + geom_point(size=2)


#Writing vs Reading Scores
p6 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, y=reading.score, colour=test.preparation.course,
                                            shape=factor(gender)))
p6 + geom_point(size=2)

p11 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, y=reading.score, colour=parental.level.of.education,
                                            shape=factor(test.preparation.course)))
p11 + geom_point(size=2)

p12 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, y=reading.score, colour=race.ethnicity,
                                             shape=factor(test.preparation.course)))
p12 + geom_point(size=2)


#Density plots for math scores
p13 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, fill=factor(gender)))
p13 + geom_density(alpha=.3) + labs(x="Math Score", y="Density", title="Density of Math Scores by Gender") + 
  theme(plot.title=element_text(hjust=0.5))

#For p14 and p15 color=factor and geom_density() of 0 was used because filling the areas is too chaotic
p14 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, colour=factor(race.ethnicity)))
p14 + geom_density() + labs(x="Math Score", y="Density", title="Density of Math Scores by Ethnicity") + 
  theme(plot.title=element_text(hjust=0.5))

p15 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, color=factor(parental.level.of.education)))
p15 + geom_density() + labs(x="Math Score", y="Density", title="Density of Math Scores by Parent's Level of Education") + 
  theme(plot.title=element_text(hjust=0.5))

p16 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, fill=factor(lunch)))
p16 + geom_density(alpha=.3) + labs(x="Math Score", y="Density", title="Density of Math Scores by Lunch option") + 
  theme(plot.title=element_text(hjust=0.5))

p17 <- ggplot(data=StudentsPerf, mapping=aes(x=math.score, fill=factor(test.preparation.course)))
p17 + geom_density(alpha=.3) + labs(x="Math Score", y="Density", title="Density of Math Scores by if Test Prep Course was Taken") + 
  theme(plot.title=element_text(hjust=0.5))


#Density plots for reading scores
p18 <- ggplot(data=StudentsPerf, mapping=aes(x=reading.score, fill=factor(gender)))
p18 + geom_density(alpha=.3) + labs(x="Reading Score", y="Density", title="Density of Reading Scores by Gender") + 
  theme(plot.title=element_text(hjust=0.5))

p19 <- ggplot(data=StudentsPerf, mapping=aes(x=reading.score, colour=factor(race.ethnicity)))
p19 + geom_density() + labs(x="Reading Score", y="Density", title="Density of Reading Scores by Ethnicity") + 
  theme(plot.title=element_text(hjust=0.5))

p20 <- ggplot(data=StudentsPerf, mapping=aes(x=reading.score, colour=factor(parental.level.of.education)))
p20 + geom_density() + labs(x="Reading Score", y="Density", title="Density of Reading Scores by Parent's Level of Education") + 
  theme(plot.title=element_text(hjust=0.5))

p21 <- ggplot(data=StudentsPerf, mapping=aes(x=reading.score, fill=factor(lunch)))
p21 + geom_density(alpha=.3) + labs(x="Reading Score", y="Density", title="Density of Reading Scores by Lunch option") + 
  theme(plot.title=element_text(hjust=0.5))

p22 <- ggplot(data=StudentsPerf, mapping=aes(x=reading.score, fill=factor(test.preparation.course)))
p22 + geom_density(alpha=.3) + labs(x="Reading Score", y="Density", title="Density of Reading Scores by if Test Prep Course was Taken") + 
  theme(plot.title=element_text(hjust=0.5))


#Density plots for writing scores
p18 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, fill=factor(gender)))
p18 + geom_density(alpha=.3) + labs(x="Writing Score", y="Density", title="Density of Writing Scores by Gender") + 
  theme(plot.title=element_text(hjust=0.5))

p19 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, colour=factor(race.ethnicity)))
p19 + geom_density() + labs(x="Writing Score", y="Density", title="Density of Writing Scores by Ethnicity") + 
  theme(plot.title=element_text(hjust=0.5))

p20 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, colour=factor(parental.level.of.education)))
p20 + geom_density() + labs(x="Writing Score", y="Density", title="Density of Writing Scores by Parent's Level of Education") + 
  theme(plot.title=element_text(hjust=0.5))

p21 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, fill=factor(lunch)))
p21 + geom_density(alpha=.3) + labs(x="Writing Score", y="Density", title="Density of Writing Scores by Lunch option") + 
  theme(plot.title=element_text(hjust=0.5))

p22 <- ggplot(data=StudentsPerf, mapping=aes(x=writing.score, fill=factor(test.preparation.course)))
p22 + geom_density(alpha=.3) + labs(x="Writing Score", y="Density", title="Density of Writing Scores by if Test Prep Course was Taken") + 
  theme(plot.title=element_text(hjust=0.5))


#Regression
#Math Score plots
#Math score and Parent's Ed
model1 <- lm(math.score ~ parental.level.of.education, data=StudentsPerf)
summary(model1)
model1$coefficients

plot(math.score ~ parental.level.of.education, ylab="Math Scores", xlab="Parent's Education Level",
     pch=19, col="lightgrey", data=StudentsPerf)
abline(model1, col="red", lwd=2)

#Math score and gender
model2 <- lm(math.score ~ gender, data=StudentsPerf)
summary(model2)
model2$coefficients

plot(math.score ~ gender, ylab="Math Scores", xlab="Gender",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model2, col="red", lwd=2)

#Math score and ethnicity
model3 <- lm(math.score ~ race.ethnicity, data=StudentsPerf)
summary(model3)
model3$coefficients

plot(math.score ~ race.ethnicity, ylab="Math Scores", xlab="Ethnicity",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model3, col="red", lwd=2)

#Math score and test prep course taken
model4 <- lm(math.score ~ test.preparation.course, data=StudentsPerf)
summary(model4)
model4$coefficients

plot(math.score ~ test.preparation.course, ylab="Math Scores", xlab="Test Preparation Course Taken",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model4, col="red", lwd=2)

#Math score and lunch
model5 <- lm(math.score ~ lunch, data=StudentsPerf)
summary(model5)
model5$coefficients

plot(math.score ~ lunch, ylab="Math Scores", xlab="Lunch",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model5, col="red", lwd=2)


#Reading Score plots
#Reading score and Parent's Ed
model6 <- lm(reading.score ~ parental.level.of.education, data=StudentsPerf)
summary(model6)
model6$coefficients

plot(reading.score ~ parental.level.of.education, ylab="Reading Scores", xlab="Parent's Education Level",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model6, col="red", lwd=2)

#Reading score and gender
model7 <- lm(reading.score ~ gender, data=StudentsPerf)
summary(model7)
model7$coefficients

plot(reading.score ~ gender, ylab="Reading Scores", xlab="Gender",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model7, col="red", lwd=2)

#Reading score and ethnicity
model8 <- lm(reading.score ~ race.ethnicity, data=StudentsPerf)
summary(model8)
model8$coefficients

plot(reading.score ~ race.ethnicity, ylab="Reading Scores", xlab="Ethnicity",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model8, col="red", lwd=2)

#Reading score and test prep course taken
model9 <- lm(reading.score ~ test.preparation.course, data=StudentsPerf)
summary(model9)
model9$coefficients

plot(reading.score ~ test.preparation.course, ylab="Reading Scores", xlab="Test Preparation Course Taken",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model9, col="red", lwd=2)

#Reading score and lunch
model10 <- lm(reading.score ~ lunch, data=StudentsPerf)
summary(model10)
model10$coefficients

plot(reading.score ~ lunch, ylab="Reading Scores", xlab="Lunch",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model10, col="red", lwd=2)


#Writing Score plots
#Writing score and Parent's Ed
model11 <- lm(writing.score ~ parental.level.of.education, data=StudentsPerf)
summary(model11)
model11$coefficients

plot(writing.score ~ parental.level.of.education, ylab="Writing Scores", xlab="Parent's Education Level",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model11, col="red", lwd=2)

#Writing score and gender
model12 <- lm(writing.score ~ gender, data=StudentsPerf)
summary(model12)
model12$coefficients

plot(writing.score ~ gender, ylab="Writing Scores", xlab="Gender",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model12, col="red", lwd=2)

#Writing score and ethnicity
model13 <- lm(writing.score ~ race.ethnicity, data=StudentsPerf)
summary(model13)
model13$coefficients

plot(writing.score ~ race.ethnicity, ylab="Writing Scores", xlab="Ethnicity",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model13, col="red", lwd=2)

#Writing score and test prep course taken
model14 <- lm(writing.score ~ test.preparation.course, data=StudentsPerf)
summary(model14)
model14$coefficients

plot(writing.score ~ test.preparation.course, ylab="Writing Scores", xlab="Test Preparation Course Taken",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model14, col="red", lwd=2)

#Writing score and lunch
model15 <- lm(writing.score ~ lunch, data=StudentsPerf)
summary(model15)
model15$coefficients

plot(writing.score ~ lunch, ylab="Writing Scores", xlab="Lunch",
     pch=19, col="lightblue", data=StudentsPerf)
abline(model15, col="red", lwd=2)


#Predict using regression model
predict(model1, newdata=data.frame(parental.level.of.education="some college"))
predict(model2, newdata=data.frame(gender="male"))
predict(model8, newdata=data.frame(race.ethnicity="group C"))
predict(model14, newdata=data.frame(test.preparation.course="completed"))
predict(model15, newdata=data.frame(lunch="standard"))

#dev.off()

#Convert to dummy variables here:
#The dummy variables are currently factors, they are listed below:
#StudentsPerf$gender 
#StudentsPerf$parental.level.of.education 
#StudentsPerf$race.ethnicity 
#StudentsPerf$lunch 
#StudentsPerf$test.preparation.course 

