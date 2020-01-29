#Data Processing 
dt<-sote_data_part
library(dplyr)
dt=dt%>%filter(grade!='NA'&Q13!='NA'&Q14!='NA'&Q13!='6'&Q15!='NA'&course.level!='NA')

dt$semester=as.factor(dt$semester)
dt$grade=as.factor(dt$grade)
dt$dept=as.factor(dt$dept)
dt$grading=as.factor(dt$grading)
dt$course.level=as.factor(dt$course.level)
dt$dept=as.factor(dt$dept)
dt$college=as.factor(dt$college)
dt$student.level=as.factor(dt$student.level)
dt$Q13=as.factor(dt$Q13)
dt$Q14=as.factor(dt$Q14)
dt$Q15=as.factor(dt$Q15)

dt1=dt
dt1$grade1='Other'
dt1[grep('A+',dt1$grade),]$grade1='A'
dt1[grep('B+',dt1$grade),]$grade1='B'
dt1[grep('C+',dt1$grade),]$grade1='C'
dt1[grep('D+',dt1$grade),]$grade1='D or F'
dt1[grep('F+',dt1$grade),]$grade1='D or F'
dt1$grade1=as.factor(dt1$grade1)
summary(dt1$grade1)
dt1$rating='Ineffective'
dt1[grep(4,dt1$Q13),]$rating='Effective'
dt1[grep(5,dt1$Q13),]$rating='Effective'
dt1$rating=as.factor(dt1$rating)
dt1$has.sat <- ifelse(dt1$rating=='Effective', 1, 0)
dt1$has.sat =as.factor(dt1$has.sat )
dt1$semester <- ifelse(dt1$semester==2164, 'Fall2016', 'Spring2017')
summary(dt1)

#Data Explorating 
barplot(table(dt1$semester), main = "Semester",
        ylab = "Count",names.arg = c("Fall 2016","Spring 2017"),cex.names =2,cex.main=2)
barplot(table(dt1$grade1), main = "Grades of Students",
        ylab = "Count", names.arg = c("A", "B", "C", "D or F", "Other"),cex.names =2,cex.main=2)
barplot(table(dt1$Q14)[1:5], main = "Expected Grades",
        ylab = "Count", names.arg = c("A", "B", "C", "D or F", "Other"),cex.names =2,cex.main=2)
t1=table(dt1$student.level)
barplot(c(t1[2],t1[6],t1[4],t1[7],t1[3],t1[1],t1[5]),main = "Student Level",
        ylab = "Count",cex.names =2,cex.main=2)
barplot(table(dt1$Q15)[1:7], main = "Year of Students",
        ylab = "Count", names.arg = c("Freshman","Sophomore","Junior","Senior",
                                      "Graduate","Credential Only",
                                      "Other"),cex.names =1.5,cex.main=2)
barplot(table(dt1$course.level), main = "Course Level",
        ylab = "Count",names.arg = c("Graduate","Lower D","Upper D"),,cex.names =2,cex.main=2)
barplot(table(dt1$dept), main = "Dept",
        ylab = "Count",cex.main=2,las=2)
barplot(table(dt1$college), main = "College",
        ylab = "Count",cex.names =2,cex.main=2)
hist(dt1$registered,probability = T)
dt1$class_strength=round(dt1$registered/dt1$cap,2)
# registered (with < 150 count)
registered150 <- dt1$registered[dt1$registered < 150]
hist(registered150, probability = TRUE, breaks = 30, xlab = "", main = "Registered")
lines(density(registered150))
# class_strength
hist(dt1$class_strength, breaks = 100, probability = TRUE, xlab = "", main = "")
lines(density(dt1$class_strength))

barplot(table(dt1$Q13)[1:5], main = "Teaching Rating",
        ylab = "Count", names.arg = c("Very Ineffective", 
                                      "Ineffective", "Somewhat Effective", 
                                      "Effective", "Very Effective"),cex.names =1.2,cex.main=2)

# Data Analysis 
dt1=dt1[,c(-2,-16,-17,-19)]
#Reduced Model
dt2.glm <- glm(has.sat ~  Q14, data=dt1,family = binomial,maxit=1)
summary(dt2.glm)
summary(dt1)
logistic.measures <- function(x) {
  coefs <- x$coefficients
  p <- length(coefs)
  if (p > 1) {
    logits <- coefs[2:p] + coefs[1]
    logits <- c(coefs[1], logits)
    odds <- exp(logits)
    probs <- odds / (1 + odds)
  }
  
  cbind(logits, odds, probs)
}
logistic.measures(dt2.glm)

#Odds Ratio
odds.ratios <- function(m, type = "local") {
  nr <- nrow(m)
  if (nr < 2) stop("number of rows is less than two")
  nc <- ncol(m)
  if (nc < 2) stop("number of columns is less than two")
  if (length(type) > 1) stop("only one type is currently allowed")
  
  opts <- c("local", "global")
  type <- pmatch(type, opts)
  if (is.na(type)) stop("only \"local\" or \"global\" allowed for type")
  
  result <- matrix(NA, nrow = nr - 1, ncol = nc - 1)
  
  if (type == 1)
    for (i in 1:(nr - 1))
      for (j in 1:(nc - 1))
        result[i, j] <- m[i, j] * m[i + 1, j + 1] / (m[i, j + 1] * m[i + 1, j])
  
  if (type == 2)
    for (i in 1:(nr - 1))
      for (j in 1:(nc - 1)) {
        num <- as.numeric(sum(m[1:i, 1:j])) * as.numeric(sum(m[(i+1):nr, (j+1):nc]))
        den <- as.numeric(sum(m[1:i, (j+1):nc])) * as.numeric(sum(m[(i+1):nr, 1:j]))
        result[i, j] <- num / den
      }
  
  result
}

t2=table(dt1$rating,dt1$Q14)
chisq.test(t2)
round(odds.ratios(t2,type="local"),2)

#Saturated Model
dt1.glm <- glm(has.sat ~  semester+grading+course.level+dept+college+student.level+
                 enroll+cap+registered+Q14+Q15+
                 grade1+class_strength, data = dt1, family = binomial,maxit=1)
summary(dt1.glm)


#Residuals
fit=dt1.glm
# deviance residuals (default)
rdev=resid(fit); plot(rdev)
# standardized deviance residuals
rdev.s <- rstandard(fit); plot(fit)

#Plotting residuals
col1=c("black","red")
cols=sapply(1:nrow(dt1),function(i) col1[dt1$rating[i]])
rdev.s <- rstandard(fit)
plot(rdev.s, pch = 20, cex = 0.8,ylim = c(-2.5, 3),
     col = cols,ylab="")
leg.text <- c("Effective","Ineffective")
title(xlab = "Index", ylab = "Standardized deviance residuals")
legend(0, 3.2, leg.text, pch = 20, col = col1, cex = 0.8, bty = "n")
abline(h = 0, lty = 3, col = "gray50")

# label outliers
indx <- which.max(rdev.s)
text(indx, rdev.s[indx], indx, pos = 4, cex = 1.5)
indx1 <- which.min(rdev.s)
text(indx1, rdev.s[indx1], indx1, pos = 4, cex = 1.5)

#Cook's distance
plot(cooks.distance(fit), type = "h", col = "gray20",
     ylab = "Cook's Distance")
indx <- which(cooks.distance(fit) > 0.006)
text(cooks.distance(fit)[indx] ~ indx, labels = indx,
     cex =2, pos = 4, offset = 0.4)
points(indx, cooks.distance(fit)[indx], pch = 21, 
       bg = cols[indx],cex=2)
legend("topleft", c("Effective","Ineffective"), pch = 21, pt.bg = col1,
       cex = 0.7, box.col = "gray80")

#Confusion Matrix, ROC curve
confusion <- function(m, threshold = 0.5) {
  pred <- ifelse(m$fitted.values > threshold, 1, 0)
  confused <- matrix(NA, nrow = 2, ncol = 3)
  
  indx <- (m$y == 1)
  confused[1, 3] <- sum(indx)
  confused[2, 3] <- sum(!indx)
  
  confused[1, 1] <- sum(m$y[indx] == pred[indx])
  confused[1, 2] <- confused[1, 3] - confused[1, 1]
  confused[2, 2] <- sum(m$y[!indx] == pred[!indx]) 
  confused[2, 1] <- confused[2, 3] - confused[2, 2]
  
  rownames(confused) <- c("1", "0")
  colnames(confused) <- c("1", "0", "total")
  
  cat("Sensitivity =", confused[1, 1] / confused[1, 3], "\n")
  cat("Specificity =", confused[2, 2] / confused[2, 3], "\n")
  confused
}

confusion(fit)
library(ROCit)
k=rocit(fit$fitted.values,fit$y)
plot(k)
k$AUC
