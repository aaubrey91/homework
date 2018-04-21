ds <- read.csv("http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt", header=FALSE)
save()
#We can ignore the last column
ds <- ds[,c(1:10)]

#1.Compute the multivariate mean vector
#ds$multivariateMean <- rowMeans(ds)
mu <- colMeans(ds) #SHOULD THIS BE ROW MEANS?


###########################################################################################################


#2. Compute the sample covariance matrix as inner products between the columns of the centered data matrix, 
#without using the covariance function in R. Compare and contrast your results with that obtained using 
#the covariance function in R.



dot_product <- function(x,y) {
  
  if (length(x) != length(y)) {
   # print("Vectors must be same size")
    stop("Vectors must be same size")
  }
  #return(sum(x*y))
  return(t(x) %*% y)
}

center_matrix <- function(dataset) {
  mu <- colMeans(dataset)
  
  Z <- data.matrix(dataset) #initialize matrix with dimensions of input
  
  ### Center Matrix ###
  for (i in 1:dim(ds)[1]) {   #rows
    for (j in 1:dim(ds)[2]) { #col
      Z[i,j] <- (ds[i,j] - mu[j])
    }
  }
  
  return(Z)
}

covariance_f <- function(x,y) {
  return(dot_product(x,y) / (length(x)-1)) #matches cov function in R when (n - 1))
}

covariance_mtrx <- function(dataset) {
  
  tmp_mtrx <- matrix(c(0), nrow=dim(dataset)[2], ncol=dim(dataset)[2])
  
  for (i in 1:dim(dataset)[2]) {
    for (j in 1:dim(dataset)[2]) {
      tmp_mtrx[i,j] = covariance_f(Z[,i], Z[,j])
    }
  }
  
  return(tmp_mtrx)
}

##Assert these are the same
print("covariance_mtrx(Z) - cov(Z)")
print(covariance_mtrx(Z) - cov(Z))

###########################################################################################################

#3. Compute the correlation between Attributes 1 and 2 by computing the cosine of the angle between the 
#centered attribute vectors. Compare the result to the correlation function in R. Plot the scatter plot between 
#these two attributes.

correlation <- function(x,y) {
  return(dot_product(x,y) / (sqrt(dot_product(x,x)) * sqrt(dot_product(y,y))))
}

# Attributes 1 & 2 where Attribute 0 is first element
print("User defined correlation function results:")
print(correlation(Z[,2], Z[,3]))

print("Correlation Function in R result:")
print(cor(Z[,2], Z[,3]))

#Plot the scatter plot
library(ggplot2)
ggplot(data.frame(Z), aes(x=Z[,2], y=Z[,3])) + geom_point()
###########################################################################################################

#4. Assuming that Attribute 1 is normally distributed, plot its probability density function.
plot(Z[,2], dnorm(Z[,2])) #index 2 as attributes start at 0 based on assignment direction


###########################################################################################################

#5. Using the covariance matrix from above, which attribute has the largest variance, and which attribute 
#has the smallest variance? Print these values.

print()

###########################################################################################################

#6. Which pair of attributes has the largest covariance, and which pair of attributes has the smallest 
#covariance? Print these values.

row <- which(max(covariance_mtrx(Z)) == covariance_mtrx(Z), arr.ind = T)[1]
col <- which(max(covariance_mtrx(Z)) == covariance_mtrx(Z), arr.ind = T)[2]

cat("MAX Covariance occurs between attributes",toString(row),"and",toString(col),".")

row <- which(min(covariance_mtrx(Z)) == covariance_mtrx(Z), arr.ind = T)[1]
col <- which(min(covariance_mtrx(Z)) == covariance_mtrx(Z), arr.ind = T)[2]
cat("MIN Covariance occurs between attributes",toString(row),"and",toString(col),".")

###########################################################################################################

#7.Compute the correlation matrix of the data using the handout method that employs the dot products of the
#Z matrix using linear algebra. Compare and contrast your results with that obtained using the covariance 
#function in R.

correlation_mtrx <- function(dataset) {
  cov_mtrx <- covariance_mtrx(dataset)
  #diag(covariance_mtrx(Z))^(-1/2) * covariance_mtrx(Z) * diag(covariance_mtrx(Z))^(-1/2)
  
  #return(((diag(cov_mtrx))^-(1/2)) * cov_mtrx * ((diag(cov_mtrx))^-(1/2)))
  #return(sqrt(diag(cov_mtrx)) * cov_mtrx * sqrt(diag(cov_mtrx)))
  #return(((diag(cov_mtrx))^((1/2)*sign(-1))) * cov_mtrx * ((diag(cov_mtrx))^((1/2)*sign(-1))))
  return((dot_product(Z[,1], Z[,2]) / (sd(Z[,1])*sd(Z[,2]))))
}

###########################################################################################################