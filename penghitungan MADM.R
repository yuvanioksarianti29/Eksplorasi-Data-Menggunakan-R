library(DescTools)
x <- runif(100)
MeanAD(x)

speed <- c(58, 88, 40, 60, 72, 66, 80, 48, NA)
MeanAD(speed)
MeanAD(speed, na.rm=TRUE)


# using the median as centerpoint
x <- c(2,3,5,3,1,15,23)

MeanAD(x, center=mean)
MeanAD(x, center=median)

# define a fixed center
MeanAD(x, center=4)

# use of weights
MeanAD(x=0:6, weights=c(21,46,54,40,24,10,5))