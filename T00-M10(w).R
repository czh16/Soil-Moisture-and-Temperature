# Loading R packages       
##summer:The total amount is 752 
##winter:The total amount is 730 
##Variables include temperature and moisture content
##temperature: T00,T10,T20,T30,T40
#moisture content: M10,M20,M30,M40
library(rEDM)
library(Kendall)

# Loading the time series 
dac <- read.csv('Edata(winter).csv',header=T) 
# Data normalization
dac.n <- scale(dac[,-1], center = TRUE, scale = TRUE)
########################################################### A¡úB
## CCM analysis of the two variables
# Design a sequence of library size
libs <- c(seq(20,80,5),seq(90,730,50))                                        ##¡ø¡øChange the atotal mount here  

# T00 cross-mapping M10 (i.e. testing M10 as a cause of T00)                  ##¡ï¡ïChange the variables here
# Determine the embedding dimension
E.test.x=NULL
for(E.t in 2:8){
  cmxy.t <- ccm(dac.n, E = E.t, lib_column = "T00", target_column = "M10",    ##¡ï¡ïChange the variables here
                lib_sizes = 730, num_samples = 1, tp=-1,random_libs = F)      ##¡ø¡øChange the atotal mount here  
  E.test.x=rbind(E.test.x,cmxy.t)}
(E_x <- E.test.x$E[which.max(E.test.x$rho)[1]])

# CCM analysis: varying library size
x_xmap_y <- ccm(dac.n, E=E_x,lib_column="T00", target_column="M10",           ##¡ï¡ïChange the variables here
                lib_sizes=libs, num_samples=200, replace=T, RNGseed=2301)

# Calculate the median, maximum, and 1st & 3rd quantiles of rho
xyq=as.matrix(aggregate(x_xmap_y[,c('rho')],by = list(as.factor(x_xmap_y$lib_size)), quantile)[,'x'])
apply(xyq[,2:5],2,MannKendall)

########################################################### B¡úA
# M10 cross-mapping T00 (i.e. testing T00 as a cause of M10)                  ##¡ï¡ïChange the variables here
# Determine the embedding dimension
E.test.y=NULL
for(E.t in 2:8){
  cmxy.t <- ccm(dac.n, E = E.t, lib_column = "M10", target_column = "T00",    ##¡ï¡ïChange the variables here
                lib_sizes = 730, num_samples = 1,tp=-1,random_libs = F)       ##¡ø¡øChange the atotal mount here
  E.test.y=rbind(E.test.y,cmxy.t)}
(E_y <- E.test.y$E[which.max(E.test.y$rho)[1]])

# CCM analysis
y_xmap_x <- ccm(dac.n, E=E_y,lib_column="M10", target_column="T00",           ##¡ï¡ïChange the variables here
                lib_sizes=libs, num_samples=200, replace=T, RNGseed=2301)

# Calculate the (25%,50%,75%,100%) quantile for predictive skills
yxq=as.matrix(aggregate(y_xmap_x[,c('rho')],by = list(as.factor(y_xmap_x$lib_size)), quantile)[,'x'])
apply(yxq[,2:5],2,MannKendall)
########################################################### 
# Plot forecast skill vs library size
# Plot X cross-mapping Y
plot(xyq[,3]~libs,type="l",col="blue",ylim=c(0,1),lwd=2,
     xlab="Library size",ylab=expression(rho)) # median predictive skill vs library size (or we can use mean predictive skill)
##lines(xyq[,2]~libs,col="red",lwd=1,lty=2) # 1st quantile 
##lines(xyq[,4]~libs,col="red",lwd=1,lty=2) # 3rd quantile

# Plot Y cross-mapping X
lines(yxq[,3]~libs,col="red",lwd=2,lty=1) # median 
legend(500,0.4,c("T00 xmap M10","M10 xmap T00"),lty=c(1,1),col=c("blue","red"))  ##¡ï¡ïChange the variables here

print(cor(dac[,'T00'],dac[,'M10']))                                              ##¡ï¡ïChange the variables here