library(BlandAltmanLeh) #To calculate mean differences and limits of agreement
library(irr) #To incorporate Intraclass Correlation (optional)

data <- read.csv("insert file path if csv") 
stats <- bland.altman.stats(data$Automated, data$Manual)
icc <- icc(data, model = "twoway", type = "agreement") #optional

plot(v.stats$means, v.stats$diffs, xlim=c(0,10), ylim=c(-0.3,0.3), 
     main="Total Retina: Bland-Altman Plot for Automated vs. Observer",  
     sub=paste("ICC=",round(icc$value,7)),
     pch=16, 
     ylab=expression("Difference= Automated-Observer," ~ mm^{3}), 
     xlab=expression("Mean=(Automated+Observer)/2," ~ mm^{3}))
abline(h=v.stats$lines, lty=c(3,1,3), lwd=c(2,2,2), col=c("red", "blue", "red"))
text(5,v.stats$upper.limit+0.01, paste("+1.96 SD =",round(v.stats$upper.limit,3)), col="red")
text(5,v.stats$mean.diffs+0.01, paste("mean diff. =",round(v.stats$mean.diffs,3)), col="blue")
text(5,v.stats$lower.limit+0.01, paste("+1.96 SD =",round(v.stats$lower.limit,3)), col="red")
