#install.packages("BlandAltmanLeh")
#library(BlandAltmanLeh)

##Toy Example: Miss Universe 1994 Top 10 scores by Two Judges
dat <- read.csv("~/missu1994.csv") #revise based on location of data set
stat <- bland.altman.stats(dat$Zapata, dat$Larue)

#Bland-Altman Plot for Judge 1 vs. Judge 2
plot(stat$means, stat$diffs, xlim=c(8.5,10), ylim=c(-2,2), 
     main="Bland-Altman Plot: Zapata vs. Larue",  
     pch=16, ylab="Difference= Zapata-Larue", xlab="Mean=(Zapata+Larue)/2")
abline(h=stat$lines, lty=c(3,1,3), lwd=c(2,2,2), col=c("red", "blue", "red"))
text(9.8,stat$upper.limit+0.1, paste("+1.96 SD =",round(stat$upper.limit,3)), col="red")
text(9.8,stat$mean.diffs+0.1, paste("mean diff. =",round(stat$mean.diffs,3)), col="blue")
text(9.8,stat$lower.limit+0.1, paste("+1.96 SD =",round(stat$lower.limit,3)), col="red")

#Contestants instead of points
plot(1, type="n", xlim=c(8.5,10), ylim=c(-2,2), 
     main="Bland-Altman Plot: Zapata vs. Larue",  
     ylab="Difference= Zapata-Larue", xlab="Mean=(Zapata+Larue)/2")
abline(h=stat$lines, lty=c(3,1,3), lwd=c(2,2,2), col=c("red", "blue", "red"))
text(stat$means, stat$diffs, dat$CONTESTANT)

#Per Category Subsets
swim <- subset(dat, CATEGORY=="Swimsuit")
  swim.means <- (swim$Zapata+swim$Larue)/2 
  swim.diffs <- swim$Zapata-swim$Larue
gown <- subset(dat, CATEGORY=="Evening Gown")
  gown.means <- (gown$Zapata+gown$Larue)/2 
  gown.diffs <- gown$Zapata-gown$Larue
inter <- subset(dat, CATEGORY=="Interview")
  inter.means <- (inter$Zapata+inter$Larue)/2 
  inter.diffs <- inter$Zapata-inter$Larue

#Per Category Plots (points)
par(mfrow=c(1,3))
par(pty="s")
  plot(swim.means, swim.diffs, xlim=c(8.5,10), ylim=c(-2,2), pch=16,
     main="Swimsuit Competition",  
     ylab="Difference= Zapata-Larue", xlab="Mean=(Zapata+Larue)/2")
  abline(h=stat$lines, lty=c(3,1,3), lwd=c(2,2,2), col=c("red", "blue", "red"))
  
  plot(gown.means, gown.diffs, xlim=c(8.5,10), ylim=c(-2,2), pch=16,
       main="Evening Gown Competition",  
       ylab="Difference= Zapata-Larue", xlab="Mean=(Zapata+Larue)/2")
  abline(h=stat$lines, lty=c(3,1,3), lwd=c(2,2,2), col=c("red", "blue", "red"))
  
  plot(inter.means, inter.diffs, xlim=c(8.5,10), ylim=c(-2,2), pch=16,
       main="Interview Portion",  
       ylab="Difference= Zapata-Larue", xlab="Mean=(Zapata+Larue)/2")
  abline(h=stat$lines, lty=c(3,1,3), lwd=c(2,2,2), col=c("red", "blue", "red"))

#dev.off()



