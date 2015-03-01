dd <- read.csv("wsjstem1.csv")
labyears <- "1990-2010"
source("stem1labels0.R")
with(dd, plot(native_coll_wkwage_change ~ immig_stem_change, xlim=c(-2,4), col=col, cex=0.6,
  ylab="Percentage change in real native college-graduate wages",
  xlab="Percentage change in foreign STEM workers"))
with(dd, text(native_coll_wkwage_change ~ immig_stem_change, labels=label, cex=0.6, pos=pos, offset=0.2, col=col))
title(main=paste("Native College Wages vs. Foreign Stem Workers,", labyears))
abline(h=100*47280/35941-100, col="blue", lty=3) # percent change in real per-capita GDP, 1990-2010
grid()
legend("topleft", inset=0, cex=0.5, lty=c(3), col=c("blue"), horiz=FALSE, c("change in real per-capita GDP, 1990-2010"))
legend("bottomright", inset=0, cex=0.5, pch=c(1,1), col=c("red","deepskyblue"), horiz=FALSE,
  c("highest influx according to article", "no data in 1990 5% IPUMS Census"))
readline("Press enter to continue, escape to exit")

x11()
dd <- read.csv("wsjstem3.csv")
labyears <- "1990-2010"
source("stem1labels0.R")
with(dd, plot(native_coll_wkwage_change ~ immig_stem_change, xlim=c(-2,4), col=col, cex=0.6,
  ylab="Percentage change in real native college-graduate wages",
  xlab="Percentage change in foreign STEM workers"))
with(dd, text(native_coll_wkwage_change ~ immig_stem_change, labels=label, cex=0.6, pos=pos, offset=0.2, col=col))
title(main=paste("Native College Wages vs. Foreign Stem Workers,", labyears))
abline(h=100*47280/35941-100, col="blue", lty=3) # percent change in real per-capita GDP, 1990-2010
grid()
legend("topleft", inset=0, cex=0.5, lty=c(3), col=c("blue"), horiz=FALSE, c("change in real per-capita GDP, 1990-2010"))
legend("bottomright", inset=0, cex=0.5, pch=c(1,1), col=c("red","deepskyblue"), horiz=FALSE,
  c("highest influx according to article", "no data in 1990 5% IPUMS Census"))
readline("Press enter to continue, escape to exit")
