library(plyr)
cc <- read.table("ipums80.txt")
cc$span <- 0
cc$span[cc$year==1980] <- 1
cc$span[cc$year==1990] <- 2
cc$span[cc$year==2000] <- 3
cc$span[cc$year==2005] <- 4
cc$span[cc$year==2010] <- 5
# dd contains 1990-2010 changes
dd <- ddply(cc, .(metarea), transform,
  immig_stem3           = immig_stem[span+3],
  immig_stem_change3b   = 100 * (immig_stem[span+3]  - immig_stem[span])  / total_pop[span],
  native_stem_change3b  = 100 * (native_stem[span+3] - native_stem[span]) / total_pop[span],
  native_coll_wkwage_change = (100 * native_coll_wkwage[span+3] / native_coll_wkwage[span]) - 100,
  immig_stem_change  = 100 * (immig_stem[span+3] /total_pop[span+3] - immig_stem[span]/ total_pop[span]),
  native_stem_change = 100 * (native_stem[span+3]/total_pop[span+3] - native_stem[span]/total_pop[span]))
dd <- dd[dd$year==1990,]
dd <- dd[!is.na(dd$immig_stem_change),]
labyears <- "1990-2010"

print("                      CORREL                                              ")
print("INTERCEPT    SLOPE     COEF   P-VALUE  Y VARIABLE ~ X VARIABLE [, WEIGHTS]")
print("---------  --------  -------  -------  -----------------------------------")
print(paste("Native College Wages vs. Foreign Stem Workers,", labyears))
lma <- (with(dd, lm(native_coll_wkwage_change ~ immig_stem_change)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_coll_wkwage_change ~ immig_stem_change",
  lma$coef[1], lma$coef[2], with(dd, cor(native_coll_wkwage_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

lma <- (with(dd, lm(native_coll_wkwage_change ~ immig_stem_change, weights=immig_stem3)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_coll_wkwage_change ~ immig_stem_change, weights=immig_stem",
  lma$coef[1], lma$coef[2], with(dd, cor(native_coll_wkwage_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

print(paste("Native STEM Workers vs. Foreign STEM Workers,", labyears))
lma <- (with(dd, lm(native_stem_change ~ immig_stem_change)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_stem_change ~ immig_stem_change",
  lma$coef[1], lma$coef[2], with(dd, cor(native_stem_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

lma <- (with(dd, lm(native_stem_change ~ immig_stem_change, weights=immig_stem3)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_stem_change ~ immig_stem_change, weights=immig_stem",
  lma$coef[1], lma$coef[2], with(dd, cor(native_stem_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

########## PLOTS ##########
source("stem1labels1.R")
with(dd, plot(native_coll_wkwage_change ~ immig_stem_change, ylim=c(-20,80), col=col,
  ylab="Percentage change in real native college-graduate wages",
  xlab="Percentage change in foreign STEM workers"))
with(dd, text(native_coll_wkwage_change ~ immig_stem_change, labels=label, cex=0.6, pos=pos, offset=0.3, col=col))
title(main=paste("Native College Wages vs. Foreign Stem Workers,", labyears))
lma <- (with(dd, lm(native_coll_wkwage_change ~ immig_stem_change)))
abline(lma)
lma <- (with(dd, lm(native_coll_wkwage_change ~ immig_stem_change, weights=immig_stem)))
abline(lma, col="red")
abline(h=100*47280/35941-100, col="blue", lty=3) # percent change in real per-capita GDP, 1990-2010
grid()
legend("bottomright", inset=0, cex=0.5, lty=c(1,1,3), col=c("black","red","blue"), horiz=FALSE,
  c("unweighted", "weighted by foreign STEM workers in 2010", "change in real per-capita GDP, 1990-2010"))
readline("Press enter to continue, escape to exit")

x11()
source("stem1labels2.R")
with(dd, plot(native_stem_change ~ immig_stem_change, col=col,
  ylab="Percentage change in native STEM workers",
  xlab="Percentage change in foreign STEM workers"))
with(dd, text(native_stem_change ~ immig_stem_change, labels=label, cex=0.6, pos=pos, offset=0.3, col=col))
title(main=paste("Native STEM Workers vs. Foreign STEM Workers,", labyears))
lma <- (with(dd, lm(native_stem_change ~ immig_stem_change)))
abline(lma)
lma <- (with(dd, lm(native_stem_change ~ immig_stem_change, weights=immig_stem)))
abline(lma, col="red")
grid()
legend("topright", inset=0, cex=0.5, lty=1, col=c("black","red"), horiz=FALSE,
  c("unweighted","weighted by foreign STEM workers in 2010"))
