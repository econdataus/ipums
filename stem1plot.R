cc <- read.table("ipums80.txt")
#cc80 <- cc[cc$year==1980,]
#cc90 <- cc[cc$year==1990,]
#cc00 <- cc[cc$year==2000,]
#cc05 <- cc[cc$year==2005,]
#cc10 <- cc[cc$year==2010,]
cc1 <- cc[cc$year==1990,]
cc2 <- cc[cc$year==2010,]
labyears <- "1990-2010"

colnames(cc1) [colnames(cc1)=="native_non_coll"]  <- "native_non_coll_1"
colnames(cc1) [colnames(cc1)=="native_coll"]      <- "native_coll_1"
colnames(cc1) [colnames(cc1)=="immig_coll"]       <- "immig_coll_1"
colnames(cc1) [colnames(cc1)=="total_coll"]       <- "total_coll_1"
colnames(cc1) [colnames(cc1)=="native_stem"]      <- "native_stem_1"
colnames(cc1) [colnames(cc1)=="immig_stem"]       <- "immig_stem_1"
colnames(cc1) [colnames(cc1)=="total_stem"]       <- "total_stem_1"
colnames(cc1) [colnames(cc1)=="native_coll_stem"] <- "native_coll_stem_1"
colnames(cc1) [colnames(cc1)=="immig_coll_stem"]  <- "immig_coll_stem_1"
colnames(cc1) [colnames(cc1)=="total_coll_stem"]  <- "total_coll_stem_1"
colnames(cc1) [colnames(cc1)=="pop_total"]        <- "pop_total_1"    

colnames(cc1) [colnames(cc1)=="native_non_coll_wkwage"]  <- "native_non_coll_wkwage_1"
colnames(cc1) [colnames(cc1)=="native_coll_wkwage"]      <- "native_coll_wkwage_1"
colnames(cc1) [colnames(cc1)=="immig_coll_wkwage"]       <- "immig_coll_wkwage_1"
colnames(cc1) [colnames(cc1)=="total_coll_wkwage"]       <- "total_coll_wkwage_1"
colnames(cc1) [colnames(cc1)=="native_stem_wkwage"]      <- "native_stem_wkwage_1"
colnames(cc1) [colnames(cc1)=="immig_stem_wkwage"]       <- "immig_stem_wkwage_1"
colnames(cc1) [colnames(cc1)=="total_stem_wkwage"]       <- "total_stem_wkwage_1"
colnames(cc1) [colnames(cc1)=="native_coll_stem_wkwage"] <- "native_coll_stem_wkwage_1"
colnames(cc1) [colnames(cc1)=="immig_coll_stem_wkwage"]  <- "immig_coll_stem_wkwage_1"
colnames(cc1) [colnames(cc1)=="total_coll_stem_wkwage"]  <- "total_coll_stem_wkwage_1"
colnames(cc1) [colnames(cc1)=="pop_total_wkwage"]        <- "pop_total_wkwage_1"    

dd <- merge(cc1, cc2, by=c("metarea"), all=TRUE)
dd$immig_stem_share_1 <- 100 * dd$immig_stem_1 / dd$pop_total_1
dd$immig_stem_share   <- 100 * dd$immig_stem   / dd$pop_total
dd$immig_stem_change  <- dd$immig_stem_share - dd$immig_stem_share_1
dd$native_stem_share_1 <- 100 * dd$native_stem_1 / dd$pop_total_1
dd$native_stem_share   <- 100 * dd$native_stem   / dd$pop_total
dd$native_stem_change  <- dd$native_stem_share - dd$native_stem_share_1
dd$native_coll_share_1 <- 100 * dd$native_coll_1 / dd$pop_total_1
dd$native_coll_share   <- 100 * dd$native_coll   / dd$pop_total
dd$native_coll_change  <- dd$native_coll_share - dd$native_coll_share_1
dd$native_coll_wkwage_change <- (100 * dd$native_coll_wkwage / dd$native_coll_wkwage_1) - 100
dd$native_stem_wkwage_change <- (100 * dd$native_stem_wkwage / dd$native_stem_wkwage_1) - 100

dd <- dd[!is.na(dd$immig_stem),]
dd <- dd[!is.na(dd$immig_stem_1),]

print("                      CORREL                                              ")
print("INTERCEPT    SLOPE     COEF   P-VALUE  Y VARIABLE ~ X VARIABLE [, WEIGHTS]")
print("---------  --------  -------  -------  -----------------------------------")
print(paste("Native College Wages vs. Foreign Stem Workers,", labyears))
lma <- (with(dd, lm(native_coll_wkwage_change ~ immig_stem_change)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_coll_wkwage_change ~ immig_stem_change",
  lma$coef[1], lma$coef[2], with(dd, cor(native_coll_wkwage_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

lma <- (with(dd, lm(native_coll_wkwage_change ~ immig_stem_change, weights=immig_stem)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_coll_wkwage_change ~ immig_stem_change, weights=immig_stem",
  lma$coef[1], lma$coef[2], with(dd, cor(native_coll_wkwage_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

print(paste("Native STEM Workers vs. Foreign STEM Workers,", labyears))
lma <- (with(dd, lm(native_stem_change ~ immig_stem_change)))
print(sprintf("%9.4f %9.4f %8.4f %8.4f  native_stem_change ~ immig_stem_change",
  lma$coef[1], lma$coef[2], with(dd, cor(native_stem_change, immig_stem_change)), anova(lma)$'Pr(>F)'[1]))

lma <- (with(dd, lm(native_stem_change ~ immig_stem_change, weights=immig_stem)))
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
