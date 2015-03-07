vpercent <- function(v1, v2) {
  v12 <- v1 + v2
  na1 <- v1[!is.na(v12)]
  na2 <- v2[!is.na(v12)]
  per <- 100 * sum(na1) / sum(na2)
  return(per)
}
table1row <- function(vv, yr, c1, c2, c3) {
  print(sprintf("%4d %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f", yr,
    c1, vpercent(vv$immig_coll, vv$total_coll),
    c2, vpercent(vv$immig_stem, vv$total_stem),
    c3, vpercent(vv$immig_coll_stem, vv$total_coll_stem)))
}
table2row <- function(vv, yr, c1, c2) {
  print(sprintf("%4d %8.2f %8.2f %8.2f %8.2f", yr,
    c1, vpercent(vv$immig_coll_stem, vv$total_pop),
    c2, vpercent(vv$total_coll_stem, vv$total_pop)))
}
table3row <- function(yrs, vv1, vv2, c1, c2) {
  v12 <- vv1$total_coll_stem + vv2$total_coll_stem
  na1 <- vv1$total_coll_stem[!is.na(v12)]
  na2 <- vv2$total_coll_stem[!is.na(v12)]
  v34 <- vv1$immig_coll_stem + vv2$immig_coll_stem
  na3 <- vv1$immig_coll_stem[!is.na(v34)]
  na4 <- vv2$immig_coll_stem[!is.na(v34)]
  print(sprintf("%9s %9.0f %9.0f %9.0f %9.0f", yrs,
    c1, sum(na2 - na1) / 1000,
    c2, sum(na4 - na3) / 1000))
}

cc <- read.table("ipums80.txt")
cc80 <- cc[cc$year==1980,]
cc90 <- cc[cc$year==1990,]
cc00 <- cc[cc$year==2000,]
cc05 <- cc[cc$year==2005,]
cc10 <- cc[cc$year==2010,]
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
colnames(cc1) [colnames(cc1)=="total_pop"]        <- "total_pop_1"    

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
colnames(cc1) [colnames(cc1)=="total_pop_wkwage"]        <- "total_pop_wkwage_1"    

dd <- merge(cc1, cc2, by=c("metarea"), all=TRUE)
dd$immig_stem_share_1 <- 100 * dd$immig_stem_1 / dd$total_pop_1
dd$immig_stem_share   <- 100 * dd$immig_stem   / dd$total_pop
dd$immig_stem_change  <- dd$immig_stem_share - dd$immig_stem_share_1
dd$native_stem_share_1 <- 100 * dd$native_stem_1 / dd$total_pop_1
dd$native_stem_share   <- 100 * dd$native_stem   / dd$total_pop
dd$native_stem_change  <- dd$native_stem_share - dd$native_stem_share_1
dd$native_coll_share_1 <- 100 * dd$native_coll_1 / dd$total_pop_1
dd$native_coll_share   <- 100 * dd$native_coll   / dd$total_pop
dd$native_coll_change  <- dd$native_coll_share - dd$native_coll_share_1
dd$native_coll_wkwage_change <- (100 * dd$native_coll_wkwage / dd$native_coll_wkwage_1) - 100
dd$native_stem_wkwage_change <- (100 * dd$native_stem_wkwage / dd$native_stem_wkwage_1) - 100

dd <- dd[!is.na(dd$immig_stem),]
dd <- dd[!is.na(dd$immig_stem_1),]

print("       % Among College  %  Employment in   % Among College   ")
print("          Educated      STEM Occupations  Educated Employment")
print("Year     Employment                       in STEM Occupations")
print("----  ----------------  ----------------  -------------------")
print("Year    Study     Calc    Study     Calc    Study     Calc")
print("----  -------  -------  -------  -------  -------  ----------")
table1row(cc80, 1980,  8.01,  9.27, 12.18)
table1row(cc90, 1990, 10.51, 12.63, 15.64)
table1row(cc00, 2000, 14.75, 19.66, 24.74)
table1row(cc05, 2005, 17.10, 22.74, 28.40)
table1row(cc10, 2010, 17.74, 23.95, 29.74)
print("")
print("College Educated STEM as % of Employment (219 Metro Areas)")
print("----------------------------------------------------------")
print("        Foreign STEM       Total STEM")
print("----  ----------------  ----------------")
print("Year    Study     Calc    Study     Calc")
print("----  -------  -------  -------  -------")
table2row(cc80, 1980,  0.26,  2.11)
table2row(cc90, 1990,  0.45,  2.90)
table2row(cc00, 2000,  0.87,  3.52)
table2row(cc05, 2005,  1.00,  3.52)
table2row(cc10, 2010,  1.10,  3.71)
print("")
print("             Change in Total    Change in Foreign")
print("            College Educ STEM   College Educ STEM")
print("---------  ------------------  ------------------")
print("Year Span     Study      Calc     Study      Calc")
print("---------  --------  --------  --------  --------")
table3row("1980-1990", cc80, cc90,  980, 204)
table3row("1990-2000", cc90, cc00, 1114, 498)
table3row("2000-2005", cc00, cc05,  254, 202)
table3row("2005-2010", cc05, cc10,  269, 131)
print("")
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
