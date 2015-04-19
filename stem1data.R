read_ip <- function(aa, year, file) {
	print(sprintf("START READ OF %s", file))
	flush.console()
	mm <- read.dta(file)
	mets <- read.table("mets219.txt")
	dim0 <- dim(mm)[2]
	print(sprintf("PROCESS %s", file))
	if (year > 2007) {
		print("Create mm$WKSWORK1")
		mm$WKSWORK1 <- 0
		#(mm$WKSWORK2)==2 is "N/A"
		mm$WKSWORK1[as.integer(mm$WKSWORK2)==2] <- 7
		mm$WKSWORK1[as.integer(mm$WKSWORK2)==3] <- 20
		mm$WKSWORK1[as.integer(mm$WKSWORK2)==4] <- 33
		mm$WKSWORK1[as.integer(mm$WKSWORK2)==5] <- 43.5
		mm$WKSWORK1[as.integer(mm$WKSWORK2)==6] <- 48.5
		mm$WKSWORK1[as.integer(mm$WKSWORK2)==7] <- 51
	}
	print(sprintf("%d  Initial", dim(mm)[1]))
	mm <- mm[as.integer(mm$age) >= 19 & as.integer(mm$age) <= 66,] # 18 to 65
	print(sprintf("%d  Age 18-65", dim(mm)[1]))
	mm <- mm[as.integer(mm$WKSWORK2) >= 2,]
	print(sprintf("%d  Worked 1 or more weeks", dim(mm)[1]))
	mm <- mm[as.integer(mm$gq) != 4,]
	print(sprintf("%d  Non-institutional", dim(mm)[1]))
	mm <- mm[as.integer(mm$OCC1990) <= 386,]
	print(sprintf("%d  Occupation not Military, Unemployed or Unknown", dim(mm)[1]))
	#mm <- mm[as.integer(mm$bpl) <= 160,]
	mm <- mm[!(as.integer(mm$bpld) %in% c(524,525,527,530,531)),]
	print(sprintf("%d  Birthplace not Abroad, At sea, Other or Missing", dim(mm)[1]))
	mm <- mm[mm$metarea %in% mets$metarea,]
	print(sprintf("%d  Metareas (219)", dim(mm)[1]))
	mm$immig    <- as.integer(as.integer(mm$citizen) >= 3)
	mm$coll     <- as.integer(as.integer(mm$educd) >= 36 & as.integer(mm$educd) <= 43)
	#mm$grad     <- as.integer(as.integer(mm$educd) >= 114)
	mm$emp      <- as.integer(as.integer(mm$empstatd) >= 2 & as.integer(mm$empstatd) <= 4)
	#mm$lf       <- as.integer(as.integer(mm$empstatd) >= 2 & as.integer(mm$empstatd) <= 10)
	mm$stem     <- as.integer(as.integer(mm$OCC1990) %in% c(27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40, 41, 42, 44, 45, 46, 47, 49, 53, 54, 89, 119, 122, 129, 130, 138))
	#mm$wkwage   <- mm$incwage / mm$WKSWORK1
	#count_nas(mm, dim0+1)
	#count_stem_nas(mm)
	#mm$stem[is.na(mm$stem)] <- 0
	nn <- aggregate(cbind(mm$perwt, as.numeric(mm$incwage), mm$WKSWORK1), by=list("metarea"=mm$metarea,"coll"=mm$coll,"stem"=mm$stem,"immig"=mm$immig,"emp"=mm$emp),FUN=sum, na.rm=FALSE)
	nn$year <- year
	if (year==1980) {
		print("Change 1980 to 2010 dollars")
		nn$V2 <- nn$V2 * 2.64631068
	} else if (year==1990) {
		print("Change 1990 to 2010 dollars")
		nn$V2 <- nn$V2 * 1.66837031
	} else if (year==2000) {
		print("Change 2000 to 2010 dollars")
		nn$V2 <- nn$V2 * 1.26629501
	} else if (year==2005) {
		print("Change 2005 to 2010 dollars")
		nn$V2 <- nn$V2 * 1.11651818
	}
	nn$wkwage <- nn$V2 / nn$V3
	aa <- rbind(aa, nn)
	print(sprintf("%d  Size of aa", dim(aa)[1]))
	assign('aa',aa,envir=.GlobalEnv)
	#aa <<- aa
	#readline("Press enter to continue, escape to exit")
}
aggdf <- function(df) {
  #print("START aggdf")
  agg <- aggregate(list(V1=df$V1, V2=df$V2, V3=df$V3), by=list(year=df$year, metarea=df$metarea), FUN=sum, na.rm=FALSE)
  return(agg)
}
mergedf <- function(df1, df2) {
  #print("START mergedf")
  mrg <- merge(df1, df2, by=c("year","metarea"), all=TRUE)
  return(mrg)
}

library(foreign) # needed for read.dta
aa <- NULL
read_ip(aa, 1980, "ip15_80.dta")
read_ip(aa, 1990, "ip15_90.dta")
read_ip(aa, 2000, "ip15_00.dta")
read_ip(aa, 2005, "ip15_05.dta")
read_ip(aa, 2010, "ip15_10.dta")

print(sprintf("ALL IPUMS FILES READ"))
#readline("Press enter to continue, escape to exit")
print(sprintf("START OF AGGREGATE AND MERGE INTO FINAL FILES"))

#native_non_coll  <- aa[aa$emp==1 & aa$immig==0 & aa$coll==0,]
#native_coll      <- aa[aa$emp==1 & aa$immig==0 & aa$coll==1,]
#immig_coll       <- aa[aa$emp==1 & aa$immig==1 & aa$coll==1,]
#total_coll       <- aa[aa$emp==1 &               aa$coll==1,]
#native_stem      <- aa[aa$emp==1 & aa$immig==0              & aa$stem==1,]
#immig_stem       <- aa[aa$emp==1 & aa$immig==1              & aa$stem==1,]
#total_stem       <- aa[aa$emp==1 &                            aa$stem==1,]
#native_coll_stem <- aa[aa$emp==1 & aa$immig==0 & aa$coll==1 & aa$stem==1,]
#immig_coll_stem  <- aa[aa$emp==1 & aa$immig==1 & aa$coll==1 & aa$stem==1,]
#total_coll_stem  <- aa[aa$emp==1 &               aa$coll==1 & aa$stem==1,]
#total_native     <- aa[aa$emp==1 & aa$immig==0,]
#total_immig      <- aa[aa$emp==1 & aa$immig==1,]
#total_pop        <- aa[aa$emp==1,] 

native_non_coll  <- aa[aa$immig==0 & aa$coll==0,]
native_coll      <- aa[aa$immig==0 & aa$coll==1,]
immig_coll       <- aa[aa$immig==1 & aa$coll==1,]
total_coll       <- aa[              aa$coll==1,]
native_stem      <- aa[aa$immig==0              & aa$stem==1,]
immig_stem       <- aa[aa$immig==1              & aa$stem==1,]
total_stem       <- aa[                           aa$stem==1,]
native_coll_stem <- aa[aa$immig==0 & aa$coll==1 & aa$stem==1,]
immig_coll_stem  <- aa[aa$immig==1 & aa$coll==1 & aa$stem==1,]
total_coll_stem  <- aa[              aa$coll==1 & aa$stem==1,]
total_native     <- aa[aa$immig==0,]
total_immig      <- aa[aa$immig==1,]
total_pop        <- aa 

native_non_coll  <- aggdf(native_non_coll)
native_coll      <- aggdf(native_coll)
immig_coll       <- aggdf(immig_coll)
total_coll       <- aggdf(total_coll)
native_stem      <- aggdf(native_stem)
immig_stem       <- aggdf(immig_stem)
total_stem       <- aggdf(total_stem)
native_coll_stem <- aggdf(native_coll_stem)
immig_coll_stem  <- aggdf(immig_coll_stem)
total_coll_stem  <- aggdf(total_coll_stem)
total_native     <- aggdf(total_native)
total_immig      <- aggdf(total_immig)
total_pop        <- aggdf(total_pop)

colnames(native_non_coll)  [colnames(native_non_coll) =="V1"] <- "native_non_coll"
colnames(native_coll)      [colnames(native_coll)     =="V1"] <- "native_coll"
colnames(immig_coll)       [colnames(immig_coll)      =="V1"] <- "immig_coll"
colnames(total_coll)       [colnames(total_coll)      =="V1"] <- "total_coll"
colnames(native_stem)      [colnames(native_stem)     =="V1"] <- "native_stem"
colnames(immig_stem)       [colnames(immig_stem)      =="V1"] <- "immig_stem"
colnames(total_stem)       [colnames(total_stem)      =="V1"] <- "total_stem"
colnames(native_coll_stem) [colnames(native_coll_stem)=="V1"] <- "native_coll_stem"
colnames(immig_coll_stem)  [colnames(immig_coll_stem) =="V1"] <- "immig_coll_stem"
colnames(total_coll_stem)  [colnames(total_coll_stem) =="V1"] <- "total_coll_stem"
colnames(total_native)     [colnames(total_native)    =="V1"] <- "total_native"    
colnames(total_immig)      [colnames(total_immig)     =="V1"] <- "total_immig"    
colnames(total_pop)        [colnames(total_pop)       =="V1"] <- "total_pop"    

native_non_coll$native_non_coll_wkwage   <- native_non_coll$V2  /  native_non_coll$V3
native_coll$native_coll_wkwage           <- native_coll$V2      / native_coll$V3
immig_coll$immig_coll_wkwage             <- immig_coll$V2       / immig_coll$V3
total_coll$total_coll_wkwage             <- total_coll$V2       / total_coll$V3
native_stem$native_stem_wkwage           <- native_stem$V2      / native_stem$V3
immig_stem$immig_stem_wkwage             <- immig_stem$V2       / immig_stem$V3
total_stem$total_stem_wkwage             <- total_stem$V2       / total_stem$V3
native_coll_stem$native_coll_stem_wkwage <- native_coll_stem$V2 / native_coll_stem$V3
immig_coll_stem$immig_coll_stem_wkwage   <- immig_coll_stem$V2  / immig_coll_stem$V3
total_coll_stem$total_coll_stem_wkwage   <- total_coll_stem$V2  / total_coll_stem$V3
total_native$total_native_wkwage         <- total_native$V2     / total_native$V3
total_immig$total_immig_wkwage           <- total_immig$V2      / total_immig$V3
total_pop$total_pop_wkwage               <- total_pop$V2        / total_pop$V3

native_non_coll$V2  <- NULL
native_coll$V2      <- NULL
immig_coll$V2       <- NULL
total_coll$V2       <- NULL
native_stem$V2      <- NULL
immig_stem$V2       <- NULL
total_stem$V2       <- NULL
native_coll_stem$V2 <- NULL
immig_coll_stem$V2  <- NULL
total_coll_stem$V2  <- NULL
total_native$V2     <- NULL
total_immig$V2      <- NULL
total_pop$V2        <- NULL

native_non_coll$V3  <- NULL
native_coll$V3      <- NULL
immig_coll$V3       <- NULL
total_coll$V3       <- NULL
native_stem$V3      <- NULL
immig_stem$V3       <- NULL
total_stem$V3       <- NULL
native_coll_stem$V3 <- NULL
immig_coll_stem$V3  <- NULL
total_coll_stem$V3  <- NULL
total_native$V3     <- NULL
total_immig$V3      <- NULL
total_pop$V3        <- NULL

cc <- native_non_coll
cc <- mergedf(cc, native_coll)
cc <- mergedf(cc, immig_coll)
cc <- mergedf(cc, total_coll)
cc <- mergedf(cc, native_stem)
cc <- mergedf(cc, immig_stem)
cc <- mergedf(cc, total_stem)
cc <- mergedf(cc, native_coll_stem)
cc <- mergedf(cc, immig_coll_stem)
cc <- mergedf(cc, total_coll_stem)
cc <- mergedf(cc, total_native)
cc <- mergedf(cc, total_immig)
cc <- mergedf(cc, total_pop)

#cc <- cc[with(cc, order(metarea,year)),]
#count_nas_all(cc)
#cc[is.na(cc)] <- 0

print("CREATE FINAL FILES ipums80.txt and ipums80.csv")
write.table(cc, "ipums80.txt")
write.table(cc, "ipums80.csv", sep=',')

