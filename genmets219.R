# Read public.dta for 1980-2010 to create data.frames
print("read(ipums80)")
dd80 <- read.dta("ip15_80.dta")
print("read(ipums90)")
dd90 <- read.dta("ip15_90.dta")
print("read(ipums00)")
dd00 <- read.dta("ip15_00.dta")
print("read(ipums05)")
dd05 <- read.dta("ip15_05.dta")
print("read(ipums10)")
dd10 <- read.dta("ip15_10.dta")

print("aggregate(dd80...)")
ee80 <- aggregate(dd80$perwt, by=list("metarea"=dd80$metarea),FUN=sum, na.rm=FALSE)
print("aggregate(dd90...)")
ee90 <- aggregate(dd90$perwt, by=list("metarea"=dd90$metarea),FUN=sum, na.rm=FALSE)
print("aggregate(dd00...)")
ee00 <- aggregate(dd00$perwt, by=list("metarea"=dd00$metarea),FUN=sum, na.rm=FALSE)
print("aggregate(dd05...)")
ee05 <- aggregate(dd05$perwt, by=list("metarea"=dd05$metarea),FUN=sum, na.rm=FALSE)
print("aggregate(dd10...)")
ee10 <- aggregate(dd10$perwt, by=list("metarea"=dd10$metarea),FUN=sum, na.rm=FALSE)

print("colnames")
colnames(ee80)[2] <- "1980"
colnames(ee90)[2] <- "1990"
colnames(ee00)[2] <- "2000"
colnames(ee05)[2] <- "2005"
colnames(ee10)[2] <- "2010"

mm <- ee80
mm <- merge(mm, ee90, by=c("metarea"), all=TRUE)
mm <- merge(mm, ee00, by=c("metarea"), all=TRUE)
mm <- merge(mm, ee05, by=c("metarea"), all=TRUE)
mm <- merge(mm, ee10, by=c("metarea"), all=TRUE)
metok <- mm[!is.na(mm$"1980") & !is.na(mm$"1990") & !is.na(mm$"2000") & !is.na(mm$"2005") & !is.na(mm$"2010"),]
#metna <- mm[is.na(mm$"1980") | is.na(mm$"1990") | is.na(mm$"2000") | is.na(mm$"2005") | is.na(mm$"2010"),]
mets  <- metok[as.integer(metok$metarea) != 1,]
write.table(mets, "mets219.txt")
write.table(mets, "mets219.csv", sep=',')
print(dim(mets))
