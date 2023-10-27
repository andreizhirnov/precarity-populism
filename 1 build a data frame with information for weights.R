library(openxlsx)

set.seed(123) 

theme_set(theme_bw())

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")
in_dir <- "./data"

load(file=file.path(in_dir,"EVES.RData"))
countries <- c("AT","FR","DE","HU","IT","PL","PT","RO","ES","SE","NL")
names(countries) <- countries
labbed <- merge(vars, labs, by.x="vallab", by.y="lname")

## regions
vars.reg <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="geo-grid")
nuts.reg <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="geo-nuts")
vars.edu <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="edu-grid")
vals.edu <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="edu-values")

## geography
## try using NUTS2 in Italy, the Netherlands, and Spain
nuts.reg <- within(nuts.reg, {
            nuts.code[which(cntry2 %in% c("IT","NL","ES"))] <- substr(nuts.code[which(cntry2 %in% c("IT","NL","ES"))],1L,3L)
            })

vars.reg <- merge(vars, vars.reg, by="name")
labs.reg <- merge(vars.reg, labs, by.x="vallab", by.y="lname")
labs.reg <- subset(labs.reg, value != micode)
labs.reg <- merge(labs.reg, nuts.reg, by = c("cntry2","vallab","value"), all.x=TRUE)
labs.reg$cntry <- substr(labs.reg$cntry2,1,2)

subset(labs.reg, is.na(nuts.code))
length(unique(labs.reg$name)) == nrow(vars.reg)
length(unique(labs.reg$cntry2))

data.reg <- subset(df, cntry %in% countries, select=c("row","cntry", vars.reg$name))
data.reg.long <- reshape(data.reg, direction="long", idvar="row", timevar="var",
        varying = vars.reg$name, times=vars.reg$name, v.names = "value", ids = 1:nrow(data.reg))
data.reg.long <- merge(data.reg.long, labs.reg, by.x=c("cntry","var","value"), by.y=c("cntry","name","value"))
data.reg.long <- subset(data.reg.long, !is.na(value) & value != micode)
length(unique(data.reg.long$row))

keeper <- aggregate(priority ~ row,data=data.reg.long,FUN=max)
df.region <- subset(merge(data.reg.long, keeper),select=c("row","nuts.code"))
colnames(df.region) <- c("row","wf.region")

## education
vars.edu <- merge(vars, vars.edu, by="name")
labs.edu <- merge(vars.edu, labs, by.x="vallab", by.y="lname")
labs.edu <- merge(labs.edu, vals.edu, by.x = c("vallab","value"), by.y = c("lname","value"), all.x=TRUE, suffixes=c("",".a")) 
subset(labs.edu, is.na(edu.code))

vars.edu.vec <- sort(unique(vars.edu$name))
data.edu <- subset(df, cntry %in% countries, select=c("row","cntry",vars.edu.vec))
data.edu.long <- reshape(data.edu, direction="long", idvar="row", timevar="var",
                         varying = vars.edu.vec, times=vars.edu.vec, v.names = "value", ids = 1:nrow(data.edu))
data.edu.long <- merge(data.edu.long, labs.edu, by.x=c("cntry","var","value"), by.y=c("cntry","name","value"))
data.edu.long <- subset(data.edu.long, !is.na(edu.code))
length(unique(data.edu.long$row))

keeper <- aggregate(priority ~ row,data=data.edu.long,FUN=max)
df.edu <- subset(merge(data.edu.long, keeper),select=c("row","edu.code"))
colnames(df.edu) <- c("row","wf.edu")

## sex, age, current economic activity status
vars.dem <- read.table(header=TRUE, sep="|", text="
var|class|priority
Q2|sex|1
Q2wave1b|sex|2        
Q1_Genderwave2|sex|3
Q3|yob|1
Q3Agewave1b|yob|2
Q2Agewave2|yob|3
Q12_1|cas|5
Q12_2|cas|6
Q12_3|cas|7
Q12_4|cas|8
Q12_5|cas|9
Q12_6|cas|10
Q12_7|cas|3
Q12_8|cas|2
Q12_9|cas|4
Q12_10|cas|1
")

dem <- subset(df, cntry %in% countries, select=c("row", vars.dem$var))
dem.long <- reshape(dem, direction="long", idvar="row", timevar="var",
                    varying = vars.dem$var, times=vars.dem$var, v.names = "value", ids = 1:nrow(dem))
dem.long <- merge(subset(dem.long, !is.na(value)),vars.dem,by="var")
dem.long <- merge(dem.long, labbed, by.x=c("var","value"), by.y=c("name","value"), all.x=TRUE)

keeper <- aggregate(priority ~ row + class, data = dem.long, FUN=max)
dem.short <- merge(dem.long, keeper)
dem.short <- within(dem.short, {
  label[class=="cas"] <- NA
  label[var %in% paste0("Q12_",1:2)] <- "EDUC"
  label[var %in% paste0("Q12_",8)] <- "INC"
  label[var %in% paste0("Q12_",7)] <- "HOME_IO"
  label[var %in% paste0("Q12_",3:6)] <- "EMP"
  label[var %in% paste0("Q12_",9)] <- "UNE"
  label[class=="cas" & is.na(label)] <- "HOME_IO"
})

dem.wide <- reshape(dem.short[c("row","class","label")], direction="wide", idvar="row", timevar="class")

dem.wide <- within(dem.wide, {
  wf.sex <- ifelse(label.sex %in% c("Male","Man"),"M","F")
  wf.sex[is.na(label.sex)] <- NA
  label.yob[which(label.yob=="2005 or later")] <- "2005"
  age <- 2018 - as.numeric(as.character(label.yob))
  wf.age <- ifelse(age>=65, "Y_GE65" ,NA)
  wf.age[is.na(wf.age) & age>=50] <- "Y50-64"  
  wf.age[is.na(wf.age) & age>=30] <- "Y30-49"    
  wf.age[is.na(wf.age) & age>=15] <- "Y15-29"
  wf.cas <- label.cas
  wf.cas[is.na(wf.cas)] <- "UNK"
})

## combine the above information
wei.factors <- subset(df, cntry %in% countries, select=c("row","cntry"))
wei.factors <- merge(wei.factors, df.region, by="row", all.x=TRUE)
wei.factors <- merge(wei.factors, df.edu, by="row", all.x=TRUE)
wei.factors <- merge(wei.factors, dem.wide, by="row", all.x=TRUE)

# wei.factors <- within(wei.factors, {
#   numi <- is.na(wf.sex) + is.na(wf.age)  + is.na(wf.edu) + is.na(wf.region)
# })
wei.factors <- subset(wei.factors, select=c("row","cntry", grep("^wf.", colnames(wei.factors), value = TRUE)))
for (v in c("wf.sex","wf.age","wf.edu","wf.region")) {
  wei.factors[[v]][is.na(wei.factors[[v]])] <- "UNK"
}

saveRDS(wei.factors, file=file.path(in_dir, "Weights_Factors.rds"))