library(openxlsx) 

set.seed(123) 

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")
in_dir <- "./data"
files_out <- "./output"

load(file=file.path(in_dir,"EVES.RData"))
countries <- c("AT","FR","DE","HU","IT","PL","RO","ES","SE","NL") 
names(countries) <- countries
labbed <- merge(vars, labs, by.x="vallab", by.y="lname")

meas <- readRDS(file.path(in_dir, "measurement_constructs.rds"))
income.grid0 <- read.csv(file.path(in_dir,"EUROSTAT_income_distribution.csv")) 

pty_info0 <- readRDS(file.path(in_dir,"parties4EVES.rds"))
pty_info <- within(pty_info0, pg <- pg2)[c("cntry","pty_abbr","pg")]

vars.reg <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="geo-grid")
nuts.reg <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="geo-nuts")

vars.edu <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="edu-grid")
vals.edu <- read.xlsx(file.path(in_dir, "translation codes.xlsx"), sheet="edu-values")

compress.to01 <- function(x) {
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

## demographic characteristics
incomes <- df[c("row","cntry","Q52","Q53")]
incomes <- within(incomes, {
  equi_f <- 1 + (Q52-1)*0.5
  equi_f[is.na(equi_f)] <- 1
  income <- Q53
  income[which(income==1)] <- NA
})

income.labs <- read.table(header = TRUE, sep="|", text="
income|lb|ub
2|0|150
3|150|300
4|300|500
5|500|1000
6|1000|1500
7|1500|2000
8|2000|2500
9|2500|3000
10|3000|5000
11|5000|7500
12|7500|10000
13|10000|10000000000
")

income.labs <- merge(data.frame(cntry=countries), income.labs)
incomes <- merge(incomes, income.labs, by=c("cntry","income"))
incomes <- within(incomes, {
  lb <- lb/equi_f
  ub <- ub/equi_f
})
incomes1 <- na.omit(incomes[c("row","cntry","lb","ub")])

income.grid <- within(income.grid0, {
  cntry <- countryname(GEO, "iso2c")
  den <- rep(NA,nrow(income.grid0)) 
  num <- rep(NA,nrow(income.grid0)) 
  den[grepl("quintile",QUANTILE)] <- 5
  den[grepl("quartile",QUANTILE)] <- 4  
  den[grepl("percentile",QUANTILE)] <- 100   
  den[grepl("decile",QUANTILE)] <- 10   
  num[grepl("First",QUANTILE)] <- 1     
  num[grepl("Second",QUANTILE)] <- 2
  num[grepl("Third",QUANTILE)] <- 3
  num[grepl("Fourth",QUANTILE)] <- 4
  num[grepl("Fifth",QUANTILE)] <- 5
  num[grepl("Sixth",QUANTILE)] <- 6
  num[grepl("Seventh",QUANTILE)] <- 7 
  num[grepl("Eighth",QUANTILE)] <- 8 
  num[grepl("Ninth",QUANTILE)] <- 9
  num[grepl("Ninety-fifth",QUANTILE)] <- 95
  num[grepl("Ninety-sixth",QUANTILE)] <- 96
  num[grepl("Ninety-seventh",QUANTILE)] <- 97
  num[grepl("Ninety-eighth",QUANTILE)] <- 98
  num[grepl("Ninety-ninth",QUANTILE)] <- 99
  value <- as.numeric(as.character(gsub("[,:]","",Value)))/12
  qup <- num/den
})
step <- 0.01
income.grid <- unique(subset(income.grid, cntry %in% countries &TIME==2018 & INDIC_IL=="Top cut-off point" & !is.na(qup) & !is.na(value), select=c("cntry","qup","value")))
temp <- dplyr::inner_join(income.grid, income.grid, by="cntry", suffix=c("",".e"))
temp$jump <- temp$qup.e - temp$qup
keeper <- aggregate(jump ~ cntry + qup, data=subset(temp, jump>0), FUN=min)
temp <- merge(temp, keeper)
temp <- merge(temp, data.frame(incr=seq(0,0.1,by=step)))
temp <- within(subset(temp, qup.e > qup +incr), {
  q <- qup + incr
  val <- value + incr*(value.e-value)/(qup.e-qup)
})
income.top <- subset(income.grid, qup==0.99, select=c("cntry","qup","value"))
colnames(income.top) <- c("cntry","q","val")

income.grid <- rbind(temp[c("cntry","q","val")] , income.top) 

temp <- dplyr::inner_join(incomes, income.grid, by = "cntry")
income.lq <- dplyr::summarize(.data=dplyr::group_by(.data=subset(temp, val <= lb), cntry, row), lq=max(q))
income.uq <- dplyr::summarize(.data=dplyr::group_by(.data=subset(temp, val >= lb), cntry, row), uq=min(q))
incomes1 <- dplyr::left_join(incomes1, income.lq)
incomes1 <- dplyr::left_join(incomes1, income.uq)
incomes1 <- within(incomes1, {
  uq[is.na(uq) & !is.na(lq)] <- 1
  lq[is.na(lq) & !is.na(uq)] <- 0
  q_income <- (lq+uq)/2
})
ecdf.income <- aggregate(q_income ~ row, data=incomes1, FUN=mean)
 
medinc <- aggregate(income~cntry, data=incomes, FUN=median)
lowinc <- aggregate(income~cntry, data=incomes, FUN=quantile, probs=0.2)

## demographics used in weights
vars.reg <- merge(vars, vars.reg, by="name")
labs.reg <- merge(vars.reg, labs, by.x="vallab", by.y="lname")
labs.reg <- subset(labs.reg, value != micode)
labs.reg <- merge(labs.reg, nuts.reg, by = c("cntry2","vallab","value"), all.x=TRUE)
labs.reg$cntry <- substr(labs.reg$cntry2,1,2) 
subset(labs.reg, is.na(nuts.code))

data.reg <- subset(df, cntry %in% countries, select=c("row","cntry", vars.reg$name))
data.reg.long <- tidyr::pivot_longer(data.reg, cols=any_of(vars.reg$name), values_drop_na = TRUE)
data.reg.long <- dplyr::inner_join(data.reg.long, labs.reg, by = c("cntry","name","value"))
data.reg.long <- subset(data.reg.long, !is.na(value) & value != micode)
length(unique(data.reg.long$row))

keeper <- aggregate(priority ~ row,data=data.reg.long,FUN=max)
df.region <- subset(merge(data.reg.long, keeper),select=c("row","nuts.code.detail"))
colnames(df.region) <- c("row","nuts.code")

vars.edu <- merge(vars, vars.edu, by="name")
labs.edu <- merge(vars.edu, labs, by.x="vallab", by.y="lname")
labs.edu <- merge(labs.edu, vals.edu, by.x = c("vallab","value"), by.y = c("lname","value"), all.x=TRUE, suffixes=c("",".a")) 
subset(labs.edu, is.na(edu.code))

vars.edu.vec <- sort(unique(vars.edu$name))
data.edu <- subset(df,select=c("row","cntry",vars.edu.vec))
data.edu.long <- reshape(data.edu, direction="long", idvar="row", timevar="var",
                         varying = vars.edu.vec, times=vars.edu.vec, v.names = "value", ids = 1:nrow(data.edu))
data.edu.long <- merge(data.edu.long, labs.edu, by.x=c("cntry","var","value"), by.y=c("cntry","name","value"))
data.edu.long <- subset(data.edu.long, !is.na(edu.code.3cat))
length(unique(data.edu.long$row))

keeper <- aggregate(priority ~ row,data=data.edu.long,FUN=max)
df.edu <- subset(merge(data.edu.long, keeper),select=c("row","edu.code.detail"))
df.edu <- within(df.edu, {
  nobachelor <- as.numeric(edu.code.detail %in% paste0("ED",1:4))
  doctorate <- as.numeric(edu.code.detail == "ED6")
})

vars.dem <- read.table(header=TRUE, sep="|", text="
var|class|priority
Q2|sex|1
Q2wave1b|sex|2        
Q1_Genderwave2|sex|3
Q3|yob|1
Q3Agewave1b|yob|2
Q2Agewave2|yob|3
Q12_1|occupation|5
Q12_2|occupation|6
Q12_3|occupation|7
Q12_4|occupation|8
Q12_5|occupation|9
Q12_6|occupation|10
Q12_7|occupation|3
Q12_8|occupation|2
Q12_9|occupation|4
Q12_10|occupation|1
")

dem <- subset(df, select=c("row", vars.dem$var))
dem.long <- reshape(dem, direction="long", idvar="row", timevar="var",
                    varying = vars.dem$var, times=vars.dem$var, v.names = "value", ids = 1:nrow(dem))
dem.long <- merge(subset(dem.long, !is.na(value)),vars.dem,by="var")
dem.long <- merge(dem.long, labbed, by.x=c("var","value"), by.y=c("name","value"), all.x=TRUE)

keeper <- aggregate(priority ~ row + class, data = dem.long, FUN=max)
dem.short <- merge(dem.long, keeper)
dem.short <- within(dem.short, {
  label[class=="occupation"] <- NA
  label[var %in% paste0("Q12_",1:2)] <- "Student"
  label[var %in% paste0("Q12_",3)] <- "Self-Employed"
  label[var %in% paste0("Q12_",4)] <- "FT Employee"
  label[var %in% paste0("Q12_",5)] <- "PT Employee"
  label[var %in% paste0("Q12_",6)] <- "Temp Employee"
  label[var %in% paste0("Q12_",7)] <- "Homemaker"
  label[var %in% paste0("Q12_",8)] <- "Retired"
  label[var %in% paste0("Q12_",9)] <- "Unemployed"
})

dem.wide <- reshape(dem.short[c("row","class","label")], direction="wide", idvar="row", timevar="class")

dem.wide <- within(dem.wide, {
  occupation <- factor(label.occupation, levels=c("FT Employee","PT Employee","Temp Employee","Self-Employed","Student","Homemaker","Retired","Unemployed"))
  employed <- as.numeric(label.occupation %in% c("FT Employee","PT Employee","Temp Employee"))
  female <- as.numeric(label.sex %in% c("Female","Vrouw"))
  female[is.na(label.sex)] <- NA
  label.yob[which(label.yob=="2005 or later")] <- "2005"
  age <- 2018 - as.numeric(as.character(label.yob))
})


# the remaining demographics
dem2 <- within(df, { 
  income <- Q53
  income[which(income==1)] <- NA
  higher.income <- as.numeric(income>medinc[match(cntry,medinc$cntry),"income"])
  low_income <- as.numeric(income<=lowinc[match(cntry,lowinc$cntry),"income"])
  income <- ordered(income)
  demsat <- as.numeric(Q39>2)
  demsat[is.na(Q39)] <- NA
  one_earner <- as.numeric(Q52==1)
  home_owner <- as.numeric(Q50 %in% 1:2)
  mortgage <- as.numeric(Q50==2)
})

minp <- 1
maxp <- 11

vpos <- within(df, {
  vpos.lr <- (Q27-minp)/(maxp-minp)
  vpos.eu <- (Q29-minp)/(maxp-minp)
})[c("row","vpos.lr","vpos.eu")]

### combine information 
bulk <- subset(meas, cntry %in% countries)  
bulk <- dplyr::left_join(bulk, vpos, by="row")
bulk <- dplyr::left_join(bulk, df.edu[c("row","nobachelor", "doctorate", "edu.code.detail")], by="row")
bulk <- dplyr::left_join(bulk, df.region, by="row")
bulk <- dplyr::left_join(bulk, dem.wide[c("row","female","age","employed","occupation")], by="row")
bulk <- dplyr::left_join(bulk, dem2[c("row","income", "higher.income", "low_income","demsat","one_earner","home_owner","mortgage")], by="row")
bulk <- dplyr::left_join(bulk, ecdf.income, by="row")

bulk <- within(bulk, {
  product <- (compress.to01(ppl)*compress.to01(ant)*compress.to01(man))^(1/3)
})

### add party choice data
eves_vcv0 <- read.csv(file.path(in_dir,"EVES vote choice vars.csv"), encoding="UTF-8", na=c(NA,""))
party_codes <- read.xlsx(file.path(in_dir,"EVES_party_variables.xlsx"), sheet=1L)

## crosswalking
m_parties <- subset(read.csv(file.path(in_dir, "party-match.csv"), encoding="UTF-8"), !is.na(pty_abbr))
xwalk_file <- file.path(in_dir, "matched_parties v5.xlsx")
sheets <- getSheetNames(xwalk_file)
names(sheets) <- sheets

xwalk.list <- lapply(sheets, function(x) {read.xlsx(xwalk_file, x)})
xwalk.names2 <- lapply(xwalk.list, function(x) {na.omit(unique(x[c("cntry", "pty_name","pty_abbr")]))})
xwalk.names2 <- do.call("rbind",xwalk.names2)
 
vcvars <- within(party_codes, {
  var[var=="Q22_1E"] <- "Q22_1ES"
  var[var=="Q22_3RP"] <- "Q22_3RO"  
  var[var=="Q22_6RP"] <- "Q22_6RO"
  priority <- rep(1, nrow(party_codes))
  priority[grepl("wave1b", var)] <- 2
  priority[grepl("1b", var)] <- 2
  priority[grepl("Q24_NL", var)] <- 2
  priority[grepl("wave2", var)] <- 3
  value[is.na(value)] <- -99
})
any(duplicated(vcvars[c("type","cntry","pty_abbr","value","priority")]))

tempvars<- unique(vcvars$var)
vc.bulk <- tidyr::pivot_longer(df[c("row",tempvars)], cols=all_of(tempvars), names_to="var", values_to="value", values_drop_na = TRUE)
vc.bulk <- dplyr::inner_join(vc.bulk, vcvars, by=c("var","value"))
vc.bulk <- dplyr::inner_join(vc.bulk, dplyr::summarize(.data=dplyr::group_by(.data=vc.bulk, row, type), priority=min(priority)))
vc.bulk.coded <- subset(vc.bulk, select=c("row","type","cntry","pty_abbr"))

## -- find the ones with something in the text field but no record on eves_vc1
textvc <- data.frame(var = grep("TEXT",eves_vcv0[,1], value=TRUE))
textvc <- within(textvc, {
  type <- ifelse(grepl("^Q23",var), "recall", "plans")
  cntry <- rep("ZZ", nrow(textvc))
  cntry[grepl("FLA", var)] <- "BE-F"
  cntry[grepl("WAL", var)] <- "BE-W" 
  cntry[grepl("AU", var)] <- "AT"
  cntry[grepl("SW", var)] <- "SE"
  cntry[var=="Q23_10_TEXT"] <- "DK"
  for (j in c("DK","FR","DE","HU","IT","PL","PT","RO","ES","NL")) {
    cntry[grepl(j, var)] <- j 
  } 
  priority <- rep(1, nrow(textvc))
  priority[grepl("wave1b", var)] <- 2
  priority[grepl("1b", var)] <- 2
  priority[grepl("Q24_NL_TEXT", var)] <- 2
  priority[grepl("wave2", var)] <- 3
})

text2 <- tidyr::pivot_longer(text[c("row",textvc$var)], cols=all_of(textvc$var), names_to="var") 
text2 <- subset(text2, value !="")
text3 <- dplyr::anti_join(dplyr::inner_join(text2, textvc, by="var"), vc.bulk.coded, by=c("row","type"))
text4 <- dplyr::left_join(text3, xwalk.names2, by=c("cntry","value"="pty_name"))
text5 <- dplyr::left_join(text4, m_parties, by=c("cntry"="cntry2", "value"), suffix=c("",".mat"))
text5 <- within(text5, {
  pty_abbr[which(is.na(pty_abbr)|pty_abbr=="")] <- pty_abbr.mat[which(is.na(pty_abbr)|pty_abbr=="")]
  pty_abbr[cntry=="IT" & tolower(value) %in% c("+ europa", "+europa")] <- "Piu Europa" 
})
text5 <- subset(text5, !is.na(pty_abbr) & cntry %in% countries)
text5 <- dplyr::inner_join(text5,
                           dplyr::summarize(.data=dplyr::group_by(.data=text5, row, type), priority=min(priority)))
text6 <- as.data.frame(text5[c("row","type","cntry","pty_abbr")])

vc.bulk.all <- rbind(vc.bulk.coded, text6, make.row.names=FALSE)
vc.wide <- dplyr::left_join(tidyr::pivot_wider(vc.bulk.all, names_from=type, values_from=pty_abbr), pty_info, by=c('cntry'='cntry', 'plans'='pty_abbr'))
bulk <- dplyr::left_join(bulk, vc.wide)

novote0 <- read.xlsx(file.path(in_dir,"EVES-novote-codes.xlsx"), sheet=1L)
tempvars<- unique(novote0$var)
novote <- dplyr::inner_join(
  tidyr::pivot_longer(df[c("row",tempvars)], cols=all_of(tempvars), names_to="var", values_to="value", values_drop_na = TRUE),
  novote0, by=c("var","value"))
bulk$recall[which(bulk$row %in% novote$row)] <- 'NO-VOTE'

saveRDS(bulk, file.path(in_dir, "individual_level_data.rds"))
