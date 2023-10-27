library(countrycode)
library(ggplot2)
library(openxlsx)
library(systemfit)
library(patchwork)
library(sf)
library(ggpattern)

set.seed(123)

theme_set(theme_bw())

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")
in_dir <- "./data"
out_dir <- "./output"

countries <- c("AT","FR","DE","HU","IT","PL","RO","ES","SE","NL") 
names(countries) <- countries 

## import shape-files
map.l0 <- st_read(file.path(in_dir, "/maps/NUTS_RG_20M_2016_4326_LEVL_0.shp/NUTS_RG_20M_2016_4326_LEVL_0.shp"))
map.l1 <- st_read(file.path(in_dir, "/maps/NUTS_RG_20M_2016_4326_LEVL_1.shp/NUTS_RG_20M_2016_4326_LEVL_1.shp"))
map.l2 <- st_read(file.path(in_dir, "/maps/NUTS_RG_20M_2016_4326_LEVL_2.shp/NUTS_RG_20M_2016_4326_LEVL_2.shp"))

## prepare to create weights
wei.factors <- readRDS(file=file.path(in_dir, "Weights_Factors.rds"))
wei.vars <- c("wf.sex","wf.age","wf.edu","wf.region")
load(file=file.path(in_dir, "Census benchmarks.RData"))

az_rake <- function(x, id, by, on, wf.ds, pop.mg) {
  dli <- split(x, x[[by]])
  over <- names(dli)
  weights <- lapply(over, function(cn) {
    wd <- subset(wf.ds, wf.ds[[by]]==cn & wf.ds[[id]] %in% dli[[cn]][[id]])
    wt.template <- lapply(on, function(j) { 
      ini <- pop.mg[[cn]][[j]]
      ini <- ini[ini[[j]]!="UNK",]
      unk <- mean(wd[[j]]=="UNK")
      unk.df <- data.frame(k="UNK",value=unk)
      colnames(unk.df)[1] <- j
      ini$value <- (1-unk)*ini$value/sum(ini$value)
      if (unk>0) ini <- rbind(ini, unk.df)
      ini
    })
    sur <- survey::svydesign(ids=as.formula(paste0('~',id)), probs= ~1, data= wd)
    sur.r <- survey::rake(sur, sample.margins = lapply(paste0("~",on), as.formula), 
                          population.margins = wt.template, 
                          control = list(maxit = 200))
    wt <- stats::weights(sur.r)
    wt <- wt/mean(wt)
    wt[wt>5] <- 5
    s <- data.frame(row=sur.r$cluster[[id]], wt=wt/mean(wt))
    merge(x, s, by=id)
  })
  weights$make.row.names <- FALSE
  do.call('rbind', weights)
}

bulk <- readRDS(file=file.path(in_dir, "individual_level_data.rds"))
bulk <- within(bulk, {
  sam1 <- as.numeric(!is.na(fin_insecurity))
  sam2 <- employed*as.numeric(!is.na(prec_work))  
})

## samples -- add weights

dvs <- c("fin_insecurity","prec_tenure","prec_work")
names(dvs) <- dvs

prec.labs <- c(prec_tenure="Precarity of Tenure", prec_work="Precarity at Work", fin_insecurity="Financial Precarity")

templates <- list("fin_insecurity"=all.select.bmks, "prec_tenure"=emp.select.bmks,"prec_work"=emp.select.bmks)
wei.vars <- c("wf.sex","wf.age","wf.edu","wf.region")

agg.list <- lapply(dvs, function(d) {
  x <- na.omit(bulk[c("row", "cntry","occupation", d)])
  y <- az_rake(x=x, id='row', by='cntry', on=wei.vars, wf.ds=wei.factors, pop.mg=templates[[d]])
  y[["val"]] <- y[[d]]*y[["wt"]]
  z <- aggregate(cbind(val,wt) ~ occupation, data = y, FUN=sum, na.action=NULL, na.rm=TRUE)
  z[[d]] <- z[["val"]]/z[["wt"]]  
  z[c("occupation",d)]
})

tab <- Reduce(function(x,y) merge(x,y,by=c("occupation"), all = TRUE), agg.list) 
tab <- tab[order(tab$fin_insecurity),]
write.csv(tab,file.path(out_dir, "precarity by occupation.csv"), na="", row.names = FALSE)

## number of cases
tab <- aggregate(cbind(sam1,sam2) ~ cntry, bulk, sum, na.action=NULL, na.rm=TRUE)
write.csv(tab,file.path(out_dir, "prec vars - number of observations by country.csv"), na="", row.names = FALSE)

## correlation
bulk.l <- tidyr::pivot_longer( bulk, cols=c(fin_insecurity, prec_tenure, prec_work), 
                               names_to="type", values_to="precarity") 
bulk.l <- subset(as.data.frame(bulk.l), (occupation %in% c("FT Employee", "PT Employee", "Temp Employee") |
                                           (type %in% c("fin_insecurity") & !is.na(occupation))) & !is.na(precarity))
act.levs <- c("FT Employee" = "Full-time\nemployee",
              "PT Employee" = "Part-time\nemployee",
              "Temp Employee" = "Temporary\nemployee",
              "Self-Employed" = "Self-\nemployed",
              "Retired" = "Retired",
              "Homemaker" = "Homemaker",
              "Student" = "Student",
              "Unemployed" = "Unemployed")


bulk.l$act <- factor(bulk.l$occupation, levels=rev(names(act.levs)), labels=rev(act.levs))
bulk.l$edu <- ifelse(bulk.l$nobachelor==1, 1L, ifelse(bulk.l$doctorate==1, 3L,2L))
bulk.l$edu <- factor(bulk.l$edu, 1L:3L, labels=c("Secondary\neducation", "Tertiary\neducation", "Doctorate"))

bulk1 <- subset(bulk, occupation %in% c("FT Employee", "PT Employee", "Temp Employee"))
bulk1 <- droplevels(bulk1)
bulk1.l <- subset(bulk.l, occupation %in% c("FT Employee", "PT Employee", "Temp Employee"))
bulk1.l <- droplevels(bulk1.l)
bulk1.l$female.f <- factor(bulk1.l$female, levels=0:1, labels=c("Male","Female"))

pic <- ggplot(bulk.l) + 
  geom_boxplot(aes(x=act, y=precarity)) +
  facet_grid(. ~ type, labeller=labeller(type=prec.labs)) +
  coord_flip() +
  labs(x=element_blank(), y ="Precarity")
ggsave(file.path(out_dir, "prec_aspects_act.pdf"), pic, width=7, height=5)

pic <- ggplot(subset(bulk1.l, !is.na(edu))) + 
  geom_boxplot(aes(x=edu, y=precarity)) +
  facet_grid(. ~ type, labeller=labeller(type=prec.labs)) +
  coord_flip() +
  labs(x=element_blank(), y ="Precarity")
ggsave(file.path(out_dir, "prec_aspects_edu.pdf"), pic, width=7, height=4)

pic <- ggplot(subset(bulk1.l, !is.na(female))) + 
  geom_boxplot(aes(x=female.f, y=precarity, group=female)) +
  facet_grid(. ~ type, labeller=labeller(type=prec.labs)) +
  coord_flip() +
  labs(x=element_blank(), y ="Precarity")
ggsave(file.path(out_dir, "prec_aspects_fem.pdf"), pic, width=7, height=3)

### urbanization
sh <- c("Cities", "Towns", "Rural")

urb.total <- read.xlsx(file.path(in_dir, "Eurostat_urbanization.xlsx"),  startRow=9, na.strings=c("",":"), sheet="Total")
urb.total$cntry <- substr(urb.total[,1], 1, 2)
urb.total <- subset(urb.total, cntry %in% countries & !apply(is.na(urb.total[as.character(2011:2018)]), 1L, all )) 

urb.totals <- as.matrix(urb.total[as.character(2016:2018)])
urb.rows <- urb.total$GEO
urb.labels <- setNames(urb.total[,2], urb.total$GEO)

urb.li <- lapply(sh, function(s) { 
  u <- read.xlsx(file.path(in_dir, "Eurostat_urbanization.xlsx"), startRow=9, na.strings=c("",":"), sheet=s)
  v <- as.matrix(u[as.character(2016:2018)])
  rownames(v) <- u[,1]
  w <- data.frame(cntry = substr(urb.rows, 1, 2), nuts = urb.rows, share = rowMeans(v[urb.rows,]/urb.totals, na.rm=TRUE))
  w$share[which(is.na(w$share)|is.nan(w$share))] <- 0
  colnames(w)[which(colnames(w)=="share")] <- paste0("share.", tolower(s))
  w
})

urb <- Reduce(function(x,y) merge(x,y, by=c("cntry", "nuts")), urb.li)
urb <- urb[order(urb$nuts), ]

## aggregate by regions
bulk2 <- within(subset(bulk, !is.na(nuts.code)), { 
  nuts.code[which(cntry %in% c("IT","NL","PL"))] <- substr(nuts.code[which(cntry %in% c("IT","NL","PL"))],1,3)
  nuts.code[which(cntry %in% c("RO"))] <- substr(nuts.code[which(cntry %in% c("RO"))],1,4)
})

templates <- list("fin_insecurity"=all.select.bmks,"prec_tenure"=emp.select.bmks,"prec_work"=emp.select.bmks,
                  "ppl"=all.select.bmks,"ant"=all.select.bmks,"man"=all.select.bmks)

wei.vars <- c("wf.sex","wf.age","wf.edu","wf.region") 
agg.list <- lapply(names(templates), function(d) {
  x <- na.omit(bulk2[c("row", "cntry","nuts.code", d)])
  y <- az_rake(x=x, id='row', by='cntry', 
                      on=wei.vars, wf.ds=wei.factors, pop.mg=templates[[d]])
  y[["val"]] <- y[[d]]*y[["wt"]]
  z <- aggregate(cbind(val,wt) ~ cntry + nuts.code, data = y, FUN=sum)
  z[[d]] <- z[["val"]]/z[["wt"]]  
  z[c("cntry","nuts.code",d)]
})
tab <- Reduce(function(x,y) merge(x,y,by=c("cntry","nuts.code"), all = TRUE), agg.list) 

## make maps
dv.labs <- c("ppl"="People-Centrism",
             "ant"="Anti-Elitism",
             "man"="Manichaean Outlook",
             "fin_insecurity"="Financial Precarity",
             "prec_tenure"="Precarity of Tenure",
             "prec_work"="Precarity at Work")
tab.l <- tidyr::pivot_longer(tab, cols=any_of(names(dv.labs)), names_to='dv', values_to='val', values_drop_na = TRUE)

box <- st_bbox(map.l0)
box["ymin"] <- 35
box["xmin"] <- -10
box["xmax"] <- 31

base.map <- st_crop(map.l0, box) 
map.l1 <- st_crop(map.l1, box)
map.l2 <- st_crop(map.l2, box)

df1 <- merge(tab.l,map.l1,by.x= "nuts.code", by.y="NUTS_ID")
df2 <- merge(tab.l,map.l2,by.x= "nuts.code", by.y="NUTS_ID")

setdiff(unique(subset(map.l1, CNTR_CODE %in% df1$cntry)[["NUTS_ID"]]), df1$nuts.code)
setdiff(unique(subset(map.l2, CNTR_CODE %in% df2$cntry)[["NUTS_ID"]]), df2$nuts.code)

df <- rbind(df1,df2) 

df <- within(df, {
  dv <- factor(dv, levels=names(dv.labs), labels=dv.labs)
})

# maps <- ggplot(data = df, mapping=aes(geometry=geometry)) +
#   geom_sf(data=base.map, fill="lightgray", lwd=0) + 
#   geom_sf(aes(fill = val), lwd=0) + 
#   scale_fill_gradient("Average value", low="white", high="darkred") +
#   facet_wrap(vars(dv), ncol=3, dir="h") +
#   theme(legend.position = "bottom", 
#         legend.key.height = unit(0.3, 'cm'),
#         axis.title = element_blank(), 
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()
#   )
maps <- ggplot(data = df, mapping=aes(geometry=geometry)) +
  geom_sf_pattern(data=base.map, fill="white", pattern_color='grey', color='grey', pattern='stripe', lwd=0.1) +
  geom_sf(aes(fill = val), lwd=0) +
  scale_fill_gradient("Average value", low="white", high="black") +
  facet_wrap(vars(dv), ncol=3, dir="h") +
  theme(legend.position = "bottom",
        legend.key.height = unit(0.3, 'cm'),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) 
ggsave(file.path(out_dir,"figure2.jpg"), maps, height=5, width=6)

### combine with urbanisation
tab2 <- merge(tab, urb, by.x=c("cntry","nuts.code"), by.y=c("cntry","nuts"))
tab.l <- tidyr::pivot_longer( 
  tidyr::pivot_longer(tab2, 
                      cols =c(fin_insecurity, prec_tenure, prec_work), names_to='type', values_to='precarity'),
  cols=starts_with("share"), names_to='iv', values_to='share')
tab.l$iv.f <- factor(tab.l$iv, levels=c("share.cities", "share.towns", "share.rural"),
                     labels=c("Cities", "Towns and Suburbs", "Rural Area")) 

pic <- ggplot(tab.l, aes(x=share, y=precarity)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x="Proportion of households living in ...", y="Precarity") +
  scale_x_continuous(labels=scales::percent) +
  facet_grid(type ~ iv.f, scales="free", labeller=labeller(type=prec.labs), switch="both")
ggsave(file.path(out_dir, "prec_aspects_urb.pdf"), pic, width=12, height=7)

## occupations
depth <- tapply(nchar(tab$nuts.code), tab$cntry, max)

occ0 <- foreign::read.dta(file.path(in_dir, "ESS9.dta"), convert.factors=FALSE)
occ <- subset(occ0, cntry %in% countries & isco08>0 & !is.na(anweight), select=c("cntry","idno","region","anweight","isco08"))
occ.li <- list(
  within(occ, {
    depth <- depth[cntry]
    nuts.code <- substr(region, 1, depth)
    occg <- ifelse(substr(isco08,1,1)<=3, "OC1-3",
                   ifelse(substr(isco08,1,1)<=5, "OC4_5",
                          ifelse(substr(isco08,1,1)<=7, "OC6_7","OC8_9")))
    idno <- region <- isco08 <- depth <- NULL
  })
)

need <- setdiff(countries, unique(occ.li[[1]]$cntry)) 
occ0 <- read.csv(file.path(in_dir, "Eurostat_occupations_data.csv"), stringsAsFactors=FALSE, na=c("",":"))
occ0$cntry <- substr(occ0$GEO,1,2)
occ0$depth <- depth[occ0$cntry]
occ <- subset(occ0, cntry %in% need & nchar(GEO)==depth & !is.na(Value) )
occ$anweight <- as.numeric(gsub(",","", occ$Value))/10000
occ$occg <-  occ$ISCO08 
occ$nuts.code <-  occ$GEO 
occ.li[[2]] <- aggregate(anweight ~ cntry + nuts.code + occg, data=occ[,colnames(occ.li[[1]])], FUN=mean)
occ <- do.call("rbind", occ.li)
total <- tapply(occ$anweight, occ$nuts.code, sum)
total <- total[which(total>=10)]
occ.agg <- aggregate(anweight ~ cntry + nuts.code + occg, data=occ, sum)
occ.agg$val <- occ.agg$anweight/total[occ.agg$nuts.code] 
tab2 <- merge(tab, occ.agg, by=c("cntry","nuts.code"))
tab.l <-tidyr::pivot_longer(tab2, 
                            cols =c(fin_insecurity, prec_tenure, prec_work), names_to='type', values_to='precarity')
tab.l$iv.f <- factor(tab.l$occg, levels=c("OC1-3", "OC4_5", "OC6_7", "OC8_9"),
                     labels=c("Managers, professionals,\ntechnicians", 
                              "Clerical, services, and\nsales workers",
                              "Skilled  argicultural workers\nand craft related trades",
                              "Plant and machine operators,\nelementary occupations" )) 
pic <- ggplot(subset(tab.l, val<.8), aes(x=val, y=precarity)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x="Proportion of working in ...", y="Precarity") +
  scale_x_continuous(labels=scales::percent) +
  facet_grid(type ~ iv.f, scales="free", labeller=labeller(type=prec.labs), switch="both")
ggsave(file.path(out_dir, "prec_aspects_occ.pdf"), pic, width=12, height=7)
 
## unemployment
unem <- read.xlsx(file.path(in_dir, "Eurostat_unemployment.xlsx"), startRow=11, na.strings=c("",":"), sheet="Data")
unem$cntry <- substr(unem[,1],1,2)
unem$unemployed <- unem[["2018"]]/100
unem <- subset(unem, cntry %in% countries, select=c("cntry","GEO","unemployed"))

tab2 <- merge(tab, unem, by.x=c("cntry","nuts.code"), by.y=c("cntry","GEO"))
tab.l <- tidyr::pivot_longer(tab2, 
                             cols =c(fin_insecurity, prec_tenure, prec_work), names_to='type', values_to='precarity')

pic <- ggplot(tab.l, aes(x=unemployed, y=precarity)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x="Local unemployment rate", y="Precarity") +
  scale_x_continuous(labels=scales::percent) +
  facet_grid(type ~ ., scales="free", labeller=labeller(type=prec.labs), switch="both")
ggsave(file.path(out_dir, "prec_aspects_unem.pdf"), pic, width=5, height=7)

lapply(split(tab.l, tab.l$type), function(u) cor(u[c("precarity", "unemployed")]))

### regression of precarity of things
unem0 <- read.csv(file.path(in_dir, "Eurostat_unemployment2_data.csv"), stringsAsFactors=FALSE, na=c("",":"))
unem0$cntry <- substr(unem0$GEO,1,2) 
unem <- subset(unem0, TIME==2018 & cntry %in% countries & GEO %in% df.region$nuts.code & !is.na(Value),
               select=c("GEO","ISCED11","SEX","AGE","Value"))

bulk3 <- subset(bulk2, grepl("Employee", occupation) & !is.na(fin_insecurity) & !is.na(prec_tenure)& !is.na(prec_work))
temp <- within(bulk3, {
  GEO <- nuts.code
  AGE <- ifelse(age < 25, "From 15 to 24 years",
                ifelse(age < 35, "From 25 to 34 years",
                       ifelse(age < 45, "From 35 to 44 years",
                              ifelse(age < 55, "From 45 to 54 years",
                                     ifelse(age < 65, "From 55 to 64 years", "From 55 to 64 years")))))
  AGE[which(is.na(age))] <- "From 15 to 74 years"
  ISCED11 <- ifelse(edu.code.detail %in% paste0("ED",0:2), "ED0-2",
                    ifelse(edu.code.detail %in% paste0("ED",5:6), "ED5-8","ED3_4"))
  ISCED11[which(is.na(edu.code.detail))] <- "TOTAL"
  SEX <- ifelse(female==1, "Females", "Males")
  SEX[which(is.na(female))] <- "Total"
})
matched <- list()
matched[[1]] <- dplyr::inner_join(temp, unem)[c("row","Value")]
temp <- subset(temp, !row %in% matched[[1]]$row)
nrow(temp)
temp$SEX <- "Total"
matched[[2]] <- dplyr::inner_join(temp, unem)[c("row","Value")]
temp <- subset(temp, !row %in% matched[[2]]$row)
nrow(temp)
temp$AGE <- "From 15 to 74 years"
matched[[3]] <- dplyr::inner_join(temp, unem)[c("row","Value")]
temp <- subset(temp, !row %in% matched[[3]]$row)
nrow(temp)
temp$ISCED11<- "TOTAL"
matched[[4]] <- dplyr::inner_join(temp, unem)[c("row","Value")]
temp <- subset(temp, !row %in% matched[[4]]$row)
nrow(temp)
unem.matched <- do.call("rbind", matched)
unem.matched$unem.rate <- unem.matched$Value/100
bulk3 <- merge(bulk3, unem.matched) 
bulk3 <- within(subset(bulk3, !is.na(age)), {
  pt_employee <- as.numeric(occupation=="PT Employee")
  temp_employee <- as.numeric(occupation=="Temp Employee")
  age_c <- (age - mean(age))/10
  age_c2 <- age_c^2  
})
for (cn in countries) {
  bulk3[[paste0("cntry",cn)]] = ifelse(bulk3$cntry==cn,1,0) 
} 

ivs <- c("unem.rate","nobachelor","doctorate","pt_employee","temp_employee","female","q_income",
         "home_owner","age_c","age_c2",
         paste0("cntry", setdiff(countries, "AT"))) 

form <- list(
  fireg = "fin_insecurity",
  ptreg = "prec_tenure",
  pwreg = "prec_work"
)

form <- lapply(form, function(dv) as.formula( paste0(dv, "~", paste0(ivs, collapse="+"))))
fitsur <- systemfit(form, data=bulk3, method="SUR")
(smr <- summary(fitsur))


bulk4 <- within(bulk3, {
  u.fin_insecurity <- smr[["residuals"]][,"fireg"]
  u.prec_tenure <- smr[["residuals"]][,"ptreg"]
  u.prec_work <- smr[["residuals"]][,"pwreg"]
})

lim <- list(
  pw = c(-1.2,1.5),
  fi=c(-1.5,2),
  pt=c(-2,2.5)
)

corrs <- list()
corrs[["uc"]] <- corrs[["co"]] <- list()
corrs[["uc"]][["pt-fi"]] <- round(cor(bulk4[c("prec_tenure","fin_insecurity")], use="pairwise.complete.obs")[1,2], 2)
corrs[["uc"]][["pw-fi"]] <- round(cor(bulk4[c("prec_work","fin_insecurity")], use="pairwise.complete.obs")[1,2], 2)
corrs[["uc"]][["pw-pt"]] <- round(cor(bulk4[c("prec_work","prec_tenure")], use="pairwise.complete.obs")[1,2], 2)
corrs[["co"]][["pt-fi"]] <- round(cor(bulk4[c("u.prec_tenure","u.fin_insecurity")], use="pairwise.complete.obs")[1,2], 2)
corrs[["co"]][["pw-fi"]] <- round(cor(bulk4[c("u.prec_work","u.fin_insecurity")], use="pairwise.complete.obs")[1,2], 2)
corrs[["co"]][["pw-pt"]] <- round(cor(bulk4[c("u.prec_work","u.prec_tenure")], use="pairwise.complete.obs")[1,2], 2)

pic <- list()
pic[["uc"]] <- pic[["co"]] <- list()

pic$uc[["pt-fi"]] <- ggplot(bulk4, aes(x=prec_tenure, y=fin_insecurity)) + 
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) + 
  geom_point(alpha=0.05, shape=1) +
  scale_fill_gradient(low="white", high="darkblue") +
  expand_limits(x=lim$pt, y=lim$fi) +
  labs(title = paste0("Precarity of tenure and Financial precarity (cor=", corrs[["uc"]][["pt-fi"]], ")"),
       subtitle="Raw values", x = "Precarity of tenure", y="Financial precarity" ) +
  theme(legend.position="none")

pic$uc[["pw-fi"]] <- ggplot(bulk4, aes(x=prec_work, y=fin_insecurity)) + 
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) + 
  geom_point(alpha=0.05, shape=1) +
  scale_fill_gradient(low="white", high="darkblue")  +
  expand_limits(x=lim$pw, y=lim$fi) +
  labs(title = paste0("Precarity at work and Financial precarity (cor=", corrs[["uc"]][["pw-fi"]], ")"),
       subtitle="Raw values",x = "Precarity at work", y="Financial precarity" )  +
  theme(legend.position="none")

pic$uc[["pw-pt"]] <- ggplot(bulk4, aes(x=prec_work, y=prec_tenure)) + 
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) + 
  geom_point(alpha=0.05, shape=1) +
  expand_limits(x=lim$pw, y=lim$pt) +
  scale_fill_gradient(low="white", high="darkblue")  +
  labs(title = paste0("Precarity at work and Precarity of tenure (cor=", corrs[["uc"]][["pw-pt"]], ")"),
       subtitle="Raw values",x = "Precarity at work", y="Precarity of tenure" )  +
  theme(legend.position="none")

pic$co[["pt-fi"]] <- ggplot(bulk4, aes(x=u.prec_tenure, y=u.fin_insecurity)) + 
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) + 
  geom_point(alpha=0.05, shape=1) +
  expand_limits(x=lim$pt, y=lim$fi) +
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = paste0("Precarity of tenure and Financial precarity (cor=", corrs[["co"]][["pt-fi"]], ")"),
       subtitle="Residuals",x = "Precarity of tenure", y="Financial precarity" ) +
  theme(legend.position="none")

pic$co[["pw-fi"]] <- ggplot(bulk4, aes(x=u.prec_work, y=u.fin_insecurity)) + 
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) + 
  geom_point(alpha=0.05, shape=1) +
  expand_limits(x=lim$pw, y=lim$fi) +
  scale_fill_gradient(low="white", high="darkblue")  +
  labs(title = paste0("Precarity at work and Financial precarity (cor=", corrs[["co"]][["pw-fi"]], ")"),
       subtitle="Residuals",x = "Precarity at work", y="Financial precarity" )  +
  theme(legend.position="none")

pic$co[["pw-pt"]] <- ggplot(bulk4, aes(x=u.prec_work, y=u.prec_tenure)) + 
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) + 
  geom_point(alpha=0.05, shape=1) +
  expand_limits(x=lim$pw, y=lim$pt) +
  scale_fill_gradient(low="white", high="darkblue")  +
  labs(title = paste0("Precarity at work and Precarity of tenure (cor=", corrs[["co"]][["pw-pt"]], ")"),
       subtitle="Residuals", x = "Precarity at work", y="Precarity of tenure" )  +
  theme(legend.position="none")
cpic <- (pic$uc[["pt-fi"]] + pic$co[["pt-fi"]]) / (pic$uc[["pw-fi"]] + pic$co[["pw-fi"]]) / (pic$uc[["pw-pt"]] + pic$co[["pw-pt"]])
ggsave(file.path(out_dir, "prec_aspects.pdf"), cpic, width=10, height=15)

### tables for latex

te.tab <- read.xlsx(file.path(in_dir, "template4tables.xlsx"), sheet="sur")
te.tab <- within(te.tab, {
  label1 <- label
  label1[which(is.na(label2))] <- paste0("\\multirow{2}{*}{",label1[which(is.na(label2))],"}")
  label <- NULL
})
te.tab.l <- tidyr::pivot_longer(te.tab, cols=c(label1,label2), names_prefix="label", names_to="subrow", values_to="label")
te.tab.l$subrow <- as.numeric(as.character(te.tab.l$subrow))
te.tab.l$label[which(is.na(te.tab.l$label))] <- ""
te.tab.l <- as.data.frame(te.tab.l)

v <- as.data.frame(smr$coefficients)
v$stars <- ifelse(v[,4] <0.01, "***", ifelse(v[,4] <0.05, "**", ifelse(v[,4] <0.1, "*","")))
v$dv <- substr(row.names(v), 1,2) 
v$term <- substr(row.names(v), 7, nchar(row.names(v)))
v$subrow.1 <- paste0(format(round(v[,"Estimate"], digits=3), trim=TRUE), v$stars)
v$subrow.2 <- paste0("(", format(round(v[,"Std. Error"], digits=3), trim=TRUE), ")")
u.d <- unique(v$dv)


v.l <- tidyr::pivot_longer(v, cols=starts_with("subrow"), names_to="subrow", values_to="val")
v.l$subrow <- as.numeric(sapply(strsplit(v.l$subrow, "\\."), "[",2))
zz <- dplyr::left_join(
  tidyr::pivot_wider(v.l[c("term","dv","subrow","val")], names_from=dv, values_from=val),
  te.tab.l)
zz <- within(as.data.frame(zz), { 
  sorter <- sorter + subrow
  value <- apply(zz[u.d], 1L, function(r) {
    u <- r
    u[which(is.na(u))] <- ""
    paste(u, collapse="&")
  })
  row <- paste0(label, "&", value, ifelse(subrow==1, "\\\\*", "\\\\"))
}) 
meat <- list(head = paste0("\\begin{longtable}{l", paste(rep("c", length(u.d)), collapse=""), "}
\\caption{Covariates of precarity, estimates of seemingly unrelated regressions}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{", u.d,"}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{", u.d,"}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(length(u.d)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(length(u.d)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Standard errors in parentheses.}\\\\ 
\\endlastfoot
"), 
             body=zz[order(zz$sorter),"row"]) 

## print
write("Tables of estimates: sur\n", file = file.path(out_dir, "est_sur.txt"))
write(meat[["head"]], file = file.path(out_dir, "est_sur.txt"), append=TRUE)
write(meat[["body"]], file = file.path(out_dir, "est_sur.txt"), append=TRUE)
write("\\end{longtable}", file = file.path(out_dir, "est_sur.txt"), append=TRUE)  

### correlations

cor(bulk1[c("fin_insecurity","prec_tenure","prec_work")], use="pairwise.complete.obs")

lapply(split(bulk1, bulk1$occupation), function(x) cor(x[c("fin_insecurity","prec_tenure","prec_work")], use="pairwise.complete.obs"))

cor(bulk[c("fin_insecurity","prec_tenure","prec_work","q_income")], use="pairwise.complete.obs")

