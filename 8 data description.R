library(countrycode)
library(ggplot2)

theme_set(theme_bw())

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")
in_dir <- "./data"
out_dir <- "./output"

countries <- c("AT","FR","DE","HU","IT","PL","RO","ES","SE","NL") 
names(countries) <- countries 

census0 <- readRDS(file.path(in_dir, "census_data.rds"))

dt <- readRDS(file=file.path(in_dir, "Data for outlook analysis.rds"))
dt <- dt[["f"]][["Pooled"]]

## list with actual data
dtli <- list()
dtli[["wf.age"]] <- within(dt, {
  type <- "wf.age"
  class <- ifelse(age>=65,'Y_GE65', ifelse(age>=50, 'Y50-64', ifelse(age>=30, 'Y30-49', 'Y15-29'))) 
})
dtli[["wf.sex"]] <- within(dt, {
  type <- "wf.sex"
  class <- ifelse(female==1,'F','M') 
})
dtli[["wf.edu"]] <- within(dt, {
  type <- "wf.edu"
  class <- ifelse(nobachelor==1,'less than tertiary','tertiary')
})
dtli[["wf.cas"]] <- within(dt, {
  type <- "wf.cas"
  class <- ifelse(occupation %in% c("FT Employee","PT Employee","Temp Employee","Self-Employed"), 'EMP',
                  ifelse(occupation=="Student", "EDUC",
                         ifelse(occupation=="Retired", "INC",
                                ifelse(occupation=="Unemployed","UNE", "HOME_IO"))))
})
dt.sum <- lapply(dtli, function(x)  {
  y <- x
  y$obs <- 1
  agg <- aggregate(cbind(obs,wt) ~ cntry + type + class, data=y, FUN=sum)
  totals <- aggregate(cbind(obs,wt) ~ cntry + type, data=y, FUN=sum)
  agg <- merge(agg, totals, by=c("cntry","type")) 
  agg1 <- within(agg, {
    value <- obs.x/obs.y
    source <- "unw"
  })[c("cntry","value","source","type","class")]
  agg2 <- within(agg, {
    value <- wt.x/wt.y
    source <- "wei"
  })[c("cntry","value","source","type","class")]
  rbind(agg1,agg2)
})

## create weights
metrics <- c("wf.sex","wf.age","wf.edu","wf.cas")
names(metrics) <- metrics

census <- subset(census0, wf.region=="TOTAL" & cntry %in% countries)
census <- within(census, {
  wf.edu[which(wf.edu %in% paste0("ED",1:4))] <- 'less than tertiary'
  wf.edu[which(wf.edu %in% paste0("ED",5:6))] <- 'tertiary'
})
## all
census.bmks <- list()
for (i in metrics) { 
  seconds <- setdiff(metrics, i)
  sub <- census[census[[i]]!="TOTAL",]
  for (j in seconds) {
    sub <- sub[sub[[j]]=="TOTAL",]
  }
  sub$class <- sub[[i]]
  agg <- aggregate(list(value=sub$value), by=as.list(sub[,c("cntry","class")]), FUN=sum)
  agg.tot <- aggregate(value ~ cntry, data=sub, FUN=sum)
  census.bmks[[i]] <- within(agg, {
    type <- i
    source <- "census"
    value <- value/agg.tot$value[match(cntry,agg.tot$cntry)] 
  })
} 
sums <- do.call("rbind", c(dt.sum,census.bmks))
rownames(sums) <- NULL

clabs <- countrycode(countries, "iso2c","country.name")
names(clabs) <- countries

temp.age <- within(subset(sums, type=="wf.age"), {
  class <- ordered(class, levels=rev(c('Y15-29','Y30-49','Y50-64','Y_GE65')), labels=rev(c("Under 30","30-49","50-64","65 and over")))
  source <- factor(source, levels=c("census","wei","unw"), labels=c("Benchmark","Weighted","Unweighted"))
  
})

temp.cas <- within(subset(sums, type=="wf.cas" & class=='EMP'), { 
  # source <- factor(source, levels=c("census","wei","unw"), labels=c("Benchmark","Weighted","Unweighted"))
  country <- factor(cntry, levels=names(clabs), labels=clabs)
})

temp <- subset(sums, (type=="wf.edu" & class=='tertiary')|(type=="wf.sex" & class=='F')|(type=="wf.cas" & class=='EMP'))
temp$country <- factor(temp$cntry, levels=rev(names(clabs)), labels=rev(clabs))
temp$type<- factor(temp$type, levels=c("wf.sex","wf.edu","wf.cas"), labels=c("Prop female","Prop with tertiary education","Prop employed"))
temp$source <-factor(temp$source, levels=c("census","wei","unw"))

pic <- ggplot(temp.age, aes(x=source, y=value, fill=class)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(x=element_blank(), y=element_blank(), fill="Age group") +
  facet_wrap(vars(cntry), ncol=2, strip.position="right", labeller=labeller(cntry=clabs)) +
  coord_flip() +
  theme(legend.position="bottom")
ggsave(file.path(out_dir, "balance_weights_age.pdf"), pic, height=5, width=8)

pic <- ggplot(temp, aes(x=country, y=value, fill=source)) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual(values=c("black","gray","white"), breaks =c("unw","wei","census"), labels=c("Unweighted", "Weighted", "Benchmark"),
                    guide = guide_legend(reverse = FALSE)) +
  coord_flip() +
  labs(y=element_blank(), x=element_blank(), fill=element_blank()) +
  theme(legend.position="bottom") +
  facet_wrap(vars(type), scales="free_x")
ggsave(file.path(out_dir, "balance_weights_se.pdf"), pic, height=5, width=6)

pic <- ggplot(temp.cas, aes(x=source, y=value)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(y="Proportion employed", x=element_blank()) +
  facet_wrap(vars(cntry), ncol=3, strip.position="right", labeller=labeller(cntry=clabs)) +
  coord_flip()  
ggsave(file.path(out_dir, "balance_weights_cas.pdf"), pic, height=4, width=8)