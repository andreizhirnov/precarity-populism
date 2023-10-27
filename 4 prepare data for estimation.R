

set.seed(123)

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")

in_dir <- "./data" 
out_dir <- "./output"
dir.create(file.path(out_dir), showWarnings = FALSE)

countries <- c("AT","FR","DE","HU","IT","PL","RO","ES","SE","NL") 
countries <- sort(countries)
names(countries) <- countries

countries.wp <- setdiff(countries, "RO")
names(countries.wp) <- countries.wp

bulk <- readRDS(file.path(in_dir, "individual_level_data.rds")) 
elec.results <- read.csv(file.path(in_dir, "EVES_rvc_weights.csv"), na="")
any(duplicated(elec.results[c("cntry","pty_abbr")]))

## prepare for applying weights
wei.factors0 <- readRDS(file=file.path(in_dir, "Weights_Factors.rds"))
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

## table(dplyr::anti_join(bulk[c("cntry","recall")], elec.results,  by=c("cntry",'recall'="pty_abbr"))$cntry)

## weights
needed.cov.intent <- list(
  p = c("fin_insecurity", "prec_tenure","prec_work","age","female","nobachelor","doctorate","home_owner","q_income","occupation"),
  f = c("fin_insecurity","age","female","nobachelor","doctorate","home_owner","q_income","occupation")
)

needed.cov.att <- list(
  p = c("fin_insecurity", "prec_tenure","prec_work","age","female","nobachelor","doctorate","home_owner","q_income","occupation"),
  f = c("fin_insecurity","age","female","nobachelor","doctorate","home_owner","q_income","occupation")
)

spec.guide=c("p","f")
names(spec.guide) <- spec.guide

dv.att <- c("ppl","ant", "man","product")
names(dv.att) <- dv.att

wei.vars <- c("wf.sex","wf.age","wf.edu","wf.region")
wei.vars2 <- c(wei.vars,"wf.pty_abbr")

rvc.bmks <- lapply(split(elec.results,elec.results$cntry), function(x) {
  u <- subset(x, pct >=5 | pty_abbr=="NO-VOTE", select=c("pty_abbr","pct"))
  colnames(u) <- c("wf.pty_abbr","value")
  r <- rbind(u, data.frame(wf.pty_abbr="OTH", value=100-sum(u$value)))
  within(r, value <- value/sum(value))
})

temp <- lapply(countries.wp, function(cn) { 
  x <- subset(bulk, cntry==cn)
  u <- dplyr::inner_join(x, elec.results, by =c("cntry",'recall'="pty_abbr"))
  within(u, {
    wf.pty_abbr <- recall
    wf.pty_abbr[which(!is.na(allied))] <- allied[which(!is.na(allied))]  
    wf.pty_abbr[which(!wf.pty_abbr %in% rvc.bmks[[cn]][["wf.pty_abbr"]])] <- "OTH"
  })[c("row","wf.pty_abbr")]
})
temp <- do.call('rbind', temp)

wei.factors <- merge(wei.factors0, temp, by="row", all.x=TRUE )
wei.factors$wf.pty_abbr[which(is.na(wei.factors$wf.pty_abbr))] <- "UNK"

for (cn in countries.wp) {
  emp.select.bmks[[cn]][["wf.pty_abbr"]] <- all.select.bmks[[cn]][["wf.pty_abbr"]] <- rvc.bmks[[cn]]  
}

## rake and prepare samples
list.att <- lapply(spec.guide, function(s) {
  x <- subset(bulk, cntry %in% countries & complete.cases(bulk[c('ppl', needed.cov.att[[s]])])) 
  if (s=='p') {
    bmks <- emp.select.bmks
  } else {
    bmks <- all.select.bmks    
  }
  w <- az_rake(x=x, id='row', by='cntry', on=wei.vars, wf.ds=wei.factors, pop.mg=bmks)
  qq <- split(w, w$cntry)
  qq[['Pooled']] <- w
  lapply(qq, function(y) {
    for (k in intersect(c("fin_insecurity","prec_tenure","prec_work", dv.att), colnames(y))) {
      y[[k]] <- scale(y[[k]])[,1L]
    }
    return(y[,c('row','cntry','wt', dv.att, needed.cov.att[[s]])])
  })
})
  
list.intent <- lapply(spec.guide, function(s) {
  x <- subset(bulk, cntry %in% countries.wp & complete.cases(bulk[c('pg', needed.cov.intent[[s]])])) 
  if (s=='p') {
    bmks <- emp.select.bmks
  } else {
    bmks <- all.select.bmks    
  } 
  w <- az_rake(x=x, id='row', by='cntry', on=wei.vars2, wf.ds=wei.factors, pop.mg=bmks)
  qq <- split(w, w$cntry)
  lapply(qq, function(y) {
    levs <- names(which(table(y$pg)>=10))
    y <- subset(y, pg %in% levs)
    y <- within(y, pg <- droplevels(factor(pg, levels=levs)))
    return(y[,c('row','cntry','wt', 'pg', 'plans', needed.cov.intent[[s]])])
  })
}) 

## export information about parties
 
u <- lapply(list.intent$f, function(x) unique(x[c("cntry","plans","pg")]))
u <- unique(do.call("rbind",u))
u <- tidyr::pivot_wider(u, names_from=pg, values_from=plans, values_fn=function(x) paste(sort(x), collapse="; "))
write.csv(u, file.path(out_dir, "party classification.csv"), na="", row.names=FALSE)

## export samples
saveRDS(list.att, file.path(in_dir, "Data for outlook analysis.rds"))
saveRDS(list.intent, file.path(in_dir, "Data for MNL analysis.rds"))


