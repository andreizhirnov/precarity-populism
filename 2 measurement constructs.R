library(openxlsx)
library(lavaan)

set.seed(123) 

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")
in_dir <- "./data"
files_out <- "./output"

load(file=file.path(in_dir,"EVES.RData"))
countries <- c("AT","FR","DE","HU","IT","PL","PT","RO","ES","SE","NL") 
names(countries) <- countries
labbed <- merge(vars, labs, by.x="vallab", by.y="lname")

## prepare for making weights
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

## models
models0 <- list(
  prec= list(
    prec_tenure=c('pt_2','pt_3','pt_4','pt_1'),
    prec_work=c('pw_1','pw_2','pw_3')
  ),
  fin= list(
    fin_insecurity=c('fi_2','fi_3','fi_4','fi_1')
  ),
  pop= list(
    ppl = c('ppl_1','ppl_2','ppl_3'),
    ant = c('ant_1','ant_2','ant_3'),
    man = c('man_1','man_2','man_3')
  ))

modvars <- lapply(models0, unlist) 
models <- lapply(models0, function(u) { 
 paste0(names(u), ' =~ ', sapply(u, paste, collapse=' + '), ';') 
})


## extract data
prep <- within(subset(df, cntry %in% countries), {
  pt_2 <- Q17_1
  pt_3 <- Q17_6
  pt_4 <- Q18
  pt_4[which(!(pt_4 %in% 1:4))] <- NA
  pt_1 <- Q16_1
  pw_1 <- Q17_4     
  pw_2 <- Q17_9   
  pw_3 <- Q17_10   
  fi_2 <- Q19_6
  fi_3 <- Q19_10
  fi_4 <- Q19_11
  fi_1 <- Q20
  fi_1[which(!(fi_1 %in% 1:4))] <- NA
  ppl_1 <- Q37_4
  ppl_2 <- Q37_5
  ppl_3 <- Q37_6
  ant_1 <- Q37_7
  ant_2 <- Q37_8
  ant_3 <- Q37_9
  man_1 <- Q37_10
  man_2 <- Q37_11
  man_3 <- Q37_12

  employee.den <- pmax(Q12_1,Q12_2,Q12_3,Q12_4,Q12_5,Q12_6,Q12_7,Q12_8,Q12_9,Q12_10,na.rm=TRUE)
  employed.ft <- employee.den - 1
  employed.pt <- employee.den - 1
  employed.temp <- employee.den - 1
  self_employed <- employee.den - 1 
  employed.ft[Q12_4==1] <- 1
  employed.pt[Q12_5==1] <- 1
  employed.temp[Q12_6==1] <- 1
  self_employed[Q12_3==1] <- 1
  employed <- pmax(employed.ft, employed.pt, employed.temp, na.rm=TRUE) 
})
prep <- subset(prep, select=c('row','cntry', 'employed', do.call('c',modvars)))
 
### loop over measurement models
scored <- list()

m <- 'prec'
for (m in names(models0)) {
  target <- modvars[[m]]
  if (m=='prec') {
    sample0 <- subset(prep, complete.cases(prep[c("row", "cntry", target)]) & employed)
    bmks <- emp.select.bmks
  } else {
    sample0 <- subset(prep, complete.cases(prep[c("row", "cntry", target)]))
    bmks <- all.select.bmks
  }
  
  sample <- az_rake(sample0, id='row', by='cntry', on=wei.vars, wf.ds=wei.factors, pop.mg=bmks)
  model_fit <- cfa(models[[m]], ordered = target, data=sample, sampling.weights="wt")
  config <- cfa(models[[m]], ordered = target, data=sample, sampling.weights="wt" , 
                group="cntry") 
  metric <- cfa(models[[m]], ordered = target, data=sample, sampling.weights="wt", 
                group="cntry", group.equal="loadings" ) 
  scalar <- cfa(models[[m]], ordered = target, data=sample, sampling.weights="wt",
                group="cntry",
                group.equal = c("loadings", "thresholds"))
  
  qq <- do.call('cbind',
                list(master.robust = setNames(fitMeasures(model_fit, c("cfi.scaled","rmsea.scaled","srmr")), c("cfi","rmsea","srmr")),
                     master = fitMeasures(model_fit, c("cfi","rmsea","srmr")),
                     config = fitMeasures(config, c("cfi","rmsea","srmr")),
                     metric = fitMeasures(metric, c("cfi","rmsea","srmr")),
                     scalar = fitMeasures(scalar, c("cfi","rmsea","srmr")))
  )
  
  write.csv(t(qq), file=file.path(files_out,paste0("fitstat_", m, ".csv")), na='')
  solution <- standardizedSolution(model_fit)
  main.est <- subset(solution, op=="=~")
  write.csv(main.est, file=file.path(files_out,paste0("estimates_", m, ".csv")), row.names=FALSE, na='')
  pred <- lavPredict(model_fit, type = "lv")
  scored[[m]] <- data.frame(sample[c('row','cntry')], pred)
  if ('prec_work' %in% colnames(scored[[m]])) {
    scored[[m]][['prec_work']] <- -scored[[m]][['prec_work']]
  }
}

scored.df <- Reduce(function(x,y) merge(x,y, all=TRUE), scored)
saveRDS(scored.df, file=file.path(in_dir,"measurement_constructs.rds"))


