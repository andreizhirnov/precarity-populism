library(ggplot2)
library(nnet)
library(MASS) 
library(countrycode)

set.seed(123)

theme_set(theme_bw())

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")

in_dir <- "./data" 
out_dir <- "./output"
dir.create(file.path(out_dir), showWarnings = FALSE)
 
list.vc <- readRDS(file.path(in_dir, "Data for MNL analysis.rds"))
(countries2 <- names(list.vc[[1]]))
names(countries2) <- countries2

te.tab <- openxlsx::read.xlsx(file.path(in_dir, "template4tables.xlsx"), sheet="rvc")
te.tab <- within(te.tab, {
  label1 <- label
  label1[which(is.na(label2))] <- paste0("\\multirow{2}{*}{",label1[which(is.na(label2))],"}")
  label <- NULL
})
te.tab.l <- tidyr::pivot_longer(te.tab, cols=c(label1,label2), names_prefix="label", names_to="subrow", values_to="label")
te.tab.l$subrow <- as.numeric(as.character(te.tab.l$subrow))
te.tab.l$label[which(is.na(te.tab.l$label))] <- ""
te.tab.l <- as.data.frame(te.tab.l)

### define RHS
## compile versions of formulae for the regressions

rhs0 <- c(
  c1=  "prec_tenure + prec_work",
  c2=  "prec_tenure + prec_work + age + female + nobachelor + doctorate + home_owner + q_income",
  c3=  "prec_tenure*prec_work + age + female + nobachelor + doctorate + home_owner + q_income",    
  f1=  "fin_insecurity",
  f2=  "fin_insecurity + age + female + nobachelor + doctorate + home_owner + q_income + occupation",
  t1=  "prec_tenure",
  t2=  "prec_tenure + age + female + nobachelor + doctorate + home_owner + q_income",
  w1=  "prec_work",
  w2=  "prec_work + age + female + nobachelor + doctorate + home_owner + q_income",
  z1=  "fin_insecurity + prec_tenure + prec_work",
  z2=  "fin_insecurity + prec_tenure + prec_work + age + female + nobachelor + doctorate + home_owner + q_income + occupation",
  z3=  "fin_insecurity*prec_tenure + fin_insecurity*prec_work + prec_work*prec_tenure + age + female + nobachelor + doctorate + home_owner + q_income + occupation"  
)

rhs.names <- names(rhs0)
names(rhs.names) <- rhs.names
rhs.samples <- c(
  c1= "p", c2= "p", c3= "p", c4= "p",
  f1= "f", f2= "f", f3= "f", f4= "f",
  t1= "p", t2= "p", t3= "p", t4= "p",
  w1= "p", w2= "p", w3= "p", w4= "p",
  z1= "p", z2= "p", z3= "p", z4= "p"  
) 

mnfrm <- sapply(rhs0, function(y) paste0("pg ~ ", y))

#### Regressions
estimates <- lapply(countries2, function(x) {
  lapply(rhs.names, function(j) {
    frm0 <- mnfrm[j]
    if (x=="Pooled") frm0 <- paste0(frm0, "+ cntry") 
    frm <- as.formula(frm0)
    xy <- list.vc[[rhs.samples[j]]][[x]]
    multinom(formula=frm, data = xy, weights=wt, maxit=500) 
  })
})
 
catlabs <- c(PR="Pop Right",PC="Pop Center", PL="Pop Left", MR="Other Right", MC="Other Center", ML="Other Left")

#### export simplified tables for tex ####

sorter <- c(setdiff(countries2, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled\n(exc Hungary and Poland)")
names(sorter.labs) <- sorter

goal <- c("c2","f2")
vc.table.tex <- lapply(countries2, function(x) { 
  s <- lapply(setNames(goal,goal), function(j) { 
    g <- estimates[[x]][[j]]
    levs <- g$lev
    levs <- levs[2L:length(levs)]
    ct <- as.matrix(coefficients(g))
    if (dim(ct)[2]==1) ct <- t(ct)
    set <- as.matrix(summary(g)$standard.errors)
    if (dim(set)[2]==1) set <- t(set)
    z <-  ct/set
    p <- pnorm(-abs(z)) 
    v <- lapply(seq_along(levs), function(rn) {
      stars <- ifelse(p[rn,] <0.01, "***", ifelse(p[rn,] <0.05, "**", ifelse(p[rn,] <0.1, "*","")))
      co <- data.frame(term = colnames(ct), subrow=1,
                       val=paste0(format(round(ct[rn,], digits=3), trim=TRUE),
                                  stars))
      se <- data.frame(term = colnames(ct), subrow=2,
                       val=paste0("(", format(round(set[rn,], digits=3), trim=TRUE), ")"))
      co$val[which(is.na(stars))] <- ""
      se$val[which(is.na(stars))] <- ""
      colnames(co)[which(colnames(co)=="val")] <- paste0(j,":",levs[rn])
      colnames(se)[which(colnames(se)=="val")] <- paste0(j,":",levs[rn])
      rbind(co,se)
    })
    Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), v)
  })
  zz <- Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), s)
  ref <- strsplit(paste0("ref:",catlabs[estimates[[x]][[1]]$lev[1]]), " ")[[1]]
  conv <- sapply(estimates[[x]][goal], "[[", "convergence")
  hg <- within(data.frame(nam=setdiff(colnames(zz), c("term","subrow"))), {
    model <- sapply(strsplit(nam, ":"), "[",1)
    converged <- conv[model]==0
    lev <- sapply(strsplit(nam, ":"), "[",2) 
    level1 <- sapply(strsplit(catlabs[lev]," "), "[", 1)
    level2 <- sapply(strsplit(catlabs[lev]," "), "[", 2)
    level3 <- rep(ref[1], length(nam))
    level4 <- rep(ref[2], length(nam))    
  })
  hg <- hg[order(hg$model, hg$lev),]
  zz <- merge(zz, te.tab.l, by=c("term","subrow"), all.x=TRUE)
  zz <- within(zz, {
    label[which(is.na(label))] <- term[which(is.na(label))]
    sorter <- sorter + subrow
  })
  zz$value <- apply(zz[,hg$nam], 1, function(r) {
    u <- r
    u[which(is.na(u))] <- ""
    paste(u, collapse="&")
  })
  zz$row <- paste0(zz$label, "&", zz$value, ifelse(zz$subrow==1, "\\\\*", "\\\\"))
  list(head = paste0("\\begin{longtable}{l", paste(rep("c", nrow(hg)), collapse=""), "}
\\caption{Covariates of vote choice, parameter estimates of MN logit, ", sorter.labs[x], "}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, ifelse(hg$converged,"","$\\dagger$"), "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level3, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level4, "}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, ifelse(hg$converged,"","$\\dagger$"), "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(nrow(hg)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(nrow(hg)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Standard errors in parentheses.}\\\\
\\endlastfoot
"), 
  body=zz[order(zz$sorter),"row"])
})

## print
write("Tables of estimates: analysis of vote choice intention\n", file = file.path(out_dir, "est_ivc.txt"))

for (cn in countries2) {
  write(vc.table.tex[[cn]][["head"]], file = file.path(out_dir, "est_ivc.txt"), append=TRUE)
  write(vc.table.tex[[cn]][["body"]], file = file.path(out_dir, "est_ivc.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(out_dir, "est_ivc.txt"), append=TRUE)  
}



sorter <- c(setdiff(countries2, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled\n(exc Hungary and Poland)")
names(sorter.labs) <- sorter

goal <- c("z1","z2")
vc.table.tex <- lapply(countries2, function(x) { 
  s <- lapply(setNames(goal,goal), function(j) { 
    g <- estimates[[x]][[j]]
    levs <- g$lev
    levs <- levs[2L:length(levs)]
    ct <- as.matrix(coefficients(g))
    if (dim(ct)[2]==1) ct <- t(ct)
    set <- as.matrix(summary(g)$standard.errors)
    if (dim(set)[2]==1) set <- t(set)
    z <-  ct/set
    p <- pnorm(-abs(z)) 
    v <- lapply(seq_along(levs), function(rn) {
      stars <- ifelse(p[rn,] <0.01, "***", ifelse(p[rn,] <0.05, "**", ifelse(p[rn,] <0.1, "*","")))
      co <- data.frame(term = colnames(ct), subrow=1,
                       val=paste0(format(round(ct[rn,], digits=3), trim=TRUE),
                                  stars))
      se <- data.frame(term = colnames(ct), subrow=2,
                       val=paste0("(", format(round(set[rn,], digits=3), trim=TRUE), ")"))
      co$val[which(is.na(stars))] <- ""
      se$val[which(is.na(stars))] <- ""
      colnames(co)[which(colnames(co)=="val")] <- paste0(j,":",levs[rn])
      colnames(se)[which(colnames(se)=="val")] <- paste0(j,":",levs[rn])
      rbind(co,se)
    })
    Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), v)
  })
  zz <- Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), s)
  ref <- strsplit(paste0("ref:",catlabs[estimates[[x]][[1]]$lev[1]]), " ")[[1]]
  conv <- sapply(estimates[[x]][goal], "[[", "convergence")
  hg <- within(data.frame(nam=setdiff(colnames(zz), c("term","subrow"))), {
    model <- sapply(strsplit(nam, ":"), "[",1)
    converged <- conv[model]==0
    lev <- sapply(strsplit(nam, ":"), "[",2) 
    level1 <- sapply(strsplit(catlabs[lev]," "), "[", 1)
    level2 <- sapply(strsplit(catlabs[lev]," "), "[", 2)
    level3 <- rep(ref[1], length(nam))
    level4 <- rep(ref[2], length(nam))    
  })
  hg <- hg[order(hg$model, hg$lev),]
  zz <- merge(zz, te.tab.l, by=c("term","subrow"), all.x=TRUE)
  zz <- within(zz, {
    label[which(is.na(label))] <- term[which(is.na(label))]
    sorter <- sorter + subrow
  })
  zz$value <- apply(zz[,hg$nam], 1, function(r) {
    u <- r
    u[which(is.na(u))] <- ""
    paste(u, collapse="&")
  })
  zz$row <- paste0(zz$label, "&", zz$value, ifelse(zz$subrow==1, "\\\\*", "\\\\"))
  list(head = paste0("\\begin{longtable}{l", paste(rep("c", nrow(hg)), collapse=""), "}
\\caption{Covariates of vote choice, parameter estimates of MN logit, ", sorter.labs[x], "}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, ifelse(hg$converged,"","$\\dagger$"), "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level3, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level4, "}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, ifelse(hg$converged,"","$\\dagger$"), "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(nrow(hg)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(nrow(hg)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Standard errors in parentheses.}\\\\ 
\\endlastfoot
"), 
       body=zz[order(zz$sorter),"row"])
})

## print
write("Tables of estimates: analysis of vote choice intention\n", file = file.path(out_dir, "est_ivc2.txt"))

for (cn in countries2) {
  write(vc.table.tex[[cn]][["head"]], file = file.path(out_dir, "est_ivc2.txt"), append=TRUE)
  write(vc.table.tex[[cn]][["body"]], file = file.path(out_dir, "est_ivc2.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(out_dir, "est_ivc2.txt"), append=TRUE)  
}

goal <- c("c3","z3")
vc.table.tex <- lapply(countries2, function(x) { 
  s <- lapply(setNames(goal,goal), function(j) { 
    g <- estimates[[x]][[j]]
    levs <- g$lev
    levs <- levs[2L:length(levs)]
    ct <- as.matrix(coefficients(g))
    if (dim(ct)[2]==1) ct <- t(ct)
    set <- as.matrix(summary(g)$standard.errors)
    if (dim(set)[2]==1) set <- t(set)
    z <-  ct/set
    p <- pnorm(-abs(z)) 
    v <- lapply(seq_along(levs), function(rn) {
      stars <- ifelse(p[rn,] <0.01, "***", ifelse(p[rn,] <0.05, "**", ifelse(p[rn,] <0.1, "*","")))
      co <- data.frame(term = colnames(ct), subrow=1,
                       val=paste0(format(round(ct[rn,], digits=3), trim=TRUE),
                                  stars))
      se <- data.frame(term = colnames(ct), subrow=2,
                       val=paste0("(", format(round(set[rn,], digits=3), trim=TRUE), ")"))
      co$val[which(is.na(stars))] <- ""
      se$val[which(is.na(stars))] <- ""
      colnames(co)[which(colnames(co)=="val")] <- paste0(j,":",levs[rn])
      colnames(se)[which(colnames(se)=="val")] <- paste0(j,":",levs[rn])
      rbind(co,se)
    })
    Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), v)
  })
  zz <- Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), s)
  ref <- strsplit(paste0("ref:",catlabs[estimates[[x]][[1]]$lev[1]]), " ")[[1]]
  conv <- sapply(estimates[[x]][goal], "[[", "convergence")
  hg <- within(data.frame(nam=setdiff(colnames(zz), c("term","subrow"))), {
    model <- sapply(strsplit(nam, ":"), "[",1)
    converged <- conv[model]==0
    lev <- sapply(strsplit(nam, ":"), "[",2) 
    level1 <- sapply(strsplit(catlabs[lev]," "), "[", 1)
    level2 <- sapply(strsplit(catlabs[lev]," "), "[", 2)
    level3 <- rep(ref[1], length(nam))
    level4 <- rep(ref[2], length(nam))    
  })
  hg <- hg[order(hg$model, hg$lev),]
  zz <- merge(zz, te.tab.l, by=c("term","subrow"), all.x=TRUE)
  zz <- within(zz, {
    label[which(is.na(label))] <- term[which(is.na(label))]
    sorter <- sorter + subrow
  })
  zz$value <- apply(zz[,hg$nam], 1, function(r) {
    u <- r
    u[which(is.na(u))] <- ""
    paste(u, collapse="&")
  })
  zz$row <- paste0(zz$label, "&", zz$value, ifelse(zz$subrow==1, "\\\\*", "\\\\"))
  list(head = paste0("\\begin{longtable}{l", paste(rep("c", nrow(hg)), collapse=""), "}
\\caption{Covariates of vote choice, parameter estimates of MN logit, ", sorter.labs[x], "}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, ifelse(hg$converged,"","$\\dagger$"), "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level3, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level4, "}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, ifelse(hg$converged,"","$\\dagger$"), "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(nrow(hg)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(nrow(hg)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Standard errors in parentheses.}\\\\ 
\\endlastfoot
"), 
       body=zz[order(zz$sorter),"row"])
})

## print
write("Tables of estimates: analysis of vote choice intention\n", file = file.path(out_dir, "est_ivc3.txt"))

for (cn in countries2) {
  write(vc.table.tex[[cn]][["head"]], file = file.path(out_dir, "est_ivc3.txt"), append=TRUE)
  write(vc.table.tex[[cn]][["body"]], file = file.path(out_dir, "est_ivc3.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(out_dir, "est_ivc3.txt"), append=TRUE)  
}

 
######  charts

## estimated separately
guide <- read.table(sep="|", header=TRUE, text = "
cond|model|label
prec_tenure|c2|precarity of tenure
prec_work|c2|precarity at work
fin_insecurity|f2|financial insecurity
")

forcharts <- lapply(1:nrow(guide), function(i) {
  w <- lapply(countries2, function(x) {
    sam0 <- list.vc[[rhs.samples[guide[i,"model"]]]][[x]]
    g <- estimates[[x]][[guide[i,"model"]]]
    lev <- g$lev
    names(lev) <- lev
    frm <- formula(g)
    terms <- setdiff(all.vars(frm), c("pg",guide[i,"cond"]))
    names(terms) <- terms 
    sam <- data.frame(lapply(terms , function(y) {
      if (y %in% c("female","nobachelor","doctorate","home_owner")) {
        0
      } else if (y=="q_income") {
        0.5
      } else if (y=="occupation") {
        unique(subset(within(sam0, occupation <- factor(occupation)), occupation=="FT Employee", select="occupation"))
      } else if (y=="cntry") {
        unique(subset(within(sam0, cntry <- factor(cntry)), cntry=="NL", select="cntry"))
      } else {
        sum(sam0$wt*sam0[[y]])/sum(sam0$wt)
      }
    })) 
    quars <- Hmisc::wtd.quantile(sam0[,guide[i,"cond"]], c(0.25, 0.75), weights=sam0$wt)
    var <- data.frame(x=c(seq(-2, 2, length.out = 100), quars))
    colnames(var) <- guide[i,"cond"]
    setdiff(terms, guide[i,"cond"])
    sam <- merge(sam, var, by=NULL)
    sam <- merge(sam, data.frame(pg="ML"), by=NULL)
    
    mm <- model.matrix(object=frm, data=sam)
    c <- coef(g)
    if (inherits(c, "matrix")) {
      m <- lapply(rownames(c), function(rn) {
        v <- c[rn,]
        names(v) <- paste0(rn,":",names(c[rn,]))
        v
      })
      m <- unlist(m)
      pr <- lapply(lev[2:length(lev)], function(rn) {
        v <- paste0(rn,":",colnames(mm))
        exp((m[v] %*% t(mm)))[,]
      })
    } else { 
      pr <- list()
      pr[[lev[2]]] <-  exp((c[colnames(mm)] %*% t(mm)))[,]
      names(pr) <- lev[2]
    } 
    pr[[lev[1]]] <- rep(1, nrow(mm))
    den <- Reduce("+", pr)
    pr <- lapply(lev, function(nr) {
      data.frame(est=pr[[nr]]/den, cond = mm[,guide[i,"cond"]], lev=nr, condvar = guide[i,"cond"], cntry=x)
    })
    do.call("rbind", pr)
  })
   do.call("rbind", w)
})
quarts <- lapply(forcharts, function(u) u[grep('(.101)|(.102)$', rownames(u), val=TRUE),])
forcharts <- na.omit(do.call("rbind", c(forcharts, make.row.names = FALSE)))
sorter <- c(setdiff(countries2, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled\n(exc Hungary and Poland)")


varlabs <- c("fin_insecurity"="Financial Precarity","prec_tenure"="Precarity of Tenure","prec_work"="Precarity at Work")

plevs <- c("PL"="Populist Left","PC"="Populist Center","PR"="Populist Right")
temp <- subset(forcharts, cntry!="Pooled" & lev %in% names(plevs))
temp <- within(temp, {
  cntry <- factor(cntry, levels=sorter, labels=sorter.labs)
  lev <- factor(lev, levels=names(plevs), labels=plevs)
  condvar <- factor(condvar, levels=c("fin_insecurity","prec_work","prec_tenure"), labels=varlabs[c("fin_insecurity","prec_work","prec_tenure")])
})
pic <- ggplot(temp, aes(x=cond, y=est, fill=lev)) +
  geom_area(stat="identity") +
  scale_fill_manual(values=c("lightgray","darkgray","black"), breaks=plevs) +
  labs(x="Precarity", y="Vote Probability", fill=element_blank()) +
  facet_grid(cntry ~ condvar) +
  theme(legend.position = "right", plot.margin=unit(rep(1,4),'cm'))
ggsave(filename = file.path(out_dir, "figure4.eps"), pic, width=7, height=9) 

### differences between quartiles
quarts.di <- lapply(quarts, function(u) {
  u$level <- ifelse(grepl('.102$', rownames(u)), 'Q3','Q1')
  aggregate(est ~ cntry + level + condvar, data=subset(u, grepl('^P', lev)), FUN=sum) 
})
quarts.di <- do.call('rbind', quarts.di)
quarts.w <- tidyr::pivot_wider(quarts.di, names_from=level, values_from=est)  
quarts.w <- within(quarts.w, diff <- Q3-Q1)
write.csv(quarts.w, file.path(out_dir, "MNL - diff by quartile.csv"))

### for the analyses with interaction terms (c3)

guide <- read.table(sep="|", header=TRUE, text = "
cond|model|label
prec_work|c3|precarity at work
")
 

bl <- data.frame(prec_work=seq(-2, 2, length.out = 100))
bl <- merge(bl, data.frame(prec_tenure=c(-1,1))) 

forcharts <- lapply(setdiff(countries2, "Pooled"), function(x) {
    i <- 1
    sam0 <- list.vc[[rhs.samples[guide[i,"model"]]]][[x]]
    g <- estimates[[x]][[guide[i,"model"]]]
    lev <- g$lev
    names(lev) <- lev
    frm <- formula(g)
    terms <- setdiff(all.vars(frm), c("pg","prec_tenure", "prec_work", guide[i,"cond"]))
    names(terms) <- terms 
    sam <-  bl
    for (y in terms) {
      if (y %in% c("female","nobachelor","doctorate","home_owner")) {
        sam[[y]] <- 0
      } else if (y=="q_income") {
        sam[[y]] <- 0.5
      } else if (y=="occupation") {
        sam <- merge(sam, unique(subset(within(sam0, occupation <- factor(occupation)), occupation=="FT Employee", select="occupation")))
      } else if (y=="cntry") {
        sam <- merge(sam, unique(subset(within(sam0, cntry <- factor(cntry)), cntry=="NL", select="cntry")))
      } else {
        sam[[y]] <- sum(sam0$wt*sam0[[y]])/sum(sam0$wt)
      }
    }
    sam$pg <- "ML"
    mm <- model.matrix(object=frm, data=sam)
    c <- coef(g)
    if (inherits(c, "matrix")) {
      m <- lapply(rownames(c), function(rn) {
        v <- c[rn,]
        names(v) <- paste0(rn,":",names(c[rn,]))
        v
      })
      m <- unlist(m)
      pr <- lapply(lev[2:length(lev)], function(rn) {
        v <- paste0(rn,":",colnames(mm))
        exp((m[v] %*% t(mm)))[,]
      })
    } else { 
      pr <- list()
      pr[[lev[2]]] <-  exp((c[colnames(mm)] %*% t(mm)))[,]
    }
    pr[[lev[1]]] <- rep(1, nrow(mm))
    
    den <- Reduce("+", pr)
    pr <- lapply(lev, function(nr) {
      data.frame(est=pr[[nr]]/den, prec_work = mm[,"prec_work"], prec_tenure = mm[,"prec_tenure"], 
                 lev=nr, cntry=x)
    })
    do.call("rbind", pr)
  })
  
forcharts <- na.omit(do.call("rbind", c(forcharts, make.row.names = FALSE)))
sorter <- c(setdiff(countries2, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled\n(exc Hungary and Poland)")

varlabs <- c("fin_insecurity"="Financial Insecurity","prec_tenure"="Precarity of Tenure","prec_work"="Precarity at Work")

plevs <- c("PL"="Populist Left","PC"="Populist Center","PR"="Populist Right")
temp <- subset(forcharts, cntry!="Pooled" & lev %in% names(plevs))
temp <- within(temp, {
  cntry <- factor(cntry, levels=sorter, labels=sorter.labs)
  lev <- factor(lev, levels=names(plevs), labels=plevs)
  prec_tenure <- factor(prec_tenure, levels=c(-1,1),
                        labels=c("Precarity of tenure:-1",
                                 "Precarity of tenure:1"))
 })
pic <- ggplot(temp, aes(x=prec_work, y=est, fill=lev)) +
  geom_area(stat="identity") +
  scale_fill_manual(values=c("lightgray","darkgray","black"), breaks=plevs) +
  labs(x="Precarity at work", y="Vote Probability", fill=element_blank()) +
  facet_grid(cntry ~ prec_tenure) +
  theme(legend.position = "right")
ggsave(filename = file.path(out_dir, "Intent-3pwpt.pdf"), pic, width=7, height=9) 

### the same for fin_insecurity
guide <- read.table(sep="|", header=TRUE, text = "
cond|model|label
fin_insecurity|z3|financial insecurity
")
 
bl <- data.frame(fin_insecurity=seq(-2, 2, length.out = 100))
bl <- merge(bl, data.frame(prec_tenure=c(-1,1))) 

forcharts <- lapply(setdiff(countries2, "Pooled"), function(x) {
  i <- 1
  sam0 <- list.vc[[rhs.samples[guide[i,"model"]]]][[x]]
  g <- estimates[[x]][[guide[i,"model"]]]
  lev <- g$lev
  names(lev) <- lev
  frm <- formula(g)
  terms <- setdiff(all.vars(frm), c("pg","prec_tenure", "fin_insecurity", guide[i,"cond"]))
  names(terms) <- terms 
  sam <-  bl
  for (y in terms) {
    if (y %in% c("female","nobachelor","doctorate","home_owner")) {
      sam[[y]] <- 0
    } else if (y=="q_income") {
      sam[[y]] <- 0.5
    } else if (y=="occupation") {
      sam <- merge(sam, unique(subset(within(sam0, occupation <- factor(occupation)), occupation=="FT Employee", select="occupation")))
    } else if (y=="cntry") {
      sam <- merge(sam, unique(subset(within(sam0, cntry <- factor(cntry)), cntry=="NL", select="cntry")))
    } else {
      sam[[y]] <- sum(sam0$wt*sam0[[y]])/sum(sam0$wt)
    }
  }
  sam$pg <- "ML"
  mm <- model.matrix(object=frm, data=sam)
  c <- coef(g)
  if (inherits(c, "matrix")) {
    m <- lapply(rownames(c), function(rn) {
      v <- c[rn,]
      names(v) <- paste0(rn,":",names(c[rn,]))
      v
    })
    m <- unlist(m)
    pr <- lapply(lev[2:length(lev)], function(rn) {
      v <- paste0(rn,":",colnames(mm))
      exp((m[v] %*% t(mm)))[,]
    })
  } else { 
    pr <- list()
    pr[[lev[2]]] <-  exp((c[colnames(mm)] %*% t(mm)))[,]
  }
  pr[[lev[1]]] <- rep(1, nrow(mm))
  
  den <- Reduce("+", pr)
  pr <- lapply(lev, function(nr) {
    data.frame(est=pr[[nr]]/den, fin_insecurity = mm[,"fin_insecurity"], prec_tenure = mm[,"prec_tenure"], 
               lev=nr, cntry=x)
  })
  do.call("rbind", pr)
})

forcharts <- na.omit(do.call("rbind", c(forcharts, make.row.names = FALSE)))
sorter <- c(setdiff(countries2, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled\n(exc Hungary and Poland)")

varlabs <- c("fin_insecurity"="Financial Precarity","prec_tenure"="Precarity of Tenure","prec_work"="Precarity at Work")

plevs <- c("PL"="Populist Left","PC"="Populist Center","PR"="Populist Right")
temp <- subset(forcharts, cntry!="Pooled" & lev %in% names(plevs))
temp <- within(temp, {
  cntry <- factor(cntry, levels=sorter, labels=sorter.labs)
  lev <- factor(lev, levels=names(plevs), labels=plevs)
  prec_tenure <- factor(prec_tenure, levels=c(-1,1),
                        labels=c("Precarity of tenure:-1",
                                 "Precarity of tenure:1"))
})
pic <- ggplot(temp, aes(x=fin_insecurity, y=est, fill=lev)) +
  geom_area(stat="identity") +
  scale_fill_manual(values=c("lightgray","darkgray","black"), breaks=plevs) +
  labs(x="Financial Precarity", y="Vote Probability", fill=element_blank()) +
  facet_grid(cntry ~ prec_tenure) +
  theme(legend.position = "right")
ggsave(filename = file.path(out_dir, "Intent-3fipw.pdf"), pic, width=7, height=9) 


### average marginal effect
guide <- read.table(sep="|", header=TRUE, text = "
cond|model|label
prec_tenure|c2|precarity of tenure
prec_work|c2|precarity at work
fin_insecurity|f2|financial insecurity
")

ame <- lapply(1:nrow(guide), function(i) {
  w <- lapply(countries2, function(x) {
    sam0 <- list.vc[[rhs.samples[guide[i,"model"]]]][[x]]
    g <- estimates[[x]][[guide[i,"model"]]]
    lev <- g$lev
    names(lev) <- lev
    frm <- formula(g)
    frms <- frm; frms[[2]] <- NULL
    mm <- model.matrix(frms, data=sam0)
    c <- coef(g)
    vc <- vcov(g)
    if (inherits(c, "matrix")) {
      m <- lapply(rownames(c), function(rn) {
        v <- c[rn,]
        names(v) <- paste0(rn,":",names(c[rn,]))
        v
      })
      m <- unlist(m)
      sim <- mvrnorm(n=1000, mu=m, Sigma=vc[names(m),names(m)])
      sim <- rbind(m, sim)
      pr <- lapply(lev[2:length(lev)], function(rn) {
        v <- paste0(rn,":",colnames(mm))
        exp((sim[,v] %*% t(mm)))
      })
      bs <- cbind(0,sim[,paste0(setdiff(lev, lev[1]),":",guide[i,"cond"])])
      colnames(bs) <- lev
    } else {
      sim <- mvrnorm(n=1000, mu=c, Sigma=vc[names(c),names(c)])
      sim <- rbind(c, sim)
      pr <- list()
      pr[[lev[2]]] <-  exp((sim[,colnames(mm)] %*% t(mm)))
      names(pr) <- lev[2]
      bs <- cbind(0, sim[,guide[i,"cond"]])
      colnames(bs) <- lev
    }
    
    pr[[lev[1]]] <- matrix(1, nrow=1001, ncol=nrow(mm))
    
    den <- Reduce("+", pr)
    sub <- Reduce("+", lapply(lev, function(j) bs[,j]*pr[[j]]/den)) 
    
    pru <- lapply(lev, function(nr) {
      np <- ((bs[,nr]-sub)*pr[[nr]]/den) %*% sam0$wt/sum(sam0$wt)
      data.frame(est=np[1L], lb = quantile(np[2L:length(np)], 0.025), ub=quantile(np[2L:length(np)], 0.975), lev=nr, condvar = guide[i,"cond"], cntry=x)
    })
    pru$make.row.names <- FALSE
    do.call("rbind", pru)
  })
  do.call("rbind", w)
})
ame <- na.omit(do.call("rbind", c(ame, list(make.row.names = FALSE))))

sorter <- c(setdiff(countries2, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled\n(exc Hungary and Poland)")

varlabs <- c("fin_insecurity"="Financial Precarity","prec_tenure"="Precarity of Tenure","prec_work"="Precarity at Work")
plevs <- c("PL"="Populist Left","PC"="Populist Center","PR"="Populist Right",
           "ML"="Non-Populist Left","MC"="Non-Populist Center","MR"="Non-Populist Right" )

for (u in names(varlabs)) {
temp <- within(subset(ame, condvar==u), { 
  party <- factor(lev, levels=names(plevs), labels=plevs)
  cntry2 <- factor(cntry, levels=sorter, labels=sorter.labs)
})

pic <- ggplot(temp, aes(x=party)) + 
  geom_linerange(aes(ymin=lb, ymax=ub)) +
  geom_point(aes(y=est), size=2, shape=17) +
  geom_hline(yintercept=0, color="red") +
  labs(x=element_blank(), y=paste0("Average Marginal Effect of ", varlabs[u])) +
  facet_wrap(vars(cntry2), nrow=3) + coord_flip()
ggsave(filename = paste0(out_dir, "/MNL_prediction_", u, ".pdf"), pic, width=7.5, height=5)
}

num.types <- table(unique(subset(ame, lev %in% c("PR","PL","PC"), select=c("cntry","lev")))["cntry"])

temp <- within(subset(ame, lev %in% c("PR","PL","PC")), {
  party <- c("PL"="Populist\nLeft","PC"="Populist\nCenter","PR"="Populist\nRight")[lev]
  context <- ifelse(cntry %in% names(which(num.types>1)), "Facing Populist Left", "Not Facing  Populist Left")
  context[which(lev!="PR")] <-""
  cntry2 <- factor(cntry, levels=rev(sorter), labels=rev(sorter.labs))
})

pic <- ggplot(temp, aes(x=est, y=cntry2, xmin=lb, xmax=ub)) +
  geom_linerange() +
  geom_point(size=2, shape=17) +
  geom_vline(xintercept=0, color="grey") +
  facet_grid(party + context ~ condvar, labeller=labeller(condvar=varlabs), scales="free", space="free_y") +
  labs(x="Average Marginal Effect", y=element_blank()) +
  theme(plot.margin=unit(rep(1,4),'cm'))
ggsave(filename = file.path(out_dir, "figure5.eps"), pic, width=7, height=7)

write.csv(temp, file.path(out_dir, "AME.csv"))










