library(ggplot2) 
library(sandwich)
library(lmtest)
library(countrycode)

theme_set(theme_bw())
set.seed(123)

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/PRECEDE project X/Precarity - Populism Paper WP2/replication")

in_dir <- "./data" 
out_dir <- "./output"
dir.create(file.path(out_dir), showWarnings = FALSE)

list.att <- readRDS(file.path(in_dir, "Data for outlook analysis.rds"))
(countries <- names(list.att[[1]]))
names(countries) <- countries

te.tab <- openxlsx::read.xlsx(file.path(in_dir, "template4tables.xlsx"), sheet="att")
te.tab <- within(te.tab, {
  label1 <- label
  label1[which(is.na(label2))] <- label1[which(is.na(label2))]
  label <- NULL
})
te.tab.l <- tidyr::pivot_longer(te.tab, cols=c(label1,label2), names_prefix="label", names_to="subrow", values_to="label")
te.tab.l$subrow <- as.numeric(as.character(te.tab.l$subrow))
te.tab.l$label[which(is.na(te.tab.l$label))] <- ""
te.tab.l <- as.data.frame(te.tab.l)

### define RHS
######## populist outlook -- by country

dvs <- c("ppl","ant","man", "product")
dvlabs <- c("People-Centrism", "Antielitism", "Manichaean Outlook", "Combined Populism")

names(dvs) <- dvs
names(dvlabs) <- dvs

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

attfrm <- lapply(dvs, function(x) lapply(rhs0, function(y) paste0(x, " ~ ", y)))

### regressions
estimates <- lapply(countries, function(x) {
  lapply(dvs, function(k) { 
    lapply(rhs.names, function(j) {
      frm0 <- attfrm[[k]][[j]]
      if (x=="Pooled") frm0 <- paste0(frm0, "+ cntry")
      frm <- as.formula(frm0)
      xy <- list.att[[rhs.samples[j]]][[x]]
      lm(formula=frm, data = xy, weights=wt) 
    })
  })
})

#### export simplified tables for tex ####

sorter <- c(setdiff(countries, c("HU","PL","RO","Pooled")), "HU","PL","RO","Pooled")
sorter.labs <- c(countrycode(setdiff(sorter, "Pooled"), "iso2c", "country.name"),"Pooled")
names(sorter.labs) <- sorter

catlabs1 <- c(ppl = "People-", ant= "Anti-", man= "Manichaean", product = "Combined")
catlabs2 <- c(ppl = "Centrism", ant= "Elitism", man= "Outlook", product = "Populism")

table.tex <- lapply(countries, function(x) {
  s <- lapply(dvs, function(z) { 
    v <- lapply(c(c2="c2",f2="f2"), function(j) {
      m <- estimates[[x]][[z]][[j]]
      ct <- coeftest(m, vcov=vcovHC(m))
      co <- data.frame(term = rownames(ct), subrow=1, 
                       val=paste0(format(round(ct[,1], digits=3), trim=TRUE), 
                                  ifelse(ct[,4] <0.01, "***", ifelse(ct[,4] <0.05, "**", ifelse(ct[,4] <0.1, "*",""))))) 
      se <- data.frame(term = rownames(ct), subrow=2, val=paste0("(", format(round(ct[,2], digits=3), trim=TRUE), ")"))
      
      mo <- data.frame(term = c("r.squared","adj.r.squared","N"), subrow=1, 
                       val=c(format(round(summary(m)$r.squared, digits=3), trim=TRUE),
                             format(round(summary(m)$adj.r.squared, digits=3), trim=TRUE),
                             sum(summary(m)$df[1L:2L])))
      colnames(co)[which(colnames(co)=="val")] <- colnames(se)[which(colnames(se)=="val")] <- colnames(mo)[which(colnames(se)=="val")] <-paste0(j,":",z)
      rbind(co,se,mo)
    })
    Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), v)
  })
  zz <- Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), s)
  hg <- within(data.frame(nam=setdiff(colnames(zz), c("term","subrow"))), {
    model <- sapply(strsplit(nam, ":"), "[",1)
    lev <- sapply(strsplit(nam, ":"), "[",2) 
    level1 <- catlabs1[lev]
    level2 <- catlabs2[lev]
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
\\caption{Covariates of populist attitudes, parameter estimates of linear regressions, ", sorter.labs[x], "}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(nrow(hg)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(nrow(hg)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Robust standard errors in parentheses.}\\\\
\\endlastfoot
"), 
       body=zz[order(zz$sorter),"row"])
})

## print
write("Tables of estimates: analysis of populist attitudes\n", file = file.path(out_dir, "est_att.txt"))

for (cn in countries) {
  write(table.tex[[cn]][["head"]], file = file.path(out_dir, "est_att.txt"), append=TRUE)
  write(table.tex[[cn]][["body"]], file = file.path(out_dir, "est_att.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(out_dir, "est_att.txt"), append=TRUE)  
}

## all precarity together
table.tex <- lapply(countries, function(x) {
  s <- lapply(dvs, function(z) { 
    v <- lapply(c(z1="z1", z2="z2"), function(j) {
      m <- estimates[[x]][[z]][[j]]
      ct <- coeftest(m, vcov=vcovHC(m))
      co <- data.frame(term = rownames(ct), subrow=1, 
                       val=paste0(format(round(ct[,1], digits=3), trim=TRUE), 
                                  ifelse(ct[,4] <0.01, "***", ifelse(ct[,4] <0.05, "**", ifelse(ct[,4] <0.1, "*",""))))) 
      se <- data.frame(term = rownames(ct), subrow=2, val=paste0("(", format(round(ct[,2], digits=3), trim=TRUE), ")"))
      mo <- data.frame(term = c("r.squared","adj.r.squared","N"), subrow=1, 
                       val=c(format(round(summary(m)$r.squared, digits=3), trim=TRUE),
                             format(round(summary(m)$adj.r.squared, digits=3), trim=TRUE),
                             sum(summary(m)$df[1L:2L])))
      colnames(co)[which(colnames(co)=="val")] <- colnames(se)[which(colnames(se)=="val")] <- colnames(mo)[which(colnames(se)=="val")] <-paste0(j,":",z)
      rbind(co,se,mo)
    })
    Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), v)
  })
  zz <- Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), s)
  hg <- within(data.frame(nam=setdiff(colnames(zz), c("term","subrow"))), {
    model <- sapply(strsplit(nam, ":"), "[",1)
    lev <- sapply(strsplit(nam, ":"), "[",2) 
    level1 <- catlabs1[lev]
    level2 <- catlabs2[lev]
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
\\caption{Covariates of populist attitudes, parameter estimates of linear regressions, ", sorter.labs[x], "}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(nrow(hg)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(nrow(hg)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Robust standard errors in parentheses.}\\\\
\\endlastfoot
"), 
       body=zz[order(zz$sorter),"row"])
})

## print
write("Tables of estimates: analysis of populist attitudes -- all precarity\n", file = file.path(out_dir, "est_att2.txt"))

for (cn in countries) {
  write(table.tex[[cn]][["head"]], file = file.path(out_dir, "est_att2.txt"), append=TRUE)
  write(table.tex[[cn]][["body"]], file = file.path(out_dir, "est_att2.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(out_dir, "est_att2.txt"), append=TRUE)  
}



## interactive effects
table.tex <- lapply(countries, function(x) {
  s <- lapply(dvs, function(z) { 
    v <- lapply(c(c3="c3", z3="z3"), function(j) {
      m <- estimates[[x]][[z]][[j]]
      ct <- coeftest(m, vcov=vcovHC(m))
      co <- data.frame(term = rownames(ct), subrow=1, 
                       val=paste0(format(round(ct[,1], digits=3), trim=TRUE), 
                                  ifelse(ct[,4] <0.01, "***", ifelse(ct[,4] <0.05, "**", ifelse(ct[,4] <0.1, "*",""))))) 
      se <- data.frame(term = rownames(ct), subrow=2, val=paste0("(", format(round(ct[,2], digits=3), trim=TRUE), ")"))
      mo <- data.frame(term = c("r.squared","adj.r.squared","N"), subrow=1, 
                       val=c(format(round(summary(m)$r.squared, digits=3), trim=TRUE),
                             format(round(summary(m)$adj.r.squared, digits=3), trim=TRUE),
                             sum(summary(m)$df[1L:2L])))
      colnames(co)[which(colnames(co)=="val")] <- colnames(se)[which(colnames(se)=="val")] <- colnames(mo)[which(colnames(se)=="val")] <-paste0(j,":",z)
      rbind(co,se,mo)
    })
    Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), v)
  })
  zz <- Reduce(function(x,y) merge(x,y,by=c("term","subrow"), all=TRUE), s)
  hg <- within(data.frame(nam=setdiff(colnames(zz), c("term","subrow"))), {
    model <- sapply(strsplit(nam, ":"), "[",1)
    lev <- sapply(strsplit(nam, ":"), "[",2) 
    level1 <- catlabs1[lev]
    level2 <- catlabs2[lev]
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
\\caption{Covariates of populist attitudes, parameter estimates of linear regressions, ", sorter.labs[x], "}\\\\
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endfirsthead
\\hline
\\multirow{2}{*}{Parameter}&", paste(paste0("\\multicolumn{1}{c}{mod ", hg$model, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level1, "}"), collapse="&"), "\\\\
&", paste(paste0("\\multicolumn{1}{c}{", hg$level2, "}"), collapse="&"), "\\\\\\hline
\\endhead
\\hline\\multicolumn{",(nrow(hg)+1),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(nrow(hg)+1),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Robust standard errors in parentheses.}\\\\
\\endlastfoot
"), 
       body=zz[order(zz$sorter),"row"])
})

## print
write("Tables of estimates: analysis of populist attitudes -- interactive effects\n", file = file.path(out_dir, "est_att3.txt"))

for (cn in countries) {
  write(table.tex[[cn]][["head"]], file = file.path(out_dir, "est_att3.txt"), append=TRUE)
  write(table.tex[[cn]][["body"]], file = file.path(out_dir, "est_att3.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(out_dir, "est_att3.txt"), append=TRUE)  
}


## table for charts
att.forcharts <- lapply(dvs, function(y) {  
  w <- lapply(countries, function(x) {
    s <- lapply(names(rhs0), function(z) {
      m <- estimates[[x]][[y]][[z]]
      ct <- coeftest(m, vcov=vcovHC(m))
      ci <- coefci(m, vcov=vcovHC(m))
      ci <- data.frame(term = rownames(ci), lb = ci[,1], ub=ci[,2]) 
      co <- data.frame(term = rownames(ct), estimate=ct[,1], cntry=x, dv=y, model=z,
                       val=paste0(format(round(ct[,1], digits=3), trim=TRUE), " [p=", format(round(ct[,4], digits=3), trim=TRUE), "]")) 
      merge(ci,co,by="term", all=TRUE)
    })
    do.call("rbind",s)
  })
  do.call("rbind",w)
})
att.forcharts <- do.call("rbind", c(att.forcharts, make.row.names = FALSE))
att.forcharts <- within(att.forcharts, dvlab <- factor(dv, levels=dvs, labels=dvlabs)) 

guide <- read.table(sep="|", header=TRUE, text = "
term|model|label
prec_tenure|c2|Precarity of Tenure
prec_work|c2|Precarity at Work
fin_insecurity|f2|Financial Precarity
")

temp <- merge(att.forcharts, guide, by=c("term","model"))

cntries <- setdiff(countries, "Pooled")
temp <- within(temp, {
  cntry <- factor(cntry, levels=c(cntries, "Pooled"), labels=c(countrycode(cntries, "iso2c", "country.name"), "Pooled")) 
})

pic <- ggplot(data=temp, aes(x=cntry, y=estimate, ymin=lb, ymax=ub)) +
  geom_point() + geom_linerange() + geom_hline(yintercept = 0, color="grey") +
  coord_flip() + 
  facet_grid(dvlab ~ label) + 
  scale_x_discrete(limits = rev) +
  labs(x=element_blank(), y=paste("Estimated effect of precarity on populist attitudes")) +
  theme(strip.text.x=element_text(size=12), strip.text.y=element_text(size=12), axis.title.x=element_text(size=12),
        plot.margin=unit(rep(1,4),'cm'))
ggsave(filename = file.path(out_dir, "figure3.eps"), pic, width=7, height=10) 


pic <- ggplot(data=subset(temp, cntry != "Pooled" & dv!='product'),
              aes(x=cntry, y=estimate, ymin=lb, ymax=ub)) +
  geom_point() + geom_linerange() + geom_hline(yintercept = 0, color="red") +
  coord_flip() + 
  facet_grid(dvlab ~ label) + 
  scale_x_discrete(limits = rev, guide = guide_axis(n.dodge = 2)) +
  labs(x=element_blank(), y=paste("Estimated linear effect of precarity")) +
  theme(strip.text.x=element_text(size=12), 
        strip.text.y=element_text(size=12), 
        axis.title.x=element_text(size=14))
ggsave(filename = file.path(out_dir, "Attitudes_l.pdf"), pic, width=8, height=6) 

