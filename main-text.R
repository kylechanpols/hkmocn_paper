## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(bookdown)
library(tidyverse)
library(lme4)
#library(QCA3)
library(ggplot2)
library(stargazer)
library(utils)
library(lme4) #Frequentist MLM
library(brms) #Bayesian MLM
library(sjPlot)
library(tmap)
#webshot::install_phantomjs() #required for exporting plotly objects
setwd("***your path here***")


## ----wcount, results="hide"-------------------------------------------------------------------------------------------------------
words<- wordcountaddin:::word_count()


## ----empirical1-prepare, results="hide"-------------------------------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(ggplot2)
library(stargazer)
setwd("***your path here***")

#Data Wrangling for Analysis

hkmo <- read_csv("hkmo-dataset.csv")

#read in protest counts
protest <- read_csv("Protest Count/cnhkmo_protest_data_REG_YEAR.csv")
protest<- protest[,-1]

#calibrate, join protest to main dataset
hkmo$case <- ifelse(hkmo$case == "hk", "Hong Kong", "Macau")
hkmo <- hkmo %>% mutate(region = case) %>% select(-case)

hkmo <- left_join(hkmo, protest, by=c("region", "year"))

hkmo$fdi <- hkmo$fdi_fromcn + hkmo$fdi_tocn

############ OLD MODELS #################
#Under-estimation due to multicollineraity if we include the log GDP variable.

#ols <- lm( I(pori_1c2s_noconfi*100) ~ dale_k_to_CN + dale_CN_to_k + tot_trade , data=hkmo)
mixed <- lme4::lmer( I(pori_1c2s_noconfi*100) ~ dale_k_to_cn + dale_cn_to_k + tot_trade +(1|year) + (1|region), data=hkmo)
#ols2 <- lm( I(pori_1c2s_noconfi*100) ~ dale_k_to_cn * dale_cn_to_k + tot_trade, data=hkmo)
mixed2 <- lme4::lmer( I(pori_1c2s_noconfi*100) ~ dale_k_to_cn * dale_cn_to_k + tot_trade +(1|year) + (1|region), data=hkmo)


#new stuff:
mixed3 <- lme4::lmer(I(pori_1c2s_noconfi*100) ~ dale_k_to_cn +
                       dale_cn_to_k + tot_trade+
                       fdi+
                       stock+
                       (1|year) + (1|region), data=hkmo) # + fdi & stock market integration indicators

mixed4 <- update(mixed3, .~. +
                   bridge_pulse+
                   railway_pulse+
                   henqin_pulse)

mixed5 <- update(mixed4, .~. + dale_k_to_cn * dale_cn_to_k )


## ----desciptives, results="asis", eval=F------------------------------------------------------------------------------------------

#
#
###############################TABLE 1 #################################
#
#

## dat<- hkmo %>% select(pori_1c2s_noconfi, n_protest, dale_cn_to_k, dale_k_to_cn, tot_trade, fdi, stock)
## means <- sapply(dat, mean, na.rm=T)
## sds <- sapply(dat, sd, na.rm=T)
## medians <- sapply(dat, median, na.rm=T)
## mins <- sapply(dat, min, na.rm=T)
## maxi <- sapply(dat, max, na.rm=T)
## 
## out <- rbind(means, medians, sds, mins, maxi)
## out <- t(out)
## colnames(out) <- c("Mean", "Median", "Std. Dev.","Min", "Max")
## rownames(out) <- c("Dissent in 1C2S %", "N Protest","UEI", "Dependence", "Total Trade (Billion USD)",
##                    "FDI Volume (Billion HKD)", "Stock Market Joint Movement")
## stargazer(as.matrix(out),header=FALSE, title ="Descriptive Statistics of the Hong Kong/Macau Analysis")


## ----report-mlm, results="asis", eval=F-------------------------------------------------------------------------------------------

#
#
###############################TABLE 2 #################################
#
#

## re1 <- VarCorr(mixed)
## re2<- VarCorr(mixed2)
## re5 <- VarCorr(mixed5)
## stargazer(mixed,mixed2,mixed5, title="Estimating the level of dissent towards 1C2S", header=F,
##           dep.var.labels = "Dissent Towards 1C2S",
##           covariate.labels = c("Dependence",
##                                "UEI",
##                                "Trade Volume",
##                                "FDI Volume",
##                                "Stock Market Integration",
##                                "HZMB",
##                                "Hong Kong-Shenzhen ERL",
##                                "Hengqin Joint Development",
##                                "Dependence * UEI",
##                                "Intercept"),
##           notes=c("Reported are estimates obtained from a multi-level",
##                      "linear regression model. Standard errors are in",
##                      "Parenthesis."),
##           add.lines=list(c("Random Effects (SD)","","",""),
##                          c("Year",round(attr(re1$year,"stddev"),3),round(attr(re2$year,"stddev"),3),round(attr(re5$year,"stddev"),3)),
##                          c("Region",round(attr(re1$region,"stddev"),3),round(attr(re2$region,"stddev"),3),round(attr(re5$region,"stddev"),3))),
##           single.row=T,
##           star.cutoffs = c(.05,.01,.001))


## ----empirical1-plots, results="hide"---------------------------------------------------------------------------------------------
#VISUALIZATION (LOW dependence
#
#
###############################FIGURE 1 #################################
#
#
tmpdat <- model.frame(mixed5)
uei_vec <- seq(from = min(hkmo$dale_cn_to_k, na.rm=T), to = max(hkmo$dale_cn_to_k,  na.rm=T), length = 100)
lambda <- min(tmpdat$dale_k_to_cn, na.rm=T)

pp <- lapply(uei_vec, function(x) {
  
    tmpdat$dale_cn_to_k <- x
    tmpdat$dale_k_to_cn <- lambda
    predict(mixed2, newdata = tmpdat, type = 'response')
    
    })

plotdat_lo <- t(sapply(pp, function(x) { # transposing w/ t() puts data into a column
    c(M = mean(x), quantile(x, c(0.025, 0.975)))
}))

plotdat_lo <- data.frame(uei_vec, plotdat_lo, rep("Low",nrow(plotdat_lo)))

colnames(plotdat_lo) <- c('uei', 'pe', 'lo', 'hi',"group")

tmpdat <- model.frame(mixed2)
lambda <- max(tmpdat$dale_k_to_cn, na.rm=T)

pp <- lapply(uei_vec, function(x) {
  
    tmpdat$dale_cn_to_k <- x
    tmpdat$dale_k_to_cn <- lambda
    predict(mixed2, newdata = tmpdat, type = 'response')
    
    })

plotdat_hi <- t(sapply(pp, function(x) { # transposing w/ t() puts data into a column
    c(M = mean(x), quantile(x, c(0.025, 0.975)))
}))

plotdat_hi <- data.frame(uei_vec, plotdat_hi, rep("High",nrow(plotdat_hi)))

colnames(plotdat_hi) <- c('uei', 'pe', 'lo', 'hi',"group")

plotdat <- rbind(plotdat_lo, plotdat_hi)


## ----fig1, results="asis", fig.cap="\\label{fig:hk-plot1}Predicting dissent towards 1C2S", eval=F---------------------------------
## ## ----newplotout, results="asis", fig.cap="Predicting dissent towards 1C2S"------------------------------------------------
## ggplot(plotdat, aes(x = uei, y = pe, ymin = lo, ymax = hi, lty=group)) +
##   geom_ribbon(alpha = .25) +
##   geom_line() +
##   labs(x = 'UEI', y = '% Dissent to 1C2S') +
##   labs(title="Observed Value Prediction of Dissent towards 1C2S")+
##   scale_linetype_discrete(name="Dependence")+
##   theme_bw()


## ----brms-eval1, include=FALSE----------------------------------------------------------------------------------------------------

#
#
###############################FIGURE 2 #################################
#
#

library(brms)
load("hk_mod2.RData")

#count_bmod <- brm(n_protest ~ dale_k_to_cn * dale_cn_to_k +
#                    scale(tot_trade)+
#                    scale(fdi) + scale(stock) +(1|region) + (1|year),
#                  autocor= ~brms::arma(time=year, gr=region, p=1,q=0, cov=T),
#                  data=hkmo,
#                  family=zero_inflated_negbinomial(),
#                  prior=c(
#                    prior(normal(0,5), "b"),
#                    prior(normal(0,5), "Intercept"),
#                    prior(normal(0,1), "sd")),
#                  seed=999,
#                  control=list(max_treedepth = 12),
#                  chains=2, cores=5, iter=20000, warmup=4000
#                  )
#summary(count_bmod)
#plot(count_bmod)

fx <- conditional_effects(count_bmod,
                    effects="dale_k_to_cn:dale_cn_to_k",
                    probs=c(NA,NA) #no CI
                    )
p1<- plot(fx)[[1]]


## ----p1-show, results="asis", fig.cap="\\label{fig:hk-plot2}Predicting the mean number of protests per year in Hong Kong and Macau", eval=F----
## p1+
##   ggplot2::xlim(c(20,500))+
##   ggplot2::ylim(c(0,20))+
##   labs(x="UEI", y="log(E(Number of protests))", caption="Shown are estimates sampled from the posterior.")+
##   scale_color_manual(values=c("#FF0000", "#00FF00", "#0000FF"),
##                      name="Dependence",
##                      labels=c("High","Mid","Low"))+
##   scale_fill_manual(values=c("#FF0000", "#00FF00", "#0000FF"),
##                      name="Dependence",
##                      labels=c("High","Mid","Low"))+
##   theme_bw()
## 


## ----cn-prep, results="hide", eval=T----------------------------------------------------------------------------------------------
#CN Model codes. Currently EVAL is set to False so these are not run when compiling.
#I am running a prefitted model with the same code from memory to save time. If you are interested
#in replication you can run these models by setting eval=T.

cn <- read_csv("cn_dataset_augmeneted_MERGED.csv")

cn<- cn[,-1]

cn <- cn %>% dplyr::filter(!is.na(region))

cn$n_protest <- ifelse(is.na(cn$n_protest), 0 , cn$n_protest)


## ----desciptives-cn, results="asis", eval=F---------------------------------------------------------------------------------------

#
#
###############################TABLE 3 #################################
#
#

## dat<- cn %>% select(n_protest, subin, shared_income, T_goods)
## means <- sapply(dat, mean, na.rm=T)
## sds <- sapply(dat, sd, na.rm=T)
## medians <- sapply(dat, median, na.rm=T)
## mins <- sapply(dat, min, na.rm=T)
## maxi <- sapply(dat, max, na.rm=T)
## 
## out <- rbind(means, medians, sds, mins, maxi)
## out <- t(out)
## colnames(out) <- c("Mean", "Median", "Std. Dev.","Min", "Max")
## rownames(out) <- c("N Protests per year", "Subsisidies (100 million CNY)", "Shared Income (100 million CNY)", "Trade Volume (100 million CNY)")
## stargazer(as.matrix(out),header=FALSE, title ="Descriptive Statistics of the Chinese Provincial Analysis")


## ----cn-bmod-fit, results="hide", eval=F------------------------------------------------------------------------------------------
## cnmod1 <- brm(n_protest ~ scale(subin)*scale(shared_income) + scale(T_goods) +(1|year) + (1|region),
##               data=cn,
##               autocor = ~brms::arma(time=year, gr= region, p=1,q=0, cov=T),
##               family=zero_inflated_negbinomial(),
##               prior=c(
##                 prior(normal(0,5), "b"),
##                 prior(normal(0,5), "Intercept"),
##                 prior(normal(0,1), "sd")),
##               seed=999,
##               control=list(max_treedepth = 12),
##               chains=2, cores=5, iter=20000, warmup=4000
## )

## ----cn-mod-load-express, results="hide"------------------------------------------------------------------------------------------
load("cn_mod2.RData")


## ----protest_map, fig.cap="\\label{fig:protest_map}Aggregated Number of Protests between 2000-2018 across China, Hong Kong and Macau", eval=F----
## load("protest_map.RData")
## tmap_arrange(CN_map, HK_map, MO_map, legend.map)


## ----cn-plot1-get, include=FALSE--------------------------------------------------------------------------------------------------
#
#
###############################FIGURE 4 #################################
#
#

cnfx<- conditional_effects(cnmod1,
                    effects="subin:shared_income")

p2<- plot(cnfx)[[1]]


## ----cn-plot1-plot, results="asis", fig.cap="\\label{fig:cn-plot1}Predicting the mean number of protests in Chinese provinces", eval=F----
## p2+
##   ggplot2::xlim(c(0,6000))+
##   ggplot2::ylim(c(0,100))+
##   labs(x="UEI", y="log(E(Number of protests))", caption="Shown are estimates wrapped around their 95% Bayesian Critical Intervals")+
##   scale_color_manual(values=c("#FF0000", "#00FF00", "#0000FF"),
##                      name="Dependence",
##                      labels=c("High","Mid","Low"))+
##   scale_fill_manual(values=c("#FF0000", "#00FF00", "#0000FF"),
##                     name="Dependence",
##                     labels=c("High","Mid","Low"))+
##   theme(plot.caption = element_text(size=10))+
##   theme_bw()


## ----bayesian_mod_to_tables, eval=F-----------------------------------------------------------------------------------------------
## tab_model(count_bmod, title="Estimating the Mean Number of Protests in Hong Kong and Macau",
##           pred.labels = c("Intercept",
##                     "Dependence",
##                     "UEI",
##                     "Trade Volume",
##                     "FDI",
##                     "Stock Market Integration",
##                     "Dependence * UEI"),
##           dv.labels = "Mean Number of Protests")
## 
## tab_model(cnmod1, title="Estimating the Mean Number of Protests in Chinese Provinces",
##           pred.labels = c("Intercept",
##                           "UEI",
##                           "Dependence",
##                           "Trade Volume",
##                           "UEI * Dependence"),
##                           dv.labels="Mean Number of Protests")

