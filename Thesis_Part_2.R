################################################################################
# Thesis Part 2.
################################################################################
library(readxl)
library(vars)
library(tseries)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(ggfortify)
library(zoo)
library(tempdisagg)
library(stats)
library(strucchange)
library(grid)
library(ggplotify)
library(cowplot)
library(gridExtra)
library(vtable)
library(fanplot)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(loo)
library(matrixcalc)


#Set file directory in advance.


######################
##### VAR & BVAR #####
######################

df1 <- read_excel('Exponent_Data.xlsx')
z_a <- ts(df1$Exponent, start = 1998)
z_q <- predict(td(z_a ~ 1, method = "denton-cholette", conversion = "average"))
df2 <- read_excel('Economic_Data.xlsx')
#View(df2)

# Copy data frame with another name "df2_log", which is used for VAR(3)

df2$Exponent <- as.numeric(z_q)
sumtable(df2)

#Copy the data for VAR(3)
df2_log <- df2[df2$FDI > 0, ]
df2_log$GDP <- log(df2_log$GDP)
df2_log$FDI <- log(df2_log$FDI)

#Create the data for VAR(5)
df2$GDP <- df2$GDP/1000000000 
df2$FDI <- df2$FDI/1000000000 


dim(df2_log)


##################
##### VAR(5) #####
##################

gdp <- ts(diff(diff(df2$GDP)), start = c(1998,1), frequency = 4)
fdi <- ts(diff(diff(df2$FDI)), start = c(1998,1), frequency = 4)
reer <- ts(diff(diff(df2$REER)), start = c(1998,1), frequency = 4)
inf <- ts(diff(diff(df2$Inflation)), start = c(1998,1), frequency = 4)
exp <- ts(diff(diff(df2$Exponent)), start = c(1998,1), frequency = 4)

adf.test(gdp)
adf.test(fdi)
adf.test(reer)
adf.test(inf)
adf.test(exp)

tsdata <- cbind(exp, gdp, fdi, reer, inf) %>%
  as.data.frame()

lag <- VARselect(tsdata, lag.max = 5)
lag
estim <- VAR(tsdata, p = 5, type = 'const', season = NULL, exogen = NULL)
summary(estim)
stargazer(estim['varresult'], type = 'text')
acf(tsdata)

causality(estim, cause = 'exp')
causality(estim, cause = 'gdp')
causality(estim, cause = 'fdi')
causality(estim, cause = 'reer')
causality(estim, cause = 'inf')


RESPONSE = "exp"
IMPULSE = c("gdp","fdi","reer", 'inf')

fits = lapply(IMPULSE,function(i){
  irf(estim,response=RESPONSE,impulse=i,
      n.ahead=10,ortho=TRUE,boot=TRUE)
})
names(fits) = IMPULSE

plotdf = lapply(names(fits),function(i){
  data.frame(
    index = 1:nrow(fits[[i]]$irf[[1]]),
    value=fits[[i]]$irf[[1]][,1],
    Lower=fits[[i]]$Lower[[1]][,1],
    Upper=fits[[i]]$Upper[[1]][,1],
    Impulse = i)
})
plotdf=do.call(rbind,plotdf)

irf1 <- ggplot(plotdf,aes(x=index,y=value)) + 
  geom_line() +facet_wrap(~Impulse) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper),fill=NA,col="salmon",linetype="dashed") + 
  geom_hline(yintercept=0,col="salmon") + theme_bw() + xlab("Time") + ylab("Value") +
  ggtitle("VAR(5)")

#irf1

arch.test(estim)
bv.cusum <- stability(estim, type = "OLS-CUSUM")
plot(bv.cusum)
normality.test(estim)


var.serial <- serial.test(estim, lags.pt = 12, lags.bg = 12, type = "PT.asymptotic")
var.serial



##################
##### VAR(3) #####
##################


gdp <- ts(diff(df2_log$GDP), start = c(1998,1), frequency = 4)
fdi <- ts(diff(df2_log$FDI), start = c(1998,1), frequency = 4)
reer <- ts(diff(df2_log$REER), start = c(1998,1), frequency = 4)
inf <- ts(diff(df2_log$Inflation), start = c(1998,1), frequency = 4)
exp <- ts(diff(df2_log$Exponent), start = c(1998,1), frequency = 4)


adf.test(gdp)
adf.test(fdi)
adf.test(reer)
adf.test(inf)
adf.test(exp)

tsdata_2 <- cbind(exp, gdp, fdi, reer, inf) %>%
  as.data.frame()

lag <- VARselect(tsdata_2, lag.max = 5)
lag
estim_2 <- VAR(tsdata_2, p = 3, type = 'const', season = NULL, exogen = NULL)
summary(estim)
stargazer(estim['varresult'], type = 'text')
acf(tsdata_2)

causality(estim_2, cause = 'exp')
causality(estim_2, cause = 'gdp')
causality(estim_2, cause = 'fdi')
causality(estim_2, cause = 'reer')
causality(estim_2, cause = 'inf')


RESPONSE = "exp"
IMPULSE = c("gdp","fdi","reer", 'inf')

fits = lapply(IMPULSE,function(i){
  irf(estim_2,response=RESPONSE,impulse=i,
      n.ahead=10,ortho=TRUE,boot=TRUE)
})
names(fits) = IMPULSE

plotdf = lapply(names(fits),function(i){
  data.frame(
    index = 1:nrow(fits[[i]]$irf[[1]]),
    value=fits[[i]]$irf[[1]][,1],
    Lower=fits[[i]]$Lower[[1]][,1],
    Upper=fits[[i]]$Upper[[1]][,1],
    Impulse = i)
})
plotdf=do.call(rbind,plotdf)

irf2 <- ggplot(plotdf,aes(x=index,y=value)) + 
  geom_line() +facet_wrap(~Impulse) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper),fill=NA,col="salmon",linetype="dashed") + 
  geom_hline(yintercept=0,col="salmon") + theme_bw() + xlab("Time") + ylab("Value") +
  ggtitle("VAR(3)")

plot_grid(irf1, irf2, labels = NULL)


plot(estim)

arch.test(estim_2)
bv.cusum <- stability(estim_2, type = "OLS-CUSUM")
plot(bv.cusum)
normality.test(estim_2)


var.serial <- serial.test(estim_2, lags.pt = 10, lags.bg = 10, type = "PT.asymptotic")
var.serial


graphics.off()

##################################################################
##################################################################



###################
##### BVAR(4) #####
###################

library(BVAR)
library(BVARverse)

minnesota <- bv_mn(
  lambda = c(0.2, 0.4, 1e-4, 5),
  alpha = bv_alpha(mode = 2),
  psi = bv_psi(),
  var = 1e07
)

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


priors <- bv_priors(
  hyper = "auto",
  mn = minnesota,
  soc = soc, sur = sur
)


mh <- bv_mh(
  scale_hess = c(0.05, 0.0001, 0.0001),
  adjust_acc = TRUE, acc_lower = 0.25,
  acc_upper = 0.45
)


run <- bvar(df2_log[, c('Exponent', 'GDP', 'FDI', 'REER', 'Inflation')], lags = 4, 
            n_draw = 50000, n_burn =25000,
            n_thin = 1, priors = priors,
            mh = mh, verbose = TRUE)


summary(run)
round(matrix(coef(run), ncol=5)[,1],4)
logLik(run)
is.positive.definite(vcov(run))



opt_irf <- bv_irf(horizon = 10, identification = TRUE)
irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))


plot(irf(run), area = TRUE,
     vars_impulse = c('GDP', 'FDI', 'REER', 'Inflation'),
     vars_response = c("Exponent"))


library(coda)
run_mcmc <- as.mcmc(run)
geweke.diag(run_mcmc)
################################################################################