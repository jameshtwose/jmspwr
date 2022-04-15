suppressPackageStartupMessages({
  library(ICC.Sample.Size) # ICC functions
  library(WebPower) # frequentist statistics functions
  library(pmsampsize) # Machine learning functions
  library(sjstats) # Linear mixed models functions
  library(pwr)
})

## ICC functions

ICCSampleSize <- function(p=0.75,p0=0.3,k=2,alpha=0.05,tails=2,power=0.8) {
  res_df <- calculateIccSampleSize(p=p,p0=p0,k=k,alpha=alpha,tails=tails,power=power)[[1]]
  res_df <- plyr::rename(res_df, c("p" = "rho", "p0" = "rho0"))
  return(res_df)
}

ICCPower <- function(p=0.75,p0=0.3,k=2,alpha=0.05,tails=2,N=60,desiredPower=0.8) {
  res_df <- calculateIccPower(p=p, p0=p0, k=k, alpha=alpha, tails=tails, N=N, desiredPower=desiredPower)[[1]]
  res_df <- plyr::rename(res_df, c("p" = "rho", "p0" = "rho0"))
  return(res_df)
}

ICCAchievableP0 <- function(p=0.75,k=2,alpha=0.05,tails=2,power=0.8,N=60) {
  res_df <- calculateAchievablep0(p=p,k=k,alpha=alpha,tails=tails,power=power,N=N)[[1]]
  res_df <- plyr::rename(res_df, c("p" = "rho", "p0" = "rho0"))
  return(res_df)
}

## Anova functions

AnovaSampleSize <- function(f_value=0.25, k=3, n=NULL, alpha_value=0.05, power_value=0.8) {
  res <- wp.anova(f=f_value, k=k, n=n, alpha=alpha_value, power = power_value)
  res_df <- data.frame(k=res$k,
                       n=res$n,
                       f=res$f,
                       alpha=res$alpha,
                       power=res$power)
  return(res_df)
} 

AnovaPower <- function(f_value=0.25, k=3, n=160, alpha_value=0.05, power_value=NULL) {
  res <- wp.anova(f=f_value, k=k, n=n, alpha=alpha_value, power = power_value)
  res_df <- data.frame(k=res$k,
                       n=res$n,
                       f=res$f,
                       alpha=res$alpha,
                       power=res$power)
  return(res_df)
} 

AnovaEffectSize <- function(f_value=NULL, k=3, n=160, alpha_value=0.05, power_value=0.8) {
  res <- wp.anova(f=f_value, k=k, n=n, alpha=alpha_value, power = power_value)
  res_df <- data.frame(k=res$k,
                       n=res$n,
                       f=res$f,
                       alpha=res$alpha,
                       power=res$power)
  return(res_df)
}

## Correlation functions

CorrelationSampleSize <- function(n=NULL, r_value=0.3, alpha_value=0.05, 
                                  power_value=0.8, alternative="two.sided") {
  res <- wp.correlation(n = n, r = r_value, power = power_value, 
                        alpha = alpha_value, alternative=alternative)
  res_df <- data.frame(
    n=res$n,
    r=res$r,
    alpha=res$alpha,
    power=res$power,
    alternative=alternative)
  return(res_df)
} 

CorrelationPower <- function(n=60, r_value=0.3, alpha_value=0.05, power_value=NULL, 
                             alternative="two.sided") {
  res <- wp.correlation(n = n, r = r_value, power = power_value, 
                        alpha = alpha_value, alternative=alternative)
  res_df <- data.frame(
    n=res$n,
    r=res$r,
    alpha=res$alpha,
    power=res$power,
    alternative=alternative)
  return(res_df)
} 

CorrelationEffectSize <- function(n=60, r_value=NULL, alpha_value=0.05, power_value=0.8, alternative="two.sided") {
  res <- wp.correlation(n = n, r = r_value, power = power_value, 
                        alpha = alpha_value, alternative=alternative)
  res_df <- data.frame(
    n=res$n,
    r=res$r,
    alpha=res$alpha,
    power=res$power,
    alternative=alternative)
  return(res_df)
} 

## Dependent t-test functions

DependentTTestSampleSize <- function(n1 = NULL, n2 = NULL, d_value=0.5, alpha_value=0.05, power_value=0.8,
                                     sort_test="paired") {
  res <- wp.t(n1 = n1, n2 = n2, d = d_value, alpha = alpha_value, power = power_value, type=sort_test)
  res_df <- data.frame(
    n=res$n,
    d=res$d,
    alpha=res$alpha,
    power=res$power,
    sort_test=sort_test)
  return(res_df)
}

DependentTTestPower <- function(n1 = 30, n2 = 30, d_value=0.5, alpha_value=0.05, power_value=NULL,
                                sort_test="paired") {
  res <- wp.t(n1 = n1, n2 = n2, d = d_value, alpha = alpha_value, power = power_value, type=sort_test)
  res_df <- data.frame(
    n1=n1,
    n2=n2,
    d=res$d,
    alpha=res$alpha,
    power=res$power,
    sort_test=sort_test)
  return(res_df)
}

DependentTTestEffectSize <- function(n1 = 30, n2 = 30, d_value=NULL, alpha_value=0.05, power_value=0.8,
                                     sort_test="paired") {
  res <- wp.t(n1 = n1, n2 = n2, d = d_value, alpha = alpha_value, power = power_value, type=sort_test)
  res_df <- data.frame(
    n1=n1,
    n2=n2,
    d=res$d,
    alpha=res$alpha,
    power=res$power,
    sort_test=sort_test)
  return(res_df)
}

## Independent t-test functions
IndependentTTestSampleSize <- function(n1 = NULL, n2 = NULL, d_value=0.5, alpha_value=0.05, power_value=0.8,
                                       sort_test="two.sample") {
  res <- wp.t(n1 = n1, n2 = n2, d = d_value, alpha = alpha_value, power = power_value, type=sort_test)
  res_df <- data.frame(
    n=res$n,
    d=res$d,
    alpha=res$alpha,
    power=res$power,
    sort_test=sort_test)
  return(res_df)
}

IndependentTTestPower <- function(n1 = 30, n2 = NULL, d_value=0.5, alpha_value=0.05, power_value=NULL,
                                  sort_test="two.sample") {
  res <- wp.t(n1 = n1, n2 = n2, d = d_value, alpha = alpha_value, power = power_value, type=sort_test)
  res_df <- data.frame(
    n=res$n,
    d=res$d,
    alpha=res$alpha,
    power=res$power,
    sort_test=sort_test)
  return(res_df)
}

IndependentTTestEffectSize <- function(n1 = 30, n2 = NULL, d_value=NULL, alpha_value=0.05, power_value=0.8,
                                       sort_test="two.sample") {
  res <- wp.t(n1 = n1, n2 = n2, d = d_value, alpha = alpha_value, power = power_value, type=sort_test)
  res_df <- data.frame(
    n=res$n,
    d=res$d,
    alpha=res$alpha,
    power=res$power,
    sort_test=sort_test)
  return(res_df)
}

## LMM functions
LMMSampleSize <- function(effect_size=0.35, alpha_value=0.05, power_value=0.8, 
                          k_cluster=100, icc=0.05, n=NULL, df.n=NULL) {
  lmmTest <- smpsize_lmm(
    eff.size = effect_size,
    power = power_value,
    sig.level = alpha_value,
    k = k_cluster,
    icc = icc,
    n = n,
    df.n = df.n
  )
  res_df <- data.frame(c(
    effect_size = effect_size, 
    lmmTest,
    amount.clusters = k_cluster, 
    power = power_value, 
    alpha = alpha_value, 
    icc = icc
  ))
  return(res_df)
} 

## ML functions
BinaryClassificationSampleSize <- function(params=5, r_squared=0.3, prevalence=0.5, shrinkage = 0.9, 
                                           cstatistic = NA) {
  res <- pmsampsize(
    type="b",
    rsquared = r_squared,
    parameters=params,
    shrinkage = shrinkage,
    prevalence = prevalence,
    cstatistic = cstatistic,
    seed = 123456,
    rate = NA,
    timepoint = NA,
    meanfup = NA,
    intercept = NA,
    sd = NA,
    mmoe = NA
  )
  
  # print(res$rsquared)
  res_df <- data.frame(
    params=res$parameters,
    n=res$sample_size,
    prevalence=res$prevalence,
    shrinkage=res$final_shrinkage,
    r_squared=res$rsquared
  )
  return(res_df)
} 

RegressionSampleSize <- function(params=5, r_squared=0.3, prevalence=NA, shrinkage = 0.9, 
                                 cstatistic = NA, intercept=1, sd=3, mmoe=1.1) {
  res <- pmsampsize(
    type="c",
    rsquared = r_squared,
    parameters=params,
    shrinkage = shrinkage,
    prevalence = prevalence,
    cstatistic = cstatistic,
    seed = 123456,
    rate = NA,
    timepoint = NA,
    meanfup = NA,
    intercept = intercept,
    sd = sd,
    mmoe = mmoe
  )
  
  res_df <- data.frame(
    params=res$parameters,
    n=res$sample_size,
    intercept=res$intercept,
    shrinkage=res$final_shrinkage,
    r_squared=res$r2a,
    sd=sd
  )
  return(res_df)
} 