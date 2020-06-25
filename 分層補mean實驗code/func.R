library(VIM)
library(DMwR)
library(mice)
library(ggplot2)
library(gridExtra)
library(dummies)

library(stratifyR)
library(tidyverse)
library(sampling)
library(stratification)


### stratify
#"COST", "REV", "ASSET", "EMP"
straty <- function(i , CV , Ls){
  
  #cut data
  load("data/y105.rda")
  
  y105_8620 = y105[which(y105$IND == 8620),]
  
  y105_8620_notax <- y105_8620 %>% filter(TAX == 2) 
  
  complete = y105_8620
  year = 105
  imput_var <- c("REV", "ASSET", "EMP")
  
  ### take the basic dataset information
  IND = unique(complete$IND)
  
  ### create the folder to store the results
  dir.create("result")
  dir.create(paste0("result/", IND))
  dir.create(paste0("result/", IND, "/", "y", year))
  
  #----------------------------
  set.seed(123)
  # y 105 year
  y105_8620_notax_5 <- strata.LH(x = y105_8620_notax[,imput_var[i]], CV = CV, Ls = Ls, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax <- cbind(y105_8620_notax , y105_8620_notax_5$stratumID )
  names(y105_8620_notax )[ncol(y105_8620_notax )] <- c("stratumID")
  
  y105_8620_notax = y105_8620_notax[order(y105_8620_notax$stratumID),]
  y105_8620_strata <- strata(y105_8620_notax ,
                             stratanames = c("stratumID") ,
                             size = y105_8620_notax_5$nh , method="srswor" )
  
  y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
  y105_8620_new <- y105_8620_new[,-ncol(y105_8620_new)] 
  
  y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
  y105_8620 <- y105_8620[,-2]
  save(y105_8620 , file = "data/y105_8620.RData")
  infor = data.frame(N = table(y105_8620_notax$stratumID) ,
                     n = y105_8620_notax_5$nh)
  infor = rbind(infor, c( i , sum(infor$N.Freq) , sum(infor$n)))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
  
  return(y105_8620_notax_5)
}

### Single imputation
imputeSingle = function(incomplete, method, var){
  if(method == "mean"){
    imp = incomplete
    missing = is.na(imp$COST)
    for (l in 1:Ls){
      y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
      y105_8620_ls <- y105_8620_notax[which(y105_8620_new$stratumID == l),]
      
      y105_8620_missing <- y105_8620_notax[-(y105_8620_strata$ID_unit),]
      y105_8620_ls_missing <- y105_8620_notax[which(y105_8620_missing$stratumID == l),]
      for(v in 1:length(var)){
        imp[which(y105_8620$SEQ %in% y105_8620_ls_missing$SEQ) , var[[v]]] <- mean(y105_8620[which(y105_8620$SEQ %in% y105_8620_ls$SEQ) , var[[v]] ], na.rm = TRUE)
      } }
    
  }else if(method == "median"){
    imp = incomplete
    missing = is.na(imp$COST)
    for(v in 1:length(var)){
      imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
    }

  }else if(method == "hotdeck"){
    imp = hotdeck(incomplete, variable = var, domain_var = setdiff(colnames(incomplete), var))[,c(1:length(incomplete))]
    imp = hotdeck(incomplete, variable = var, ord_var = "SAL")[,c(1:length(incomplete))]

  }else if(method == "knn"){
    imp = knnImputation(incomplete, k = 11)

    incomplete[,var] = imp[,var]
  }
  return(imp)
}

### multiple imputation
imputeMI = function(incomplete, method, var, m ,maxit){
  post = make.post(incomplete)
  post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
  imp = mice(incomplete, method = method, m = m, maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
  est = estimate_multiple(imp)
}

### multiple imputation by take mean of every imputation
imputeMIbar = function(imp, data, var, m){
  #output m imputed datasets
  imp1 = complete(imp, "all")
  
  # take m imputed variables mean
  for(i in 1:length(imp1)){
    if(i == 1){
      v = imp1[[i]][,var]
    }else{
      v = v + imp1[[i]][,var]
    }
  }
  v = v / m
  imp2 = data
  imp2[,var] = v
  ### calculate estimates
  return(imp2)
}

### regression estimate for single imputation
estimate_single = function(imp){
  fit = lm(SAL ~ ., data = imp)
  if(nrow(summary(fit)$coefficients) == nrow(confint(fit))){
    est = cbind(summary(fit)$coefficients[,c(1,2,4)], confint(fit))
  }else{
    est = cbind(summary(fit)$coefficients[,c(1,2,4)],
                confint(fit)[complete.cases(confint(fit)),])
  }
  colnames(est) = c("estimate", "SD", "pvalue", "CI_lower", "CI_upper")
  return(est)
}

### regression estimate for multiple imputation
estimate_multiple = function(imp){
  fit = lm.mids(formula = SAL ~ ., data = imp)
  est = summary(pool(fit), "all", conf.int = TRUE)
  est = est[,c("estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
  colnames(est) = c("estimate", "SD", "pvalue", "CI_lower", "CI_upper")
  return(est)
}

### to combine COMP1, COMP2, COMP3, COMP4 for a new variable COMP ###
dummy.COMP = function(x){
  for(o in 1:nrow(x)){
    if(x$COMP1[[o]] == 2){
      x$COMP[[o]] = 0
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(1,1,1))){
      x$COMP[[o]] = 1
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(1,1,2))){
      x$COMP[[o]] = 2
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(1,2,1))){
      x$COMP[[o]] = 3
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(1,2,2))){
      x$COMP[[o]] = 4
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(2,1,1))){
      x$COMP[[o]] = 5
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(2,1,2))){
      x$COMP[[o]] = 6
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(2,2,1))){
      x$COMP[[o]] = 7
    }
    if(all(x[o,c("COMP2", "COMP3", "COMP4")] == c(2,2,2))){
      x$COMP[[o]] = 8
    }
  }
  x$COMP = factor(x$COMP)
  return(x)
}

### In variables selection, we need to decide whether variables need to select
### use regression to detect the significant variables.
reg = function(data, var){
  var_other = setdiff(names(data), var)
  var_pvalue = c()
  for(i in 1:length(var)){
    if(i == 1){
      model = lm(COST ~ var_other, data = data)
    }else if(i == 2){
      model = lm(REV ~ var_other, data = data)
    }else if(i == 3){
      model = lm(ASSET ~ var_other, data = data)
    }else if(i == 4){
      model = lm(INVESTAS ~ var_other, data = data)
    }
    
    ### store the regression results in coefficients
    coefficients = summary(model)$coefficients
    
    ### take the variables names and the p-value
    var_pvalue = cbind(var_pvalue, coefficients[2:nrow(coefficients),4])
    rownames(var_pvalue) = rownames(coefficients)[2:nrow(coefficients)]
    
  }
  return(var_sig)
}


### calculate summary statistic , sum, and ks-test by log or origin scale
type = function(data, t){
  if(t == 1){
    data = data
  }else{
    data = expm1(data)
  }
  
  return(data)
}
