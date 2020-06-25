library(VIM)
library(DMwR)
library(mice)
library(ggplot2)
library(gridExtra)
library(dummies)
library(stratifyR)
library(tidyverse)
library(sampling)

### Single imputation
imputeSingle = function(incomplete, method, var){
  if(method == "mean"){
    imp = incomplete
    missing = is.na(imp$COST)
    if (names(realstrata)[ncol(realstrata)]  == "stratumID"){
    for(i in 1:10){
      for(v in 1:length(var)){
        # imp[missing, var[[v]]] = mean(imp[, var[[v]]], na.rm = TRUE)
       imputation_mean <- tapply(realstrata[,var[[v]]], realstrata$stratumID , mean )
       imp[which(nostrata$stratumID == i) , var[[v]]] = imputation_mean[i]
        }
    } 
    }else if (names(realstrata)[ncol(realstrata)]  == "stratanames"){  
    for(i in 1:8){
      for(v in 1:length(var)){
        # imp[missing, var[[v]]] = mean(imp[, var[[v]]], na.rm = TRUE)
        imputation_mean <- tapply(realstrata[,var[[v]]], realstrata$stratanames , mean )
        imp[which(nostrata$stratanames == paste0("group" , i)) , var[[v]]] = imputation_mean[i]
      }
    } 
    }else if (names(realstrata)[ncol(realstrata)]  == "strata"){  
      for(i in 1:9){
        for(v in 1:length(var)){
          # imp[missing, var[[v]]] = mean(imp[, var[[v]]], na.rm = TRUE)
          imputation_mean <- tapply(realstrata[,var[[v]]], realstrata$strata , mean )
          imp[which(nostrata$strata == paste0("group" , i)) , var[[v]]] = imputation_mean[i]
        }
      } 
    }   

  }else if(method == "median"){
    imp = incomplete
    missing = is.na(imp$COST)
    if (names(realstrata)[ncol(realstrata)]  == "stratumID"){ 
      for(i in 1:10){
        for(v in 1:length(var)){
        #imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
        imputation_median <- tapply(realstrata[,var[[v]]], realstrata$stratumID , median )
        imp[which(nostrata$stratumID == i) , var[[v]]] = imputation_median[i]
         }
      } 
    }else if(names(realstrata)[ncol(realstrata)]  == "stratanames"){  
      for(i in 1:8){
        for(v in 1:length(var)){
          #imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
          imputation_median <- tapply(realstrata[,var[[v]]], realstrata$stratanames , median )
          imp[which(nostrata$stratanames == paste0("group" , i)) , var[[v]]] = imputation_median[i]
        }
      } 
    }else if(names(realstrata)[ncol(realstrata)]  == "strata"){  
      for(i in 1:9){
        for(v in 1:length(var)){
          #imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
          imputation_median <- tapply(realstrata[,var[[v]]], realstrata$strata , median )
          imp[which(nostrata$strata == paste0("group" , i)) , var[[v]]] = imputation_median[i]
        }
      } 
    }
  }else if(method == "hotdeck"){
    # imp = hotdeck(incomplete, variable = var, domain_var = setdiff(colnames(incomplete), var))[,c(1:length(incomplete))]
    # imp = hotdeck(incomplete, variable = var, ord_var = "SAL")[,c(1:length(incomplete))]
    if (names(realstrata)[ncol(realstrata)]  == "stratumID"){ 
      for(i in 1:10){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean))]
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, domain_var = setdiff(colnames(innewmean_x), var))[,c(1:length(innewmean_x))])
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, ord_var = "SAL")[,c(1:length(innewmean_x))]) 
      } 
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10 )
      
    }else if(names(realstrata)[ncol(realstrata)]  == "stratanames"){ 
      for(i in 1:8){
        innewmean_x = innewmean[which(innewmean$stratanames == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "stratanames")]
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, domain_var = setdiff(colnames(innewmean_x), var))[,c(1:length(innewmean_x))])
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, ord_var = "SAL")[,c(1:length(innewmean_x))]) 
      } 
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8 )
      
    }else if(names(realstrata)[ncol(realstrata)]  == "strata"){ 
      for(i in 1:9){
        innewmean_x = innewmean[which(innewmean$strata == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "strata")]
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, domain_var = setdiff(colnames(innewmean_x), var))[,c(1:length(innewmean_x))])
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, ord_var = "SAL")[,c(1:length(innewmean_x))]) 
      } 
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9 )
    }
    
  }else if(method == "knn"){
    # imp = knnImputation(incomplete, k = 11)
    # incomplete[,var] = imp[,var]
    if (names(realstrata)[ncol(realstrata)]  == "stratumID"){ 
      for(i in 1:10){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean))]
        assign( paste0("imp" , i) , knnImputation(innewmean_x, k = 11))
      }
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10 )
    }
    else if (names(realstrata)[ncol(realstrata)]  == "stratanames"){ 
      for(i in 1:8){
        innewmean_x = innewmean[which(innewmean$stratanames == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "stratanames")]
        assign( paste0("imp" , i) , knnImputation(innewmean_x, k = 11))
      }
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8 )
    }
    else if (names(realstrata)[ncol(realstrata)]  == "strata"){ 
      for(i in 1:9){
        innewmean_x = innewmean[which(innewmean$strata == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "strata")]
        assign( paste0("imp" , i) , knnImputation(innewmean_x, k = 11))
      }
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9 )
    }
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
imputeMIbar = function(method, data, var, m){
  maxit = 10
  post = make.post(innewmean_2)
  post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
  
  if( method == "norm"){
    if (names(realstrata)[ncol(realstrata)]   == "stratumID"){ 
      for(i in 1:10){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean))]
        post = make.post(innewmean_x)
        post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
        norm5  = mice(data = innewmean_x, method = "norm" , m = m , maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
        imp1 = complete(norm5, "all")
        # take m imputed variables mean
        for(l in 1:length(imp1)){
          if(l == 1){
            v = imp1[[l]][,var]
          }else{
            v = v + imp1[[l]][,var]
          }
        }
        v = v / m
        imp2 = innewmean_x
        imp2[,var] = v
        assign( paste0("imp2" , i) , imp2 )
      }
      imp2 = rbind( imp21,imp22,imp23,imp24,imp25,imp26,imp27,imp28,imp29,imp210 )
      return(imp2)
    } else if (names(realstrata)[ncol(realstrata)]   == "stratanames"){ 
      for(i in 1:8){
        innewmean_x = innewmean[which(innewmean$stratanames == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "stratanames")]
        post = make.post(innewmean_x)
        post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
        norm5  = mice(data = innewmean_x, method = "norm" , m = m , maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
        imp1 = complete(norm5, "all")
        # take m imputed variables mean
        for(l in 1:length(imp1)){
          if(l == 1){
            v = imp1[[l]][,var]
          }else{
            v = v + imp1[[l]][,var]
          }
        }
        v = v / m
        imp2 = innewmean_x
        imp2[,var] = v
        assign( paste0("imp2" , i) , imp2 )
      }
      imp2 = rbind( imp21,imp22,imp23,imp24,imp25,imp26,imp27,imp28 )
      return(imp2)
    } else if (names(realstrata)[ncol(realstrata)]   == "strata"){ 
      for(i in 1:9){
        innewmean_x = innewmean[which(innewmean$strata == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "strata")]
        post = make.post(innewmean_x)
        post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
        norm5  = mice(data = innewmean_x, method = "norm" , m = m , maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
        imp1 = complete(norm5, "all")
        # take m imputed variables mean
        for(l in 1:length(imp1)){
          if(l == 1){
            v = imp1[[l]][,var]
          }else{
            v = v + imp1[[l]][,var]
          }
        }
        v = v / m
        imp2 = innewmean_x
        imp2[,var] = v
        assign( paste0("imp2" , i) , imp2 )
      }
      imp2 = rbind( imp21,imp22,imp23,imp24,imp25,imp26,imp27,imp28,imp29 )
      return(imp2)
    }
  # norm5 = mice(data = incomplete_2, method = method , m = m, maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
  # imp1 = complete(norm5, "all")
  }else if( method == "pmm"){
      if (names(realstrata)[ncol(realstrata)]  == "stratumID"){ 
      for(i in 1:10){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean))]
        pmm5  = mice(data = innewmean_x, method = "pmm" , m = m , maxit = maxit, visitSequence = "monotone", print = FALSE)
        imp1 = complete(pmm5, "all")
        # take m imputed variables mean
        for(l in 1:length(imp1)){
          if(l == 1){
            v = imp1[[l]][,var]
          }else{
            v = v + imp1[[l]][,var]
          }
        }
        v = v / m
        imp2 = innewmean_x
        imp2[,var] = v
        assign( paste0("imp2" , i) , imp2 )
      }
      imp2 = rbind( imp21,imp22,imp23,imp24,imp25,imp26,imp27,imp28,imp29,imp210 )
      return(imp2)
    }else if (names(realstrata)[ncol(realstrata)]  == "stratanames"){ 
      for(i in 1:8){
        innewmean_x = innewmean[which(innewmean$stratanames == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "stratanames")]
        pmm5  = mice(data = innewmean_x, method = "pmm" , m = m , maxit = maxit, visitSequence = "monotone", print = FALSE)
        imp1 = complete(pmm5, "all")
        # take m imputed variables mean
        for(l in 1:length(imp1)){
          if(l == 1){
            v = imp1[[l]][,var]
          }else{
            v = v + imp1[[l]][,var]
          }
        }
        v = v / m
        imp2 = innewmean_x
        imp2[,var] = v
        assign( paste0("imp2" , i) , imp2 )
      }
      imp2 = rbind( imp21,imp22,imp23,imp24,imp25,imp26,imp27,imp28 )
      return(imp2)
    }else if (names(realstrata)[ncol(realstrata)]  == "strata"){ 
      for(i in 1:9){
        innewmean_x = innewmean[which(innewmean$strata == paste0("group",i) ), ]
        innewmean_x = innewmean_x[,-which(names(innewmean_x)== "strata")]
        pmm5  = mice(data = innewmean_x, method = "pmm" , m = m , maxit = maxit, visitSequence = "monotone", print = FALSE)
        imp1 = complete(pmm5, "all")
        # take m imputed variables mean
        for(l in 1:length(imp1)){
          if(l == 1){
            v = imp1[[l]][,var]
          }else{
            v = v + imp1[[l]][,var]
          }
        }
        v = v / m
        imp2 = innewmean_x
        imp2[,var] = v
        assign( paste0("imp2" , i) , imp2 )
      }
      imp2 = rbind( imp21,imp22,imp23,imp24,imp25,imp26,imp27,imp28,imp29 )
      return(imp2)
    }
    # norm5 = mice(data = incomplete_2, method = method , m = m, maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
    # imp1 = complete(norm5, "all")
  }
  
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
x = newmean
for(o in 1:nrow(x)){


    x$COMP[[o]] = 2

}
### to combine COMP1, COMP2, COMP3, COMP4 for a new variable COMP ###
dummy.COMP = function(x){
  for(o in 1:nrow(x)){
    # x$COMP[[o]] = -1
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
