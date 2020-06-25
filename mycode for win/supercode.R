
options( warn = -1 )
library(openxlsx)

# impute
source("func.R", encoding = "UTF-8")

#stratify data
#"COST", "REV", "ASSET", "EMP"
straty(3,0.001,8)

### input dataset
load("/Users/mac/Desktop/desktop/宗記/stratify/data/y105_8620.RData")

### set seed
set.seed(999)

for(j in 1:2){
    ### basic information for the different year

      complete = y105_8620
      year = 105
      var = c("COST", "REV", "ASSET", "INVESTAS")
    
    ### take the basic dataset information
    IND = unique(complete$IND)
    
    ### create the folder to store the results
    dir.create("result")
    dir.create(paste0("result/", IND))
    dir.create(paste0("result/", IND, "/", "y", year))
    
    ### combine COMP1, COMP2, COMP3, COMP4 to a variable
    complete = dummy.COMP(complete)
    
    ### make dataset missing by variable TAX
    missing = which(complete$TAX == 2)
    
    ### manual settings
    ### variable detection to delete useless variables
    # "OUTSALE", "TRICOST", "TRIREV"
    complete = complete[, setdiff(names(complete), c("RES", "TRICOST", "TRIREV", "OUTSALE", "ECOYN", "F020300", "TRICOST", 
                                                     "SEQ", "IND", "EMP_L", "SAL_L", "COSTOP", "REVOP", "COMP1", "COMP2", "COMP3", "COMP4", "SH4", "TAX", "IND_M", "IND_S"))]
    
    complete[, c("EMP", "SAL", var)] = log1p(complete[, c("EMP", "SAL", var)])
    incomplete = complete
    incomplete[missing, var] = NA
    
    if(j == 1){
      sel = "varA"
      
      var_all = setdiff(names(complete), var)
      
      complete_2 = complete[,c(var_all, var)]
      incomplete_2 = incomplete[,c(var_all, var)]
    }else{
      sel = "varS"
      
      var_pvalue = c()
      sig = c()
      for(k in 1:length(var)){
        complete_reg = complete[,c(setdiff(names(complete), var), var[[k]])]
        
        if(k == 1){
          full = lm(COST ~ ., data = complete_reg)
          model = step(full, scope = list(upper = full), direction = "both")
          
        }else if(k == 2){
          full = lm(REV ~ ., data = complete_reg)
          model = step(full, scope = list(upper = full), direction = "both")
          
        }else if(k == 3){
          full = lm(ASSET ~ ., data = complete_reg)
          model = step(full, scope = list(upper = full), direction = "both")
          
        }else if(k == 4){
          full = lm(INVESTAS ~ ., data = complete_reg)
          model = step(full, scope = list(upper = full), direction = "both")
          
        }
        ###
        formula = summary(model)$call$formula
        sig = c(sig, strsplit(as.character(formula)[[3]], " + ", fixed = TRUE)[[1]])
        ### take the variables names
        var_pvalue = rbind(var_pvalue, c(as.character(formula)[[3]], summary(model)$r.squared, summary(model)$adj.r.squared))
      }
      rownames(var_pvalue) = var
      write.csv(var_pvalue, file = paste0("result/", IND, "/sig.csv"))
      
      ### select the variables
      var_sig = unique(sig)
      var_sig = intersect(colnames(complete), var_sig)
      complete_2 = complete[, c(var_sig, var)]
      incomplete_2 = incomplete[, c(var_sig, var)]
      
    }
    
    ### imputation by every method
    Method = list()
    ### single imputation
    Method$Real = complete_2
    Method$Complete = na.omit(incomplete_2)
    Method$mean = imputeSingle(incomplete = incomplete_2, var = var, method = "mean")
    Method$median = imputeSingle(incomplete = incomplete_2, var = var, method = "median")
    Method$hotdeck = imputeSingle(incomplete = incomplete_2, var = var, method = "hotdeck")
    Method$knn = imputeSingle(incomplete = incomplete_2, var = var, method = "knn")
    
    ### multiple imputation
    maxit = 10
    
    post = make.post(incomplete_2)
    post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
    
    norm5 = mice(data = incomplete_2, method = "norm", m = 5, maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
    norm10 = mice(data = incomplete_2, method = "norm", m = 10, maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
    
    pmm5 = mice(data = incomplete_2, method = "pmm", m = 5, maxit = maxit, visitSequence = "monotone", print = FALSE)
    pmm10 = mice(data = incomplete_2, method = "pmm", m = 10, maxit = maxit, visitSequence = "monotone", print = FALSE)
    
    ### single imputation by take mean from multiple imputation datasets
    Method$norm5_bar = imputeMIbar(imp = norm5, data = incomplete_2, var = var, m = 5)
    Method$norm10_bar = imputeMIbar(imp = norm10, data = incomplete_2, var = var, m = 10)
    Method$pmm5_bar = imputeMIbar(imp = pmm5, data = incomplete_2, var = var, m = 5)
    Method$pmm10_bar = imputeMIbar(imp = pmm10, data = incomplete_2, var = var, m = 10)
    
    ### multiple imputation results
    Method$norm5 = norm5
    Method$norm10 = norm10
    Method$pmm5 = pmm5
    Method$pmm10 = pmm10
    
    ### save impute datasets
    save(Method, file = paste0("result/", IND, "/y", year, "/", sel, ".rda"))
  
}


# mse
library(MLmetrics)
library(openxlsx)
options(scipen = 999)

load("/Users/mac/Desktop/desktop/宗記/stratify/data/y105_8620.RData")

wb_mse = createWorkbook()

  ### basic information for the different year

    year = 105
    var = c("COST", "REV", "ASSET", "INVESTAS")
    complete = y105_8620

  ### take the basic dataset information
  IND = 8620
  missing = which(complete$TAX == 2)
  
  ### create the folder to store the results
  dir.create("result")
  dir.create(paste0("result/", IND))
  ### j == 1 : using all variables to impute, j == 2 using significant variables to impute
  for(j in 1:2){
    if(j == 1){
      sel = "varA"
      
    }else{
      sel = "varS"
      
    }
    ### input result
    load(paste0("result/", IND, "/y", year, "/", sel, ".rda"))

    ### add worksheet for mse
    addWorksheet(wb_mse, sheetName = paste0("y", year, "_", sel))

    ### MSE
    mse = c()
    for(m in 3:10){
      mse_i = c()
      for(v in 1:length(var)){
        test = MSE(y_pred = Method[[m]][[var[[v]]]], y_true = Method$Real[[var[[v]]]])
        mse_i = cbind(mse_i, format(round(test, digits = 4), nsmall = 4))
      }
      mse = rbind(mse, mse_i)
    }
    rownames(mse) = names(Method)[3:10]
    colnames(mse) = var
    
    writeData(wb_mse, sheet = paste0("y", year, "_", sel), x = mse, startCol = 2, startRow = 2, rowNames = TRUE)
    

  }

saveWorkbook(wb_mse, file = paste0("result/", IND, "/mse.xlsx"), overwrite = TRUE)

rm(list = ls())

### summary for imputed data

load("/Users/mac/Desktop/desktop/宗記/stratify/data/y105_8620.RData")
library(openxlsx)
source("func.R", encoding = "UTF-8")
### basic information for the different year

year = 105
var = c("COST", "REV", "ASSET", "INVESTAS")
complete = y105_8620

### take the basic dataset information
IND = 8620

missing = which(complete$TAX == 2)

### create the folder to store the results
dir.create("result")
dir.create(paste0("result/", IND))
dir.create(paste0("result/", IND, "/", "y", year))


### j == 1 : using all variables to impute, j == 2 using significant variables to impute
for(j in 1:2){
  if(j == 1){
    sel = "varA"
    
  }else{
    sel = "varS"
    
  }
  ### input result
  load(paste0("result/", IND, "/y", year, "/", sel, ".rda"))
  dir.create(paste0("result/", IND, "/", "y", year, "/", sel))
  ### take log and return origin scale to calculate summary statistic and ks-test
  for(l in 1:2){
    
    wb_summary = createWorkbook()
    ### summary statistic for missing value by each method
    for(v in 1:length(var)){
      addWorksheet(wb_summary, sheetName = var[[v]])
      
      for(i in c(1, 3:10)){
        kst = ks.test(x = type(data = Method[[i]][missing, var[[v]]], t = l), y = type(data = Method$Real[missing, var[[v]]], t = l))
        
        sum_i = c(sapply(type(data = Method[[i]][missing, var], t = l), summary)[,v], 
                  sum(type(data = Method[[i]][missing, var[[v]]], t = l)),
                  round(cbind(kst$statistic, kst$p.value), digits = 4))
        
        if(i == 1){
          sum = sum_i
        }else{
          sum = rbind(sum, sum_i)
        }
        
      }
      rownames(sum) = names(Method)[c(1, 3:10)]
      writeData(wb_summary, sheet = var[[v]], x = sum, startCol = 2, startRow = 2, rowNames = TRUE)
    }
    saveWorkbook(wb_summary, file = paste0("result/", IND, "/y", year, "/", sel, "/", "type", l, ".xlsx"), overwrite = TRUE)
  }
  
  
  
  
}

rm(list = ls())


