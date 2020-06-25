
  for(y in 1:4){
# 選擇4個CV
q = c(0.01,0.005,0.0025,0.001)
library(openxlsx)
options( warn = -1 )
# stratify
source("strata.function.R", encoding = "UTF-8")

#cut data
load("data/y105.rda")
y105_8620 = y105[which(y105$IND == 8620),]
#去除原本就是TAX = 1的資料
y105_8620 <- y105_8620[-which(y105_8620$TAX == 1),]
# TAX ==2 
y105_8620_notax <- y105_8620 %>% filter(TAX == 2)

complete = y105_8620
year = 105
imput_var <- c("REV", "ASSET", "EMP")

#1:REV 單變量插補
#2:ASSET 單變量插補
#3:REV*ASSET*EMP 三變量插補
#4:REV*ASSET 雙變量插補
set(4,q[y])
#------------
### input dataset
# load("/Users/mac/Desktop/desktop/宗記/stratify/data/y105_8620.RData")
# load("result/8620/nostrata.RData")
# load("result/8620/realstrata.RData")
var = c("COST", "REV", "ASSET", "INVESTAS")
nostrata[, c("EMP", "SAL", var)] = log1p(nostrata[, c("EMP", "SAL", var)])
realstrata[, c("EMP", "SAL", var)] = log1p(realstrata[, c("EMP", "SAL", var)])
realstrata$TAX <-  1
newmean = rbind(nostrata , realstrata)
newmean = dummy.COMP(newmean)
miss = which(newmean$TAX == 2)
newmean = newmean[, setdiff(names(newmean), c("RES", "TRICOST", "TRIREV", "OUTSALE", "ECOYN", "F020300", "TRICOST", 
                                              "SEQ", "IND", "EMP_L", "SAL_L", "COSTOP", "REVOP", "COMP1", "COMP2", "COMP3", "COMP4", "SH4", "TAX", "IND_M", "IND_S"))]
innewmean = newmean
innewmean[miss, var] = NA

sel = "varA"

var_all = setdiff(names(newmean), var)

newmean_2 = newmean[,c(var_all, var)]
innewmean_2 = innewmean[,c(var_all, var)]

source("func.R", encoding = "UTF-8")
    ### imputation by every method
    Method = list()
    ### single imputation
    Method$Real = newmean_2
    Method$Complete = na.omit(innewmean_2)
    Method$mean = imputeSingle(incomplete = innewmean_2, var = var, method = "mean")
    Method$median = imputeSingle(incomplete = innewmean_2, var = var, method = "median")
    Method$hotdeck = imputeSingle(incomplete = innewmean_2, var = var, method = "hotdeck")
    Method$knn = imputeSingle(incomplete = innewmean_2, var = var, method = "knn")
    
    ### multiple imputation
    
    # maxit = 10
    # post = make.post(innewmean_2)
    # post[var] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 999999999999))"
    
    ### single imputation by take mean from multiple imputation datasets
    Method$norm5_bar = imputeMIbar(method = "norm", data = innewmean_2, var = var, m = 5)
    Method$norm10_bar = imputeMIbar(method = "norm", data = innewmean_2, var = var, m = 10)
    Method$pmm5_bar = imputeMIbar(method = "pmm", data = innewmean_2, var = var, m = 5)
    Method$pmm10_bar = imputeMIbar(method = "pmm", data = innewmean_2, var = var, m = 10)
    
    # ### multiple imputation results
    # Method$norm5 = norm5
    # Method$norm10 = norm10
    # Method$pmm5 = pmm5
    # Method$pmm10 = pmm10
    # 
    ### save impute datasets
    save(Method, file = paste0("result/", IND, "/y", year, "/", sel, ".rda"))
  



# mse
library(MLmetrics)
library(openxlsx)
options(scipen = 999)

wb_mse = createWorkbook()

  ### create the folder to store the results
  dir.create("result")
  dir.create(paste0("result/", IND))
  ### j == 1 : using all variables to impute, j == 2 using significant variables to impute
      sel = "varA"
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


saveWorkbook(wb_mse, file = paste0("result/", IND, "/mse.xlsx"), overwrite = TRUE)

# rm(list = ls())

### summary for imputed data
### basic information for the different year

  ### input result
  dir.create(paste0("result/", IND, "/", "y", year, "/", sel))
  ### take log and return origin scale to calculate summary statistic and ks-test
  for(l in 1:2){
    
    wb_summary = createWorkbook()
    ### summary statistic for missing value by each method
    for(v in 1:length(var)){
      addWorksheet(wb_summary, sheetName = var[[v]])
      
      for(i in c(1, 3:10)){
        kst = ks.test(x = type(data = Method[[i]][miss, var[[v]]], t = l), y = type(data = Method$Real[miss, var[[v]]], t = l))
        sum_i = c(sapply(type(Method[[i]][miss, var], t = l), summary)[,v], 
                  sum(type(Method[[i]][miss, var[[v]]], t = l)),
                  round(100*abs(sum(type(Method[[i]][miss, var[[v]]], t = l)) - sum(type(Method[[1]][miss, var[[v]]], t = l)))/sum(type(Method[[1]][miss, var[[v]]], t = l)),2),
                  round(cbind(kst$statistic, kst$p.value), digits = 4),
                  round(100 * sum((Method[[i]][,"ASSET"]- Method[[i]][, "INVESTAS"]) > 0)/nrow(Method[[1]]),2 ),
                  nrow(Method[[1]]) - sum((Method[[i]][,"ASSET"]- Method[[i]][, "INVESTAS"]) > 0),
                  round(100 * sum(abs(Method[[i]][,"REV"]- Method[[i]][,"COST"])/Method[[i]][,"REV"] <= 0.3)/nrow(Method[[1]]),2 ),
                  nrow(Method[[1]]) - sum(abs(Method[[i]][,"REV"]- Method[[i]][,"COST"])/Method[[i]][,"REV"] <= 0.3))
       names(sum_i)[7:14] = c("Total" , "Delta" , "ks-value" , "p_value" ,"ASSET-INVESTAS","False1",'REV-COST',"False2" ) 
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
  rm(list = ls())
 }
# 


