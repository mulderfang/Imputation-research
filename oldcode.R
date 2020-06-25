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

straty_3 <- function(CV){
  #cut data
  load("data/y105.rda")
  
  y105_8620 = y105[which(y105$IND == 8620),]
  #去除原本就是TAX = 1的資料
  y105_8620 <- y105_8620[-which(y105_8620$TAX == 1),]
  y105_8620_notax <- y105_8620 %>% filter(TAX == 2) 
  
  complete = y105_8620
  year = 105
  imput_var <- c("EMP" , "REV", "ASSET" )
  
  ### take the basic dataset information
  IND = unique(complete$IND)
  
  ### create the folder to store the results
  dir.create("result")
  dir.create(paste0("result/", IND))
  dir.create(paste0("result/", IND, "/", "y", year))
  
  #----------------------------
  set.seed(123)
  # y 105 year
  y105_8620_notax_1 <- strata.LH(x = y105_8620_notax[,imput_var[2]], CV = CV, Ls = 3, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax_2 <- strata.LH(x = y105_8620_notax[,imput_var[3]], CV = CV, Ls = 3, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV <= y105_8620_notax_1$bh[1], "a" , y105_8620_notax$REV )
  
  # REV <= 1
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV <= y105_8620_notax_1$bh[1] & 
                                     y105_8620_notax$ASSET <= y105_8620_notax_2$bh[1]
                                   , "group1" , y105_8620_notax$strata)
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV <= y105_8620_notax_1$bh[1] & 
                                     y105_8620_notax$ASSET <= y105_8620_notax_2$bh[2] &
                                     y105_8620_notax$ASSET >  y105_8620_notax_2$bh[1]
                                   , "group2" , y105_8620_notax$strata)
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV <= y105_8620_notax_1$bh[1] & 
                                     y105_8620_notax$ASSET >= y105_8620_notax_2$bh[2]
                                   , "group3" , y105_8620_notax$strata)
  
  # 1 < REV < 2 
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV >  y105_8620_notax_1$bh[1] & 
                                     y105_8620_notax$REV <= y105_8620_notax_1$bh[2] & 
                                     y105_8620_notax$ASSET <= y105_8620_notax_2$bh[1] 
                                   , "group4" , y105_8620_notax$strata)
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV >  y105_8620_notax_1$bh[1] & 
                                     y105_8620_notax$REV <= y105_8620_notax_1$bh[2] & 
                                     y105_8620_notax$ASSET <= y105_8620_notax_2$bh[2] &
                                     y105_8620_notax$ASSET >  y105_8620_notax_2$bh[1]
                                   , "group5" , y105_8620_notax$strata)
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV >  y105_8620_notax_1$bh[1] & 
                                     y105_8620_notax$REV <= y105_8620_notax_1$bh[2] & 
                                     y105_8620_notax$ASSET >  y105_8620_notax_2$bh[2]
                                   , "group6" , y105_8620_notax$strata)
  
  # REV > 2 
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV >  y105_8620_notax_1$bh[2] &
                                     y105_8620_notax$ASSET <= y105_8620_notax_2$bh[1] , 
                                   "group7" , y105_8620_notax$strata)
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV >  y105_8620_notax_1$bh[2] &
                                     y105_8620_notax$ASSET <= y105_8620_notax_2$bh[2] &
                                     y105_8620_notax$ASSET >  y105_8620_notax_2$bh[1] , 
                                   "group8" , y105_8620_notax$strata)
  y105_8620_notax$strata <- ifelse(y105_8620_notax$REV >  y105_8620_notax_1$bh[2] &
                                     y105_8620_notax$ASSET >  y105_8620_notax_2$bh[2] , 
                                   "group9" , y105_8620_notax$strata)
  
  table(y105_8620_notax$strata)
  #Optimum Strata Boundaries (OBS)
  
  #quantile
  base <- 
    round(mean(y105_8620_notax_1$n , y105_8620_notax_2$n))/
    (y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]+
       y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]+
       y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[3]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[3]+
       y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[1]+
       y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[2]+
       y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[3])
  
  quantile <- round(c(y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*base,
                      y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*base,
                      y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[3]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[3]*base,
                      y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[1]*base,
                      y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[2]*base,
                      y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[3]*base))
  
  
  for(i in 1:9){
    if( table(y105_8620_notax$strata)[i] < quantile[i] ){
      quantile[i] <- table(y105_8620_notax$strata)[i]
    }}
  y105_8620_notax = y105_8620_notax[order(y105_8620_notax$strata),]
  # y105_8620_strata <- strata(y105_8620_notax ,
  #                            strata = c("stratumID") ,
  #                            size = y105_8620_notax_5$nh , method="srswor" )
  y105_8620_strata <- strata(y105_8620_notax ,
                             stratanames = c("strata") ,
                             size = quantile , method="srswor" )
  
  y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
  realmean <- y105_8620_new[,-2]
  ymean <- y105_8620_notax[-(y105_8620_strata$ID_unit),-2]
  
  y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
  
  y105_8620 <- y105_8620[,-2]
  save(y105_8620 , file = "data/y105_8620.RData")
  infor = data.frame(N = table(y105_8620_notax$strata) ,
                     n = quantile , 
                     bh = c(NA , NA , 
                            imput_var[2] , y105_8620_notax_1$bh[1] , y105_8620_notax_1$bh[2],
                            imput_var[3] , y105_8620_notax_1$bh[1] , y105_8620_notax_1$bh[2], NA))
  infor = rbind(infor, c( NA ,sum(table(y105_8620_notax$strata)) , sum(infor$n)  , "REV ASSET" ))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
  save(ymean , file = paste0("result/", IND, "/ymean.RData"))
  save(realmean , file = paste0("result/", IND, "/realmean.RData"))
}

### stratify
#"COST", "REV", "ASSET", "EMP"
straty_2 <- function(CV){
  #cut data
  load("data/y105.rda")
  
  y105_8620 = y105[which(y105$IND == 8620),]
  #去除原本就是TAX = 1的資料
  y105_8620 <- y105_8620[-which(y105_8620$TAX == 1),]
  y105_8620_notax <- y105_8620 %>% filter(TAX == 2) 
  
  complete = y105_8620
  year = 105
  imput_var <- c("EMP" , "REV", "ASSET" )
  
  ### take the basic dataset information
  IND = unique(complete$IND)
  
  ### create the folder to store the results
  dir.create("result")
  dir.create(paste0("result/", IND))
  dir.create(paste0("result/", IND, "/", "y", year))
  
  #----------------------------
  set.seed(123)
  # y 105 year
  y105_8620_notax_1 <- strata.LH(x = y105_8620_notax[,imput_var[1]], CV = CV, Ls = 2, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax_2 <- strata.LH(x = y105_8620_notax[,imput_var[2]], CV = CV, Ls = 2, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax_3 <- strata.LH(x = y105_8620_notax[,imput_var[3]], CV = CV, Ls = 2, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP <=y105_8620_notax_1$bh[1], "a" , y105_8620_notax$EMP)
  
  # EMP <= 1 REV <= 1 ASSET <= 1
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   <= y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET <= y105_8620_notax_3$bh[1] ,
                                        "group1" , y105_8620_notax$stratanames)
  # EMP <= 1 REV <= 1 ASSET > 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   <= y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET >  y105_8620_notax_3$bh[1] ,
                                        "group2" , y105_8620_notax$stratanames)
  # EMP <= 1 REV > 1 ASSET <= 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   >  y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET <= y105_8620_notax_3$bh[1] ,
                                        "group3" , y105_8620_notax$stratanames)
  # EMP <= 1 REV > 1 ASSET > 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   >  y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET >  y105_8620_notax_3$bh[1] ,
                                        "group4" , y105_8620_notax$stratanames)
  # EMP > 1 REV <= 1 ASSET <= 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   <= y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET <= y105_8620_notax_3$bh[1] ,
                                        "group5" , y105_8620_notax$stratanames)
  # EMP > 1 REV <= 1 ASSET > 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   <= y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET >  y105_8620_notax_3$bh[1] ,
                                        "group6" , y105_8620_notax$stratanames)
  # EMP > 1 REV > 1 ASSET <= 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   >  y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET <= y105_8620_notax_3$bh[1] ,
                                        "group7" , y105_8620_notax$stratanames)
  # EMP > 1 REV > 1 ASSET > 1 
  y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$bh[1] & 
                                          y105_8620_notax$REV   >  y105_8620_notax_2$bh[1] &
                                          y105_8620_notax$ASSET >  y105_8620_notax_3$bh[1] ,
                                        "group8" , y105_8620_notax$stratanames)
  
  
  table(y105_8620_notax$stratanames)
  #Optimum Strata Boundaries (OBS)
  
  #quantile
  base <- 
    round(mean(y105_8620_notax_1$n , y105_8620_notax_2$n , y105_8620_notax_3$n))/
    (y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]+
       y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]+
       y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]+
       y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]+
       y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]  )
  
  quantile <- round(c(y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]*base,
                      y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]*base,
                      y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]*base,
                      y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]*base,
                      y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]*base))
  
  
  for(i in 1:8){
    if( table(y105_8620_notax$stratanames)[i] < quantile[i] ){
      quantile[i] <- table(y105_8620_notax$stratanames)[i]
    }}
  y105_8620_notax = y105_8620_notax[order(y105_8620_notax$stratanames),]
  # y105_8620_strata <- strata(y105_8620_notax ,
  #                            stratanames = c("stratumID") ,
  #                            size = y105_8620_notax_5$nh , method="srswor" )
  y105_8620_strata <- strata(y105_8620_notax ,
                             stratanames = c("stratanames") ,
                             size = quantile , method="srswor" )
  
  y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
  realmean <- y105_8620_new[,-2]
  ymean <- y105_8620_notax[-(y105_8620_strata$ID_unit),-2]
  
  y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
  
  y105_8620 <- y105_8620[,-2]
  save(y105_8620 , file = "data/y105_8620.RData")
  infor = data.frame(N = table(y105_8620_notax$stratanames) ,
                     n = quantile , 
                     bh = c(NA ,imput_var[1] , y105_8620_notax_1$bh , 
                            imput_var[2] , y105_8620_notax_2$bh , 
                            imput_var[3] , y105_8620_notax_3$bh , NA))
  infor = rbind(infor, c( NA ,sum(table(y105_8620_notax$stratanames)) , sum(infor$n)  , "EMP REV ASSET" ))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
  save(ymean , file = paste0("result/", IND, "/ymean.RData"))
  save(realmean , file = paste0("result/", IND, "/realmean.RData"))
}

### stratify
#"COST", "REV", "ASSET", "EMP"
straty <- function(i , CV , Ls){
  #cut data
  load("data/y105.rda")
  
  y105_8620 = y105[which(y105$IND == 8620),]
  #去除原本就是TAX = 1的資料
  y105_8620 <- y105_8620[-which(y105_8620$TAX == 1),]
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
  realmean <- y105_8620_new[,-2]
  ymean <- y105_8620_notax[-(y105_8620_strata$ID_unit),-2]
  
  y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
  
  y105_8620 <- y105_8620[,-2]
  save(y105_8620 , file = "data/y105_8620.RData")
  infor = data.frame(N = y105_8620_notax_5$Nh ,
                     n = y105_8620_notax_5$nh , 
                     mean =  y105_8620_notax_5$meanh ,
                     bh = c(NA ,y105_8620_notax_5$bh))
  infor = rbind(infor, c( sum(infor$N) , sum(infor$n) , y105_8620_notax_5$mean , imput_var[[i]] ))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
  save(ymean , file = paste0("result/", IND, "/ymean.RData"))
  save(realmean , file = paste0("result/", IND, "/realmean.RData"))
}

### Single imputation
imputeSingle = function(incomplete, method, var){
  load("result/8620/ymean.RData")
  load("result/8620/realmean.RData")
  ymean[, c("EMP", "SAL", var)] = log1p(ymean[, c("EMP", "SAL", var)])
  realmean[, c("EMP", "SAL", var)] = log1p(realmean[, c("EMP", "SAL", var)])
  realmean$TAX <-  1
  newmean = rbind(ymean , realmean)
  newmean = dummy.COMP(newmean)
  miss = which(newmean$TAX == 2)
  newmean = newmean[, setdiff(names(newmean), c("RES", "TRICOST", "TRIREV", "OUTSALE", "ECOYN", "F020300", "TRICOST", 
                                                "SEQ", "IND", "EMP_L", "SAL_L", "COSTOP", "REVOP", "COMP1", "COMP2", "COMP3", "COMP4", "SH4", "TAX", "IND_M", "IND_S"))]
  
  newmean[, c("EMP", "SAL", var)] = log1p(newmean[, c("EMP", "SAL", var)])
  innewmean = newmean
  innewmean[miss, var] = NA
  if(method == "mean"){
    imp = incomplete
    missing = is.na(imp$COST)
    if (names(realmean)[ncol(realmean)]  == "stratumID"){
      for(i in 1:Ls){
        for(v in 1:length(var)){
          # imp[missing, var[[v]]] = mean(imp[, var[[v]]], na.rm = TRUE)
          imputation_mean <- tapply(realmean[,var[[v]]], realmean$stratumID , mean )
          imp[which(y105_8620$SEQ %in% ymean[which(ymean$stratumID == i),"SEQ"]) , var[[v]]] = imputation_mean[i]
        }
      } 
    }else if (names(realmean)[ncol(realmean)]  == "stratanames"){  
      for(i in 1:8){
        for(v in 1:length(var)){
          # imp[missing, var[[v]]] = mean(imp[, var[[v]]], na.rm = TRUE)
          imputation_mean <- tapply(realmean[,var[[v]]], realmean$stratanames , mean )
          imp[which(y105_8620$SEQ %in% ymean[which(ymean$stratanames == paste0("group" , i)),"SEQ"]) , var[[v]]] = imputation_mean[i]
        }
      } 
    }else if (names(realmean)[ncol(realmean)]  == "strata"){  
      for(i in 1:9){
        for(v in 1:length(var)){
          # imp[missing, var[[v]]] = mean(imp[, var[[v]]], na.rm = TRUE)
          imputation_mean <- tapply(realmean[,var[[v]]], realmean$strata , mean )
          imp[which(y105_8620$SEQ %in% ymean[which(ymean$strata == paste0("group" , i)),"SEQ"]) , var[[v]]] = imputation_mean[i]
        }
      } 
    }   
    
  }else if(method == "median"){
    imp = incomplete
    missing = is.na(imp$COST)
    if (names(realmean)[ncol(realmean)]  == "stratumID"){ 
      for(i in 1:Ls){
        for(v in 1:length(var)){
          #imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
          imputation_median <- tapply(realmean[,var[[v]]], realmean$stratumID , median )
          imp[which(y105_8620$SEQ %in% ymean[which(ymean$stratumID == i),"SEQ"]) , var[[v]]] = imputation_median[i]
        }
      } 
    }else if(names(realmean)[ncol(realmean)]  == "stratanames"){  
      for(i in 1:8){
        for(v in 1:length(var)){
          #imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
          imputation_median <- tapply(realmean[,var[[v]]], realmean$stratanames , median )
          imp[which(y105_8620$SEQ %in% ymean[which(ymean$stratanames == paste0("group" , i)),"SEQ"]) , var[[v]]] = imputation_median[i]
        }
      } 
    }else if(names(realmean)[ncol(realmean)]  == "strata"){  
      for(i in 1:9){
        for(v in 1:length(var)){
          #imp[missing, var[[v]]] = median(imp[, var[[v]]], na.rm = TRUE)
          imputation_median <- tapply(realmean[,var[[v]]], realmean$strata , median )
          imp[which(y105_8620$SEQ %in% ymean[which(ymean$strata == paste0("group" , i)),"SEQ"]) , var[[v]]] = imputation_median[i]
        }
      } 
    }
  }else if(method == "hotdeck"){
    # imp = hotdeck(incomplete, variable = var, domain_var = setdiff(colnames(incomplete), var))[,c(1:length(incomplete))]
    # imp = hotdeck(incomplete, variable = var, ord_var = "SAL")[,c(1:length(incomplete))]
    if (names(newmean)[ncol(newmean)-1]  == "stratumID"){ 
      for(i in 1:Ls){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, domain_var = setdiff(colnames(innewmean_x), var))[,c(1:length(innewmean_x))])
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, ord_var = "SAL")[,c(1:length(innewmean_x))]) 
      } 
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10 )
      
    }else if(names(newmean)[ncol(newmean)-1]  == "stratanames"){ 
      for(i in 1:8){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, domain_var = setdiff(colnames(innewmean_x), var))[,c(1:length(innewmean_x))])
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, ord_var = "SAL")[,c(1:length(innewmean_x))]) 
      } 
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8 )
      
    }else if(names(newmean)[ncol(newmean)-1]  == "strata"){ 
      for(i in 1:9){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, domain_var = setdiff(colnames(innewmean_x), var))[,c(1:length(innewmean_x))])
        assign( paste0("imp" , i) , hotdeck(innewmean_x, variable = var, ord_var = "SAL")[,c(1:length(innewmean_x))]) 
      } 
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9 )
    }
    
  }else if(method == "knn"){
    # imp = knnImputation(incomplete, k = 11)
    # incomplete[,var] = imp[,var]
    if (names(newmean)[ncol(newmean)-1]  == "stratumID"){ 
      for(i in 1:Ls){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        assign( paste0("imp" , i) , knnImputation(innewmean_x, k = 11))
      }
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10 )
    }
    else if (names(newmean)[ncol(newmean)-1]  == "stratanames"){ 
      for(i in 1:8){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        assign( paste0("imp" , i) , knnImputation(innewmean_x, k = 11))
      }
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10 )
    }
    else if (names(newmean)[ncol(newmean)-1]  == "strata"){ 
      for(i in 1:9){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        assign( paste0("imp" , i) , knnImputation(innewmean_x, k = 11))
      }
      imp = rbind( imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10 )
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
  load("result/8620/ymean.RData")
  load("result/8620/realmean.RData")
  ymean[, c("EMP", "SAL", var)] = log1p(ymean[, c("EMP", "SAL", var)])
  realmean[, c("EMP", "SAL", var)] = log1p(realmean[, c("EMP", "SAL", var)])
  realmean$TAX <-  1
  newmean = rbind(ymean , realmean)
  newmean = dummy.COMP(newmean)
  miss = which(newmean$TAX == 2)
  newmean = newmean[, setdiff(names(newmean), c("RES", "TRICOST", "TRIREV", "OUTSALE", "ECOYN", "F020300", "TRICOST", 
                                                "SEQ", "IND", "EMP_L", "SAL_L", "COSTOP", "REVOP", "COMP1", "COMP2", "COMP3", "COMP4", "SH4", "TAX", "IND_M", "IND_S"))]
  
  newmean[, c("EMP", "SAL", var)] = log1p(newmean[, c("EMP", "SAL", var)])
  innewmean = newmean
  innewmean[miss, var] = NA
  innewmean_2 = innewmean[,c(var_all, var)]
  if( method == "norm"){
    if (names(newmean)[ncol(newmean)-1]  == "stratumID"){ 
      for(i in 1:Ls){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
        maxit = 10
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
    }
    # norm5 = mice(data = incomplete_2, method = method , m = m, maxit = maxit, post = post, visitSequence = "monotone", print = FALSE)
    # imp1 = complete(norm5, "all")
  }else if( method == "pmm"){
    if (names(newmean)[ncol(newmean)-1]  == "stratumID"){ 
      for(i in 1:Ls){
        innewmean_x = innewmean[which(innewmean$stratumID == i ), ]
        innewmean_x = innewmean_x[,-(ncol(newmean)-1)]
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
