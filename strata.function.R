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

set = function(a,b){
  if(a == 1 | a == 2){
    IND = paste0(unique(complete$IND),"_",a,"_",b) 
    
    ### create the folder to store the results
    dir.create("result")
    dir.create(paste0("result/", IND))
    dir.create(paste0("result/", IND, "/", "y", year))
    # #stratify data
    # # 1 = REV", " 2 = ASSET"
    straty_1( a , CV = b )
  }else if(a == 3){
    IND = paste0(unique(complete$IND),"_",a,"_",b) 
    
    ### create the folder to store the results
    dir.create("result")
    dir.create(paste0("result/", IND))
    dir.create(paste0("result/", IND, "/", "y", year))
    # #stratify data
    # # 1 = REV", " 2 = ASSET"
    straty_2(a,CV = b)
  }else if(a == 4){
    IND = paste0(unique(complete$IND),"_",a,"_",b) 
    
    ### create the folder to store the results
    dir.create("result")
    dir.create(paste0("result/", IND))
    dir.create(paste0("result/", IND, "/", "y", year))
    # #stratify data
    # # 1 = REV", " 2 = ASSET"
    straty_3(a, CV = b)}
  IND <<- paste0(unique(complete$IND),"_",a,"_",b)
}



### stratify
#"REV", "ASSET", "EMP"
straty_1 <- function(i,CV){
  set.seed(123)
  IND = paste0(unique(complete$IND),"_",i,"_",CV) 
  # y 105 year
  y105_8620_strata <- strata.LH(x = y105_8620_notax[,imput_var[[i]]], CV = CV, Ls = 10, 
                                alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax <- cbind(y105_8620_notax , y105_8620_strata$stratumID )
  names(y105_8620_notax )[ncol(y105_8620_notax)] <- c("stratumID")
  
  y105_8620_notax = y105_8620_notax[order(y105_8620_notax$stratumID),]
  y105_8620_sample <- strata(y105_8620_notax ,
                             stratanames = c("stratumID") ,
                             size = y105_8620_strata$nh , method="srswor" )
  #抽樣
  realstrata <<- y105_8620_notax[y105_8620_sample$ID_unit,-2]
  #剩下沒被抽的
  nostrata <<- y105_8620_notax[-(y105_8620_sample$ID_unit),-2]
  # save(nostrata , file = paste0("result/", IND, "/nostrata.RData"))
  # save(realstrata , file = paste0("result/", IND, "/realstrata.RData"))
  
  y105_8620[which(y105_8620$SEQ %in% realstrata$SEQ),]$TAX <- 1
  #UNIT 去除 (全部都0)
  y105_8620 <<- y105_8620[,-2]
  # save(y105_8620 , file = "data/y105_8620.RData")
  
  infor = data.frame(N = y105_8620_strata$Nh ,
                     n = y105_8620_strata$nh , 
                     mean =  y105_8620_strata$meanh ,
                     bh = c(NA ,y105_8620_strata$bh))
  infor = rbind(infor, c( sum(infor$N) , sum(infor$n) , y105_8620_strata$mean , imput_var[[i]] ))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
  
}
# "REV", "ASSET", "EMP"
straty_2 <- function(i,CV){
  IND = paste0(unique(complete$IND),"_",i,"_",CV) 
  set.seed(123)
  # y 105 year
  y105_8620_notax_1 <- strata.LH(x = y105_8620_notax[,imput_var[3]], CV = CV, Ls = 2, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax_2 <- strata.LH(x = y105_8620_notax[,imput_var[1]], CV = CV, Ls = 2, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax_3 <- strata.LH(x = y105_8620_notax[,imput_var[2]], CV = CV, Ls = 2, 
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
  #                            size = y105_8620_strata$nh , method="srswor" )
  y105_8620_sample <- strata(y105_8620_notax ,
                             stratanames = c("stratanames") ,
                             size = quantile , method="srswor" )
  
  #抽樣
  realstrata <<- y105_8620_notax[ y105_8620_sample$ID_unit,-2]
  #剩下沒被抽的
  nostrata <<- y105_8620_notax[-( y105_8620_sample$ID_unit),-2]
  # save(nostrata , file = paste0("result/", IND, "/nostrata.RData"))
  # save(realstrata , file = paste0("result/", IND, "/realstrata.RData"))
  
  y105_8620[which(y105_8620$SEQ %in% realstrata$SEQ),]$TAX <- 1
  #UNIT 去除 (全部都0)
  y105_8620 <- y105_8620[,-2]
  # save(y105_8620 , file = "data/y105_8620.RData")
  
  infor = data.frame(N = table(y105_8620_notax$stratanames) ,
                     n = quantile , 
                     bh = c(NA ,imput_var[1] , y105_8620_notax_1$bh , 
                            imput_var[2] , y105_8620_notax_2$bh , 
                            imput_var[3] , y105_8620_notax_3$bh , NA))
  infor = rbind(infor, c( NA ,sum(table(y105_8620_notax$stratanames)) , sum(infor$n)  , "EMP REV ASSET" ))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
  
}

straty_3 <- function(i,CV){
  IND = paste0(unique(complete$IND),"_",i,"_",CV) 
  set.seed(123)
  # y 105 year
  y105_8620_notax_1 <- strata.LH(x = y105_8620_notax[,imput_var[1]], CV = CV, Ls = 3, 
                                 alloc = c(0.5, 0, 0.5),  algo = "Kozak")
  y105_8620_notax_2 <- strata.LH(x = y105_8620_notax[,imput_var[2]], CV = CV, Ls = 3, 
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
  #                            size = y105_8620_strata$nh , method="srswor" )
  y105_8620_sample <- strata(y105_8620_notax ,
                             stratanames = c("strata") ,
                             size = quantile , method="srswor" )
  #抽樣
  realstrata <<- y105_8620_notax[ y105_8620_sample$ID_unit,-2]
  #剩下沒被抽的
  nostrata <<- y105_8620_notax[-(y105_8620_sample$ID_unit),-2]
  # save(nostrata , file = paste0("result/", IND, "/nostrata.RData"))
  # save(realstrata , file = paste0("result/", IND, "/realstrata.RData"))
  
  y105_8620[which(y105_8620$SEQ %in% realstrata$SEQ),]$TAX <- 1
  #UNIT 去除 (全部都0)
  y105_8620 <<- y105_8620[,-2]
  # save(y105_8620 , file = "data/y105_8620.RData")
  
  infor = data.frame(N = table(y105_8620_notax$strata) ,
                     n = quantile , 
                     bh = c(NA , NA , 
                            imput_var[1] , y105_8620_notax_1$bh[1] , y105_8620_notax_1$bh[2],
                            imput_var[2] , y105_8620_notax_2$bh[1] , y105_8620_notax_2$bh[2], NA))
  infor = rbind(infor, c( NA ,sum(table(y105_8620_notax$strata)) , sum(infor$n)  , "REV ASSET" ))
  write.csv(infor, file = paste0("result/", IND, "/infor.csv"))
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
