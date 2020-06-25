install.packages("stratifyR")
library(stratifyR)
library(tidyverse)
library(sampling)
library(stratification)

#cut data
load("data/y105.rda")

y105_8620 = y105[which(y105$IND == 8620),]

y105_8620_tax <- y105_8620 %>% filter(TAX == 1) 
y105_8620_notax <- y105_8620 %>% filter(TAX == 2) 

#----------------------------
# y 105 year
y105_8620_notax_1 <- strata.data(y105_8620_notax$EMP, h = 2 ,   n=20000 )
y105_8620_notax_2 <- strata.data(y105_8620_notax$REV, h = 2 ,   n=20000 )
y105_8620_notax_3 <- strata.data(y105_8620_notax$ASSET, h = 2 , n=20000 )

y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP <=y105_8620_notax_1$OSB[1], "a" , y105_8620_notax$EMP)

# EMP <= 1 REV <= 1 ASSET <= 1
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   <= y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET <= y105_8620_notax_3$OSB[1] ,
                                      "group1" , y105_8620_notax$stratanames)
# EMP <= 1 REV <= 1 ASSET > 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   <= y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET >  y105_8620_notax_3$OSB[1] ,
                                      "group2" , y105_8620_notax$stratanames)
# EMP <= 1 REV > 1 ASSET <= 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   >  y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET <= y105_8620_notax_3$OSB[1] ,
                                      "group3" , y105_8620_notax$stratanames)
# EMP <= 1 REV > 1 ASSET > 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   <= y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   >  y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET >  y105_8620_notax_3$OSB[1] ,
                                      "group4" , y105_8620_notax$stratanames)
# EMP > 1 REV <= 1 ASSET <= 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   <= y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET <= y105_8620_notax_3$OSB[1] ,
                                      "group5" , y105_8620_notax$stratanames)
# EMP > 1 REV <= 1 ASSET > 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   <= y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET >  y105_8620_notax_3$OSB[1] ,
                                      "group6" , y105_8620_notax$stratanames)
# EMP > 1 REV > 1 ASSET <= 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   >  y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET <= y105_8620_notax_3$OSB[1] ,
                                      "group7" , y105_8620_notax$stratanames)
# EMP > 1 REV > 1 ASSET > 1 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP   >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV   >  y105_8620_notax_2$OSB[1] &
                                      y105_8620_notax$ASSET >  y105_8620_notax_3$OSB[1] ,
                                      "group8" , y105_8620_notax$stratanames)


table(y105_8620_notax$stratanames)
#Optimum Strata Boundaries (OBS)

#quantile
base <- 
  20000/(  y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]+
         y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]+
         y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]+
         y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]+
         y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]+
         y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]+
         y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]+
         y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]  )
         
quantile <- c(  y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]*base,
                y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]*base,
                y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]*base,
                y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]*base,
                y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[1]*base,
                y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*y105_8620_notax_3$nh[2]*base,
                y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[1]*base,
                y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*y105_8620_notax_3$nh[2]*base  )
round(quantile)
y105_8620_notax = y105_8620_notax[order(y105_8620_notax$stratanames),]

y105_8620_strata <- strata(y105_8620_notax ,
                           stratanames = c("stratanames") ,
                           size = round(quantile) , method="srswor" )

y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
y105_8620_new <- y105_8620_new[,-ncol(y105_8620_new)] 

y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
y105_8620 <- y105_8620[,-2]
save(y105_8620 , file = "data/y105_8620.RData")

#---------------stratification = 3 ---------------------
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP <=y105_8620_notax_1$OSB[1], "a" , y105_8620_notax$EMP)

# EMP <= 1
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP <= y105_8620_notax_1$OSB[1] & 
                                        y105_8620_notax$REV <= y105_8620_notax_2$OSB[1]
                                      , "group1" , y105_8620_notax$stratanames)
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP <= y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV <= y105_8620_notax_2$OSB[2] &
                                      y105_8620_notax$REV >  y105_8620_notax_2$OSB[1]
                                      , "group2" , y105_8620_notax$stratanames)
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP <= y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$REV >= y105_8620_notax_2$OSB[2]
                                      , "group3" , y105_8620_notax$stratanames)

# 1 < EMP < 2 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$EMP <= y105_8620_notax_1$OSB[2] & 
                                      y105_8620_notax$REV <= y105_8620_notax_2$OSB[1] 
                                      , "group4" , y105_8620_notax$stratanames)
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$EMP <= y105_8620_notax_1$OSB[2] & 
                                      y105_8620_notax$REV <= y105_8620_notax_2$OSB[2] &
                                      y105_8620_notax$REV >  y105_8620_notax_2$OSB[1]
                                      , "group5" , y105_8620_notax$stratanames)
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP >  y105_8620_notax_1$OSB[1] & 
                                      y105_8620_notax$EMP <= y105_8620_notax_1$OSB[2] & 
                                      y105_8620_notax$REV >  y105_8620_notax_2$OSB[2]
                                      , "group6" , y105_8620_notax$stratanames)

# EMP > 2 
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP >  y105_8620_notax_1$OSB[2] &
                                      y105_8620_notax$REV <= y105_8620_notax_2$OSB[1] , 
                                      "group7" , y105_8620_notax$stratanames)
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP >  y105_8620_notax_1$OSB[2] &
                                      y105_8620_notax$REV <= y105_8620_notax_2$OSB[2] &
                                      y105_8620_notax$REV >  y105_8620_notax_2$OSB[1] , 
                                      "group8" , y105_8620_notax$stratanames)
y105_8620_notax$stratanames <- ifelse(y105_8620_notax$EMP >  y105_8620_notax_1$OSB[2] &
                                      y105_8620_notax$REV >  y105_8620_notax_2$OSB[2] , 
                                      "group9" , y105_8620_notax$stratanames)

table(y105_8620_notax$stratanames)
#Optimum Strata Boundaries (OBS)

#quantile
base <- 
400/(y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]+
y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]+
y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[3]+
y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]+
y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]+
y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[3]+
y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[1]+
y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[2]+
y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[3])

quantile <- c(y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[1]*base,
              y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[2]*base,
              y105_8620_notax_1$nh[1]*y105_8620_notax_2$nh[3]*base,
              y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[1]*base,
              y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[2]*base,
              y105_8620_notax_1$nh[2]*y105_8620_notax_2$nh[3]*base,
              y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[1]*base,
              y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[2]*base,
              y105_8620_notax_1$nh[3]*y105_8620_notax_2$nh[3]*base)
#------------count--------------------
y105_8620_notax = y105_8620_notax[order(y105_8620_notax$stratanames),]

x <- y105_8620_notax[sample(nrow(y105_8620_notax), 10000), ]

y105_8620_strata <- strata(y105_8620_notax ,
                           stratanames = c("stratanames") ,
                           size = round(quantile) , method="srswor" )

y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
y105_8620_new <- y105_8620_new[,-ncol(y105_8620_new)] 

y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
y105_8620 <- y105_8620[,-2]

save(y105_8620 , file = "data/y105_8620.RData")

