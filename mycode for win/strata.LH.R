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
y105_8620_notax_5 <- strata.LH(x = y105_8620_notax$ASSET, CV = 0.01, Ls = 8, 
                                alloc = c(0.5, 0, 0.5) , algo = "Kozak")

y105_8620_notax <- cbind(y105_8620_notax , y105_8620_notax_5$stratumID )
names(y105_8620_notax )[ncol(y105_8620_notax )] <- c("stratumID")
table(y105_8620_notax$stratumID)
y105_8620_strata <- strata(y105_8620_notax ,
                           stratanames = c("stratumID") ,
                           size = y105_8620_notax_5$nh  )

y105_8620_new <- y105_8620_notax[y105_8620_strata$ID_unit,]
y105_8620_new <- y105_8620_new[,-ncol(y105_8620_new)] 

y105_8620[which(y105_8620$SEQ %in% y105_8620_new$SEQ),]$TAX <- 1
y105_8620 <- y105_8620[,-2]
save(y105_8620 , file = "data/y105_8620.RData")

