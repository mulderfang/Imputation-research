
library(car)
load("result/8620_3_0.06/nostrata.RData")
load("result/8620_3_0.06/realstrata.RData")
#cut raw data
load("data/y105.rda")
y105_8620 = y105[which(y105$IND == 8620),]

library(epicalc)

raw = rbind(nostrata , realstrata )
raw$label = "raw"
realstrata$label = "sample"
data = rbind( raw , realstrata )
data$stratanames = as.numeric(data$stratanames)
type = names(table(realstrata$stratanames))
pyramid( data$stratanames ,data$label ,  binwidth = 1 , bar.label = TRUE ,cex.axis = 1,
         col.gender = c("navy","skyblue") ,percent = "none" , main = "Using samplingstrata in CV = 0.06" )

data_in = data[-which(data$INVESTAS > quantile(data$INVESTAS ,probs = 0.98 )) , ]
pyramid( data_in$INVESTAS ,data_in$label ,  binwidth =10 , bar.label = TRUE ,cex.axis = 1, 
         col.gender = c("navy","skyblue") ,percent = "none" , main = "Using samplingstrata in CV = 0.06" )

