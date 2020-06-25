y100 = read.csv("data/Y100TAX.csv")
y105 = read.csv("data/Y105TAX.csv")

### Change Year to Exist Year
y100_YEAR = y100$YEAR
for(i in 1:length(y100_YEAR)){
  y100_YEAR[[i]] = 100 - y100_YEAR[[i]]
}
y100$YEAR = y100_YEAR

y105_YEAR = y105$YEAR
for(i in 1:length(y105_YEAR)){
  y105_YEAR[[i]] = 105 - y105_YEAR[[i]]
}
y105$YEAR = y105_YEAR

### transform categorical variable 
for(i in c(2,3,5,15:23, 26:29)){
  y100[[i]] = factor(y100[[i]])
}

for(i in c(2,3,5,16:24, 27:30)){
  y105[[i]] = factor(y105[[i]])
}

### tranform 2 to 0
y100$RENT = as.numeric(as.character(y100$RENT))
y100$RES = as.numeric(as.character(y100$RES))
y100$BRAND = as.numeric(as.character(y100$BRAND))
y100$ECOYN = as.numeric(as.character(y100$ECOYN))
y100$OUTSALE = as.numeric(as.character(y100$OUTSALE))

y100[which(y100$RENT == 2), "RENT"] = 0
y100[which(y100$RES == 2), "RES"] = 0
y100[which(y100$BRAND == 2), "BRAND"] = 0
y100[which(y100$ECOYN == 2), "ECOYN"] = 0
y100[which(y100$OUTSALE == 2), "OUTSALE"] = 0
y100$RENT = as.numeric(as.character(y100$RENT))
y100$RES = as.numeric(as.character(y100$RES))
y100$BRAND = as.numeric(as.character(y100$BRAND))
y100$ECOYN = as.numeric(as.character(y100$ECOYN))
y100$OUTSALE = as.numeric(as.character(y100$OUTSALE))

y105[which(y105$RENT == 2), "RENT"] = 0
y105[which(y105$RES == 2), "RES"] = 0
y105[which(y105$BRAND == 2), "BRAND"] = 0
y105[which(y105$ECOYN == 2), "ECOYN"] = 0
y105[which(y105$OUTSALE == 2), "OUTSALE"] = 0

### transform categorical variable 
for(i in c(2,3,5,15:23, 26:28)){
  y100[[i]] = factor(y100[[i]])
}

for(i in c(2,3,5,16:24, 27:30)){
  y105[[i]] = factor(y105[[i]])
}

dir.create("data")

save(y100, file = "data/y100.rda")
save(y105, file = "data/y105.rda")