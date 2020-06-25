# histogram
library(ggplot2)
#input data 
#varA.rda have 10 method different imputation data
load("/Users/mac/Desktop/desktop/宗記/stratify/result.bethel/8620_3_0.001_1.001/y105/varA.rda")

# nostrata = missing
load("/Users/mac/Desktop/desktop/宗記/stratify/result.bethel/8620_3_0.001_1.001/nostrata.RData")
#realstrta = sampling data by stratify 
load("/Users/mac/Desktop/desktop/宗記/stratify/result.bethel/8620_3_0.001_1.001/realstrata.RData")

# set stratify data TAX = 1
realstrata$TAX <- 1
y105_8620 = rbind(nostrata,realstrata)

 ### basic information for the different year
    year = 105
    var = c("COST", "REV", "ASSET", "INVESTAS")
    complete = y105_8620
    missing = which(complete$TAX == 2)
  ### take the basic dataset information
  IND = 8620
  missing = which(complete$TAX == 2)
  
  ### create the folder to store the results
  dir.create("result")
  dir.create(paste0("result/", IND))
  dir.create(paste0("result/", IND, "/", "y", year))
  ### j == 1 : using all variables to impute, j == 2 using significant variables to impute
      sel = "varA"
      len = 153
      
      ### histogram for best imputed data
    best = c("Real","hotdeck")
    for(m in 1:2){
      for(v in 1:length(var)){
        bin = 10
        dir.create(paste0("result/", IND, "/", "y", year, "/", sel))
        dir.create(paste0("result/", IND, "/", "y", year, "/", sel, "/all"))
        
        png(filename = paste0("result/", IND, "/y", year, "/", sel, "/all/", m, best[m], "_", v, ".png"),
            width = len, height = len)
        if(v == 1){
          
          print(ggplot(Method[[best[m]]][missing,]) + 
                  geom_histogram(aes(x = COST, y = ..density..), colour = "black", fill = "white",bins = bin) + 
                  geom_density(aes(x = COST, y = ..density..), alpha = .2, fill = "#FF6666") + 
                  xlab(best[m]) + ggtitle("COST")  + theme(axis.line = element_line(size=1, colour = "black")))
          
        }else if(v == 2){
          print(ggplot(Method[[best[m]]][missing,]) + 
                  geom_histogram(aes(x = REV, y = ..density..), colour = "black", fill = "white",bins = bin) + 
                  geom_density(aes(x = REV, y = ..density..), alpha = .2, fill = "#FF6666") 
                + xlab(best[m]) + ggtitle("REV") + theme(axis.line = element_line(size=1, colour = "black")))
          
        }else if(v == 3){
          
          print(ggplot(Method[[best[m]]][missing,]) + 
                  geom_histogram(aes(x = ASSET, y = ..density..), colour = "black", fill = "white",bins = bin) + 
                  geom_density(aes(x = ASSET, y = ..density..), alpha = .2, fill = "#FF6666")
                + xlab(best[m]) + ggtitle("ASSET") + theme(axis.line = element_line(size=1, colour = "black")))
          
        }else{
          
          print(ggplot(Method[[best[m]]][missing,]) + 
                  geom_histogram(aes(x = INVESTAS, y = ..density..), colour = "black", fill = "white",bins = bin) + 
                  geom_density(aes(x = INVESTAS, y = ..density..), alpha = .2, fill = "#FF6666") + 
                  xlab(best[m]) + ggtitle("INVESTAS") + theme(axis.line = element_line(size=1, colour = "black")))
        }
        dev.off()
      }
    }

    
    
    
    
    