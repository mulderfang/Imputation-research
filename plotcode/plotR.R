library(readxl)
library(xlsx)
library(ggplot2)
library(tidyverse)
library(gridExtra )
install.packages("showtext")
library(showtext)
showtext.auto(enable = TRUE)
font.add("中黑體 2體", "中黑體 2.ttc")

#auto plot 
  nick_xu = function(imputation , v ){
    plot <- c()
    
    #getwd setting the path where your file location
    DataDir = "/Users/mac/Desktop/desktop/宗記/stratify/result.proportion"
    allFiles = list.files(DataDir)
    #get the file name
    xlsx.name = allFiles[which(grepl("6496", allFiles))]
    xfile = list()
    # sheet = c("COST", "REV", "ASSET", "INVESTAS")
    var = c("COST", "REV", "ASSET", "INVESTAS")
    method_name = data.frame(Method = c("Real","mean","median","hotdeck","knn","norm5_bar","norm10_bar","pmm5_bar","pmm10_bar"))
    
   #for read the path where your excel is 
    a = "/Users/mac/Desktop/desktop/宗記/stratify/result.proportion/6496_"
    b = "/y105/varA/type2.xlsx"
    
   # seize the imputation method
    implist =  grep( paste0("_",imputation,"_") ,  xlsx.name )
    for(i in 1:length(implist)){
      #read excel : we need to set the file path automatically , so using the paste0( a , xxxxx , b ) 
     xfile =  read.xlsx(file = paste0( a , imputation , substr(xlsx.name[implist[i]],7,nchar( xlsx.name[implist[i]])) , b ) ,
                                                       sheetIndex = var[v] ,startRow = 2 , endRow = 11 , header = T , colIndex =10 , encoding = "UTF-8")
     
     # change number to percentage number
     # names(xfile) = as.numeric(substr(xlsx.name[implist[i]],8,nchar( xlsx.name[implist[i]])))
     names = c( "n3:832" , "n2:332" , "n1:167" )
     names(xfile) = names[i]
     # write into data.frame
     method_name[,i+1]=xfile
    }
    #plot
    #just plot it !!!!!!
    x = method_name[-1,]
    plot.new <- x %>%
      gather(sort(colnames(x)[2: ncol(x)]),
             key = "CV",
             value = "Delta")
    p =ggplot(data = plot.new , mapping = aes(x = CV , y = Delta, group = Method , 
                                              color = Method , linetype = Method , fill = Method)) + 
       geom_line() + geom_point(size=2 , alpha = 0.8)+theme_classic()
    
    # "Sampling Proportion"   "Coefficient of Variation"
    print(p + labs(x = "抽樣數" , y = "絕對誤差率" , cex.lab = 2 , title = var[v] , family = "中黑體 2")
          + theme(axis.text.x = element_text(size = 10 , face = "bold"))
          + theme(axis.text.y = element_text(size = 10 , face = "bold")))
  }

  # save plot 
  savenick = function(i,j){
    #again save plot automatically , so set path (a , xxxx , b)
  a = "plot/6496_"
  b = ".jpeg"
  c = paste0(a,i,"_plot")
  dir.create(paste0("plot"))
  dir.create(c)
  var = c("COST", "REV", "ASSET", "INVESTAS")
  jpeg( file = paste0(c,"/",var[j],b), width = 1626, height = 1250, res = 300)
  nick_xu(i,j)
  dev.off()
  }
  
  #using nick_xu & savenick function to plot 
  # m : imputation function 
  # i : var 

    for(i in 1:4){
  savenick(3,i)
    }


savenick(3,4)

