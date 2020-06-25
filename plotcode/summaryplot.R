
options(scipen=999)
eq2 = density(y105_6496$REV, bw = 30)
hist (y105_6496$REV,
      main = "",
      xlab = "REV", ylab = "" , cex.lab = 2, 
      col = "maroon", las = 1, cex.axis = 1.4 , breaks = 200)

hist (y105_6496$EMP,
      main = "",
      xlab = "EMP", ylab = "" , cex.lab = 2, 
      col = "maroon", las = 1, cex.axis = 1.4 , breaks = 200)

hist (y105_6496$ASSET,
      main = "",
      xlab = "ASSET",ylab = "" , cex.lab = 2, 
      col = "maroon", las = 1, cex.axis = 1.4 , breaks = 200)





