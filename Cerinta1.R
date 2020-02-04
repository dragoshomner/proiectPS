df <- data.frame("Girth"=trees$Girth, "Height"=trees$Height, "Volume"=trees$Volume)

#Printam mediile 
sprintf("Girth mean is: %f",mean(df[,1]))
sprintf("Height mean is: %f",mean(df[,2]))
sprintf("Volume mean is: %f",mean(df[,3]))

#Printam variantele
sprintf("Girth variance is: %f",var(df[,1]))
sprintf("Height variance is: %f",var(df[,2]))
sprintf("Volume variance is: %f",var(df[,3]))

#Printam quantilele
print("Girth quantile is:")
print(quantile(df[,1]))
print("Height quantile is:")
print(quantile(df[,2]))
print("Volume quantile is:")
print(quantile(df[,3]))


#Printam boxplot-uri
par(mfrow=c(1,3)) #impartim zona de grafic in 3 coloane

boxplot(df[,1], main="Girth", sub=paste("Outliere:", boxplot.stats(trees$Girth)$out),col="red")
boxplot(df[,2], main="Height", sub=paste("Outliere:", boxplot.stats(trees$Height)$out),col="green")
boxplot(df[,3], main="Volume", sub=paste("Outliere:", boxplot.stats(trees$Volume)$out),col="blue")
