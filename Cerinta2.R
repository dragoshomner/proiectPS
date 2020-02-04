df <- data.frame("Girth"=trees$Girth, "Height"=trees$Height, "Volume"=trees$Volume)
par(mfrow=c(1,2))

#Regresie liniara
simpleModel <- lm(Volume ~ Girth , data = df)
summary(simpleModel)
plot(Volume ~ Girth , data = df, col="blue", pch=8)
abline(simpleModel, col="black")

#plot(Volume ~ Height , data = df) 
#sm <- lm(Volume ~ Height , data = df)
#abline(sm,col="red")
#summary(sm) -> obtinem un R square de 35%, foarte prost, comparativ cu 95% si 94% in cazul
#in care tinem cont de grosime sau de ambele
#Am constatat ca grosimea e mai relevanta pentru volum decat inaltimea, in urma ultimelor linii de cod


#Adaugarea unei noi variabile age
df <- data.frame("Girth"=trees$Girth, "Height"=trees$Height, "Volume"=trees$Volume)
girthMean <- mean(df[,1])
girthDeviation <- sd(df[,1])
age <- rnorm(31, mean = girthMean , sd = girthDeviation )
df$Age <- age
plot(Volume ~ Age, data=df, col="red", pch=12)

#Regresie liniara multipla
multipleModel <- lm(Volume ~ Girth + Height, data = df)
summary(multipleModel)

#Compararea modelelor de regresie liniara simpla si multipla folosind AIC si BIC
aics <- AIC(simpleModel)
aicm <- AIC(multipleModel)

bics <- BIC(simpleModel)
bicm <- BIC(multipleModel)

#Cu cat AIC/BIC sunt mai mici, cu atat mai bine
sprintf("AIC pentru modelul de regresie liniara simpla este %f ,iar pentru modelul de regresie liniara multipla este %f",aics,aicm)
sprintf("BIC pentru modelul de regresie liniara simpla este %f ,iar pentru modelul de regresie liniara multipla este %f",bics,bicm)


