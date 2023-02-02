# Regression

#babies data-----
babies.data <- read.table("~/Desktop/Data_Sets/babies.data.txt", header = TRUE) # make sure you are in the correct directory
head(babies.data)
attach(babies.data)
dim(babies.data)

fit0 <- lm(birth.weight ~ 1)
fit1 <- lm(birth.weight ~ mom.weight)
summary(fit1)

plot(mom.weight,birth.weight,  type = 'p', col = 'black', pch=16, 
      main = "birth weight predicted by mom's weight", 
     xlab = "mom's weight", ylab = "birth weight")
abline(fit1, col = 'red')


#correlation
cor(babies.data, birth.weight)

fit2 <- lm(birth.weight ~ gestation)
summary(fit2)

plot(gestation, birth.weight)
abline(fit2, lty = 2, col = "red")

identify(gestation, birth.weight)

trim.data <- babies.data[c(-239, -820), ]
detach(babies.data)
attach(trim.data)
dim(trim.data)

fit3 <- lm(birth.weight ~ gestation + mom.smokes)
plot(predict(fit3), residuals(fit3))

par(mfrow = c(2,2))
plot(fit3)
dev.off()

