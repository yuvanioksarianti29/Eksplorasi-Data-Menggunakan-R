library(car)
summary(UScereal)

par(mfrow=c(1,1))
qqPlot(UScereal$fibre)
par(mfrow=c(1,2))

#penghitungan rataan dan simpangan baku untuk seluruh amatan
rataan.all <- mean(UScereal$fibre)
simp.baku.all <- sd(UScereal$fibre)

#penghitungan rataan dan simpangan baku tanpa menyertakan tiga amatan
#dengan nilai yang ektrim tinggi
#kebetulan dalam kasus ini, tiga nilai tertinggi adalah tiga
#amatan pertama
#tapi dalam prakteknya, amatan tertinggi tidak selalu 
#terletak pada urutan pertama dalam data
#penghitungan rataan dan simpangan baku untuk seluruh amatan
rataan.outlier.rem <- mean(UScereal$fibre[-(1:3)])
simp.baku.outlier.rem <- sd(UScereal$fibre[-(1:3)])

#plot dengan nilai rataan dan simpangan baku semua amatan
plot(UScereal$fibre,main="All observations")
abline(h=mean(UScereal$fibre),lty=1)
abline(h=mean(UScereal$fibre)+sd(UScereal$fibre),lty=2)

#plot dengan nilai rataan dan simpangan baku semua amatan
plot(UScereal$fibre,main="Outliers removed")
abline(h=mean(UScereal$fibre[-(1:3)]),lty=1)
abline(h=mean(UScereal$fibre[-(1:3)])+sd(UScereal$fibre[-(1:3)]),lty=2)


