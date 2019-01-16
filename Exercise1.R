library(fda)
#1.a
data(pinch)

bspline15<-create.bspline.basis(c(1,151),nbasis=15, norder=4)

basismatrix<-eval.basis((1:151), bspline15)
coef.pinch<-lsfit(basismatrix,pinch, intercept=FALSE)$coef
t<-seq(1,151, 0.1)

basismatrix1<-eval.basis(t, bspline15)
plot((1:151), pinch[,1],type="n",xlab="x",ylab="y")#empty plot
for(i in c(1:20)){
  smooth.pinch<-basismatrix1%*%coef.pinch[,i]
  lines(t,smooth.pinch)
}
#1.b
mean.pinch<-rowMeans(pinch)
mean.coef.pinch<-lsfit(basismatrix,mean.pinch, intercept=FALSE)$coef
smooth.pinch<-basismatrix1%*%mean.coef.pinch
lines(t,smooth.pinch, col="red")
#1.c
H.matrix<-basismatrix%*%solve(t(basismatrix)%*%basismatrix)%*%t(basismatrix)
fitted.pinch<-H.matrix%*%pinch
SSE.pinch<-colSums((fitted.pinch-pinch)^2)
#2.a
List.pinch<-smooth.basis((1:151),pinch,bspline15)
#2.b
derivatives.first<-deriv.fd(List.pinch$fd,1)
plot(derivatives.first)
derivatives.second<-deriv.fd(List.pinch$fd,2)
plot(derivatives.second)
#2.c
eval.pinch <- eval.fd((1:151),List.pinch$fd,1)
plot(derivatives.first[1])
eval.pinch1 <- eval.fd((1:151),derivatives.first[1])
for(i in c(1:150)){
  if (eval.pinch1[i]*eval.pinch1[i+1]<=0){
    print(i+0.5)
  }
}
#3.a
data<-read.table("ninoSST.txt", sep="\t", header=TRUE)
x<-data.frame(data$YEAR,data$NINO3)
reshaped.data<-unstack(x,NINO3~YEAR)

#3.b
fourier5<-create.fourier.basis(c(1,12), 5)
basismatrix5<-eval.basis((1:12), fourier5)
coef.nino<-lsfit(basismatrix5,reshaped.data, intercept=FALSE)$coef
t<-seq(1,12, 0.1)
basismatrix51<-eval.basis(t, fourier5)
plot(c(1,12), c(min(NINO3), max(NINO3)+1), type="n", xlab="Month", ylab="STT")
for(i in c(1:64)){
  smooth.nino<-basismatrix51%*%coef.nino[,i]
  lines(t,smooth.nino)
}
#3.c
mean.nino<-rowMeans(reshaped.data)
mean.coef.nino<-lsfit(basismatrix5,mean.nino, intercept=FALSE)$coef
smooth.nino<-basismatrix51%*%mean.coef.nino
lines(t,smooth.nino, col="red")
#3.d
fourier7<-create.fourier.basis(c(1,12), 7)
basismatrix7<-eval.basis((1:12), fourier7)
coef.nino<-lsfit(basismatrix7,reshaped.data, intercept=FALSE)$coef
t<-seq(1,12, 0.1)
basismatrix71<-eval.basis(t, fourier7)
plot(c(1,12), c(min(NINO3), max(NINO3)+1), type="n", xlab="Month", ylab="STT")
for(i in c(1:64)){
  smooth.nino<-basismatrix71%*%coef.nino[,i]
  lines(t,smooth.nino)
}

mean.coef.nino<-lsfit(basismatrix7,mean.nino, intercept=FALSE)$coef
smooth.nino<-basismatrix71%*%mean.coef.nino
lines(t,smooth.nino, col="red")
#fourier5 works better