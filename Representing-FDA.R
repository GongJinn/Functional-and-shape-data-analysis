### Functional and Shape Data Analysis
### Representing Functional Data
### Spring 2019

### Example 2.2

library(fda)

bsbasis<-create.bspline.basis(c(0,3),breaks=c(0,1,2,3), norder=3)
eval.basis(2.5,bsbasis)

daybasis<-create.fourier.basis(c(0,365), 3)
constant<-create.constant.basis(c(0,3))
polynomial<-create.monomial.basis(c(0,3),nbasis=3)

### Example 2.3

heightbasis12<-create.bspline.basis(c(1,18),nbasis=12, norder=6)

heightmat.boy<-growth$hgtm
heightmat.girl<-growth$hgtf

basismatrix<-eval.basis(growth$age, heightbasis12)

coef.boy<-lsfit(basismatrix,heightmat.boy, intercept=FALSE)$coef
coef.girl<-lsfit(basismatrix,heightmat.girl, intercept=FALSE)$coef

t<-seq(1,18, 0.0001)
basismatrix<-eval.basis(t, heightbasis12)
smooth.boy1<-basismatrix%*%coef.boy[,1]

plot(growth$age, heightmat.boy[,1])
lines(t,smooth.boy1)

H.matrix<-basismatrix%*%solve(t(basismatrix)%*%basismatrix)%*%t(basismatrix)
fitted.boy<-H.matrix%*%heightmat.boy
fitted.girl<-H.matrix%*%heightmat.girl

plot(fitted.boy[,1],heightmat.boy[,1])


### Example 2.4

heightList.boy<-smooth.basis(growth$age,heightmat.boy, heightbasis12)
heightList.girl<-smooth.basis(growth$age,heightmat.girl, heightbasis12)

plot(heightList.boy, col="blue")
lines(heightList.girl, col="red")

lines(mean(heightList.boy$fd), lwd=3)
lines(mean(heightList.girl$fd), lwd=3)


eval.fd(t,heightList.boy$fd)

t<-seq(1,18, 0.001)

plot(growth$age, heightmat.boy[,1])
lines(t,smooth.boy1)
lines(t,eval.fd(t,heightList.boy$fd)[,1], col="blue")

D.boy<-eval.fd(t,heightList.boy$fd,1)
D.girl<-eval.fd(t,heightList.girl$fd,1)

matplot(t, D.boy, type="n")
matlines(t, D.boy, col="blue")
matlines(t, D.girl, col="red")

D2.boy<-eval.fd(t,heightList.boy$fd,2)
D2.girl<-eval.fd(t,heightList.girl$fd,2)

matplot(t, D2.boy, type="n")
matlines(t, D2.boy, col="blue")
matlines(t, D2.girl, col="red")

Dfd.boy<-deriv.fd(heightList.boy$fd,1)
plot(Dfd.boy)

### Example 2.5

norder<-6
nbasis<-length(growth$age)+norder-2
breaks<-growth$age
heightbasis<-create.bspline.basis(c(1,18),nbasis=nbasis, norder=norder, breaks=breaks)
heightfdPar<-fdPar(heightbasis, Lfd=2, lambda=0.01)
heightList.girlL<-smooth.basis(growth$age,heightmat.girl, heightfdPar)




 










