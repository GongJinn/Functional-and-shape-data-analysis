### Functional and Shape Data Analysis
### Introduction to Functional Data Analysis
### Spring 2019

### Example 1.1

library(fda)

data(growth)

matplot(growth$age, growth$hgtm, pch=19, col="blue")
matlines(growth$age, growth$hgtm, col="blue")

matpoints(growth$age, growth$hgtf, col="red")
matlines(growth$age, growth$hgtf, col="red")

### Example 1.2

data(CanadianWeather)

CW.Temp<-CanadianWeather$dailyAv[ , , "Temperature.C"]
matplot(1:365, CW.Temp, pch=19, col="blue")

### Example 1.3

data<-read.csv("ninoSST.csv", header=TRUE)
attach(data)

plot(c(1,12), c(min(NINO3), max(NINO3)), type="n", xlabel="Month", ylabel="STT")

for(i in 1:length(unique(YEAR))){

	y<-NINO3[YEAR==unique(YEAR)[i]]

	lines(1:12,y)

	if(unique(YEAR)[i]%in%c(1965, 1972, 1982, 1997)){
		lines(1:12,y, col="red", lwd=2)
	}
	if(unique(YEAR)[i]%in%c(1955, 1973, 1975, 1988, 1999)){
		lines(1:12,y, col="blue", lwd=2)
	}
}




