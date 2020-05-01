library(ggplot2)
library(data.table)
library(scales)
#library(artidata.viz)
set.seed(240193)
N=10000

dtA=data.table(x=runif(N,-3,3),
               y=runif(N,-3,3),
               z="A")

dtB=data.table(x=rnorm(N),
               y=rnorm(N),
               z="B")

dtC=data.table(x=seq(-3,3,length.out = N))
dtC[,":="(y=0.5*x+rnorm(N,sd=0.5),
          z="C"),]

dtD=data.table(x=c(rnorm(N/2,-1,.75),rnorm(N/2,1,.75)),
               y=c(rnorm(N/2,1,.75),rnorm(N/2,-1,.75)),
               z="D")

dt1=rbindlist(list(dtA,dtB,dtC,dtD))
scatter=ggplot(dt1,aes(x=x,y=y))+geom_point(size=0.1)+facet_wrap(vars(z))+theme_minimal()
ggsave("plot/scatter plot.png",scatter,width=30,height=20,units="cm")
ggsave("plot/minimal-stderr.png",hist2d(dt1,facet=1,hasLine=T,nBin=100,statsLine=c("rsquare","count","stderr")),width=30,height=20,units="cm")

ggsave("plot/minimal-50.png",hist2d(dt1,facet=1,hasLine=T,nBin=50),width=30,height=20,units="cm")
ggsave("plot/minimal-100.png",hist2d(dt1,facet=1,hasLine=T,nBin=100),width=30,height=20,units="cm")


dt2=rbindlist(list(dt1,data.table(x=seq(-1,1,length.out=1000),y=0,z="B")))
scatter2=ggplot(dt2,aes(x=x,y=y))+geom_point(size=0.1)+facet_wrap(vars(z))+theme_minimal()
ggsave("plot/scatter plot 2.png",scatter2,width=30,height=20,units="cm")
ggsave("plot/minimal-100 2.png",hist2d(dt2,facet=1,nBin=100),width=30,height=20,units="cm")
