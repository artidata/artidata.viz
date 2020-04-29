library(ggplot2)
library(data.table)
library(scales)
#library(artidata.viz)
set.seed(240193)
N=100

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
ggsave("plot/minimal100.png",hist2d(dt1,facet=1,hasLine=T),width=30,height=20,units="cm")

ggsave("plot/grey.png",hist2d(dt1,facet=1,theme="grey"),width=30,height=20,units="cm")
ggsave("plot/dark.png",hist2d(dt1,facet=1,theme="dark"),width=30,height=20,units="cm")
ggsave("plot/minimal-line.png",hist2d(dt1,facet=1,hasLine=T),width=30,height=20,units="cm")
ggsave("plot/grey-line.png",hist2d(dt1,facet=1,theme="grey",hasLine=T),width=30,height=20,units="cm")
ggsave("plot/dark-line.png",hist2d(dt1,facet=1,theme="dark",hasLine=T),width=30,height=20,units="cm")
ggsave("plot/minimal-grid.png",hist2d(dt1,facet=1,hasGrid=T),width=30,height=20,units="cm")
ggsave("plot/grey-grid.png",hist2d(dt1,facet=1,theme="grey",hasGrid=T),width=30,height=20,units="cm")
ggsave("plot/dark-grid.png",hist2d(dt1,facet=1,theme="dark",hasGrid=T),width=30,height=20,units="cm")
scatter=ggplot(dt1,aes(x=x,y=y))+geom_point(size=0.1)+facet_wrap(vars(z))+theme_minimal()
ggsave("plot/scatter plot.png",scatter,width=30,height=20,units="cm")


dtE=data.table(x=rep(seq(-2,2,length.out=N),2))
dtE[1:N,y:=sqrt(2^2-x^2)]
dtE[(N+1):(2*N),y:=-sqrt(2^2-x^2)]
dtE[,":="(x=x+rnorm(2*N),y=y+rnorm(2*N))]

ggplot(data=dtE,aes(x=x,y=y))+
  geom_point(size=0.001)
