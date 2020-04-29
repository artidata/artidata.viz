# library(data.table)
# library(ggplot2)
# library(scales)
#
#
# #dtTree will assume that no missing data
# dtTree <- data.table(node = c("A","Aa","Aa1","Aa2","Ab","Ac",
#                               "B","Ba","Bb","Bb1","Bb2",
#                               "C"),
#                      level = c(1,2,3,3,2,2,
#                                1,2,2,3,3,
#                                1),
#                      parent = c("root","A","Aa","Aa","A","A",
#                                 "root","B","B","Bb","Bb",
#                                 "root"),
#                      value = c(.3,.2,.15,.05,.06,.04,
#                                .5,.4,.1,.05,.05,
#                                .2))
#
# #dtTree <- dtTree[,.(parent = node,parentValue = value)][dtTree,on=c(parent = "parent")]
# #dtTree[is.na(parentValue),parentValue:= 1]
#
# dtTree <- dtTree[order(-value)]
# dtTree[,cum := cumsum(value)/sum(value),.(parent)]
#
# dtTree[,cumPrev := shift(cum,1,0,"lag"),.(parent)]
# dtTree[level == 1,":="(xmin = cumPrev,
#                        xmax = cum,
#                        ymin = 0,
#                        ymax = 1)]
# dtTree <- merge(dtTree,
#                 dtTree[level == 1,.(parent=node,xmin,xmax,ymin,ymax)],
#                 by="parent",all.x = T,suffixes = c("",".p"))
#
# dtTree[level == 2,":="(xmin = xmin.p,
#                        xmax = xmax.p,
#                        ymin = ymin.p+cumPrev*(ymax.p-ymin.p),
#                        ymax = ymin.p+cum*(ymax.p-ymin.p))]
#
# dtTree <- merge(dtTree[,.(node,level,cum,cumPrev,xmin,xmax,ymin,ymax,parent)],
#                 dtTree[,.(parent=node,xmin,xmax,ymin,ymax)],
#                 by="parent",all.x = T,suffixes = c("",".p"))
#
# dtTree[level == 3,":="(xmin = xmin.p + cumPrev*(xmax.p-xmin.p),
#                        xmax = xmin.p + cum*(xmax.p-xmin.p),
#                        ymin = ymin.p,
#                        ymax = ymax.p)]
# ggplot(data = dtTree)+
#   geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,color="black")+
#   scale_y_continuous(breaks = seq(0,1.0,0.1),labels=percent)+
#   scale_x_continuous(breaks = seq(0,1.0,0.1),labels=percent)+
#   coord_equal()+
#   theme_minimal()+
#   theme(panel.grid.minor = element_blank(),
#         axis.title = element_blank())+
