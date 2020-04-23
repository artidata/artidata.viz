#' Simple Linear Regression (internal)
#'
#' With data.table Grouping
#'
#' @param dt
#' @param x
#' @param y
#' @param z
#'
#' @return data.table object
#'
#' @examples
slr=function(dt,.x,.y,.z){
  dtSlr=dt[,{fit=lm(get(.y)~get(.x));
  .(b0=summary(fit)$coef[1,1],
    se0=summary(fit)$coef[1,2],
    b1=summary(fit)$coef[2,1],
    se1=summary(fit)$coef[2,2],
    r2=summary(fit)$r.square,
    n =.N)},
  by=.z]
  dtSlr
}

#' 2-Dimensional Histogram
#'
#' 2D Histogram is an alternative to traditional scatter plot.
#' Similar with histogram, it constructs bins of regular size, and count the number of observations found in each bin.
#' However, 2 dimensions are involved in the binning.
#' It also use colour gradient instead of bar height to represent the count number.
#' This plotting technique will be more useful when there are points which are closed or even stacked in the scatter plot (overplotting).
#'
#' @param dt
#' Data in the class of \code{\link[data.table]{data.table}}.
#' Do not name column variable with \code{.x} or \code{.y}.
#'
#' @param x,y
#' The \emph{name} of the columns in the \code{dt} that will be used as coordinates of the 2D histogram.
#' The columns must be of numeric vectors.
#' These are \code{(x,y)} coordinates when constructing a scatter plot.
#' If \code{x} and/or \code{y}  is not supplied, the first and/or second column of \code{dt} will be used.
#'
#' @param labX,labY
#' Character strings to be used as axis labels.
#' If not supplied, the value(s) of \code{x} or \code{y} will be used.
#'
#' @param limX,limY
#' The limits of coordinates of x or y axis that will be shown in the plot.
#' It does \emph{not} affect the regression line if \code{hasLine} is \code{TRUE}.
#' If not supplied, The values will be the minimum and the maximum value of corresponding dimension.
#'
#' @param facet
#' Any of \code{{0,1,2}}.
#' It refers to the \pkg{ggplot2}'s faceting technique.
#' Multiple 2D histograms will be displayed as a sequence of panels which enables grouping comparison.
#' The value represents the number of additional categorical variable(s).
#'
#' @param z
#' A vector of column name(s) of \code{dt} that will be used for faceting.
#' The length of the vector must match the number in \code{facet}.
#' Please, set it to \code{NULL} when \code{facet==0}.
#'
#' @param nBin
#'
#' @param widthBin
#' If not supplied, Freedman and Diaconis’s rule is applied to each dimension.
#'
#' @param palette
#'
#' @param hasLine
#' logical with \code{FALSE} as the default.
#' If \code{TRUE}, draw and annotate a simple linear regression line of \code{y} against \code{x}
#'
#'
#' @return
#'
#' @export
#'
#' @import data.table ggplot2
#' @examples
hist2d=function(dt,
                x,y,
                title,labX,labY,
                limX,limY,
                facet=0,
                z=NULL,
                widthBin,
                nBin,
                hasLine=F,
                palette="Reds",
                theme="minimal",
                hasGrid=F,
                trans="identity"){

  if(missing(dt)) stop("hist2d requires a supply of dt, one data.table with at least 2 columns of numeric vectors.")
  if(!"data.table"%in%class(dt)) stop("Parameter dt must be of data.table object, a data.frame must first be converted to a data.table object.")
  if(facet>2|facet<0) stop("Parameter facet refers to ggplot2 facetting technique that requires 1 or 2 values. When facet=0, there is no facetting required")
  if(!theme%in%c("minimal","grey","dark")) stop("Supported themes are minimal, grey or dark. You can still modify the returned Grob object afterward.")
  if(missing(widthBin)==F&missing(nBin)==F) stop("Only either one of widthBin or nBin should be supplied.")

  if(missing(x)) x=colnames(dt)[1]
  if(missing(y)) y=colnames(dt)[2]

  if(facet==1&is.null(z)) z=colnames(dt)[3]
  if(facet==2&is.null(z)) z=colnames(dt)[3:4]

  if(missing(title)) title=paste0("2D-Histogram of ",y," against ",x,if(is.null(z)==F) paste0(" grouped by ",z),".")
  if(missing(labX)) labX=x
  if(missing(labY)) labY=y

  #awkward naming for variable name masking problem
  .x=x
  .y=y
  .z=z

  if(missing(limX)) limX=dt[,c(min(get(.x)),max(get(.x)))]
  if(missing(limY)) limY=dt[,c(min(get(.y)),max(get(.y)))]

  if(missing(widthBin)&missing(nBin)){
    if(facet==0){
      widthBin=dt[,c(IQR(get(.x))/.N^(1/3),IQR(get(.y))/.N^(1/3))]
    }else{
      widthBin=dt[,.(IQR(get(.x))/.N^(1/3),IQR(get(.y))/.N^(1/3)),by=.z][,c(mean(V1),mean(V2))]
    }
  }else if(missing(widthBin)&missing(nBin)==F){
    widthBin=c(limX[2]-limX[1],limY[2]-limY[1])/nBin
  }

  hexBlack=hcl(0,0,0)
  hexGrey20=hcl(0,0,20)
  hexGrey40=hcl(0,0,40)
  hexGrey80=hcl(0,0,80)
  hexGrey90=hcl(0,0,90)
  hexWhite=hcl(0,0,100)

  hist2d=ggplot(dt,aes(x=get(.x),y=get(.y)))+
    geom_bin2d(binwidth=widthBin,
               color=fcase(theme=="minimal",hexGrey20,
                           theme=="grey",hexWhite,
                           theme=="dark",hexGrey80))+
    labs(title=title,x=labX,y=labY,fill="Count")+
    scale_fill_gradientn(colours=if(theme!="dark"){brewer_pal("seq",palette)(9)}else{brewer_pal("seq",palette,-1)(9)},
                         #breaks=c(1,10,100,1000,10000,100000), # hard coding
                         labels=comma,
                         trans=trans,
                         guide=guide_colorbar(
                           ticks.colour=if(theme!="dark"){hexBlack}else{hexWhite},
                           frame.colour=if(theme!="dark"){hexBlack}else{hexWhite},
                           frame.linewidth=1,
                           ticks.linewidth=1))+
    coord_cartesian(xlim=limX,ylim=limY)+
    theme(axis.title=element_text(size=rel(1.1)),
          strip.text=element_text(size=rel(1.1)))

  if(theme=="minimal"){
    hist2d=hist2d+
      theme(panel.background=element_rect(fill=hexWhite,color=hexBlack),
            panel.border=element_rect(fill=NA,color=hexBlack),
            strip.background=element_rect(fill=hexWhite,color=NA))
  }else if(theme=="grey"){
    hist2d=hist2d+
      theme(panel.background=element_rect(fill=hexGrey80,color=hexBlack),
            panel.border=element_rect(fill=NA,color=hexBlack),
            strip.background=element_rect(fill=hexWhite,color=NA),
            strip.text=element_text(color=hexBlack))
  }else if(theme=="dark"){
    hist2d=hist2d+
      theme(panel.background=element_rect(fill=hexBlack,color=hexWhite),
            panel.border=element_rect(fill=NA,color=hexWhite),
            plot.background=element_rect(fill=hexBlack,color=hexBlack),
            text=element_text(color=hexWhite),
            axis.text=element_text(color=hexWhite),
            axis.ticks=element_line(color=hexWhite),
            strip.text=element_text(color=hexWhite),
            strip.background=element_rect(fill=hexBlack,color=NA),
            legend.background=element_rect(fill=hexBlack),
            legend.key=element_rect(color=hexWhite))}

  if(hasGrid==F){
    hist2d=hist2d+theme(panel.grid=element_blank())
  }else if(hasGrid==T){
    if(theme=="minimal"){
      hist2d=hist2d+theme(panel.grid=element_line(color=hexGrey80))
    }else if(theme=="grey"){
      hist2d=hist2d+theme(panel.grid=element_line(color=hexGrey90))
    }else if(theme=="dark"){
      hist2d=hist2d+theme(panel.grid=element_line(color=hexGrey20))
    }
  }

  if(facet == 1){
    hist2d=hist2d +
      facet_wrap(vars(get(.z[1])))
  } else if(facet == 2){
    hist2d=hist2d +
      facet_grid(vars(get(.z[1])),vars(get(.z[2])))
  }


  if(hasLine==T){
    dtLm=slr(dt,.x,.y,.z)
    dtLm[,label:= paste0("list(italic(Y)==",signif(b1,3),"[(",signif(se1,3),")]","*italic(X)",ifelse(b0>=0,"+",""),signif(b0,3),"[(",signif(se1,3),")]",",italic(R^2)==",signif(r2,3),",",
                        "italic(n)==",n,")")]
    hist2d=hist2d +
      geom_abline(data=dtLm,aes(intercept=b0,slope=b1),
                  color=if(theme!="dark"){hexBlack}else{hexWhite},size=1)+
      geom_label(data=dtLm,
                 aes(x=mean(limX), y=Inf, label=label),
                 vjust="inward", hjust="inward",parse=T,size=rel(4),
                 label.r=unit(0,"line"),
                 color=if(theme!="dark"){hexGrey40}else{hexGrey80},
                 fill=if(theme!="dark"){hexWhite}else{hexBlack})
  }
  hist2d
}
