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
slr <- function(dt,.x,.y,.z){
  dtSlr <- dt[,{fit = lm(get(.y)~get(.x));
  .(b0 = summary(fit)$coef[1,1],
    b1 = summary(fit)$coef[2,1],
    r2 = summary(fit)$r.square,
    n =.N)},
  by = .z]
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
#' Behind the scene,
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
#' @import data.table
#' @examples
hist2d <- function(dt,
                   x,y,
                   labX,labY,
                   limX,limY,
                   facet = 0,
                   z = NULL,
                   nBin = c(10,10),
                   widthBin,
                   hasLine = F,
                   palette = "Reds",
                   trans = "identity"){

  stopifnot("data.table"%in%class(dt))

  if(missing(x)) x <- colnames(dt)[1]
  if(missing(y)) y <- colnames(dt)[2]
  if(missing(labX)) labX <- x
  if(missing(labY)) labY <- y

  #awkward naming for variable name masking problem
  .x <- x
  .y <- y
  .z <- z

  if(missing(limX)) limX <- dt[,c(min(get(.x)),max(get(.x)))]
  if(missing(limY)) limY <- dt[,c(min(get(.y)),max(get(.y)))]

  if(missing(widthBin)) widthBin <- c(limX[2]-limX[1],limY[2]-limY[1])/nBin

  hist2d <- ggplot(dt,aes(x = get(.x), y = get(.y)))+
    geom_bin2d(binwidth = widthBin)+
    labs(x = labX,y = labY, fill = "Count")+
    scale_fill_gradientn(colours = brewer_pal(palette=palette)(9),
                         #breaks = c(1,10,100,1000,10000,100000), # hard coding
                         labels = comma,
                         trans = trans,
                         guide = guide_colorbar(
                           ticks.colour = "black",
                           frame.colour = "black"))+
    coord_cartesian(xlim = limX,ylim = limY)+
    theme_linedraw()+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill=hcl(0,0,85)),
          axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1)))

  if(facet == 1){
    hist2d <- hist2d +
      facet_wrap(vars(get(.z[1])))

  } else if(facet == 2){
    hist2d <- hist2d +
      facet_grid(vars(get(.z[1])),vars(get(.z[2])))
  }

  hist2d <- hist2d +
    theme(strip.background = element_rect(fill = "white",color = "black"),
          strip.text = element_text(color = "black",size = rel(1.2)))

  if(hasLine==T){
    dtLm <- slr(dt,.x,.y,.z)
    dtLm[,label:= paste0("list(italic(Y)==",signif(b1,3),"*italic(X)",ifelse(b0>=0,"+",""),signif(b0,3),",italic(R^2)==",signif(r2,3),",",
                        "italic(n)==",n,")")]
    hist2d <- hist2d +
      geom_abline(data = dtLm, aes(intercept=b0,slope=b1))+
      geom_label(data = dtLm,
                 aes(x=mean(limX), y=Inf, label=label),
                 vjust="inward", hjust="inward",parse = T,
                 label.r = unit(0,"line"))
  }
  hist2d
}
