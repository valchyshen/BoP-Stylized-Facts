# My keys & labels
bea.api.Key  <- "E5EBB4AD-4E48-4EC7-B63A-73BAFDE57869"   # API code for BEA library
fred.api.key <- "741b5fa267d09bae576b2f83bc3cdf3b"       # API code for FredR library
my_watermark <- "@AlexValchyshen"
my_label     <- paste0("(c) ",format(Sys.Date(),"%Y"), " Alexander Valchyshen, twitter: @AlexValchyshen.")
my_label2    <- paste0("Produced by: ","@AlexValchyshen (https://twitter.com/AlexValchyshen), ",format(Sys.Date(),"%Y"),".")

# Data libraries
library(bea.R); 
library(fredr); fredr_set_key(fred.api.key)              # Registering with FredR
library(BISdata)                                         # BIS
library(imfr)                                            # IMF
#???                                                     # World Bank
library(ecb)                  # https://cran.r-project.org/web/packages/ecb/ecb.pdf
library(ustyc)                # https://github.com/mrbcuda/ustyc
library(CBRT)                 # Central Bank of Turkey

library(ggmap)
register_google(key = "AIzaSyAJoaOFcJ2f2jJC05Aff9gCqUwnGXCMBIw")

# System libraries
library(RSelenium)

# Dataset & Plot libraries
library(rlang)
library(gdata)
library(pipeR)
library(dplyr)
library(gplots)
library(ggplot2)
library(ggvis)
library(plotly)
library(ggplotify)
library(lubridate)
library(ggflags)
library(ggside)
library(ggthemes)
library(ggExtra)
library(ggpubr)
library(ggdendro)
library(ggraph)
library(tidygraph)
library(purrr)
library(gganimate)
library(ggimage)
library(magick)
library(cowplot)
library(RColorBrewer)
library(ggHoriPlot)        # Source: https://rivasiker.github.io/ggHoriPlot/
library(metR)              # https://cran.r-project.org/web/packages/metR/index.html
                           # metR example: https://eliocamp.github.io/codigo-r/en/2021/09/contour-labels/
library(rvest)             # https://github.com/tidyverse/rvest
library(terrainr)
library(sf) 
library(tidyverse)
library(gridExtra)
library(rgdal)
library(tmap)
library(patchwork)
library(ggbump)
library(ggstream)
library(painbow)           # https://github.com/steveharoz/painbow/?1
library(ggpval)            # https://cran.r-project.org/web/packages/ggpval/vignettes/ggpval.html
library(qqplotr)
library(qqboxplot)
library(GGally)            # http://vita.had.co.nz/papers/gpp.pdf, http://ggobi.github.io/ggally/

# Markdown libraries
library(bookdown)
library(knitr)
library(kableExtra)
library(stringr)
library(strucchange)
library(latex2exp)         # Using LaTex features in plot(), ggplot(), etc.

# Statistical & Econometric libraries
library(RCurl)
library(XML)
library(xml2)
library(magrittr)
library(xlsx)
library(rJava)
library(RJDemetra)
library(STAT)
library(seasonal)
library(forecast)
library(TTR)
library(urca)
library(readxl) 
library(writexl)
library(zoo)
library(lmtest)
library(tseries)
library(psych)
library(scales)
library(stringr)
library(stats)
library(timetk)
library(SuperLearner)# Baumann et al (2021) https://www-degruyter-com.proxy.library.umkc.edu/document/doi/10.1515/jci-2020-0016/html
library(mgcv)        # Simon Wood (2017) General Additive Models.
library(stats4)      # this is required for next library
library(cAIC4)       # from thw working paper 'What causes inflation?'
library(lme4)        # see below
library(gamm4)       # Wood, S. and Scheipl, F. (2017) gamm4: Generalized Additive Mixed Models... 
                     # ...using 'mgcv' and 'lme4'. URL: https://CRAN.R-project.org/package=gamm4.
library(dendextend)  # Extended dendrogram package
library(heatmap3)    # Enhanced Heatmap Representation w Dendrogram & Partition
library(twistR)      # https://github.com/lukepilling/twistR
library(modelsummary)     # https://github.com/vincentarelbundock/modelsummary
library(marginaleffects)  # https://vincentarelbundock.github.io/marginaleffects/
library(bradfordr)        # https://github.com/victordogo/bradfordr
library(sfcr)             # Stock-Flow Consistent R library https://github.com/joaomacalos/sfcr
library(dmetar)           # book: https://www.routledge.com/Doing-Meta-Analysis-with-R-A-Hands-On-Guide/Harrer-Cuijpers-Furukawa-Ebert/p/book/9780367610074
library(furrr)
# Vocabulary size measuring, etc.
library(zipfR)            # http://zipfr.r-forge.r-project.org/materials/zipfr-tutorial.pdf
# times series packages
# example: https://www.r-bloggers.com/2021/10/tidy-time-series-forecasting-in-r-with-spark/
library(sparklyr)
library(tidymodels)
library(modeltime)
library(timetk)
# Tableau
# https://www.business-science.io/code-tools/2021/03/23/ggplot-code-with-tableau-esquisse.html 
library(esquisse)         
library(ahead)            # https://t.co/2dw7eFDEvy?amp=1
library(equatiomatic)     # The {equatiomatic} package is so so helpful for documentation. 
# Converts model objects into their formulas, algebraic or with coefficient estimates.  
# Highly recommended.

# Text analysis
library(tm)
library(syuzhet) # https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
library(pdftools)
library(pdfminer)
library(SnowballC)

#------------------------------------------------------------------------------#
# Load required packages and source all programs to be used in HLW estimation.
#------------------------------------------------------------------------------#
if (!require("tis")) {install.packages("tis"); library("tis")} # Time series package
if (!require("mFilter")) {install.packages("mFilter"); library("mFilter")} # HP filter
if (!require("nloptr")) {install.packages("nloptr"); library("nloptr")} # Optimization

#------------------------------------------------------------------------------#
#' Add text with background box to a plot
#'
#' \code{boxtext} places a text given in the vector \code{labels} 
#' onto a plot in the base graphics system and places a coloured box behind 
#' it to make it stand out from the background.
#' 
#' @param x numeric vector of x-coordinates where the text labels should be 
#' written. If the length of \code{x} and \code{y} differs, the shorter one 
#' is recycled.
#' @param y numeric vector of y-coordinates where the text labels should be 
#' written. 
#' @param labels a character vector specifying the text to be written.
#' @param col.text the colour of the text 
#' @param col.bg color(s) to fill or shade the rectangle(s) with. The default 
#' \code{NA} means do not fill, i.e., draw transparent rectangles.
#' @param border.bg color(s) for rectangle border(s). The default \code{NA}
#' omits borders. 
#' @param adj one or two values in [0, 1] which specify the x (and optionally 
#' y) adjustment of the labels. 
#' @param pos a position specifier for the text. If specified this overrides 
#' any adj value given. Values of 1, 2, 3 and 4, respectively indicate 
#' positions below, to the left of, above and to the right of the specified 
#' coordinates.
#' @param offset when \code{pos} is specified, this value gives the offset of 
#' the label from the specified coordinate in fractions of a character width.
#' @param padding factor used for the padding of the box around 
#' the text. Padding is specified in fractions of a character width. If a 
#' vector of length two is specified then different factors are used for the
#' padding in x- and y-direction.    
#' @param cex numeric character expansion factor; multiplied by 
#' code{par("cex")} yields the final character size. 
#' @param font the font to be used
#'
#' @return Returns the coordinates of the background rectangle(s). If 
#' multiple labels are placed in a vactor then the coordinates are returned
#' as a matrix with columns corresponding to xleft, xright, ybottom, ytop. 
#' If just one label is placed, the coordinates are returned as a vector.
#' @author Ian Kopacka
#' @examples
#' ## Create noisy background
#' plot(x = runif(1000), y = runif(1000), type = "p", pch = 16, 
#' col = "#40404060")
#' boxtext(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480", 
#'     pos = 4, font = 2, cex = 1.3, padding = 1)
#' @export
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA, 
                    border.bg = NA, adj = NULL, pos = NULL, offset = 0.5, 
                    padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){
  
  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex
  
  ## Is y provided:
  if (missing(y)) y <- x
  
  ## Recycle coords if necessary:    
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]           
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }       
  }
  
  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)
  
  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)
  
  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)            
    }        
  } else {
    adj <- c(0.5, 0.5)
  }
  
  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }       
  } else {
    offsetVec <- c(0, 0)
  }
  
  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }
  
  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]
  
  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth    
  graphics::rect(xleft = xMid - rectWidth/2, 
                 ybottom = yMid - rectHeight/2, 
                 xright = xMid + rectWidth/2, 
                 ytop = yMid + rectHeight/2,
                 col = col.bg, border = border.bg)
  
  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font, 
                 adj = c(0.5, 0.5))    
  
  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }    
}