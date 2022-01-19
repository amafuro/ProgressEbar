#' @title ProgressEbar
#' @description \code{Progress.Ebar} Setting the progress bar parameters
#'
#' @importFrom insight color_text
#' @param max set the max loop numbers
#' @param char using style1,2,3.   only half-width alphanumeric character
#' @param char.color "red", "yellow", "green", "blue", "violet", "cyan" or "grey"
#' @param bgchar using style3
#' @param text using style4.
#' @export
#' @examples
#' #n<-100
#' #pe<-Progress.Ebar(max = n，style＝3)
Progress.Ebar<- function(min = 1, max = 1,style = 3,
                         char = "/", char.color = "cyan",
                         bgchar="/", bgchar.color = "red",
                         text = "逃げちゃだめだ ",text.color = "blue") {
  initial = 0
  width = NA
  file = ""
  if (!identical(file, "") && !(inherits(file, "connection") &&
                                isOpen(file)))
    stop("'file' must be \"\" or an open connection object")
  if (!style %in% 1L:4L)
    style <- 1
  .val <- initial
  .killed <- FALSE
  .nb <- 0L
  .pc <- -1L
  nw <- nchar(char, "w")
  if (is.na(width)) {
    width <- getOption("width")
    if (style == 3L)
      width <- width - 10L
    width <- trunc(width/nw)
  }
  if (max <= min)
    stop("must have 'max' > 'min'")
  up1 <- function(value) {
    if (!is.finite(value) || value < min || value > max)
      return()
    .val <<- value
    nb <- round(width * (value - min)/(max - min))
    if (.nb < nb) {
      cat(insight::color_text(strrep(char, nb - .nb),char.color), file = file)
      flush.console()
    }
    else if (.nb > nb) {
      cat("\r", strrep(" ", .nb * nw), "\r",
          strrep(char, nb), sep = "", file = file)
      flush.console()
    }
    .nb <<- nb
  }
  up2 <- function(value) {
    if (!is.finite(value) || value < min || value > max)
      return()
    .val <<- value
    nb <- round(width * (value - min)/(max - min))
    if (.nb <= nb) {
      cat("\r",(insight::color_text(strrep(char, nb), char.color)), sep = "",
          file = file)
      flush.console()
    }
    else {
      cat("\r", strrep(" ", .nb * nw), "\r",
          strrep(char, nb), sep = "", file = file)
      flush.console()
    }
    .nb <<- nb
  }
  up3 <- function(value) {
    if (!is.finite(value) || value < min || value > max)
      return()
    .val <<- value
    nb <- round(width * (value - min)/(max - min))
    pc <- round(100 * (value - min)/(max - min))
    if (nb == .nb && pc == .pc)
      return()
    cat(paste0("\r  |", insight::color_text(strrep(" ", nw * width +
                                                     6),char.color)), file = file)
    cat(paste(c("\r  |", insight::color_text(rep.int(char, nb),char.color), insight::color_text(rep.int(bgchar,
                                                                                                        nw * (width - nb)),bgchar.color), sprintf("| %3d%%", pc)),
              collapse = ""), file = file)
    flush.console()
    .nb <<- nb
    .pc <<- pc
  }
  up4 <- function(value) {
    if (!is.finite(value) || value < min || value > max)
      return()
    .val <<- value
    nw <- nchar(text, "w")
    width <- trunc(width/nw)
    nb <- round(width * (value - min)/(max - min))
    pc <- round(100 * (value - min)/(max - min))
    if (nb == .nb && pc == .pc)
      return()
    cat(paste0("\r  |", strrep("　", nw * width +
                                 6)), file = file)
    cat(paste(c("\r Progress:%3d%　", insight::color_text(rep.int(text, nb),text.color),
                rep.int("", nw * (width - nb)), sprintf("\r Progress:%3d%%", pc)),
              collapse = ""), file = file)
    flush.console()
    .nb <<- nb
    .pc <<- pc
  }
  getVal <- function() .val
  kill <- function() if (!.killed) {
    cat("\n", file = file)
    flush.console()
    .killed <<- TRUE
  }
  up <- switch(style, up1, up2, up3, up4)
  up(initial)
  structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")

}
#' @title set.PE
#' @description \code{set.PE} Set up a progress bar
#' @export
#' @examples
#' #n<-100
#' #pe<-Progress.Ebar(min = 1,max = n)
#' #for (i in seq(n)) {
#' #set.PE(pe,i)
#' #Sys.sleep(1 / 100)
#' #}




set.PE<-function (pe, value) {
  title = NULL
  label = NULL
  if (!inherits(pe, "txtProgressBar"))
    stop(gettextf("'pe' is not from class %s", dQuote("txtProgressBar")),
         domain = NA)
  oldval <- pe$getVal()
  pe$up(value)
  invisible(oldval)
}

#' @title PW.G.labeling
#' @description \code{PW.G.labeling} pw is patchworked objects
#' @importFrom patchwork patchworkGrob
#' @export
#' @examples
#' #library(Progress.Ebar)
#' #plotPW<-p1+p2+p3+p4
#' #PW.G.labeling(pw=plotPW,titlelab="Pokemon",xlab="name",ylab="HP")

PW.G.labeling<-function(pw=NULL,titlelab="title",xlab="xlab",ylab="ylab",
                        titlefont=25,xfont=20,yfont=20,xhigh=0.3){
  gt<-patchwork::patchworkGrob(pw)
  tl=grid::textGrob(titlelab,gp=grid:gpar(fontsize=titlefont))
  ll=grid::textGrob(ylab,gp=grid:gpar(fontsize=yfont),rot = 90)
  bl=grid::textGrob(xlab,gp=grid:gpar(fontsize=xfont),vjust = xhigh)
  pwf<-gridExtra::grid.arrange(gt,top=tl,left=ll,bottom=bl)
  print(pwf)
}

