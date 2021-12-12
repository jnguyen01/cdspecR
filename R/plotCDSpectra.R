#' @title Overlay of All CD Spectra
#'
#' @description overlay all circular dichroism spectras
#'
#' @param data an object of class 'cd'
#'
#' @param ... passing arguments to \code{plot} function.
#'
#' @export
#'
#' @return a plot
#'
#' @importFrom cultevo temperature.colors
#'
#' @examples
#'
#' \dontrun {
#'
#' #import data
#' protein <- importCD()
#'
#' #create spectras
#' plotCDSpectra(protein)
#'
#' }
#'
#'
#'

plotCDSpectra <- function(data, ...) {

  if(!inherits(data, "cd")) stop("data must  be class 'cd'")

  df <- data[[1]]
  info <- data[[2]]

  seq_wave <- seq(info[2], info[1], by=info[3])
  seq_temp <- seq(info[4], info[5], by=info[6])

  #seq_wave <- select()

  plot(seq_wave, df[,1], type="n", ylab="Ellipticity (mdeg)", xlab="Wavelength (nm)", xaxt="n",
       main=paste("CD Spectra for Cell", info[7]), ylim=c(min(df), max(df)), ...)
  axis(side=1, at=seq_wave)

  #col <- rainbow(length(seq_temp))
  #col <- rev(heat.colors(length(seq_temp)))
  col <- temperature.colors(length(seq_temp))[-1]
  dim(col) <- length(seq_temp)
  rownames(col) <- seq_temp

  for(i in as.character(seq_temp)) {

    lines(seq_wave, df[,i], col=col[i])

  }

  legend("topleft", legend=paste(seq_temp, "°C"), fill=col, bty="n", cex=0.5, ncol=6)




  }
