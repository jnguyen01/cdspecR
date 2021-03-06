#' @title Melting Temperature Graph of All CD Spectras
#'
#' @description This function returns overlays all the CD  in one graph.
#'
#' @param data object outputted from \link{infoCD} function.
#'
#' @param show.legend shows the legend of the melting temperature. Set show.legend=FALSE to hide.
#'
#' @param ... passing arguments to \code{plot} function.
#'
#' @export
#'
#' @return a color-coded plot of each CD spectras on one graph.
#'
#' @importFrom cultevo temperature.colors
#'
#' @examples
#'
#' \dontrun{
#' #import data
#' protein <- importCD()
#' #create spectras
#' plotCDSpectra(protein)
#' }

spectra <- function(data, show.legend=TRUE, ...) {

  if(!inherits(data, "cd")) stop("data must  be class 'cd'")

  df <- data[[1]]
  info <- data[[2]]

  seq_wave <- rev(seq(info[2], info[1], by=info[3]))
  seq_temp <- seq(info[4], info[5], by=info[6])

  #seq_wave <- select()

  plot(seq_wave, rev(df[,1]), type="n", ylab="Ellipticity (mdeg)", xlab="Wavelength (nm)",
       ylim=c(min(df), max(df)), ...)
  #axis(side=1, at=seq_wave)

  #col <- rainbow(length(seq_temp))
  #col <- rev(heat.colors(length(seq_temp)))
  col <- cultevo::temperature.colors(length(seq_temp))[-1]
  dim(col) <- length(seq_temp)
  rownames(col) <- seq_temp

  for(i in as.character(seq_temp)) {

    lines(seq_wave, df[,i], col=col[i])

  }

  if(show.legend!=FALSE){
  legend("topright", legend=paste(seq_temp, "°C"), col=col, bty="n", lty=1, lwd=1.2, cex=0.6, ncol=6)
  }

  }
