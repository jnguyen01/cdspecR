#' @title Melting Data Plot
#'
#' @description A plot of the ellipticity values at a given wavelength over
#' temperature.
#'
#' @param data a class object 'cd'
#'
#' @param wavelength specific wavelength of interest for plotting multiplicities as a function
#' of temperature
#'
#' @param col color of the points (col="red" is default)
#'
#' @param pch shape of the points (pch=16 is default)
#'
#' @param ... passing arguments to \link{plot} function
#'
#' @export
#'
#' @seealso
#'
#'
#'
#

plotCDMelt <- function(data, wavelength, title, col="red", pch=16, ...) {

  if(!inherits(data, "cd")) stop("data must  be class 'cd'")

  if(missing(wavelength)) {
  stop("please put in specific wavelength")
  }

  else
    wavelength <- as.character(substitute(wavelength))

  df <- data[[1]]
  info <- data[[2]]
  #temp <- seq(info[4], info[5], by=info[6])
  temp <- as.numeric(names(df))
  ellip <- unlist(df[wavelength,])

  #Plot of Melting CD @ Specific Wavelength
  plot(temp, ellip, main=title,
       xlab=expression(paste("Temperature ", "(", degree,"C)")),
       ylab=c("Ellipticity (mdeg)", paste("@",wavelength, "nm")),
       col=col, pch=pch, ...)

  axis(side=1, at=temp[1:length(temp)])
  #cat("Q: At what temperatures do you assume fraction fully-folded and fully-unfolded? \n")
  final <- list(ellip, temp, info)
  names(final) <- c("ellips", "temp_celsius", "info")
  class(final) <- c("vh", "list")
  return(final)

}














