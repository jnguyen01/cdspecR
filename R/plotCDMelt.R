#' @title Thermal Melting Plot of Ellipticities
#'
#' @description a plot of the ellipticity values (at user-specified wavelength), as a function of
#' temperature.
#'
#' @param data the object outputted from \link{importCD} function.
#'
#' @param wavelength specific wavelength of interest for plotting ellipticities as a function
#' of temperature.
#'
#' @param col color of the points on the graph.
#'
#' @param pch shape of the points on the graph.
#'
#' @param ... passing arguments to \link{plot} function.
#'
#' @export
#'
#' @examples
#' \dontrun {
#'
#' protein <- importCD()
#' protein220nm <- plotCDMelt(protein, wavelength=220)
#'
#' }
#'
#

plotCDMelt <- function(data, wavelength, col="red", pch=16, ...) {

  #if(!inherits(data, "cd")) stop("data must  be class 'cd'")

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
  plot(temp, ellip,
       xlab=expression(paste("Temperature ", "(", degree,"C)")),
       ylab=c("Ellipticity (mdeg)", paste("@",wavelength, "nm")),
       col=col, pch=pch, ...)

  axis(side=1, at=temp[1:length(temp)])
  #cat("Q: At what temperatures do you assume fraction fully-folded and fully-unfolded? \n")
  final <- list(ellip, temp, info)
  names(final) <- c("ellips", "temp_celsius", "info")
  #class(final) <- c("cd", "list")
  return(final)

}














