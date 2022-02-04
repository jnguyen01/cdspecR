#' @title Calculating Thermodynamic Parameters using van't Hoff (VH) Equation
#'
#' @description This function requires the user to specify the start and end temperature (in Celsius)
#' range for the transition curve, where a plot of natural log of equilibrium constants versus 1/Temperature (in Kelvin).
#
#' @param data an object of class 'vh'; outputted from \link{analyzeMelt} function.
#'
#' @param start temperature, in celsius, of the start of the transition curve.
#'
#' @param end temperature, in celsius, of the end of the transition curve.
#'
#' @param digits how many digits shown for thermodynamic parameters.
#'
#' @param show.legend shows the thermodynamic parameters using the Van't Hoff. Set show.legend=FALSE to
#' hide the legend.
#'
#' @param ... passing arguments to \link{plot} function.
#'
#' @export
#'
#' @import dplyr
#' @import greekLetters
#' @importFrom graphics abline axis lines text legend
#' @importFrom stats coefficients lm
#'
#' @examples
#' \dontrun{
#' protein <- importCD() %>%
#' plotCDMelt(pwavelength=210) %>%
#' analyzeCDMelt()
#'}
#'
#'

thermodynamics<- function(data, start, end, digits=3, show.legend=TRUE, ...) {

  suppressMessages(require(dplyr))
  suppressMessages(require(greekLetters))

  if(!inherits(data, "vh")) stop("data must  be class 'vh'")

  transitioncurve <- seq(start, end, by=data$info[6])

  df <- filter(data$data, temp_celsius %in% transitioncurve)

  # Linear Regression

  fit <- lm(df$lnKeq ~ df$`1/temp_kelvin`)
  slope <- coefficients(fit)[2]
  intercept <- coefficients(fit)[1]
  sum_fit <- summary(fit)
  rsquared <- round(sum_fit$r.squared, 3)

  # Thermodynamic Parameters

  # gas constant | Units: J mol-1 K-1
  R <- 8.3145
  # enthalpy | Units: J mol-1
  enthalpy <- round((-slope * R), digits)
  # entropy | Units: J K-1
  entropy <- round((intercept * R), digits)
  #melting temp in celsius
  Tm <- round((enthalpy/entropy) - 273, 2)

  #ddG calculations
  dGfolded <- (enthalpy - (entropy*(start+273)))
  dGunfolded <- (enthalpy - (entropy*(end+273)))
  ddG <- round(dGunfolded - dGfolded, digits)/1000


  #Van't Hoff Plot
  plot(df$`1/temp_kelvin`, df$lnKeq,
       xlab= "1/T (1/K)", ylab="ln(Keq)",
       col="blue", pch=16, ...)
  #adding line of best fit
  abline(fit, col="red", lwd=1.1)
  #adding temperatures in celsius above each point
  text(df$`1/temp_kelvin`, df$lnKeq, labels=rownames(df), cex=0.8, pos=1)
  #adding thermodynamic parameters onto plot

  #plot legend if true
  if(show.legend!=FALSE) {
    legend("topright", legend=c(paste("ln(Keq)= ", round(slope, digits) , "(1/T) +", round(intercept, digits)),
                                as.expression(bquote(R^2 ~ ": " ~ .(rsquared))),
                                as.expression(bquote(Delta ~ "H :" ~ .(enthalpy/1000) ~"kJ" ~ mol^-1)),
                                as.expression(bquote(Delta ~ "S :" ~.(entropy) ~ "J" ~ K^-1 ~ mol^-1)),
                                as.expression(bquote(T[m] ~ ": "~ .(Tm)~"°C")),
                                as.expression(bquote(Delta ~ Delta ~ G[unfolded-folded] ~ ": " ~ .(ddG) ~ "kJ " ~ mol^-1))),
           cex=0.75, bty="n")
  }

  thermo <- vector()

  thermo <- c(thermo, enthalpy/1000, entropy, dGfolded/1000, dGunfolded/1000, ddG, Tm)

  names(thermo) <- c(paste(greek$Delta, "H (kJ/mol)"),
                     paste(greek$Delta, "S (J/mol K)") ,
                     paste(greek$Delta, "G_folded (kJ/mol)"),
                     paste(greek$Delta, "G_unfolded (kJ/mol)"),
                     paste(greek$Delta, greek$Delta,  "G (kJ/mol)"),
                     "Melting Temp. (°C)")



  lst <- list(thermo, df)

  return(lst)

}

