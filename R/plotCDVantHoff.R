#' @title Calculating Thermodynamic Parameters using Van't Hoff Equation
#'
#' @description Assuming a two-state model of melting a protein, this function automates the calculation of fraction unfolded
#' from a fully-folded state to fully-unfolded state. At each temperature, equilibrium constants are derived and plotted based off of the
#' Van't Hoff equation. From this linear model, enthalpy, entropy, Gibbs free energy of the system, and melting temperature of the
#' system are estimated.
#
#' @param data an object of class 'vh'
#'
#' @param folded_temp temperature (in Celsius) at which the biomolecule is assumed fully-folded.
#' Default folded_temp is your first temperature reading.
#'
#' @param unfolded_temp temperature (in Celsius) at which the biomolecule is assumed fully-unfolded.
#' Default unfolded_temp is your final temperature reading.
#'
#' @param remove.temp remove specified data points from the linear regression model.
#'
#' @param digits how many digits shown for your thermodynamic parameters.
#'
#' @param ... passing arguments to \link{plot} function.
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun {
#'
#'
#'
#' protein <- importCD() %>%
#' plotCDMelt(wavelength=210) %>%
#' plotCDVantHoff(folded_temp=10, unfolded_temp=95)
#'
#'
#'
#' }
#'

plotCDVantHoff <- function(data, folded_temp, unfolded_temp, digits=3, remove.temp=NULL, title="Van't Hoff Equation", ...) {

  suppressMessages(require(dplyr))

  if(!inherits(data, "vh")) stop("data must  be class 'vh'")

  if(missing(folded_temp) | missing(unfolded_temp)) {

    folded_temp <- data$info[4]

    unfolded_temp <- data$info[5]

    }

  folded_temp <- as.numeric(substitute(folded_temp))
  unfolded_temp <- as.numeric(substitute(unfolded_temp))

  folded_unfolded_range <- seq(folded_temp, unfolded_temp, by=data$info[6])

  df <- data.frame(temp_celsius=data$temp_celsius, ellips=data$ellips, check.names=FALSE) %>%
  filter(temp_celsius %in% folded_unfolded_range) %>%
  mutate((1/(temp_celsius+273)), .before=2)

  colnames(df)[2] <- "1/temp_kelvin"

  #Fractional Change in Unfolded Protein
  folded_elip <- df[as.character(folded_temp), "ellips"]
  unfolded_elip <- df[as.character(unfolded_temp), "ellips"]
  delta <- unfolded_elip - folded_elip

  #Fraction Unfolded at Each Temp

  defaultW <- getOption("warn")
  options(warn = -1)

  df <- df %>% mutate(FractionUnfolded = (ellips - folded_elip) / (delta)) %>%
  mutate(Keq = (FractionUnfolded/(1-FractionUnfolded))) %>%
  mutate(lnKeq = log(Keq)) %>% filter(is.finite(lnKeq))

  options(warn = defaultW)

  ## Van't Hoff Equation

  # Remove data points on plot
  if(is.null(remove.temp)==FALSE){

    df <- filter(df, !(temp_celsius %in% remove.temp))
  }

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
  dGfolded <- (enthalpy - (entropy*(folded_temp+273)))
  dGunfolded <- (enthalpy - (entropy*(unfolded_temp+273)))
  ddG <- round(dGunfolded - dGfolded, digits)/1000


  #Van't Hoff Plot
  plot(df$`1/temp_kelvin`, df$lnKeq,
       xlab= "1/T (1/K)", ylab="ln(Keq)", ylim=c(min(df$lnKeq)*2, max(df$lnKeq)*2), main=title,
       col="blue", pch=16, ...)
  #adding line of best fit
  abline(fit, col="red", lwd=1.1)
  #adding temperatures in celsius above each point
  text(df$`1/temp_kelvin`, df$lnKeq, labels=rownames(df), cex=0.8, pos=1)
  #adding thermodynamic parameters onto plot
  legend("topright", legend=c(paste("y = ", round(slope, digits) , "x +", round(intercept, digits)),
                                  as.expression(bquote(R^2 ~ ": " ~ .(rsquared))),
                                  as.expression(bquote(Delta ~ "H :" ~ .(enthalpy/1000) ~"kJ" ~ mol^-1)),
                                  as.expression(bquote(Delta ~ "S :" ~.(entropy) ~ "J" ~ K^-1 ~ mol^-1)),
                                  as.expression(bquote(T[m] ~ ": "~ .(Tm)~"°C")),
                                  as.expression(bquote(Delta ~ Delta ~ G[unfolded-folded] ~ ": " ~ .(ddG) ~ "kJ " ~ mol^-1))), bty="n", cex=0.75)

  print(df)





}

