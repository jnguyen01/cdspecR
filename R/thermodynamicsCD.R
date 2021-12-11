#' @title Calculating Thermodynamic Parameters
#'
#' @description This function automates the calculation of fraction unfolded from the ellipicities in a two-state model,
#' equilibrium constant, and thermodynamics of the system. Additionally, a Van't Hoff plot is provided with enthalpy,
#' entropy, and melting temperature values.
#
#' @param data a list containing ellipticities, temperatures in Celsius, and information about the experiment.
#' (i.e, output of plotCDMelt() function)
#'
#' @param fully_folded_temp temperature (in Celsius) at which the biomolecule is assumed fully-folded.
#'
#' @param fully_unfolded_temp temperature (in Celsius) at which the biomolecule is assumed denatured.
#'
#' @param remove.temp remove data points from the Van't Hoff plot (see example below)
#'
#' @param digits how many digits shown for slope, y-intercept, enthalpy and entropy.
#'
#' @param ... passing the arguments to \link{plot} function
#'
#' @export
#'
#' @import dplyr
#'

thermodynamicsCD <- function(data, fully_folded_temp, fully_unfolded_temp, remove.temp=NULL, digits=3, ...) {

  suppressMessages(require(dplyr))

  if(missing(fully_folded_temp) | missing(fully_unfolded_temp)) {

    fully_folded_temp <- data$info[4]

    fully_unfolded_temp <- data$info[5]

    }

  fully_folded_temp <- as.numeric(substitute(fully_folded_temp))
  fully_unfolded_temp <- as.numeric(substitute(fully_unfolded_temp))

  folded_unfolded_range <- seq(fully_folded_temp, fully_unfolded_temp, by=data$info[6])

  df <- data.frame(temp_celsius=data$temp_celsius, ellips=data$ellips, check.names=FALSE) %>%
    filter(temp_celsius %in% folded_unfolded_range) %>%
    mutate((1/(temp_celsius+273)), .before=2)

  colnames(df)[2] <- "1/temp_kelvin"

  #Fractional Change in Unfolded Protein
  folded_elip <- df[as.character(fully_folded_temp), "ellips"]
  unfolded_elip <- df[as.character(fully_unfolded_temp), "ellips"]
  delta <- unfolded_elip - folded_elip

  #Fraction Unfolded at Each Temp
  df <- df %>% mutate(FractionUnfolded = (ellips - folded_elip) / (delta)) %>%
    mutate(Keq = (FractionUnfolded/(1-FractionUnfolded))) %>%
    mutate(lnKeq = log(Keq)) %>% filter(is.finite(lnKeq))

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

  #Van't Hoff Plot
  plot(df$`1/temp_kelvin`, df$lnKeq,
       xlab= "1/T (1/K)", ylab="ln(Keq)", main="Van't Hoff Equation",
       col="blue", pch=16, ...)
  #adding line of best fit
  abline(fit, col="red", lwd=1.1)
  #adding temperatures in celsius above each point
  text(df$`1/temp_kelvin`, df$lnKeq, labels=rownames(df), cex=0.8, pos=1)
  #adding thermodynamic parameters onto plot
  legend("topright", legend=paste("Model: y = ", round(slope, digits) , "x +", round(intercept, digits), "\n",
                                  "Enthalpy: ", enthalpy, " J mol-1 \n",
                                  "Entropy: ", entropy, " J K-1 \n",
                                  "Melting Temp.: ", Tm, "°C \n",
                                  "R.Squared:", rsquared, sep=""), cex=0.75, bty="n")

  #return(df)




}

