#' @title Calculation of Fraction Unfolded from Native to Denature State
#'
#' @description This function calculates the fraction unfolded, equilibrium constants, and natural log of
#' equilibrium constants from the native state to the denatured state. Afterwards, a plot of the fraction unfolded as
#' a function of temperature is given.
#'
#' @param data list outputted from \link{plotCDMelt} function.
#'
#' @param folded_temp temperature, in celsius, where the biomolecule is assumed fully-folded.
#'
#' @param unfolded_temp temperature, in celsius, where the biomolecule is assumed fully-unfolded.
#'
#' @param pch the shape of the data points.
#'
#' @param ... passing arguments to \link{plot} function.
#'
#' @export
#'
#' @importFrom stats loess predict
#'
#' @examples
#' \dontrun{
#'
#' protein <- importCD()
#'
#' protein220nm <- plotCDMelt(protein, wavelength=220)
#'
#' analyzeCDMelt(protein220nm, folded_temp=15, unfolded_temp=95)
#'
#'
#' }
#'
#'
#'


analyzeCDMelt <- function(data, folded_temp, unfolded_temp, pch=16, ...) {

suppressMessages(require(dplyr))
suppressMessages(require(greekLetters))

#if(!inherits(data, "vh")) stop("data must  be class 'vh'")

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

lo <- stats::loess(df$FractionUnfolded ~ df$temp_celsius)

plot(df$temp_celsius, df$FractionUnfolded,
     xlab="Temperature (°C)",
     ylab="Fractional Unfolded", pch=pch, ...)

lines(stats::predict(lo), col="red", lwd=2)

lst <- list(df, data$info)

class(lst) <- c("vh", "list")

names(lst) <- c("data", "info")

return(lst)

}
