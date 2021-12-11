#' @title Extract Information from a CD Melting Data File
#'
#' @description
#'
#' The info function extracts experimental parameters in the CSV files exported
#' from Jasco J-810 Circular Dichroism Spectropolarimeter
#'
#' @param file a single CSV file from the Jasco J-810 CD experiment
#'
#' @param params prints information about the experiment
#'
#' @param data_pitch interval for the wavelengths
#'
#' @return an atomic vector
#'
#' @importFrom readr read_lines
#'
#' @examples
#'
#' \dontrun{
#
#'  details <- info()
#'
#' }
#'

infoCD <- function(file, data_pitch, params=FALSE) {

  if(missing(file)) file <- file.choose()

  x <- readLines(file, n=19)
  x_extended <- readLines(file)

  title <- substr(x[1], start=7, stop=100)
  #date <- substr(x[5], start=6, stop=100)
  #time <- substr(x[6], start=6, stop=13)
  start_wave <- substr(x[13], start=10, stop=100)
  end_wave <- substr(x[14], start=9, stop=100)
  #units <- substr(x[11], start=8, stop=17)

  for(i in seq(x_extended)){

    if(x_extended[i]=="[Detailed Information]")
      x_extended_v2 <- readr::read_lines(file, skip=i)
    }

  date <- substr(x_extended_v2[1], start=15, stop=35)
  cell <- substr(x_extended_v2[19], start=8, stop=9)
  if(missing(data_pitch)){
    stop("Please put in data pitch value")
    }
  wave_interval <- data_pitch
  #wave_interval <- substr(x_extended_v2[9], start=15, stop=18)
  final_temp <- substr(x_extended_v2[39], start=20, stop=22)
  start_temp <- substr(x_extended_v2[38], start=18,stop=20)
  temp_interval <- substr(x_extended_v2[40], start=23, stop=23)


  important <-  data.frame(start_wave=as.numeric(start_wave),
                    end_wave=as.numeric(end_wave),
                    wave_interval=as.numeric(wave_interval),
                    start_temp=as.numeric(start_temp),
                    end_temp=as.numeric(final_temp),
                    temp_interval=as.numeric(temp_interval), cell=as.numeric(cell))

  important <-  sapply(important, as.vector)
  return(important)

  #Additional Information
  if(params==TRUE) {
  cat("Information about this CD Melt:")
  cat("\n File:", paste(basename(file)),"\n",
      "Experiment Title:", title, "\n",
      "Date of Experiment (MM/DD/YYYY) and Time:", date, "\n",
      "Cell:", cell, "\n")

    cat("\n")
    cat("Parameters For This CD Melt: \n",
      "Temperature Start (C):", start_temp, "\n",
      "Temperature Final (C):", final_temp, "\n",
      "Temperature Interval (C):", temp_interval, "\n",
      "Starting Wavelength(nm): ", start_wavelength, "\n",
      "Ending Wavelength(nm): ", end_wavelength,"\n",
      "Data Pitch/Interval(nm):", wave_interval)
}

  }

