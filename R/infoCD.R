#' @title Extract Information from a CD Melting Data File
#'
#' @description
#'
#' The info function extracts experimental parameters in the CSV files exported
#' from Jasco J-810 Circular Dichroism Spectropolarimeter
#'
#' @param file a single CSV file from the Jasco J-810 CD experiment
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

infoCD <- function(file) {

  if(missing(file)) file <- file.choose()

  x <- readLines(file, n=19)
  x_extended <- readLines(file)

  #title <- substr(x[1], start=7, stop=100)
  #title <- gsub("TITLE,", "\\1", x[1])
  #date <- substr(x[5], start=6, stop=100)
  #time <- substr(x[6], start=6, stop=13)
  #start_wave <- substr(x[13], start=10, stop=100)
  start_wave <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x[13]))
  end_wave <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x[14]))
  #end_wave <- substr(x[14], start=9, stop=100)
  #units <- substr(x[11], start=8, stop=17)

  for(i in seq(x_extended)){

    if(x_extended[i]=="[Detailed Information]")
      x_extended_v2 <- readr::read_lines(file, skip=i)
    }

  #date <- substr(x_extended_v2[1], start=15, stop=35)
  #cell <- substr(x_extended_v2[19], start=8, stop=9)
  cell <- as.numeric(gsub(".?([0-9]?)", "\\1", x_extended_v2[19]))
  #wave_interval <- data_pitch
  #wave_interval <- substr(x_extended_v2[9], start=15, stop=18)
  wave_interval <- as.numeric(gsub(".?([0-9].)?", "\\1", x_extended_v2[9]))
  start_temp <- as.numeric(gsub(".?([0-9]+)?", "\\1", x_extended_v2[38]))
  final_temp <- as.numeric(gsub(".?([0-9]+)?", "\\1", x_extended_v2[39]))
  #final_temp <- substr(x_extended_v2[39], start=20, stop=22)
  #start_temp <- substr(x_extended_v2[38], start=18,stop=20)
  #temp_interval <- substr(x_extended_v2[40], start=23, stop=23)
  temp_interval <- as.numeric(gsub(".?([0-9]+)?", "\\1", x_extended_v2[40]))

  important <-  data.frame(start_wave=start_wave,
                    end_wave=end_wave,
                    wave_interval=wave_interval,
                    start_temp=start_temp,
                    end_temp=final_temp,
                    temp_interval=temp_interval,
                    cell=cell)

  important <-  sapply(important, as.vector)
  return(important)
}

