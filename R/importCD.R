#' @title Import Circular Dichroism Files From Jasco J-810
#'
#' @description
#' This is a user-friendly, importing function that takes CSV files exported from
#' the Jasco J-810 Circular Dichroism Spectropolarimeter.
#'
#' @param files path to where csv files for the CD data are located. If missing, the
#' user is prompted to select files interactively.
#'
#' @export
#'
#' @return a list containing a data frame and an atomic vector
#'
#' @importFrom readr read_csv
#'
#' @examples
#'
#' \dontrun{
#'
#' #selecting file interactively
#' protein <- importCD()
#'
#' }
#'
#'

 importCD <- function(files) {

  if(missing(files)) files <- choose.files()

  info <- infoCD(file=files[1])

  pattern <- seq(info[2], info[1], by=info[3])

  tbl_lst <- lapply(files,
                    function(x) readr::read_csv(x,
                                         col_names = FALSE,
                                         skip=19,
                                         skip_empty_rows = TRUE,
                                         n_max =(length(pattern)),
                                         show_col_types=FALSE
                    ))


  #set up data frame w/ wavelengths as first column
  df <- data.frame(wavelength=rev(pattern))

  #get rid of voltage column
  tbl_lst_mod <- lapply(tbl_lst, function(x) x[-3])

  #append ellipicity to main_df
  for (i in tbl_lst_mod) df <- cbind(df, i[[2]])

  #all # of temperatures in data
  temp_names <- seq(info[4], info[5], by=info[6])

  #get rid of wavelengths in first column
  main_df <- df[,-1]

  #rename columns to match temperature
  colnames(main_df) <- temp_names[1:length(files)]

  #rename rows to match wavelengths
  rownames(main_df) <- df[,1]

  #list of sorted df and info
  final <- list(main_df, info)

  class(final) <- c("cd", "list")

  return(final)

}
