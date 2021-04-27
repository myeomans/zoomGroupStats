
#' Time Calculator
#'
#' @description Function to parse the time to create a total number of seconds
#' @param incTime character time string
#' @return numeric total seconds
#' @keywords internal
#'
timeCalc = function(incTime) { # $MIKE$ we should use an actual package for UTC to do this
  inc_hours = as.numeric(substr(incTime,1,2))
  inc_mins = as.numeric(substr(incTime,4,5))
  inc_secs = as.numeric(substr(incTime,7,12))
  inc_total_secs = inc_hours*60*60 + inc_mins*60 + inc_secs
  return(inc_total_secs)
}


#' Date Calculator from Date
#'
#' @description transforms the Zoom output for a timestamp into YYYY-MM-DD HH:MM:SS
#' @param oldDate character date string
#' @return character new format for date
#' @keywords internal
#'
convDate = function(oldDate) {
  month = substr(oldDate,1,2)
  day = substr(oldDate,4,5)
  year = substr(oldDate,7,10)
  hour = substr(oldDate,12,13)
  min = substr(oldDate,15,16)
  sec = substr(oldDate,18,19)
  tod = substr(oldDate,21,22)
  if(tod == "PM" && as.numeric(hour) < 12) {
    hour = as.character((as.numeric(hour) + 12))
  }
  newDate = paste(year,"-", month, "-", day, " ", hour,":",min,":",sec, sep="")
  return(newDate)
}
