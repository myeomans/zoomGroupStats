#' Time Windowing
#'
#' @description Produces a set of time windows for a conversation.
#' @param inputData data.table Transcript data file to be analysed   #$MIKE$ - data.table? data.frame?
#' @param inputType character Type of data input
#' @param windowSize numeric Size of windows.
#' @details This function takes in the transcript output of one of the other functions (either processZoomChat or processZoomTranscript).
#' @return  A list with two objects.
#' \item{inputData}{data.frame the input data.}
#' \item{windows}{vector The window assignments}
#'
#'@internal
makeTimeWindows = function(inputData, inputType, windowSize) {
  inputData=inputData[order(inputData$utterance_id), ] # $MIKE$ this should already be in order somewhere else
  if(inputType == "transcript") {

    inputData$window_id = NA
    inputData$window_start_seconds = NA
    inputData$window_end_seconds = NA
    count = 1
    for(i in ceiling(max(inputData$utterance_end_seconds)/windowSize):0) {
      window_start_seconds = i*windowSize
      window_end_seconds = (i+1)*windowSize

      inputData$window_id = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, (i+1), inputData$window_id)

      inputData$window_start_seconds = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, window_start_seconds, inputData$window_start_seconds)

      inputData$window_end_seconds = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, window_end_seconds, inputData$window_end_seconds)

      time_window_info = data.frame((i+1), window_start_seconds, window_end_seconds)

      names(time_window_info) = c("window_id", "window_start_seconds", "window_end_seconds")

      if(count == 1) {
        res.line = time_window_info
      } else {
        res.line = rbind(res.line, time_window_info)
      }
      count = count + 1
    }

  }
  return(list(inputData=inputData,
              windows=res.line[order(res.line$window_id), ]))
}
