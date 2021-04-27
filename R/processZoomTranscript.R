#' Zoom Recording Transcript File Processing
#'
#' @description This function parses the data from the transcript file (.vtt) that is downloaded from the Zoom website.
#' @param fileName character the path to the local file where the transcript file (vtt) is saved.
#' @param recordingStartDateTime character the date/time that the meeting recording started $MIKE$ format?
#' @param languageCode character language code. Default is "en".
#' @details # Note: I plan to fix at a later point in time the timing issue. Specifically, it is not clear
#' where in Zoom's system I can get the actual time that the recording was started. This
#' is a problem for linking the transcript file up with the chat file.
#' One workaround for now (for research) would be to set recordings to auto-start. This is not ideal, though.
#' we should be able to know when the recording was started. It is embedded in the video, so could pull from there.
#' @return A list of analyses
#'    \item{utterance_id}{an incremented numeric identifier for a marked speech utterance}
#'    \item{utterance_start_seconds}{the number of seconds from the start of the recording (when it starts)}
#'    \item{utterance_start_time}{the timestamp for the start of the utterance}
#'    \item{utterance_end_seconds}{the number of seconds from the start of the recording (when it ends)}
#'    \item{utterance_end_time}{the timestamp for the end of the utterance}
#'    \item{utterance_time_window}{the number of seconds that the utterance took}
#'    \item{user_name}{the name attached to the utterance}
#'    \item{utterance_message}{the text of the utterance}
#'    \item{utterance_language}{the language code for the transcript}
#' @examples
#'
#' \dontrun{
#' tr.out = processZoomTranscript(fileName="~/Desktop/transcript.vtt",
#' recordingStartDateTime="2020-04-01 17:56:34",
#' languageCode="en")
#' }
#'
#'@import data.table
#'@export
processZoomTranscript = function(fileName, recordingStartDateTime, languageCode="en") {

	# Parse the transcript file -- vtt is a structured format.
	f = readLines(fileName)

	# there are three main pieces of data for each marked "utterance" - an id, a window of time, and the text
	utterance_id = as.integer(f[seq(3,length(f), 4)])
	utteranceWindow = f[seq(4,length(f), 4)]
	utteranceText = f[seq(5,length(f), 4)]

	# Parse the time window into two separate elements
	utterance_start_time = unlist(strsplit(utteranceWindow, " --> "))[seq(1, length(utteranceWindow)*2, 2)]
	utterance_end_time = unlist(strsplit(utteranceWindow, " --> "))[seq(2, length(utteranceWindow)*2, 2)]

	# Preserve this to use in a dynamic conversation analysis
	utterance_start_seconds = timeCalc(utterance_start_time)
	utterance_end_seconds = timeCalc(utterance_end_time)

	# Now turn these into actual datetime values
	recordingStartDateTime = as.POSIXct(recordingStartDateTime)
	utterance_start_time = recordingStartDateTime + utterance_start_seconds
	utterance_end_time = recordingStartDateTime + utterance_end_seconds

	# Create a time window (in seconds) for the utterances -- how long is each in seconds
	utterance_time_window = as.numeric(difftime(utterance_end_time, utterance_start_time, units="secs"))

	# Parse the utterance message itself
	utterance_message = substr(utteranceText, regexpr("[:]", utteranceText)+2)

	# Get the user name that spoke the text
	user_name = substr(utteranceText, 1, regexpr("[:]", utteranceText)-1)

	# Prepare the output file
	res.out = data.frame(utterance_id,
	                     utterance_start_seconds,
	                     utterance_start_time,
	                     utterance_end_seconds,
	                     utterance_end_time,
	                     utterance_time_window,
	                     user_name,
	                     utterance_message,
	                     stringsAsFactors=FALSE)

	# Mark as unidentified any user with a blank user_name
	res.out$user_name = ifelse(res.out$user_name == "" | is.na(res.out$user_name),
	                          "UNIDENTIFIED",
	                          res.out$user_name)

	# Add the language code
	res.out$utterance_language = languageCode

	return(res.out)
}
