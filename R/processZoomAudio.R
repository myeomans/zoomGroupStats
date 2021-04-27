#' Zoom Audio File Processing
#'
#' @description This function parses transcriptions from AWS transcribe.
#' @param bucketName character	name of the s3 bucket where the finished transcript is stored
#' @param jobName character	name of the transcription job
#' @param localDir character a local directory where you can save the aws json file and also a plain text file of the transcribed text
#' @param speakerNames character vector The Zoom user names of the speakers, in the order in which they appear in the audio clip.
#' @param recordingStartDateTime character the date/time that the meeting recording started $MIKE$ format?
#' @param writeTranscript boolean Do you want to output a plain text file of the transcript? Default is TRUE
#' @details This function parses the JSON transcription completed by AWS transcribe. The output is the same as the processZoomTranscript function.
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
#' audio.out = processZoomAudio(bucketName = "my-transcription-bucket",
#' jobName = "mylocalfile.m4a",
#' localDir = "path-to-local-directory-for-output",
#' speakerNames = c("Tom Smith", "Jamal Jones", "Jamika Jensen"),
#' recordingStartDateTime = "2020-06-20 17:00:00",
#' writeTranscript=TRUE)
#' }
#'
#'@import data.table
#'@export
processZoomAudio = function(bucketName, jobName, localDir, speakerNames=c(), recordingStartDateTime,
                            writeTranscript=TRUE) {


	transcriptName = paste(jobName, "json", sep=".")
	svc = paws::s3()  # $MIKE$ from paws, yes?
	transcript = svc$get_object(Bucket = bucketName, Key = transcriptName)
	# Write the binary component of the downloaded object to the local path
	writeBin(transcript$Body, con = paste(localDir, transcriptName, sep="/"))
	tr.json = jsonlite::fromJSON(paste(localDir, transcriptName, sep="/"))

	if(writeTranscript) {
		outTranscript = paste(localDir, "/", jobName, ".txt", sep="")
		write(tr.json$results$transcripts$transcript, outTranscript)
	}

	# This IDs the words as AWS broke out the different segments of speech
	for(i in 1:length(tr.json$results$speaker$segments$items)){

		res.line = tr.json$results$speaker$segments$items[[i]]
		res.line$segment_id = i
		if(i == 1) {
			res.out = res.line
		} else {
			res.out = rbind(res.out, res.line)
		}

	}

	segments = res.out
	segment_cuts = tr.json$results$speaker$segments[,c("start_time", "speaker_label", "end_time")]

	# Pull this apart to just get the word/punctuation with the most confidence
	# Not currently dealing with any of the alternatives that AWS could give
	for(i in 1:length(tr.json$results$items$alternatives)) {

		res.line = tr.json$results$items$alternatives[[i]]

		if(i == 1) {
			res.out = res.line
		} else {
			res.out = rbind(res.out, res.line)
		}

	}

	words = cbind(res.out, tr.json$results$items[,c("start_time", "end_time", "type")])
	words = words[words$type == "pronunciation", ]
	words_segments = merge(words, segments, by=c("start_time", "end_time"), all.x=T)

	words_segments$start_time = as.numeric(words_segments$start_time)
	words_segments$end_time = as.numeric(words_segments$end_time)

	words_segments = words_segments[order(words_segments$start_time), ]
	segment_ids = unique(words_segments$segment_id)
	i = 1


	segment_cuts$utterance_id = NA
	segment_cuts$utterance_message = NA
	for(i in 1:length(segment_ids)) {
		utterance_id = segment_ids[i]
		segment_cuts[i, "utterance_id"] = utterance_id
		segment_cuts[i, "utterance_message"] = paste0(words_segments[words_segments$segment_id == utterance_id, "content"], collapse=" ")
	}

	if(length(speakerNames) > 0) {
		user_names = data.frame(0:(length(speakerNames)-1), speakerNames, stringsAsFactors=F)
		names(user_names) = c("speaker_label", "user_name")
		user_names$speaker_label = paste("spk",user_names$speaker_label, sep="_")
		segment_cuts = merge(segment_cuts, user_names, by="speaker_label", all.x=T)
	}

	names(segment_cuts)[2:3] = c("utterance_start_seconds", "utterance_end_seconds")
	segment_cuts[, 2:3] = lapply(segment_cuts[, 2:3], function(x) as.numeric(x))
	segment_cuts = segment_cuts[order(segment_cuts$utterance_start_seconds), ]

	# Now turn these into actual datetime values
	recordingStartDateTime = as.POSIXct(recordingStartDateTime)
	segment_cuts$utterance_start_time = recordingStartDateTime + segment_cuts$utterance_start_seconds
	segment_cuts$utterance_end_time = recordingStartDateTime + segment_cuts$utterance_end_seconds

	# Create a time window (in seconds) for the utterances -- how long is each in seconds
	segment_cuts$utterance_time_window = as.numeric(difftime(segment_cuts$utterance_end_time, segment_cuts$utterance_start_time, units="secs"))

	# Prepare the output file
	res.out = segment_cuts[, c("utterance_id", "utterance_start_seconds", "utterance_start_time", "utterance_end_seconds", "utterance_end_time", "utterance_time_window", "user_name", "utterance_message")]

	# Mark as unidentified any user with a blank username
	res.out$user_name = ifelse(res.out$user_name == "" | is.na(res.out$user_name), "UNIDENTIFIED", res.out$user_name)

	# Add the language code
	#res.out$utterance_language = languageCode #$MIKE$ Where is this supposed to come from?

	return(res.out)

}
