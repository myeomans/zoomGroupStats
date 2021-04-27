#' Window Analyses
#'
#' @description Produces Windowed Conversation Anlaysis
#' @param inputData data.table a transcript data file to be analysed   #$MIKE$ - data.table? data.frame?
#' @param inputType character type of data input
#' @param speakerId character name of column identifying speaker names
#' @param windowSize numeric Number of turns in each window of analysis.
#' @param doSentiment boolean do you want to run a sentiment analysis on this input file? Default is FALSE
#' @param sentiDone boolean Does the file already have sentiment included? Default is FALSE
#' @details Something else
#' @return  A list with two data.frame objects.
#'    \itemize{
#'      \item{by_chat} data.frame A set of measures aggregated for the overall file (either chat or transcript).
#'        \itemize{
#'          \item{}{}
#'          \item{}{}
#'          \item{}{}
#'        }
#'      \item{by_user} data.frame A set of measures aggregated to the level of the individual speaker (as identified by speakerId)
#'        \itemize{
#'          \item{}{}
#'          \item{}{}
#'        }
#'    }
#' @examples
#'
#' \dontrun{
#' audio.out = processZoomAudio(bucketName = "my-transcription-bucket",
#' jobName = "mylocalfile.m4a",
#' localDir = "path-to-local-directory-for-output",
#' speakerNames = c("Tom Smith", "Jamal Jones", "Jamika Jensen"),
#' recordingStartDateTime = "2020-06-20 17:00:00", writeTranscript=TRUE)
#' }
#'
#'
#'@import data.table
#'@export
windowedTextConversationAnalysis = function(inputData, inputType, speakerId, windowSize,
                                            doSentiment=FALSE, sentiDone=FALSE) {
	t.out.windowed = makeTimeWindows(inputData, inputType, windowSize)
	inputData = t.out.windowed$inputData # $MIKE$ why does inputData change in the mTW function?

	# Create a blank set that gives each user an opportunity to have an aggregate
	# metric during each of the time windows
	participants = sort(unique(inputData[,speakerId]))
	user_name = rep(participants,max(inputData$window_id))
	window_id = sort(rep(1:max(inputData$window_id), length(participants)))
	p3 = data.frame(user_name, window_id, stringsAsFactors=F)
	p4 = merge(p3, t.out.windowed$windows, by="window_id")

	# Now, loop through the time windows
	count = 1

	for(win in 1:max(inputData$window_id)) {

		windowed.input = inputData[inputData$window_id == win, ]

		if(nrow(windowed.input) > 0) {

			res.line = textConversationAnalysis(inputData=windowed.input,
			                                    inputType="transcript",
			                                    speakerId = speakerId,
			                                    doSentiment=doSentiment,
			                                    sentiDone=sentiDone)

			grp.res.line = res.line[[1]]
			grp.res.line$window_id = win

			ind.res.line = res.line[[2]]
			ind.res.line$window_id = win

			if(count == 1) {
				grp.res.out = grp.res.line
				ind.res.out = ind.res.line
			} else {
				grp.res.out = rbind(grp.res.out, grp.res.line)
				ind.res.out = rbind(ind.res.out, ind.res.line)
			}
			count = count + 1
		}
	}

	p5 = merge(p4, ind.res.out, by=c("user_name", "window_id"), all.x=T)
	p5$utterance_time_window_sum = ifelse(is.na(p5$utterance_time_window_sum), 0, p5$utterance_time_window_sum)
	p5$num_utterances = ifelse(is.na(p5$num_utterances), 0, p5$num_utterances)

	grp1 = merge(t.out.windowed[[2]], grp.res.out, by=c("window_id"), all.x=T)
	grp1$utterance_time_window_sum = ifelse(is.na(grp1$utterance_time_window_sum), 0, grp1$utterance_time_window_sum)
	grp1$num_utterances = ifelse(is.na(grp1$num_utterances), 0, grp1$num_utterances)
	grp1$total_recorded_time = ifelse(is.na(grp1$total_recorded_time), 0, grp1$total_recorded_time)

	grp1$num_unique_speakers = ifelse(is.na(grp1$num_unique_speakers), 0, grp1$num_unique_speakers)


	return(list(by_chat = grp1,
	            by_user = p5))
}
