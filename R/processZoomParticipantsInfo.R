#' Participants info parsing
#' @description Parses the information from the downloadable meeting information file in Zooms "reports" section.
#' @param inputPath character The path to the Zoom export file
#' @details The function presumes that you have
#' checked the box to include the meeting information in the file. That means
#' that there is a header (2 rows) containing the zoom meeting information.
#' Following that header are four columns:
#' Name (Original Name):		This is the name the user logged in first using
#' User Email: 				The user's email address (if logged in to Zoom)
#' Total Duration: 			How many minutes the user spent in the session
#' Guest:					Is this person a guest or not?
#'
#' @return  A list with two data.frame objects.
#'    \itemize{
#'      \item{by_chat} data.frame There is a single record containing the following info:
#'        \itemize{
#'          \item{meetingId}{Numeric meeting identifier from Zoom}
#'          \item{meetingTopic}{String meeting description}
#'          \item{meetingStartTime}{Start date-time of the meeting in YYYY-MM-DD HH:MM:SS}
#'          \item{meetingEndTime}{End date-time of the meeting in YYYY-MM-DD HH:MM:SS}
#'          \item{meetingDuration}{Number of minutes that the meeting lasted}
#'          \item{numParticipants}{Number of unique Zoom login names (original name) for this meeting}
#'        }
#'      \item{by_user} data.frame A set of measures aggregated to the level of the individual speaker (by unique login)
#'        \itemize{
#'          \item{userName}{String user name for this user}
#'          \item{userEmail}{String email address attached to this user's Zoom account}
#'          \item{userDuration}{Number of minutes that this user was logged in}
#'          \item{userGuest}{Indicator of whether this person is a guest user of Zoom}
#'        }
#'    }
#' @examples
#'
#' \dontrun{
#' by_user = processZoomParticipantsInfo("~/Desktop/zoom_meeting_participants_export.csv")
#' }
#'
#'@export
processZoomParticipantsInfo = function(inputPath) {

	by_chat = utils::read.table(inputPath, header=F, nrows=1, skip=1, sep=",", stringsAsFactors=F,
	                             col.names=c("meetingId", "meetingTopic", "meetingStartTime", "meetingEndTime",
	                                         "userEmail", "meetingDuration", "numParticipants", "blankCol"))
	by_chat$blankCol = NULL

	# Change the date column to something more useable in the other functions
	by_chat$meetingStartTime = convDate(by_chat$meetingStartTime)
	by_chat$meetingEndTime = convDate(by_chat$meetingEndTime)

	by_user = data.frame(utils::read.delim(inputPath, header=T, skip=3, sep=",", stringsAsFactors=F))
	names(by_user) = c("userName", "userEmail", "userDuration", "userGuest")

	outInfo = list(by_chat = by_chat, by_user = by_user)

	return(outInfo)
}
