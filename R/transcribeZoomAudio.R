#' Zoom Audio File Processing
#'
#' @description This function starts an audio transcription job on AWS.
#' @param fileLocation character either "local" or "s3" - if local, then this function will upload the file to the specified bucket
#' @param filePath character	the path to the local file or to the s3 file (depending on whether it is "local" or "s3")
#' @param bucketName character	name of the s3 bucket where the finished transcript is stored
#' @param jobName character	name of the transcription job
#' @param  numSpeakers numeric Specify how many speakers you expect (this helps AWS identify the speakers in the clip).
#' @param languageCode character language code. Default is "en".
#' @details This function is useful for batch uploading audio files and starting transcription jobs for them.
#' This can be done with a local file (uploads to a specified s3 bucket) or with a file that already exists in an s3 bucket
#' @return This does not return anything useful
#' @examples
#'
#' \dontrun{
#' transcribeZoomAudio(fileLocation="local",
#' bucketName="my-transcription-bucket",
#' filePath="mylocalfile.m4a",
#' jobName="mylocalfile.m4a",
#' languageCode="en-US")
#' }
#'
#'@export
transcribeZoomAudio = function(fileLocation, filePath, bucketName, jobName, numSpeakers, languageCode="en") {
	# First, if the file location is local, then upload it into the designated s3 bucket
	if(fileLocation == "local") { # $MIKE$ we should be able to detect if a file location is on S3 automatically
		localFilePath = filePath
		svc = paws::s3()
		upload_file = file(localFilePath, "rb")
		upload_file_in = readBin(upload_file, "raw", n = file.size(localFilePath))
		svc$put_object(Body = upload_file_in, Bucket = bucketName, Key = jobName)
		filePath = paste("s3://", bucketName, "/",jobName, sep="")
		close(upload_file)
	}

	svc = paws::transcribeservice()
	svc$start_transcription_job(TranscriptionJobName = jobName,
	                            LanguageCode = languageCode,
	                            Media = list(MediaFileUri = filePath),
	                            OutputBucketName = bucketName,
	                            Settings = list(ShowSpeakerLabels=TRUE,
	                                            MaxSpeakerLabels=numSpeakers))
	# $MIKE$ this should at least return some confirmation the job is done!
}
