#------------------------------------------------------------------------------------------------------------------------
assessTableAndLoadTrack <- function(session, originalFilename, uploadedFilePath, trackName, trackColor, trackType)
{
   printf("----- entering assessTableAndLoadTrack")

  tryCatch({
     tbl <- read.table(uploadedFilePath, sep="\t", as.is=TRUE, header=FALSE)
     stopifnot(nrow(tbl) >= 1)
     stopifnot(ncol(tbl) >= 3)
     if(nchar(trackName) == 0)
        trackName <- originalFilename
     if(trackType == "bed3"){
        stopifnot(ncol(tbl) >= 3)
        stopifnot(is.character(tbl[,1]))
        stopifnot(is.numeric(tbl[,2]))
        stopifnot(is.numeric(tbl[,3]))
        colnames(tbl) <- c("chr", "start", "end")
        loadBedTrack(session, trackName, tbl, color=trackColor, trackHeight=50, deleteTracksOfSameName=TRUE, quiet=TRUE)
        }
     if(trackType == "bedGraph"){
        stopifnot(ncol(tbl) >= 4)
        stopifnot(is.character(tbl[,1]))
        stopifnot(is.numeric(tbl[,2]))
        stopifnot(is.numeric(tbl[,3]))
        stopifnot(is.numeric(tbl[,4]))
        colnames(tbl) <- c("chr", "start", "end", "value")
        loadBedGraphTrack(session, trackName, tbl, color=trackColor, trackHeight=50, autoscale=TRUE,
                          deleteTracksOfSameName=TRUE, quiet=TRUE)
        }
     },
  error=function(e){
     msg <- e$message
     #print(msg)
     title <- sprintf("Load track error, unexpected format for %s", trackType)
     showModal(modalDialog(title=title, msg))
     }) # tryCatch

} # assessTableAndLoadTrack
#------------------------------------------------------------------------------------------------------------------------
.createTrackFileUploader <- function(session, input, output)
{
   trackFileUploadInfo <- reactiveValues(uploadedFilename = NULL,
                                         trackType=NULL,
                                         trackColor=NULL,
                                         trackName="")

   trackFileUploadDialog <- function(failed = FALSE) {
      modalDialog(
        fileInput("loadTrackFromBedFile", label = h3("Add track from local file")),
        radioButtons("trackType", "Track Type", c("Simple Bed (bed-3)"="bed3", "Bed Graph"="bedGraph")),
        textInput("trackName", "Track Name"),
        colourpicker::colourInput("trackColorChoice", "Track Color", "red", palette="limited", showColour="background"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("trackFileUploadOkButton", "OK")
          )
        )
      } # trackFileUploadDialog function

    observeEvent(input$addTrackFromFileButton, {
      showModal(trackFileUploadDialog())
      })

    observeEvent(trackFileUploadInfo$filename, {
        printf("--- source file name: %s",    trackFileUploadInfo$filename)
        })

    observeEvent(input$trackType, {
      trackFileUploadInfo$trackType = input$trackType
      })

    observeEvent(input$trackName, {
      trackFileUploadInfo$trackName = input$trackName
      })

    observeEvent(input$trackColorChoice, {
      trackFileUploadInfo$trackColor = input$trackColorChoice
      })

    observeEvent(input$loadTrackFromBedFile, {
       tbl.uploadInfo <- input$loadTrackFromBedFile
       trackFileUploadInfo$filename <- input$loadTrackFromBedFile$name
       tmp.filename <- tbl.uploadInfo$datapath[1]
       trackFileUploadInfo$uploadedFilename <- tmp.filename
       printf("load this file: %s", tmp.filename)
       })

    observeEvent(input$trackFileUploadOkButton, {
      originalFilename <- trackFileUploadInfo$filename
      uploadedFilePath <- trackFileUploadInfo$uploadedFilename
      trackName <- trackFileUploadInfo$trackName
      trackColor <- trackFileUploadInfo$trackColor
      trackType <- trackFileUploadInfo$trackType
      removeModal()
      assessTableAndLoadTrack(session, originalFilename, uploadedFilePath, trackName, trackColor, trackType)
      })


} # .createTrackFileUploader
#------------------------------------------------------------------------------------------------------------------------
