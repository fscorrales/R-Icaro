
ExportButtonUI <- function(id, LabelButton) {
  
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("modDownload"), LabelButton,  width = "125px")
  )
}

ExportButtonServer <- function(id, DF, FileName = "R INVICO", 
                                    Extension = "xlsx") {
  
  moduleServer(id, function(input, output, session) {
    
    output$modDownload <- downloadHandler(
      filename = function() {
        paste(FileName,"-", Sys.Date(), ".", Extension, sep="")
      },
      content = function(file) {
        if (Extension == "xlsx") {
          openxlsx::write.xlsx(DF(), file)
        }
      }
    )
    
  })
}