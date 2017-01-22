library(jsonlite)
library(plotly)
library(dplyr)
library(reshape2)
library(shiny)

ui<-fluidPage(title="Wikipedia Articles Trends",
              h2("Wikipedia Old Trends"),
              column(4,
                     textInput("entry", "Article name", "Love"),
                     textInput("language", "Wikipedia language", "en"),
                     dateRangeInput("daterange", "Date range:",
                                    start = "2008-01-01",
                                    end   = "2010-03-01"),
                     downloadButton("downloadData", "Download")
                     
              ),
              column(6,
                     plotlyOutput("ggplotTrends", width = "400px"))
)

server<-function(input, output){
  observe({
    entryDate<-as.list(format(seq(from=input$daterange[1], to=input$daterange[2], by='months'), "%Y%m")  )
    
    wikipediaLanguage<-input$language
    entryName<-input$entry
    
    final_data<-do.call(cbind,
                        lapply(entryDate, function(x) {
                          data <- fromJSON(paste0("http://stats.grok.se/json/", wikipediaLanguage, "/", x, "/", entryName))
                          almost_final_data <- do.call(rbind, data)
                        }))
    
    df1<-final_data["daily_views",]
    f2<-melt(df1)
    df2<-f2[, c("L1", "value")]
    
    output$ggplotTrends<-renderPlotly({
      base <- ggplot(data=df2, aes(x=as.Date(L1), y=value)) + 
        geom_line() +
        labs(title=entryName, y="Views" ,x= "Dates") 
      
      base + scale_x_date(date_labels = "%m-%Y")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.time(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(df2, file)
      }
    )
  })
}

shinyApp(ui, server)

