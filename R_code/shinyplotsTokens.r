# --- shiny plot ---

library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(DT)

Sys.setlocale(category = "LC_ALL", locale = "Chinese")

# ui.R definition
ui <- fluidPage(

  theme = shinytheme("spacelab"),

  fixedRow(column(12, plotlyOutput("Plot1", height = "600px"))),
  tags$hr(),
  fixedRow(column(12, dataTableOutput("Plot3", width = "100%", height = "auto")))
)

# server.R definition
server <- function(input, output){

    plot.df <- data.frame(x = d.mds$x, # x-coordinates of MDS-plot (presupposes dataframe)
                          y = d.mds$y, # y-coordinates of MDS-plot
                          Lect = d.mds$lect,
                          Word = d.mds$variant,
                          Text = d.mds$text, # concordance of tokens (word)
                          Text_ppmi = d.mds$text_ppmi) # concordance of tokens (lemma, pos, ppmi)
    
    plot.df$Text <- as.character(plot.df$Text)
    plot.df$Text_ppmi <- as.character(plot.df$Text_ppmi)

    output$Plot1 <- renderPlotly({
      # create a two-dimensional scatterplot of tokens
      key = rownames(plot.df)
      
      p <- ggplot(plot.df, aes(x,y,key=key)) +
        geom_point(aes(colour=Word, shape=Lect, text=paste('<b>Country</b>: ', d.mds$lect,
                                                              '<br><b>Word</b>: ', d.mds$variant,
                                                              #'</br><b>Sense</b>: ', d.tsne$sense,
                                                              '<br><b>Context</b>: ', d.mds$text))) +
        #scale_shape_manual(values=c(3,16)) +
        #scale_color_manual(values=c("black","red","blue")) +
        theme_bw()

      q <- ggplotly(p, tooltip = "text" , opacity=0.5, source="subset")
      q %>% layout(hoverlabel = list(bgcolor=c("rgb(255,255,204")),
                   xaxis = list(scaleanchor = "y", scaleratio = 1),
                   dragmode =  "select")

    })
  
  output$Plot3 <- renderDataTable({
    #create a table with significant first order context words and their frequency

    event.data <- event_data("plotly_selected", source = "subset")
    
    if(is.null(event.data) == T) return(NULL)

    # should be adjusted to the specific concordances
    splitpat <- paste0("(\\s|<br>|",paste0("<b><i>(",paste(paste0(plot.df$Word,"/Na"),collapse="|"),")</b></i>)"))
    tokentext <- unlist(strsplit(plot.df[event.data$key,"Text_ppmi"],splitpat))
    # tokenlemma <- as.character(plot.df[event.data$key,"Word"])
    tokentext_noempty <- tokentext[tokentext > 0]
    tokenassoc <- as.character()
    for (i in 1:length(tokentext_noempty)) {
      if (grepl("/[0-9\\.]{1,5}$",tokentext_noempty[i]) == TRUE){
        tokenassoc <- append(tokenassoc,tokentext_noempty[i])
      }
    }
    
    lemma <- gsub("(.+?)/[^/]+/[0-9\\.]{1,5}$","\\1",tokenassoc)
    pos <- gsub(".+?/([^/]+)/[0-9\\.]{1,5}$","\\1",tokenassoc)
    assocvalue <- gsub(".+?/[^/]+/([0-9\\.]{1,5})$","\\1",tokenassoc)
    t <- data.frame(cbind(lemma,pos,assocvalue))
    
    t2 <- plyr::count(t)
    t3 <- t2[order(t2$freq, decreasing = T),]
    colnames(t3) <- c("lemma","pos","assocvalue","freq")
    
    dt <- datatable(t3)
  })
}

shinyApp(ui, server)
