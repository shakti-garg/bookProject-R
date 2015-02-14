library(shiny)

library(ggplot2)
library(reshape2)

appearData <- read.csv(file="data/1-appearance.csv", as.is=TRUE);
appearData$BookName <- as.factor(appearData$BookName)
appearData$ChapterName <- as.factor(appearData$ChapterName)
appearData$Percentile <- as.numeric(appearData$Percentile)

bookNameList <- as.list(as.character(unique(appearData$BookName)))

sentiData <- read.csv(file="data/2-sentiment.csv", as.is=TRUE);
sentiData$BookName <- as.factor(sentiData$BookName)
sentiData$ChapterName <- as.factor(sentiData$ChapterName)
sentiData$Percentile <- as.numeric(sentiData$Percentile)
sentiData$Sentiment <- factor(sentiData$Sentiment, levels=c("Very positive","Positive","Neutral","Negative", "Very negative"))


shinyServer(function(input, output, session) {
  
  output$bookFilter <- renderUI({ radioButtons(inputId="bookName", label = h4("Book Name"), 
                                               choices = bookNameList, selected = bookNameList[[1]])
  })
  
  getChapterNames <- reactive({
    if(length(input$bookName) != 0){
      filteredData <- appearData[appearData$BookName == input$bookName, ]
      
      unique(as.character(filteredData$ChapterName))
    }
  })
  
  observe({
    if(length(input$bookName) != 0){
      numChapters <- length(getChapterNames())
      output$chapterFilter <- renderUI({ sliderInput("chapters", "Chapter Numbers:", min=1, 
                                                     max=numChapters, step=1, 
                                                     value=c(1,numChapters))  
      })
    }
  })                        
  
  filterAppearData <- reactive({
    if(length(input$bookName) != 0 & length(input$chapters) != 0){
      filteredData <- appearData[appearData$BookName == input$bookName, ]
      
      selectedChapNames <- unique(as.character(filteredData$ChapterName))[c(input$chapters[1]:input$chapters[2])]
      filteredData <- filteredData[as.character(filteredData$ChapterName) %in% selectedChapNames,]
      
      charTable <- data.frame(table(filteredData$CharacterName))
      charTable$Var1 <- as.character(charTable$Var1)
      charLevels <- charTable$Var1[order(charTable$Freq, decreasing=FALSE)]
      filteredData$CharacterName <- factor(filteredData$CharacterName,levels=charLevels)
      
      filteredData$color <- sapply(filteredData$CharacterName, FUN=function(x) {log(log(log(charTable$Freq[charTable$Var1 == x])))})
      
      filteredData
    }
  })
  
  output$appearancePlot <- renderPlot({
    plotData <- filterAppearData()
    
    if(length(plotData) != 0){
      chapLabels <- getChapterNames()
      #num_chars <- sum(data.frame(table(plotData$Character))$Freq != 0)
      #size <- min(12, exp(4.97 - 0.96*log(num_chars)))
      
      cGraph <- ggplot(plotData, aes(x=Percentile, y=CharacterName, colour=color))
      cGraph <- cGraph + geom_point(shape="|", size=4)
      cGraph <- cGraph + scale_x_continuous("Chapter", expand=c(0,0), limits=c(input$Chapters[1],input$Chapters[2]), breaks=seq(0,length(chapLabels)-1)+0.5, labels=chapLabels)
      cGraph <- cGraph + theme(axis.text.x = element_text(colour="black", angle = 45, hjust = 1, vjust=1.03),
                             axis.text.y = element_text(colour = "black"),
                             axis.title.x = element_text(vjust=5),
                             axis.ticks = element_blank())
      cGraph <- cGraph + theme(plot.background = element_rect(fill = "transparent",colour = NA))
      cGraph <- cGraph + theme(panel.grid.minor = element_line(colour = "white", size = 1),
                             panel.grid.major = element_blank())
      cGraph <- cGraph + scale_size_continuous(guide = FALSE)
      cGraph <- cGraph + scale_alpha_continuous(guide = FALSE)
      cGraph <- cGraph + scale_colour_gradient(high='#B40404', low='#C65454', guide = FALSE)
    
      cGraph
    }
  }, height=700)
  
  filterSentiData <- reactive({
    if(length(input$bookName) != 0 & length(input$chapters) != 0){
      filteredData <- sentiData[sentiData$BookName == input$bookName, ]
      
      selectedChapNames <- unique(as.character(filteredData$ChapterName))[c(input$chapters[1]:input$chapters[2])]
      filteredData <- filteredData[as.character(filteredData$ChapterName) %in% selectedChapNames,]
      
      charTable <- data.frame(table(filteredData$CharacterName))
      charTable$Var1 <- as.character(charTable$Var1)
      charLevels <- charTable$Var1[order(charTable$Freq, decreasing=FALSE)]
      filteredData$CharacterName <- factor(filteredData$CharacterName,levels=charLevels)
      
      filteredData
    }
  })
  
  output$sentimentPlot <- renderPlot({
    plotData <- filterSentiData()
    
    if(length(plotData) != 0){
      chapLabels <- getChapterNames()
      #num_chars <- sum(data.frame(table(plotData$Character))$Freq != 0)
      #size <- min(12, exp(4.97 - 0.96*log(num_chars)))
      
      cGraph <- ggplot(plotData, aes(x=Percentile, y=CharacterName, colour=Sentiment))
      cGraph <- cGraph + geom_point(shape="|", size=4)
      cGraph <- cGraph + scale_x_continuous("Chapter", expand=c(0,0), limits=c(input$Chapters[1],input$Chapters[2]), breaks=seq(0,length(chapLabels)-1)+0.5, labels=chapLabels)
      cGraph <- cGraph + theme(axis.text.x = element_text(colour="black", angle = 45, hjust = 1, vjust=1.03),
                               axis.text.y = element_text(colour = "black"),
                               axis.title.x = element_text(vjust=5),
                               axis.ticks = element_blank())
      cGraph <- cGraph + theme(plot.background = element_rect(fill = "transparent",colour = NA))
      cGraph <- cGraph + theme(panel.grid.minor = element_line(colour = "white", size = 1),
                               panel.grid.major = element_blank())
      cGraph <- cGraph + scale_size_continuous(guide = FALSE)
      cGraph <- cGraph + scale_alpha_continuous(guide = FALSE)
      cGraph <- cGraph + scale_colour_manual(values=c("Very negative"="red","Negative"="hotpink","Neutral"="Yellow","Positive"="cyan","Very positive"="green"), 
                                             guide=guide_legend(override.aes=aes(shape=15)))
      
      cGraph
    }
  }, height=700)
  
  filterCooccurenceData <- reactive({
    if(length(input$bookName) != 0 & length(input$chapters) != 0){
      filteredData <- appearData[appearData$BookName == input$bookName, ]
      
      selectedChapNames <- unique(as.character(filteredData$ChapterName))[c(input$chapters[1]:input$chapters[2])]
      filteredData <- filteredData[as.character(filteredData$ChapterName) %in% selectedChapNames,]
      
      filteredData$CharacterName <- as.character(filteredData$CharacterName)
      filteredData$Chapter <- floor(filteredData$Percentile)
      
      cooccurenceTable <- melt(crossprod(table(filteredData[c('Chapter','CharacterName')])))
      colnames(cooccurenceTable)[2] <- "CharacterName1"
      
      maxValue <- max(cooccurenceTable$value)
      cooccurenceTable$value <- log10(9*(cooccurenceTable$value / maxValue)+1)
     
      cooccurenceTable
    }
  })
  
  output$cooccurrencePlot <- renderPlot({
    plotData <- filterCooccurenceData()
    
    if(length(plotData) != 0){
      cGraph <- ggplot(plotData, aes(x=CharacterName, y=CharacterName1, alpha=value))
      cGraph <- cGraph + geom_tile(aes(fill=factor(1)), colour='white')
      cGraph <- cGraph + xlab(NULL) + ylab(NULL)
      cGraph <- cGraph + theme(axis.text.x = element_text(colour="black", angle = 45, hjust = 1, vjust=1.03),
                               axis.text.y = element_text(colour = "black"),
                               axis.ticks = element_blank())
      cGraph <- cGraph + theme(panel.grid.minor = element_line(colour = "white", size = 1),
                               panel.grid.major = element_blank())
      cGraph <- cGraph + scale_fill_manual(values=c("#B40404"), guide = FALSE)
      cGraph <- cGraph + scale_alpha_continuous(guide = FALSE)
      cGraph <- cGraph + coord_fixed(ratio=1)
      
      cGraph
    }
  }, height=700)
})