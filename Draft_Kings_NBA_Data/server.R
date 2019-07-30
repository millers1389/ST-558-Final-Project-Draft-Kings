#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

#source("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/DraftKingsData.R")
# 
#source("../DraftKingsData.R")

library(tidyverse)
library(haven)
library(knitr)
library(dplyr)
library(rgl)
library(tree)
library(shiny)
library(ggplot2)
library(ggiraphExtra)
library(gbm)
library(DT)
library(plotly)


draftKingsData <- read_csv("../draftKingsData.csv")


draftKingsData$PlayerName <- as.factor(draftKingsData$PlayerName)

draftKingsData$`Inj` <- ifelse(is.na(draftKingsData$Inj), "None", draftKingsData$Inj)

draftKingsData$Inj <- as.factor(draftKingsData$Inj)

draftKingsData$Pos <- as.factor(draftKingsData$Pos)

draftKingsData$Team <- as.factor(draftKingsData$Team)

draftKingsData$Opp <- as.factor(draftKingsData$Opp)

draftKingsData$Rest <- as.numeric(draftKingsData$Rest)

draftKingsData$DvP <- substr(draftKingsData$DvP, 1, nchar(draftKingsData$DvP, type="chars")-1)

draftKingsData$DvP <- as.numeric(draftKingsData$DvP)

draftKingsData


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    getData <- reactive({
        playerName <- input$player
        
        playerData <- draftKingsData %>% filter(PlayerName == playerName) %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        playerData
    })
    
    #getData2 <- reactive({
        #variableName <- input$variables

        variableData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min )
        variableData
    #})
    
    getData3 <- reactive({
        nonZero <- input$zero
        
        nonZeroData <- variableData %>% filter(FP != 0)
        nonZeroData
        
    })
    
    getData4 <- reactive({
        clusterData <- input$cluster
        
        clusterData <- draftKingsData %>% select(FP, Value, USG, PER, SFGA, DEff, DvP, SFP, Pace, SMin, Salary, Rest)
        clusterData
    })
    
    getData5 <- reactive({
        modelData <- input$lr
        
        modelData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        modelData
    })
    
    getData6 <- reactive({
        
        modelData2 <- na.omit(draftKingsData) %>% filter(PlayerName == input$player2) %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        train <- sample(1:nrow(na.omit(modelData2)), size = nrow(na.omit(modelData2))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData2), train)
        predDataTrain <- modelData2[train, ]
        predDataTest <- modelData2[test, ]
    })
    
    
    output$variablePlot <- renderPlotly({
        #get data
        variableData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min )
        
        nonZeroData <- getData3()

        #base plotting object
        if(input$variables == "Actual Fantasy Points" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = FP))
            
            g + geom_histogram(fill = "navy", color = "gold")
        } else if(input$variables=="Actual Fantasy Points"){
            g <- ggplot(variableData, aes(x = FP))
            
            g + geom_histogram(fill = "navy", color = "gold")
        } else if(input$variables == "Player Efficiency Rating" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = PER, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Player Efficiency Rating"){
            g <- ggplot(variableData, aes(x = PER, y = FP))
        
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Usage" & input$zero){
            g <- ggplot(nonZeroData, aes(x = USG, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Usage"){
            g <- ggplot(variableData, aes(x = USG, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Salary" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = Salary, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Salary"){
            g <- ggplot(variableData, aes(x = Salary, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Rest" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = Rest, y = FP, fill=Rest))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Rest"){
            g <- ggplot(variableData, aes(x = Rest, y = FP, fill = Rest))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Position" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = Pos, y = FP, fill = Pos))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Position"){
            g <- ggplot(variableData, aes(x = Pos, y = FP, fill = Pos))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Opponent Defensive Efficiency" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = DEff, y = FP))
            
            g + geom_bar(stat = "summary", fun.y = "mean", fill="navy")
        } else if(input$variables=="Opponent Defensive Efficiency"){
            g <- ggplot(variableData, aes(x = DEff, y = FP))
            
            g + geom_bar(stat = "summary", fun.y = "mean", fill="navy")
        } else if(input$variables=="Opponent Defense vs Position" & input$zero ==TRUE){
            g <- ggplot(nonZeroData, aes(x = DvP, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        }else if(input$variables=="Opponent Defense vs Position"){
            g <- ggplot(variableData, aes(x = DvP, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        }else if(input$variables=="Opponent Pace" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = Pace, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        }else if(input$variables=="Opponent Pace"){
            g <- ggplot(variableData, aes(x = Pace, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        }else if(input$variables=="Average Field Goals Attempted" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = SFGA, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        }else if(input$variables=="Average Field Goals Attempted"){
            g <- ggplot(variableData, aes(x = SFGA, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Average Minutes" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = SMin, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Average Minutes"){
            g <- ggplot(variableData, aes(x = SMin, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Season Average Fantasy Points" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = SFP, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Season Average Fantasy Points"){
            g <- ggplot(variableData, aes(x = SFP, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Value" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = Value, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Value"){
            g <- ggplot(variableData, aes(x = Value, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Salary Vs. Value" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = Salary, y = Value))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Salary Vs. Value"){
            g <- ggplot(variableData, aes(x = Salary, y = Value))
            
            g + geom_point(color="seagreen") + geom_smooth()
        }
        
    })
    #     
        
    output$playerTable <- renderTable({   
        
        playerData <- getData()
        
        if(input$stat=="Average Fantasy Points"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Fantasy Points" = mean(FP))
        } else if(input$stat== "Average Value"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Value" = mean(Value))
        } else if(input$stat=="Average Minutes"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Minutes" = mean(SMin))
        } else if(input$stat == "Average Usage"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Usage" = mean(USG))
        } else if(input$stat == "Average Player Efficiency Rating"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Player Efficiency Rating" = mean(PER))
        } else if(input$stat == "Average Field Goals Attempted"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Field Goals Attempted" = mean(SFGA))
        }
        
    })
        
#    ###CLUSTER GRAPH 
    selectedData <- reactive({
        clusterData <- getData4()
        na.omit(clusterData)[, c(input$xvar, input$yvar)]
    })

    clusters <- reactive({
        kmeans(selectedData(), input$cluster)
    })

    output$clusterPlot <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    
    ####MODELING
    
    output$Formula <- renderPrint({
        if(is.null(input$lr)){
            "Please Select Predictor Variables"
        } else{
        
        paste("Regression Formula: FP", "~", paste(input$lr, collapse= " + "))
        }
    })
    
    output$lrPlot <- renderPlot({

        if(is.null(input$lr)){
            print("Please select a predictor")
        } else{
        
        modelData <-getData5()

        #Create training set and testing set
        set.seed(31)
        train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData), train)
        predDataTrain <- modelData[train, ]
        predDataTest <- modelData[test, ]

        formula <- as.formula(paste("FP", "~", paste(input$lr, collapse= " + ")))
        
    lrFit <- lm(formula = formula, data = predDataTrain)
    
    lrPredict <- predict(lrFit, predDataTest)
    
    par(mfrow=c(2,2))
    # if(length(input$lr) > 0 ){
    #     ggPredict(lrFit)
    # } else "Select Predictor Variables"
    plot(lrFit)

        }



    })
    
    
    output$lrTable <- renderTable({
        
        if(is.null(input$lr)){
            "Please select a predictor"
        } else {
            modelData <-getData5()
            
            set.seed(31)
            train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
            test <- dplyr::setdiff(1:nrow(modelData), train)
            predDataTrain <- modelData[train, ]
            predDataTest <- modelData[test, ]
            
            formula <- as.formula(paste("FP", "~", paste(input$lr, collapse= " + ")))
            
            lrFit <- lm(formula = formula, data = predDataTrain)
            
            lrPredict <- predict(lrFit, predDataTest)
            #mean(as.vector(lrPredict)-as.vector(predDataTest$FP))^2
            
            RMSE<-sqrt(mean((lrPredict-predDataTest$FP)^2, na.rm = TRUE))
            require(MuMIn)
            fitStats <- data.frame(fitStat = c("RMSE", "Adj R Square", "AIC", "AICc", "BIC"),
                                   "Linear Regression Fit" = round(c(RMSE,summary(lrFit)$adj.r.squared, AIC(lrFit), MuMIn::AICc(lrFit), BIC(lrFit)), 3))
    
            
            
            
           
        }
    })
    
    output$boostFormula <- renderPrint({
        if(is.null(input$boosted)){
            "Please Select Predictor Variables"
        } else{
            
            paste("Boosted Model Formula: FP", "~", paste(input$boosted, collapse= " + "))
        }
    })
    
    output$boostPlot <- renderPlot({
        
        if(is.null(input$boosted)){
            "Please Select Predictor Variables"
        } else{
            
        modelData <-getData5()
        
        #Create training set and testing set
        set.seed(31)
        train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData), train)
        predDataTrain <- modelData[train, ]
        predDataTest <- modelData[test, ]
        
        formula <- as.formula(paste("FP", "~", paste(input$boosted, collapse= " + ")))
        
        boostFit <- gbm(formula, data = predDataTrain, distribution = "gaussian", 
                        n.trees = input$trees, shrinkage = input$shrinkage, interaction.depth = input$interaction)
        
        boostPred <- predict(boostFit, 
                             newdata = dplyr::select(predDataTest, -FP), 
                             n.trees = input$trees)
        
        
             # if(length(input$lr) > 0 ){
        #     ggPredict(lrFit)
        # } else "Select Predictor Variables"
       
        
        # plot(boostPred, type="l")
        
        summary(boostFit)
        
        }
    })
    
    output$boostTable <- renderTable({
        
        if(is.null(input$boosted)){
            "Please Select Predictor Variables"
        } else{
            
        modelData <-getData5()
        
        #Create training set and testing set
        set.seed(31)
        train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData), train)
        predDataTrain <- modelData[train, ]
        predDataTest <- modelData[test, ]
        
        formula <- as.formula(paste("FP", "~", paste(input$boosted, collapse= " + ")))
        
        boostFit <- gbm(formula, data = predDataTrain, distribution = "gaussian", 
                        n.trees = input$trees, shrinkage = input$shrinkage, 
                        interaction.depth = input$interaction)
        
        boostPred <- predict(boostFit, 
                             newdata = dplyr::select(predDataTest, -FP), 
                             n.trees = input$trees)
        
        
       summary(boostFit)
        
        }
    })
    
    output$Predictions <- renderTable({
        
        modelData2 <-getData6()

        #Create training set and testing set
        
        formula1 <- as.formula(paste("FP", "~", "USG", "+", "PER", "+", "SFGA", "+", "ProjMin", "+", "DEff", "+", "Pace", "+", "DvP", "+", "SFP"))
        # 
        lrFit <- lm(formula = formula1, data = predDataTrain)
        # 
        lrPredict <- predict(lrFit, newdata= data.frame(PlayerName = input$player2, USG = input$Usage, PER = input$PER, SFGA = input$SFGA, ProjMin = input$Mins, DEff = input$DEff, Pace = input$Pace, DvP = input$DvP, SFP = input$SFP))
        # 
        # formula2 <- as.formula(paste("FP", "~", paste(input$boosted, collapse= " + ")))
        # 
        boostFit <- gbm(formula1, data = predDataTrain, distribution = "gaussian",
                        n.trees = input$trees, shrinkage = input$shrinkage, interaction.depth = input$interaction)
        # 
        boostPred <- predict(boostFit,
                             newdata= data.frame(PlayerName = input$player2, USG = input$Usage, PER = input$PER, SFGA = input$SFGA, ProjMin = input$Mins, DEff = input$DEff, Pace = input$Pace, DvP = input$DvP, SFP = input$SFP),
                             n.trees = input$trees2,
                             shrinkage = input$shrinkage2,
                             interaction.depth= input$interaction2)
      
        
        
        predictionTable <- data.frame("Player Name" = input$player2, "Predicted Fantasy Points: Linear Regression Model" = lrPredict, "Predicted Fantasy Points: Boosted Tree Model" = boostPred)
        colnames(predictionTable) = c("Player Name", "Linear Regression Model", "Boosted Tree Model")
        predictionTable
    })
    
    ###OUTPUT DATATABLE
    output$draftKingsDataset <- DT::renderDataTable({
        DT::datatable(draftKingsData, options = list(scrollX=TRUE))
    })
    
    observe({
        usageVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(USG) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "Usage", "Fill in Average Usage",
                 value = round(usageVal,2),  min=0, max=40)
        
        PerVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(PER) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "PER", "Fill in Player Efficiency Rating",
                           value = round(PerVal,2),  min=0, max=40)

        FgaVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(SFGA) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "SFGA", "Fill in Average Field Goal Attempts",
                           value = round(FgaVal,2),  min=0, max=40)

        MinVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(ProjMin) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "Mins", "Fill in Projected Minutes",
                           value = round(MinVal,2),  min=0, max=40)

        SFPVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(SFP) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "SFP", "Fill in Average Fantasy Points",
                           value = round(SFPVal,2),  min=0, max=100)
    })
    
    
    output$saveData1 <- downloadHandler(
        filename = function(){
            "playerData.csv"
        },
        content = function(file){
            
            playerData <- getData()
            
            write_csv(playerData, file)
        }
    )
    
    
    
    output$saveData2 <- downloadHandler(
        filename = function(){
            "variableData.csv"
        },
        content = function(file){
            
            if(input$zero != TRUE){
            variableData 
            
            write_csv(variableData, file)
            } else {
                nonZeroData <- getData3() 
                
                write_csv(nonZeroData, file)
            
            }
        }
    )
   
    
    output$saveData3 <- downloadHandler(
        filename = function(){
            "clusterData.csv"
        },
        content = function(file){
            
            clusterData <- getData4()
            
            write_csv(clusterData, file)
        }
    )
    
    output$saveData4 <- downloadHandler(
        filename = function(){
            "modelData.csv"
        },
        content = function(file){
            
            modelData <- getData5() 
            
            write_csv(modelData, file)
        }
    )
    
    output$saveData5 <- downloadHandler(
        filename = function(){
            "modelData.csv"
        },
        content = function(file){
            
            modelData <- getData5() 
            
            write_csv(modelData, file)
        }
    )
    
    output$saveData6 <- downloadHandler(
        filename = function(){
            "modelData2.csv"
        },
        content = function(file){
            
            modelData2 <- getData6() 
            
            write_csv(modelData2, file)
        }
    )
    
    
    # output$savePlot2 <- downloadHandler(
    #     filename = function() { 
    #         "variablePlot.png" 
    #                 },
    #     content = function(file) {
    #         
    #         
    #         ggsave(file, plot = clusterPlot)
    #     }
    # )
    
})
#             
        
        
        
        
    
    

    
