#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

source("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/DraftKingsData.R")

source("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/getStat.R")

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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    getData <- reactive({
        playerName <- input$player
        
        playerData <- draftKingsData %>% filter(PlayerName == playerName) %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        playerData
    })
    
    getData2 <- reactive({
        variableName <- input$variables

        variableData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min )
        variableData
    })
    
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

    output$variablePlot <- renderPlotly({
        #get data
        variableData <- getData2()
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
        #add effects if requested
    #     if (input$conservation & input$alpha) {
    #         g + geom_point(size = input$size, aes(col = conservation, alpha = sleep_rem))
    #     } else if (input$conservation) {
    #         g + geom_point(size = input$size, aes(col = conservation))
    #     } else {
    #         g + geom_point(size = input$size)
    #     }
    # })
    
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
        #if(input$player == ){
            # if(input$stat=="Avg Fantasy Points" & input$stat=="Avg Value"){
            # playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Value" = mean(`Actual Val`))
            # } else if(input$stat== "Avg Value"){
            #     playerData %>% group_by(`Player Name`) %>% summarise("Average Value" = mean(`Actual Val`))
            # } else if(input$stat=="Avg Fantasy Points"){
            # playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
            # }
        #} else("Select Statistics")
    
    
    ###Hierarchical Clustering
    
    
    # output$hierCluster <- renderPlotly({
    #     clusterData <- getData4()
    # 
    # 
    # hierClust <- hclust(dist(data.frame(input$xvar, input$yvar)))
    # plot(hierClust, xlab = "")
    # 
    # 
    # })
    
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
        
        
        as.formula(paste(input$Resp, "~", paste(input$lr, collapse= " + ")))
    })
    
    output$lrPlot <- renderPlot({

        modelData <-getData5()

        #Create training set and testing set
        set.seed(31)
        train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData), train)
        predDataTrain <- modelData[train, ]
        predDataTest <- modelData[test, ]

        formula <- as.formula(paste(input$Resp, "~", paste(input$lr, collapse= " + ")))
        
    lrFit <- lm(formula = formula, data = predDataTrain)
    
    lrPredict <- predict(lrFit, predDataTest)
    
    par(mfrow=c(2,2))
    # if(length(input$lr) > 0 ){
    #     ggPredict(lrFit)
    # } else "Select Predictor Variables"
    plot(lrFit)




    })
    
    
    output$lrTable <- renderTable({
        
        modelData <-getData5()
        
        set.seed(31)
        train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData), train)
        predDataTrain <- modelData[train, ]
        predDataTest <- modelData[test, ]
        
        formula <- as.formula(paste(input$Resp, "~", paste(input$lr, collapse= " + ")))
        
        lrFit <- lm(formula = formula, data = predDataTrain)
        
        lrPredict <- predict(lrFit, predDataTest)
        mean(as.vector(lrPredict)-as.vector(predDataTest$FP))^2
        
        # require(MuMIn)
        # fitStats <- data.frame(fitStat = c("Adj R Square", "AIC", "AICc", "BIC", "RMSE"),
        #                        "Linear Regression Fit" = round(c(summary(lrFit)$adj.r.squared, AIC(lrFit),
        #                                       MuMIn::AICc(lrFit), BIC(lrFit), sqrt(mean((lrPredict-predDataTest$FP)^2))), 3))
        # 
        # fitStats
        
        
    })
    
    output$boostPlot <- renderPlot({
        
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
        
        
    })
    
    output$boostTable <- renderTable({
        
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
        
        
    })
    
    
    output$draftKingsDataset <- DT::renderDataTable({
        draftKingsData
    })
    
    
})
#             
        
        
        
        # playerDataTable <- c()
        # for (i in 1:length(input$stat)){
        # playerDataTable[i] <- playerData %>% group_by(`Player Name`) %>% summarise(mean(input$stat[i]))
        # return(input$stat)
        # }
        # statVec <- sapply(FUN = getStat, X = input$stat)
        # statVec
        # if(length(statVec)==0){
        #     "Select some statistics"
        # } else{for(i in 1:length(statVec)){
        # playerData %>% group_by(`Player Name`) %>% summarise(mean(statVec[i]))
        # }}
        # playerDataTable <- c()
        # for (i in 1:length(statVec)){
        # playerDataTable[i] <- playerData %>% group_by(`Player Name`) %>% summarise(mean(statVec[i]))
        # return(playerDataTable)
        # }
    # })
    
    #create table
    # output$playerTable <- renderTable({
    #     
    #     playerData <- getData()
    # 
    #     if(input$stat[1]){
    #         playerData %>% group_by(`Player Name`) %>% summarise("Average Usage" = mean(USG))
    #     } else if (input$stat[2]){
    #         PlayerData %>% group_by(`Player Name`) %>% summarise("Average Player Efficiency Rating" = mean(PER))
    #     } else if(input$stat[3]){
    #         playerData %>% group_by(`Player Name`) %>% summarise("Average Minutes" = mean(`Proj Min`))
    #     } else if(input$stat[4]){
    #         playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
    #     } else if(input$stat[5]){
    #         playerData %>% group_by(`Player Name`) %>% summarise("Average Value" = mean(`Actual Val`))
    #     }
    #     
    # })
# })
            
            
#         if(input$stat == TRUE){
#             playerData %>% group_by(`Player Name`) %>% summarise(mean(input$stat))
#         }
#     })
# })
        
        # if(input$stat=="Avg Fantasy Points"){
        #     playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
        # } else {
        #     if(input$stat=="Avg Usage"){
        #         playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG))
        #     } else {
        #         if(input$stat=="Avg Player Efficiency Rating"){
        #             playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER))
        #         } else{
        #             if(input$stat=="Avg Minutes"){
        #                 playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER), "Average Minutes" = mean(`Proj Min`))
        #             } else {
        #                 if(input$stat=="Avg Value"){
        #                     playerData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER), "Average Minutes" = mean(`Proj Min`), "Average Value" = mean(`Actual Val`))
        #                 } else{
        #                     "Player" = input$player
        #                     }
        #             }}}}
        #     })
        # })
        
    #     if(input$stat=="Avg Fantasy Points"){
    #         if(input$stat=="Avg Usage"){
    #             if(input$stat=="Avg Player Efficiency Rating"){
    #                 if(input$stat=="Avg Minutes"){
    #                     if(input$stat=="Avg Value"){
    #                         newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER), "Average Minutes" = mean(`Proj Min`), "Average Value" = mean(`Actual Val`))
    #                     } else{
    #                         newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER), "Average Minutes" = mean(`Proj Min`))
    #                         }} else{
    #                             newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER))
    #                             }} else{
    #                             newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG))
    #                                 }} else{
    #                             newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
    #                                     }} else{
    #                                         input$player
    #                                     }
    #                 
    #     })
    # })
                            
#                         newData %>% group_by(`Player Name`) %>% summarise("Average Minutes" = mean(`Proj Min`))
#                     }
#                     newData %>% group_by(`Player Name`) %>% summarise("Average Player Efficiency Rating" = mean(PER))
#                 }
#                 newData %>% group_by(`Player Name`) %>% summarise("Average Usage" = mean(USG))
#             }
#             newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
#         } else{
#             "Player" = input$player
#         }
#     })
# })
        
#         if(input$stat=="Avg Fantasy Points"){
#             newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
#         } else{
#             if(input$stat=="Avg Usage"){
#                 newData %>% group_by(`Player Name`) %>% summarise("Average Usage" = mean(USG))
#             } else{
#                 if(input$stat=="Avg Usage"){
#                     newData %>% group_by(`Player Name`) %>% summarise("Average Usage" = mean(USG))
#                 } else{
#                     if(input$stat=="Avg Player Efficiency Rating"){
#                         newData %>% group_by(`Player Name`) %>% summarise("Average Player Efficiency Rating" = mean(PER))
#                     } else{
#                         if(input$stat=="Avg Minutes"){
#                             newData %>% group_by(`Player Name`) %>% summarise("Average Minutes" = mean(`Proj Min`))
#                         } else{
#                             if(input$stat=="Avg Value"){
#                                 newData %>% group_by(`Player Name`) %>% summarise("Average Value" = mean(`Actual Val`))
#                             }
#                         }}}}}
#     })
# })
        
        # else {
        #     if(input$stat=="Avg Usage"){
        #         newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG))
        #     } else {
        #         if(input$stat=="Avg Player Efficiency Rating"){
        #             newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER))
        #         } else{
        #             if(input$stat=="Avg Minutes"){
        #                 newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER), "Average Minutes" = mean(`Proj Min`))
        #             } else {
        #                 if(input$stat=="Avg Value"){
        #                     newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`), "Average Usage" = mean(USG), "Average Player Efficiency Rating" = mean(PER), "Average Minutes" = mean(`Proj Min`), "Average Value" = mean(`Actual Val`))
        #                 } else{
        #                     input$player
        #                     }
        #             }}}}
#     })
# })
        
        # if(input$stat=="Avg Fantasy Points"){
        # newData %>% group_by(`Player Name`) %>% summarise("Average Fantasy Points" = mean(`Actual FP`))
        # } else{
        #         input$player
        #    } 
        #, "Average Value" = mean(`Actual Val`))
        
        
    
    

    
