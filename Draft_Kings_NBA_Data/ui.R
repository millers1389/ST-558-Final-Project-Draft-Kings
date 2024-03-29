#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#


library(shiny)
library(shinydashboard)
library(plotly)


#DASHBOARD
dashboardPage(skin = "blue",
      dashboardHeader(title = "Draft Kings NBA Application", titleWidth = 1000),
              
      dashboardSidebar(sidebarMenu(
          ##TABS
          #TAB1 INFO
          menuItem("Data and App Information", 
                   tabName = "info", icon = icon("archive")),
          
          #TAB 2 GRAPHS
          menuItem("Draft Kings Data Exploration", 
                   tabName = "graphs", icon = icon("basketball-ball")),
          
          #TAB3 USL
          menuItem("Unsupervised Learning", 
                   tabName = "usl", icon = icon("basketball-ball")),
                  
          #TAB4 MODEL
          menuItem("Modeling", 
                   tabName = "model", icon = icon("basketball-ball")),
          
          #TAB5 DATA
          menuItem("Draft Kings NBA Data", 
                   tabName = "data", icon = icon("basketball-ball"))
          ) #Closes Sidebar Menu
      ), #Closes Dashboard Sidebar
      
      
      dashboardBody(
          tabItems(
              
              
              #FIRST TAB BODY
              tabItem(tabName = "info",
                # fluidRow(
                #   withMathJax(),
                  column(6,
                    h1("About the Data"),
                    box(width = 12,
                        tabsetPanel(
                                     
                        #FIRST TAB IN FIRST BOX
                        tabPanel("Data Set Description",
                              h5("This application uses data found in a Draft Kings NBA data set compiled by Alan Du on Kaggle called Daily Fantasy Basketball - DraftKings NBA. The data can be found at the following website:"),
                              a("Draft Kings Data", href="https://www.kaggle.com/alandu20/daily-fantasy-basketball-draftkings"),
                        
                              h5("The data set includes about a month of Draft Kings NBA game data from November 27, 2017 to December 28, 2017. This data includes individual player statistics, opposing team statistics, projected fantasy points on a given night, and the actual fantasy points scored by that player that night, along with other statistics to predict daily fantasy basketball success such as player value and salary."),
                              h5("The data set also included Draft Kings contest information, player salary information, and Draft King payout structures. However, this appliction only uses a compilation of the month of Draft Kings NBA data ")
                        ),#Closes First Tab
                                         
                        #SECOND TAB IN FIRST BOX
                        tabPanel("Data Manipulation",
                              h5("In order to get the data in a usable form, I had to compile each day's data into one data frame for the entire month."),
                              h5("I read in each individual CSV file (one for each day) and joined them into a data frame that consisted of the entire month of data."),
                              h5("I coerced certain variables to the proper class type so that they can be used properly in the modeling and predictions of the application. I also removed all players with an 'out' injury designation so they would not impact the predictions."),
                              h5("Lastly, I removed some of the variables that did not aid in the modeling of the data.")
                        ), #Closes Second Tab
                                         
                        #THIRD TAB IN FIRST BOX
                        tabPanel("Variables Used",
                              h2("Variables"),
                              h5("Player Name"),
                              h5("Inj - Injury Status"),
                              h5("Pos - Position"),
                              h5("Salary - Draft Kings Salary"),
                              h5("Team - Player's Team"),
                              h5("Opp - Opposing Team"),
                              h5("Rest - Number of Days off since Last Game"),
                              h5("USG - Usage Rate"),
                              h5("PER - Player Efficiency Rating"),
                              h5("Pace - Opponenet Pace of Play"),
                              h5("DEff - Opponent Defensive Efficiency"),
                              h5("DvP - Opponent Defensive Rating vs Position"),
                              h5("L2FGA - Average Field Goals Attempted over Last 2 Games"),
                              h5("L5FGA - Average Field Goals Attempted over Last 5 Games"),
                              h5("SFGA - Season Average Field Goals Attempted"),
                              h5("L2Min - Average Minutes Played over Last 2 Games"),
                              h5("L5Min - Average Minutes Played over Last 5 Games"),
                              h5("SMin - Season Average Minutes Played"),
                              h5("L2FP - Average Fantasy Points over Last 2 Games"),
                              h5("L5FP - Average Fantasy Points over Last 5 Games"),
                              h5("SFP - Season Average Fantasy Points"),
                              h5("Floor - Season Low Fantasy Points"),
                              h5("CeilFP - Season High Fantasy Points"),
                              h5("ProjMin - Projected Minutes"),
                              h5("FP - Actual Fantasy Points Scored that Night 
                                 (Response Variable)"),
                              h5("Value - Actual Draft Kings Value (Response Variable)")
                         )#Closes Third Tab - Tab Panel
                        )#Closes TabSet Panel
                    ) #Closes Box
                  ), #Closes Column
                          
                #Second Box in First Tab
                column(6,
                    h1("Application Functionality"),
                    box(width = 12,
                        h5("This application uses the Draft Kings NBA data to further analyze trends in the data. The application has 4 additional tabs: Data Exploration, Unsupervised Learning (K-Means Clustering), Modeling (Linear Regression and Boosted), and Data, which allow the user to select particular parameters and further explore trends in this data. The ultimate goal is for the user to select predictor variables for the model and input values for these predictors to predict the fantasy points scored by a player.")
                    ) #Closes Box
                ) #Closes Column
            # ) #Closes Fluid Row
        ), #Closes TabItem
              
              
              #SECOND TAB BODY
              tabItem(tabName = "graphs",
                      h1("Visualizing the Data"),
                tabsetPanel(
                    #FIRST SIDETAB OF SECOND TAB
                    tabPanel("Player Data Table",
                        sidebarLayout(
                            sidebarPanel(
                                
                            h3("Choose a Player:"),
                            selectizeInput("player", "Player",
                                selected = "LeBron James",
                                choices = levels(as.factor(draftKingsData$PlayerName))),
                            
                            h3("Choose a Stat:"),
                            selectizeInput("stat", "Statistic",
                               selected = "Average Fantasy Points",
                               choices = c("Average Fantasy Points", 
                                           "Average Value", 
                                           "Average Minutes", 
                                           "Average Usage", 
                                           "Average Player Efficiency Rating",
                                           "Average Field Goals Attempted")),
                            
                            downloadButton("saveData1", "Download Data")
                           
                             
                                
                            # h3("Select Average Statistics to Include:"),
                            #     checkboxGroupInput("stat", label ="Stats",
                            #         choices=c(#"Avg Usage",
                            #                 # "Avg Player Efficiency Rating",
                            #                 # "Avg Minutes",
                            #                 "Avg Fantasy Points",  
                            #                 "Avg Value"),
                            #                 width="100%")
                              ),#Closes Sidebar Panel
                            
                            
                    #FIRST MAIN TAB
                    mainPanel(
                        tableOutput("playerTable")
                        ) #Closes mainPanel
                    )#closes sidebar Layout
                ),#Closes tabPanel
                    
                    #####SECOND TAB GRAPHS
                    tabPanel("Variable Graphs",
                        sidebarLayout(
                            #SECOND SIDETAB
                            sidebarPanel(
                                
                                h3("Choose a variable to see trends in fantasy points:"),
                                selectizeInput("variables", "Variables", 
                                      selected = "Actual Fantasy Points", 
                                      choices = c("Actual Fantasy Points", "Usage", 
                                                  "Player Efficiency Rating", 
                                                  "Salary", "Rest", "Position", 
                                                  "Opponent Defensive Efficiency", 
                                                  "Opponent Defense vs Position", 
                                                  "Opponent Pace", 
                                                  "Average Field Goals Attempted", 
                                                  "Average Minutes", "
                                                  Season Average Fantasy Points", 
                                                  "Value", "Salary Vs. Value")),
                                
                                conditionalPanel("input.variables", 
                                    checkboxInput("zero", 
                                        h5("Remove observations with 0 fantasy points"))),
                                
                                downloadButton("savePlot1", "Save Plot"),
                                downloadButton("saveData2", "Download Data")
                                
                                ), #closes sidebarPanel
                            
                            #SECOND MAIN TAB
                      mainPanel(
                        plotlyOutput("variablePlot")
                          ) #Closes mainpanel
                          )#closes sidebar layout
                          ) #Closes tabPanel
                      ) #Closes tabset Panel
                      
                            ), #Closes TabItem
              ###CLOSES SECOND TAB
              
              #TAB3 USL
              tabItem(tabName = "usl",
                h1("Unsupervised Learning: Clustering"),
                sidebarLayout(
                  sidebarPanel(
                    h3("Select a variable:"),
                    
                    selectizeInput("xvar", "Variables", 
                          choices = c("Actual Fantasy Points" = "FP", 
                                    "Value" = "Value", "Usage" = "USG", 
                                    "Player Efficiency Rating" = "PER", "
                                    Salary" = "Salary", "Rest" = "Rest", 
                                    "Opponent Defensive Efficiency" = "DEff", 
                                    "Opponent Defense vs Position"= "DvP", 
                                    "Opponent Pace" = "Pace", 
                                    "Average Field Goals Attempted" = "SFGA", 
                                    "Average Minutes"= "SMin", 
                                    "Season Average Fantasy Points" = "SFP")),#Closes selectizeInput
                    
                    br(),
                    
                    h3("Select a DIFFERENT variable:"),
                    selectizeInput("yvar", "Variables", 
                          choices = c("Season Average Fantasy Points" = "SFP", 
                                      "Actual Fantasy Points" = "FP", "Value" = "Value",
                                      "Usage" = "USG", "Player Efficiency Rating" = "PER",
                                      "Salary" = "Salary", "Rest" = "Rest", 
                                      "Opponent Defensive Efficiency" = "DEff", 
                                      "Opponent Defense vs Position"= "DvP", 
                                      "Opponent Pace" = "Pace", 
                                      "Average Field Goals Attempted" = "SFGA", 
                                      "Average Minutes"= "SMin")),#Closes selectizeInput
                    
                    numericInput('cluster', 'Cluster Count', 3,
                                 min = 1, max = 9),
                    
                    downloadButton("savePlot2", "Save Plot"),
                    downloadButton("saveData3", "Download Data")
                  ),#Closes sidebarPanel
                  mainPanel(
                      plotOutput("clusterPlot")
                      )#Closes mainPanel
                  )#Closes sidebarLayout
                ),#Closes tabItem
        
                    
                    
              #TAB4 MODEL
              tabItem(tabName = "model",
                h1("Predicting the Data"),
                tabsetPanel(
                    #FIRST SIDETAB OF SECOND TAB
                    tabPanel("Linear Regression",
                      sidebarLayout(
                          sidebarPanel(
                              h3("Choose predictor variables for linear regression model:"),
                              checkboxGroupInput("lr", "Linear Regression Predictors",
                                  choices = c("Usage" = "USG", "Player Efficiency Rating" =
                                    "PER", "Rest" = "Rest", 
                                    "Opponent Defensive Efficiency" = "DEff",
                                    "Opponent Defense vs Position"= "DvP", 
                                    "Opponent Pace" = "Pace", 
                                    "Last 2 Games: Average Field Goals Attempted" = "L2FGA",
                                    "Last 5 Games: Average Field Goals Attempted" = "L5FGA",
                                    "Average Field Goals Attempted" = "SFGA", 
                                    "Projected Minutes" = "ProjMin", 
                                    "Last 5 Games: Average Fantasy Points" = "L5FP", 
                                    "Season Average Fantasy Points" = "SFP" )), #Closes CheckboxGroup
                              
                              downloadButton("savePlot3", "Save Plot"),
                              downloadButton("saveData4", "Download Data")
                                    
                                ),#Closes sidebarPanel
                          mainPanel(

                              textOutput("Formula"),
                              plotOutput("lrPlot"),
                              tableOutput("lrTable")
                                )
                                )#Closes Sidebar Layout
                            ),#Closes TabPanel

                    tabPanel("Boosted Model",
                        sidebarLayout(
                          sidebarPanel(
                            h3("Choose predictor variables for boosted regression model:"),
                            
                            checkboxGroupInput("boosted", "Boosted Model Predictors", 
                                  choices = c("Usage" = "USG", "Player Efficiency Rating" =
                                      "PER", "Rest" = "Rest", 
                                      "Opponent Defensive Efficiency" = "DEff",
                                      "Opponent Defense vs Position"= "DvP", 
                                      "Opponent Pace" = "Pace", 
                                      "Last 2 Games: Average Field Goals Attempted" = "L2FGA",
                                      "Last 5 Games: Average Field Goals Attempted" = "L5FGA",
                                      "Average Field Goals Attempted" = "SFGA", 
                                      "Projected Minutes" = "ProjMin", 
                                      "Last 5 Games: Average Fantasy Points" = "L5FP", 
                                      "Season Average Fantasy Points" = "SFP" )),
                            
                            numericInput("trees", 
                                         "Select Number of Trees (between 100 and 5000):",
                                         1000, min=100, max = 5000),
                                           
                            numericInput("shrinkage", 
                                         "Select Shrinkage:", .01, min=0, max = 1),
                            
                            numericInput("interaction", 
                                         "Select Interaction Depth:", 1, min=1, max = 20),
                            
                            downloadButton("savePlot4", "Save Plot"),
                            downloadButton("saveData5", "Download Data")
                            
                            
                                       ),#Closes sidebarPanel
                          mainPanel(
                            textOutput("boostFormula"),
                            plotOutput("boostPlot"),
                            tableOutput("boostTable")
                                           )
                                   )#Closes sidebarLayout
                          ),# Closes tabPanel


                          tabPanel("Making Predictions",
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Select Predictor Parameters"),
                                           h3("Choose a Player:"),
                                           selectizeInput("player2", "Player",
                                            selected = "LeBron James",
                                            choices =
                                                levels(as.factor(draftKingsData$PlayerName))),
                                           numericInput("Usage", "Fill in Average Usage",
                                                        value = 20,  min=0, max=40),

                                           numericInput("PER", "Fill in PER",
                                                        value = 20,  min=0, max=40),

                                           numericInput("SFGA", "Fill in Season Average Field Goal Attempts",
                                                        value = 10,  min=0, max=40),

                                           numericInput("Mins", "Fill in Projected Minutes",
                                                        value = 24,  min=0, max=48),
                                           numericInput("DEff", "Fill in Defensive Efficiency",
                                                      value = 103,  min=97, max=109),
                                           numericInput("Pace", "Fill in Opponent Pace",
                                                        value = 103, min=97, max=109),
                                           numericInput("DvP",
                                                        "Fill in Opponent Defense Vs Position",
                                                        value = 0, min=-18, max=13),
                                           numericInput("SFP", "Fill in Season Average Fantasy Points",
                                                        value = 10,  min=0, max=100),
                                           br(),
                                           h3("Select tuning parameters for boosted model prediction"),
                                           

                                           numericInput("trees2", "Select Number of Trees (between 100 and 5000):",
                                                        1000, min=100, max = 5000),
                                           numericInput("shrinkage2", "Select Shrinkage:",
                                                        value = .01, min=.00001, max = 1),
                                           numericInput("interaction2",
                                                        "Select Interaction Depth:",
                                                        1, min=1, max = 20),
                                           
                                           downloadButton("savePlot5", "Save Plot"),
                                           downloadButton("saveData6", "Download Data")
                                       ),#Closes sidebarPanel
                                       
                                       mainPanel(
                                         h1("Predicted Fantasy Points"),
                                         tableOutput("Predictions")

                                       )
                                   )#Closes sidebarLayout
                          )# Closes tabPanel

                          )#Closes TabsetPanel
                      ),#Closes Tab Item,

              #TAB5 DATA
              tabItem(tabName = "data",
                      fluidRow(
                        column(12,
                               h1("The Data"),
                               DT::dataTableOutput("draftKingsDataset"))))
                      ) #Closes TabItems
          ) #Closes Dashboard Body
      ) #Closes Dashboard Page
