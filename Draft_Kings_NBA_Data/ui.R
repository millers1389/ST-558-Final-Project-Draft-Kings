#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

dashboardPage(skin = "blue",
              dashboardHeader(title = "Draft Kings NBA Application", titleWidth = 1000),
              
              dashboardSidebar(sidebarMenu(
                  menuItem("Data and Application Information", tabName = "info", icon = icon("archive")),
                  menuItem("Draft Kings NBA Data Exploration", tabName = "graphs", icon = icon("basketball-ball")),
                  menuItem("Unsupervised Learning", tabName = "usl", icon = icon("basketball-ball")),
                  menuItem("Modeling", tabName = "model", icon = icon("basketball-ball")),
                  menuItem("Draft Kings NBA Data", tabName = "data", icon = icon("basketball-ball"))
                  ) #Closes Sidebar Menu
              ), #Closes Dashboard Sidebar
              
              
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "info",
                              fluidRow(
                                  withMathJax(),
                                  column(6,
                                         h1("About the Data"),
                                         box(background = "light-blue", width = 12,
                                             tabsetPanel(
                                             tabPanel("Data Set Description",
                                                      
                                                          h5("This application uses data found in a Draft Kings NBA data set compiled by Alan Du on Kaggle called Daily Fantasy Basketball - DraftKings NBA.(LINK TO WEBSITE)"),
                                                          h5("The data set includes about a month of Draft Kings NBA game data from November 27, 2017 to December 28, 2017. This data includes individual player statistics, opposing team statistics, projected fantasy points on a given night, and the actual fantasy points scored by that player that night, along with other statistics to predict daily fantasy basketball success such as player value and salary."),
                                                          h5("The data set also included Draft Kings contest information, player salary information, and Draft King payout structures. However, this appliction only uses a compilation of the month of Draft Kings NBA data ")
                                                      ),
                                             
                                             tabPanel("Data Manipulation",
                                                      h5("In order to get the data in a usable form, I had to compile each day's data into one data frame for the entire month."),
                                                      h5("I read in each individual CSV file (one for each day) and joined them into a data frame that consisted of the entire month of data."),
                                                      h5("I coerced certain variables to the proper class type so that they can be used properly in the modeling and predictions of the application. I also removed all players with an 'out' injury designation so they would not impact the predictions."),
                                                      h5("Lastly, I removed some of the variables that did not aid in the modeling of the data.")),
                                             
                                             tabPanel("Variables Used",
                                                      h5("Player Name"),
                                                      h5("Inj - Injury Status"),
                                                      h5("Pos - Position"),
                                                      h5("Salary - Draft Kings Salary"),
                                                      h5("Team - Player's Team"),
                                                      h5("Opp - Opposing Team"),
                                                      h5("Rest - Number of Days off since Last Game"),
                                                      h5("USG - Usage Rate"),
                                                      h5("PER - Player Efficiency Rating"),
                                                      h5("Opp Pace - Opponenet Pace of Play"),
                                                      h5("Opp DEff - Opponent Defensive Efficiency"),
                                                      h5("Opp DvP - Opponent Defensive Rating vs Position"),
                                                      h5("L2 FGA - Average Field Goals Attempted over Last 2 Games"),
                                                      h5("L5 FGA - Average Field Goals Attempted over Last 5 Games"),
                                                      h5("S FGA - Season Average Field Goals Attempted"),
                                                      h5("L2 Min - Average Minutes Played over Last 2 Games"),
                                                      h5("L5 Min - Average Minutes Played over Last 5 Games"),
                                                      h5("S Min - Season Average Minutes Played"),
                                                      h5("L2 FP - Average Fantasy Points over Last 2 Games"),
                                                      h5("L5 FP - Average Fantasy Points over Last 5 Games"),
                                                      h5("S FP - Season Average Fantasy Points"),
                                                      h5("Floor FP - Season Low Fantasy Points"),
                                                      h5("Ceil FP - Season High Fantasy Points"),
                                                      h5("Proj Min - Projected Minutes"),
                                                      h5("Actual FP - Actual Fantasy Points Scored that Night (Response Variable)"),
                                                      h5("Actual Value - Actual Draft Kings Value (Response Variable)")
                                                      )#Closes Tab Panel
                                             ) #Closes TabSet Panel
                                             ) #Closes Box
                                         ), #Closes Column
                                  
                                             
                                                      
                                         # box(background = "light-blue", width = 12,
                                         #     tabBox(title = "Data Description",
                                         #            # The id lets us use input$tabset1 on the server to find the current tab
                                         #            id = "datadesc", height = "250px",
                                         #            tabPanel("Data Set Description", "This application uses data found in a Draft Kings NBA data set compiled by Alan Du on Kaggle called Daily Fantasy Basketball - DraftKings NBA.(LINK TO WEBSITE)
                                         #                     The data set includes about a month of Draft Kings NBA game data from November 27, 2017 to December 28, 2017. This data includes individual player statistics, opposing team statistics, projected fantasy points on a given night, and the actual fantasy points scored by that player that night, along with other statistics to predict daily fantasy basketball success such as player value and salary.
                                         #                     The data set also included Draft Kings contest information, player salary information, and Draft King payout structures. However, this appliction only uses a compilation of the month of Draft Kings NBA data "),
                                         # 
                                         #            tabPanel("Data Manipulation", "In order to get the data in a usable form, I had to compile each day's data into one data frame for the entire month.
                                         #                     I read in each individual CSV file (one for each day) and joined them into a data frame that consisted of the entire month of data.
                                         #                     I coerced certain variables to the proper class type so that they can be used properly in the modeling and predictions of the application. I also removed all players with an 'out' injury designation so they would not impact the predictions.
                                         #                     Lastly, I removed some of the variables that did not aid in the modeling of the data."),
                                         # 
                                         #            tabPanel("Variables Used", "Player Name
                                         #            Inj - Injury Status
                                         #            Pos - Position
                                         #            Salary - Draft Kings Salary
                                         #            Team - Player's Team
                                         #            Opp - Opposing Team
                                         #            Rest - Number of Days off since Last Game
                                         #            USG - Usage Rate
                                         #            PER - Player Efficiency Rating
                                         #            Opp Pace - Opponenet Pace of Play
                                         #            Opp DEff - Opponent Defensive Efficiency
                                         #            Opp DvP - Opponent Defensive Rating vs Position
                                         #            L2 FGA - Average Field Goals Attempted over Last 2 Games
                                         #            L5 FGA - Average Field Goals Attempted over Last 5 Games
                                         #            S FGA - Season Average Field Goals Attempted
                                         #            L2 Min - Average Minutes Played over Last 2 Games
                                         #            L5 Min - Average Minutes Played over Last 5 Games
                                         #            S Min - Season Average Minutes Played
                                         #            L2 FP - Average Fantasy Points over Last 2 Games
                                         #            L5 FP - Average Fantasy Points over Last 5 Games
                                         #            S FP - Season Average Fantasy Points
                                         #            Floor FP - Season Low Fantasy Points
                                         #            Ceil FP - Season High Fantasy Points
                                         #            Proj Min - Projected Minutes
                                         #            Actual FP - Actual Fantasy Points Scored that Night (Response Variable)
                                         #            Actual Value - Actual Draft Kings Value (Response Variable)")
                                         #     )
                                         #     )
                                         # ),
                                  column(6,
                                         h1("Application Functionality"),
                                         box(background = "light-blue", width = 12,
                                             h4("This application uses the Draft Kings NBA data to further analyze trends in the data. The application has 4 additional tabs: Data Exploration, Unsupervised Learning (Clustering), Modeling, and Data, which allow the user to select particular parameters and further explore trends in this data.")
                                             ) #Closes Box
                                         ) #Closes Column
                              ) #Closes Fluid Row
                                  ) #Closes TabItem
                              ) #Closes TabItems
                  ) #Closes Dashboard Body
              ) #Closes Dashboard Page
