

shinyUI(fluidPage(
  titlePanel("r/12winarenalog Data Analysis"),
  "Click through the tabs below to view summary statistics for 12 arena win logs from the subreddit ",
  a(href="http://www.reddit.com/r/12winArenaLog/", "r/12winarenalog"),
  ".  This website is updated daily.",
  hr(),
  
  fluidRow(
    tabsetPanel(type="tabs",
                tabPanel("Win Summary",
                         textOutput("ClassWinTotals_help"),
                         plotOutput(outputId = "ClassWinTotals", height = "400px", width = "600px")),
                
                tabPanel("Card Rarity",  
                         textOutput("ClassRarityOverview_help"),
                         
                         # Instruction text
                         helpText("Click through card rarity to see the distribution of each category."),
                         
                         # Choose card rarity to be displayed
                         fluidRow(
                           column(4,
                             selectInput(inputId = "rarity",
                                         label = "Card Rarity",
                                         choices = c("Common", "Basic", "Rare", "Epic", "Legendary"),
                                         selected = "Common"))
                         ),
                         
                         # Plot the card rarity which is compsed of two plots
                         
                         plotOutput(outputId = "ClassRarityOverview")),
                
                tabPanel("Top 6 Class Cards",  
                         textOutput("ClassTopCards6_help"),
                         
                         # Instruction text
                         helpText("Click through classes to see the Top 6 cards for that class."),
                         
                         # Choose class to be displayed
                         fluidRow(
                           column(4,
                                  selectInput(inputId = "class_ClassTopCards6",
                                              label = "Class",
                                              choices = c("Mage", "Shaman", "Warrior", "Warlock", "Priest", "Hunter", "Druid", "Rogue", "Paladin"),
                                              selected = "Mage"))
                         ),
                         
                         # Plot the Top 6 cards for the chosen class
                         
                         plotOutput(outputId = "ClassTopCards6", height = "500px", width = "600px")),
                
                tabPanel("Top Neutral Cards",
                         textOutput("TopNeutralCards_help"),
                         
                         # Instruction text
                         helpText("Adjust the number of displayed rows by selecting a different 'records per page'.  You
                                  can also search for cards using the text box at the right.  Finally, order the number of times
                                  the card shows up by clicking on the arrows the right of each column."),
                         
                         dataTableOutput(outputId = "TopNeutralCards")),
                
                tabPanel("Mana Curve",  
                         textOutput("ManaCurve_help"),
                         
                         # Instruction text
                         helpText("Click through classes to see the Mana Curve for that class."),
                         
                         # Choose class to be displayed
                         fluidRow(
                           column(4,
                                  selectInput(inputId = "class_ManaCurve",
                                              label = "Class",
                                              choices = c("Mage", "Shaman", "Warrior", "Warlock", "Priest", "Hunter", "Druid", "Rogue", "Paladin"),
                                              selected = "Mage"))
                         ),
                         
                         # Plot the Mana Curve for the chosen class
                         
                         plotOutput(outputId = "ManaCurve", height = "500px", width = "600px")),
                
                tabPanel("SWM Ratio",
                         textOutput("SWM_Ratio_help"),
                         plotOutput(outputId = "SWM_Ratio")),
                
                tabPanel("CN Ratio",
                         textOutput("CM_Ratio_help"),
                         plotOutput(outputId = "CM_Ratio")),
                
                tabPanel("Charge/Taunt Summary",
                         textOutput("ChargeTauntSum_help"),
                         plotOutput(outputId = "ChargeTauntSum"))
    )
  )
  
))
