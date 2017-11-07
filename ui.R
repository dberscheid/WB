#-----------------------------------------------------
# Define UI 
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    
    
    sidebarPanel(width = 12,
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ";")
    )
    
    
  ),
  dashboardBody( 
    
    
    tabBox(
      title = "",
      id ="tabset1",
      width = 12,
      
      tabPanel("Datenansicht",
               
               #title
               headerPanel(""),
               
               
               fluidRow(
                 
                 mainPanel(
                   
                   # Output: Data file ----
                   tableOutput("contents")
                   
                 )
                 
               )
               
               
      ),
      # tabPanel("Übersicht",
      #          
      #          #title
      #          headerPanel("Zusammenfassung"),
      #          
      #          
      #          fluidRow(
      #            
      #          )
      #          
      #          
      #          
      # ),
      tabPanel("Analysen",
               
               
               
               fluidRow(             
                 
                 box(width = 12, collapsible = TRUE,
                     h3(textOutput("caption5")),
                     mainPanel(width = 9,
                               plotOutput("uebersicht")
                     )
                 )
               ),
               
               
               
               fluidRow(
                 box(width = 12, collapsible = TRUE,
                     h3(textOutput("caption2")),
                     
                     sidebarPanel( width = 2, 
                                   selectInput("jahr2", "Auswahl des Jahres:", listjahr2, width = 200)
                     ),
                     mainPanel(width = 10,
                               box(width = 8,
                                   
                                   plotOutput("plotVerteilung2")),
                               box(width = 4,
                                   verbatimTextOutput("summaryOutput")
                               )   
                     )
                 )
               ),
               fluidRow(             
                 
                 box(width = 12, collapsible = TRUE,
                     h3(textOutput("caption")),
                     sidebarPanel(width = 3,
                                  selectInput("weinart", "Auswahl der Weinsorte:", listweinart, width = 200),
                                  selectInput("jahr", "Auswahl des Jahres:", listjahr, width = 200)
                     ),
                     mainPanel(width = 9,
                               plotOutput("plotVerteilung")
                     )
                 )
               ),
               fluidRow(             
                 
                 box(width = 12, collapsible = TRUE,
                     h3(textOutput("caption3")),
                     sidebarPanel(width = 3,
                                  selectInput("timeStart", "Auswahl des Jahres:", timeStart, width = 200)
                     ),
                     mainPanel(width = 9,
                               plotOutput("plotZeitverlauf")
                     )
                 )
               ),
               
               
               fluidRow(             
                 
                 box(width = 12, collapsible = TRUE,
                     h3(textOutput("caption4")),
                     sidebarPanel(width = 3,
                                  selectInput("name", "Auswahl des Weines:", names, width = 200)
                     ),
                     mainPanel(width = 9,
                               plotOutput("verlaufEinWein")
                     )
                 )
               ) 
               
      ),
      tabPanel("Jahrgang 2017",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen17", width = 6),
                            valueBoxOutput("AnzahlAbfülltage17", width = 6),
                            
                            valueBoxOutput("rebsorten17", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten17", width = 12),
                            valueBoxOutput("weißweine17", width = 4),
                            valueBoxOutput("rotweine17", width = 4),
                            valueBoxOutput("rose17", width = 4),
                            valueBoxOutput("blancdenoir17", width = 4),
                            valueBoxOutput("weißherbst17", width = 4)
                        )
                 )
               )
      ),
      
      tabPanel("Jahrgang 2016",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen16", width = 6),
                            valueBoxOutput("AnzahlAbfülltage16", width = 6),
                            
                            valueBoxOutput("rebsorten16", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten16", width = 12),
                            valueBoxOutput("weißweine16", width = 4),
                            valueBoxOutput("rotweine16", width = 4),
                            valueBoxOutput("rose16", width = 4),
                            valueBoxOutput("blancdenoir16", width = 4),
                            valueBoxOutput("weißherbst16", width = 4)
                        )
                 )
               )
      ),
      
      
      tabPanel("Jahrgang 2015",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen15", width = 6),
                            valueBoxOutput("AnzahlAbfülltage15", width = 6),
                            
                            valueBoxOutput("rebsorten15", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten15", width = 12),
                            valueBoxOutput("weißweine15", width = 4),
                            valueBoxOutput("rotweine15", width = 4),
                            valueBoxOutput("rose15", width = 4),
                            valueBoxOutput("blancdenoir15", width = 4),
                            valueBoxOutput("weißherbst15", width = 4)
                        )
                 )
               )
               
      ),
      tabPanel("Jahrgang 2014",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen14", width = 6),
                            valueBoxOutput("AnzahlAbfülltage14", width = 6),
                            
                            valueBoxOutput("rebsorten14", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten14", width = 12),
                            valueBoxOutput("weißweine14", width = 4),
                            valueBoxOutput("rotweine14", width = 4),
                            valueBoxOutput("rose14", width = 4),
                            valueBoxOutput("blancdenoir14", width = 4),
                            valueBoxOutput("weißherbst14", width = 4)
                        )
                 )
               )
      ),
      
      tabPanel("Jahrgang 2013",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen13", width = 6),
                            valueBoxOutput("AnzahlAbfülltage13", width = 6),
                            
                            valueBoxOutput("rebsorten13", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten13", width = 12),
                            valueBoxOutput("weißweine13", width = 4),
                            valueBoxOutput("rotweine13", width = 4),
                            valueBoxOutput("rose13", width = 4),
                            valueBoxOutput("blancdenoir13", width = 4),
                            valueBoxOutput("weißherbst13", width = 4)
                        )
                 )
               )
      ),
      
      tabPanel("Jahrgang 2012",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen12", width = 6),
                            valueBoxOutput("AnzahlAbfülltage12", width = 6),
                            
                            valueBoxOutput("rebsorten12", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten12", width = 12),
                            valueBoxOutput("weißweine12", width = 4),
                            valueBoxOutput("rotweine12", width = 4),
                            valueBoxOutput("rose12", width = 4),
                            valueBoxOutput("blancdenoir12", width = 4),
                            valueBoxOutput("weißherbst12", width = 4)
                        )
                 )
               )
      ),
      
      tabPanel("Jahrgang 2011",
               #title
               headerPanel("Der Jahrgang auf einen Blick"),
               
               fluidRow(
                 column(width = 4,
                        box(width = 12,
                            valueBoxOutput("flaschen11", width = 6),
                            valueBoxOutput("AnzahlAbfülltage11", width = 6),
                            
                            valueBoxOutput("rebsorten11", width = 12)
                        )
                 ),
                 column(width = 7,
                        box(width = 12,
                            valueBoxOutput("sorten11", width = 12),
                            valueBoxOutput("weißweine11", width = 4),
                            valueBoxOutput("rotweine11", width = 4),
                            valueBoxOutput("rose11", width = 4),
                            valueBoxOutput("blancdenoir11", width = 4),
                            valueBoxOutput("weißherbst11", width = 4)
                        )
                 )
               )
      ),
      
      
      
      tabPanel("Abfüllrechner",
               headerPanel("Schätzung der abzufüllenden Mengen"),
               
               fluidRow(
                 
                 box(width = 12, collapsible = TRUE,
                     h3(textOutput("caption10")),
                     
                     
                     sidebarPanel(width = 3,
                                  selectInput("AuswahlJahrgang", "Auswahl des Jahrgangs:", timeStart, width = '100%', selected = 2017)
                     ),
                     sidebarPanel(width = 3,
                                  selectInput("AuswahlWein", "Auswahl der Weinsorte:", names, width = 200, selected = "Riesling Qualitätswein Classic ")
                     ),
                     sidebarPanel(width = 3,
                                  selectInput("AuswahlDatumStart", "Gewünschtes Abfülldatum (YYYY-MM-DD)", futureDates, width = 300, selected = "2017-12-01")
                     ),
                     sidebarPanel(width = 3,
                                  selectInput("AuswahlDatumEnde", "Ausgetrunken bis (YYYY-MM-DD)", futureDates, width = 200, selected = "2018-03-01")
                     )
                 )
               ),
               
               
               fluidRow(
                 box(width = 12, collapsible = TRUE,
                     
                     h2(textOutput("captionregressionDurchschnitt")),
                     
                     mainPanel(width = 2,
                               verbatimTextOutput("predictionDurchschnitt"))
                     
                     
                 ),
                 box(width = 12, collapsible = TRUE,
                     
                     h2(textOutput("captionregression")),
                     
                     mainPanel(width = 2,
                               verbatimTextOutput("modelPrediction"))
                     
                 ),
                 box(width = 12, collapsible = TRUE,
                     # h3(textOutput("caption10")),
                     
                     mainPanel(width = 12,
                               verbatimTextOutput("modelSummary") #
                     )
                 )
               )  
      )
    )
  )
)