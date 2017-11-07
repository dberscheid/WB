
# Shiny App für Weingut Braun 


# install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(scales)


# #starting here
# load("1.RData")
data <- read.csv("qualitaetspruefung_021067.csv", sep = ";", header = TRUE, encoding = "UTF-8")

#needed variables
listjahr <-c("2014","2015","2016","2017")
listweinart <- levels(data$Weinart)
listjahr2 <-c("2014","2015","2016","2017")
listweinart2 <- levels(data$Weinart)
listTime <- levels(as.factor(data$Antrags.Jahr))
timeStart <-c("2014","2015","2016","2017")
names <- levels(data$Bezeichnung.Z2)
futureDates <- seq.Date(as.Date("2017-11-01"), as.Date("2020-01-01"), by = "day")

#-------------------------------------
# source("WeingutApp/server.R", encoding="UTF-8")

source("ui.R", encoding="UTF-8")

#-------





#---------------------------------#
#---Data Cleaning & Preparation---#
#---------------------------------#
data$Menge.Fass         <- NULL
data$Dienststelle       <- NULL 
data$Betriebs.Nr        <- NULL
data$Produktart         <- NULL
data$Cuvee.Nr           <- NULL 
data$Rebsorte.2         <- NULL 
data$Rebsorte.3         <- NULL
data$Gärverfahren       <- NULL
data$Gütezeichen        <- NULL
data$Hochgewächs        <- NULL
data$Lageprofil         <- NULL
data$Sonderbezeichnung  <- NULL
data$Sonderherkunft     <- NULL 
data$Begründung         <- NULL 
data$Rücknahme          <- NULL

#Preparation Datum 
data$Bescheiddatum <- as.Date(data$Bescheiddatum)
library(lubridate)
myDate <- as.POSIXct(data$Bescheiddatum)
month <- format(myDate,"%m")
day <- format(myDate,"%d")

data$month <- as.numeric(month)
data$day <- as.numeric(day)

data$monthDeviation <- 03-data$month # e.g. -3 ist 3 Monate zu sp?t 
data$monthDevInDays <- data$monthDeviation * 30 
data$dayDeviation <- 15-data$day    # angenommen, dass Mitte M?rz das optimale Abf?lldatum ist
totalDeviation <- data$monthDevInDays + data$dayDeviation
data$totalDeviation <- totalDeviation


#kleinerer Datensatz
df <- data %>% 
  select(Menge.Flasche, Jahrgang, Weinart, Rebsorte.1,Bezeichnung.Z2, Qualität,Geschmack.beantr,Classic_Selection,
         Geschmack.gesetzl, Präm.Auszeichnung, Bescheiddatum, totalDeviation)




# Preparation timeTillEmpty
df$Bescheiddatum <- lubridate::ymd(df$Bescheiddatum)
dplyr::arrange(df, Bescheiddatum)

df <- df %>% 
  arrange(Bescheiddatum) %>% 
  arrange(Bezeichnung.Z2) 


#make subset for each wine 
for(j in 1:length(unique(df$Bezeichnung.Z2))) {
  df %>% 
    filter(Bezeichnung.Z2 == unique(Bezeichnung.Z2)[j])
  
  for(i in 1:length(df$Bescheiddatum)-1){
    
    df$timeTillEmpty[i] <- df$Bescheiddatum[i+1]-df$Bescheiddatum[i]
    df$timeTillEmpty[i+1] <- NA
  }
}

df$timeTillEmpty[which(df$timeTillEmpty<0)] <- NA #these cases do not make sense

#zwei Einträge für eine Abfüllung momentan nur 3 Cases vorhanden. Daher zunächst nicht beachtet
df$timeTillEmpty[which(df$timeTillEmpty==0)] 



df$totalDeviation <- NULL  

#++++++++++++++++++++++++++
# Zielvariable: durchschnittl. Flaschen pro Tag 
#achtung: timeTillEmpty ist nuur grob richtig, da pro Monat 30 Tage angenommen

#wurde in den Anfangsjahren nur teilweise das Zeug eingetragen und hochgeladen?
#wenn nur ab Jahrgang 2013 berücksichtig wird, sind die Ergebnisse plausibler
# wenn erst ab 2014 dann leider noch besser


#nur Übergangslösung
#wenn eine Abfüllung in zwei Einträge gekommen ist, damm müssen die zusammengelegt werden

df$timeTillEmpty[which(df$timeTillEmpty==0)]   #  50  52 113
#hässliche Variante
df$Menge.Flasche[50+1] <- sum(df$Menge.Flasche[50], df$Menge.Flasche[50+1])
df <- df[-50,]

which(df$timeTillEmpty==0) #51 112
df$Menge.Flasche[51+1] <- sum(df$Menge.Flasche[51], df$Menge.Flasche[51+1])
df <- df[-51,]

which(df$timeTillEmpty==0) #111
df$Menge.Flasche[111+1] <- sum(df$Menge.Flasche[111], df$Menge.Flasche[111+1])
df <- df[-111,]

which(df$timeTillEmpty==0) # none 


#seasonal variable:
myDate <- as.POSIXct(df$Bescheiddatum)

df$frühling  <- NA 
df$sommer <- NA
df$herbst <- NA 
df$winter <- NA 


df$months <- NA 

for(i in 2:length(df$Bescheiddatum)-1){
  
  if(is.na(df$timeTillEmpty[i])) {
    df$timeTillEmpty[i]
  } else {
    
    
    seqstart <- as.Date(df$Bescheiddatum[i])
    seqend <- as.Date(df$Bescheiddatum[i+1])
    
    seq_dates <- seq.Date(seqstart, seqend, by = "month")
    
    months <- format(seq_dates, "%m")
    months <- toString(months)
    
    df$months[i] <- months
    
    df$months[i+1] <- NA
    
    if(grepl("03|04|05" ,df$months[i])){
      df$frühling[i] <- 1
    } else {
      df$frühling[i] <- 0
    }
    
    
    if (grepl("06|07|08" ,df$months[i])){
      df$sommer[i] <- 1
    } else {
      df$sommer[i] <- 0
    }
    if (grepl("09|10|11" , df$months[i])) {
      df$herbst[i] <- 1 
    } else {
      df$herbst[i] <- 0
    } 
    
    
    if (grepl("12|01|02", df$months[i])) {
      df$winter[i] <- 1
    } else {
      df$winter[i] <- 0
    }
    
  }   
  
  
}


df$frühling <- factor(df$frühling)
df$sommer <- factor(df$sommer)
df$herbst <- factor(df$herbst)
df$winter <- factor(df$winter)




#removes many rows with na, maybe there s a better solution
#die Letzten Abfüllungen sind jeweils noch nicht leer, daher kann man sie noch nicht in der Regression berücksichtigen
df <- df[!is.na(df$timeTillEmpty),] 


#Berechne Zielvariable
df <- df %>% 
  mutate(BottlesPerDay = Menge.Flasche/timeTillEmpty) 

df$timeTillEmpty <- NULL
df$Menge.Flasche <- NULL






#Do not use earliest years as the entries are probably not complete
df <- df %>% 
  filter(Jahrgang >= 2013)



#Graubburgunder classic und grauer burgunder classic zusammenlegen
#Behandlung Grauer Burgunger und Weißer Burgunder
df$Rebsorte.1 <- gsub("Weißburgunder", "Weißer Burgunder", df$Rebsorte.1)
df$Rebsorte.1 <- gsub("Grauburgunder", "Grauer Burgunder", df$Rebsorte.1)
unique(df$Rebsorte.1)

df$Rebsorte.1 <- factor(df$Rebsorte.1)


#relevel for another reference category
# df$Weinart <- factor(df$Weinart, levels = c("Weißwein","Rotwein","Weißherbst","blanc de noir"))
# df <- within(df, Weinart <- relevel(Weinart, ref = 2))
df <- within(df, Bezeichnung.Z2 <- relevel(Bezeichnung.Z2, ref = 30))


#---------------------------
#season variable makes sense 
#-> add this feature later



#--------------------------
df <- df %>% 
  select( Bezeichnung.Z2, BottlesPerDay, frühling, sommer, herbst, winter) 

lm2 <- lm(df$BottlesPerDay ~ ., data = df) 

# summary(lm2) 



customFunction <- function(AuswahlJahrgang,AuswahlWein, AuswahlDatumStart, AuswahlDatumEnde) {
  #braucht nu noch auswahl jahrgang etc
  
  
  
  newdata <- as.data.frame(AuswahlJahrgang)
  newdata <- newdata %>%
    rename(Jahrgang = AuswahlJahrgang) %>%
    cbind(AuswahlWein) %>%
    rename(Bezeichnung.Z2 = AuswahlWein) %>%
    cbind(AuswahlDatumStart) %>%
    rename(abfuelldatumWunsch = AuswahlDatumStart) %>%
    cbind(AuswahlDatumEnde) %>%
    rename(leerWunsch = AuswahlDatumEnde)
  
  seq_dates <- seq.Date(as.Date(newdata$abfuelldatumWunsch), as.Date(newdata$leerWunsch), by = "month")
  
  
  months <- format(seq_dates, "%m")
  months <- toString(months)
  #
  newdata$months <- months
  #
  
  #
  if(grepl("03|04|05" ,newdata$months)){
    newdata$frühling <- 1
  } else {
    newdata$frühling <- 0
  }
  #
  #
  if (grepl("06|07|08" ,newdata$months)){
    newdata$sommer <- 1
  } else {
    newdata$sommer <- 0
  }
  if (grepl("09|10|11" , newdata$months)) {
    newdata$herbst <- 1
  } else {
    newdata$herbst <- 0
  }
  
  
  if (grepl("12|01|02", newdata$months)) {
    newdata$winter <- 1
  } else {
    newdata$winter <- 0
  }
  
  newdata$months <- NULL
  
  newdata$frühling <- factor(newdata$frühling)
  newdata$sommer <- factor(newdata$sommer)
  newdata$herbst <- factor(newdata$herbst)
  newdata$winter <- factor(newdata$winter)
  
  
  
  newdata$abfuelldatumWunsch <- as.Date(newdata$abfuelldatumWunsch, "%d.%m.%Y")
  newdata$leerWunsch <- as.Date(newdata$leerWunsch, "%d.%m.%Y")
  
  newdata$predictions <- predict(lm2, newdata = newdata)
  newdata$predMenge <- newdata$predictions
  newdata$predictions <- NULL
  
  
  newdata$predMengeFlaschen <-  round((newdata$leerWunsch - newdata$abfuelldatumWunsch)  * newdata$predMenge,
                                      digits = 0)
  
  
  return(newdata$predMengeFlaschen)
}


functionDurchschnittl <- function(AuswahlJahrgang,AuswahlWein, AuswahlDatumStart, AuswahlDatumEnde) {
  #braucht nu noch auswahl jahrgang etc
  
  
  
  newdata <- as.data.frame(AuswahlJahrgang)
  newdata <- newdata %>%
    rename(Jahrgang = AuswahlJahrgang) %>%
    cbind(AuswahlWein) %>%
    rename(Bezeichnung.Z2 = AuswahlWein) %>%
    cbind(AuswahlDatumStart) %>%
    rename(abfuelldatumWunsch = AuswahlDatumStart) %>%
    cbind(AuswahlDatumEnde) %>%
    rename(leerWunsch = AuswahlDatumEnde)
  
  seq_dates <- seq.Date(as.Date(newdata$abfuelldatumWunsch), as.Date(newdata$leerWunsch), by = "month")
  
  
  months <- format(seq_dates, "%m")
  months <- toString(months)
  #
  newdata$months <- months
  #
  
  #
  if(grepl("03|04|05" ,newdata$months)){
    newdata$frühling <- 1
  } else {
    newdata$frühling <- 0
  }
  #
  #
  if (grepl("06|07|08" ,newdata$months)){
    newdata$sommer <- 1
  } else {
    newdata$sommer <- 0
  }
  if (grepl("09|10|11" , newdata$months)) {
    newdata$herbst <- 1
  } else {
    newdata$herbst <- 0
  }
  
  
  if (grepl("12|01|02", newdata$months)) {
    newdata$winter <- 1
  } else {
    newdata$winter <- 0
  }
  
  newdata$months <- NULL
  
  newdata$frühling <- factor(newdata$frühling)
  newdata$sommer <- factor(newdata$sommer)
  newdata$herbst <- factor(newdata$herbst)
  newdata$winter <- factor(newdata$winter)
  
  
  
  newdata$abfuelldatumWunsch <- as.Date(newdata$abfuelldatumWunsch, "%d.%m.%Y")
  newdata$leerWunsch <- as.Date(newdata$leerWunsch, "%d.%m.%Y")
  
  newdata$predictions <- predict(lm2, newdata = newdata)
  newdata$predMenge <- newdata$predictions
  newdata$predictions <- NULL
  
  
  newdata$predMengeFlaschen <-  round((newdata$leerWunsch - newdata$abfuelldatumWunsch)  * newdata$predMenge,
                                      digits = 0)
  
  
  return(newdata$predMenge)
}














#---------------------------------------------------------
#Server function
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   encoding = "UTF-8"
    )
    
    #---------------------------------#
    #---Data Cleaning & Preparation---#
    #---------------------------------#
    df$Menge.Fass         <- NULL
    df$Dienststelle       <- NULL 
    df$Betriebs.Nr        <- NULL
    df$Produktart         <- NULL
    df$Cuvee.Nr           <- NULL 
    df$Rebsorte.2         <- NULL 
    df$Rebsorte.3         <- NULL
    df$Gärverfahren       <- NULL
    df$Gütezeichen        <- NULL
    df$Hochgewächs        <- NULL
    df$Lageprofil         <- NULL
    df$Sonderbezeichnung  <- NULL
    df$Sonderherkunft     <- NULL 
    df$Begründung         <- NULL 
    df$Rücknahme          <- NULL
    
    data <- df 
    return(data)
    
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    "Mengenverteilung nach Weinsorte & Jahr"
  })
  
  # Generate a plot of the requested variable 
  reactivedata <- reactive({
    
    dataChosen <- data %>% 
      select(Weinart, Rebsorte.1, Menge.Flasche, Antrags.Jahr) %>% 
      filter(Weinart == as.character(input$weinart)) %>% 
      filter(Antrags.Jahr == as.numeric(input$jahr)) %>% 
      group_by(Rebsorte.1) %>% 
      summarise(SummeFlaschen = sum(Menge.Flasche))
    
  })
  
  output$plotVerteilung <- renderPlot({
    
    dataChosen <- reactivedata()
    
    ggplot(data = dataChosen, aes(dataChosen$Rebsorte.1, dataChosen$SummeFlaschen)) +
      geom_bar(stat = "identity") + #gives values instead of counts
      xlab("Rebsorten") +
      ylab("Summe abgefüllter Flaschen") +
      ggtitle(paste("Häufigkeitsverteilung der Sorte", input$weinart, "in", input$jahr)) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1))
    
    
  })
  #------------------------------------   
  
  formulaText2 <- reactive({
    paste("Anteile der Weinsorten im Jahr",input$jahr2)
  })
  
  # Return the formula text for printing as a caption
  output$caption2 <- renderText({
    formulaText2()
  })
  
  reactivedata2 <- reactive({
    #Choosing neccessary data
    dataAggreg <- data %>% 
      select(Weinart, Menge.Flasche, Antrags.Jahr, Jahrgang) %>%    #Rebsorte.1
      # filter(Weinart == as.character(WeinSorte)) %>% 
      filter(Antrags.Jahr == as.numeric(input$jahr2)) %>% 
      group_by(Weinart) %>% 
      summarise(SummeFlaschen = sum(Menge.Flasche)) 
    
  })
  
  output$plotVerteilung2 <- renderPlot({
    
    dataAggreg <- reactivedata2()
    
    # Barplot
    bp<- ggplot(data = dataAggreg, aes(x="", y=(dataAggreg$SummeFlaschen), fill=dataAggreg$Weinart))+
      geom_bar(width = 1, stat = "identity")
    bp
    
    #Pie chart
    pie <- bp + coord_polar("y", start=0)
    pie 
    
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=5, face="bold")
      )
    
    pie + scale_fill_brewer("Weinsorten") + 
      blank_theme +
      theme(axis.text.x=element_blank())+
      geom_text(aes(y = dataAggreg$SummeFlaschen/length(dataAggreg$SummeFlaschen) + 
                      c(0, cumsum(dataAggreg$SummeFlaschen)[-length(dataAggreg$SummeFlaschen)]), 
                    label = percent(round(dataAggreg$SummeFlaschen/sum(dataAggreg$SummeFlaschen), digits = 2)))) 
    # ggtitle(paste("Prozentuale Anteile der Weinsorten in", jahr2))
    
    
  })
  
  
  
  
  output$summaryOutput <- renderPrint({
    
    dataAggreg <- reactivedata2()
    
    p <- percent(dataAggreg$SummeFlaschen/sum(dataAggreg$SummeFlaschen))
    cat("Exakte Anteile \n",
        paste0("\n",dataAggreg$Weinart,":","\t\t",  p), labels = levels(dataAggreg$Weinart))
    
  })
  
  
  #---------------------------------------------------------
  
  output$caption3 <- renderText({
    "Abfülltage aller Weine"
  })
  
  
  reactivedata3 <- reactive({
    
    dataTime <- data %>% 
      filter(as.Date(Bescheiddatum) >= paste0(input$timeStart,"-01-01")) %>% 
      filter(as.Date(Bescheiddatum) <= paste0(input$timeStart, "-12-31"))
    
  })
  
  output$plotZeitverlauf <- renderPlot({
    
    dataTime <- reactivedata3()
    
    #Hilfe für vertikale Jahresgrenzen im Plot
    years <-  c("2015-01-01","2016-01-01","2017-01-01")
    years <- as.data.frame(years)
    years$years <- as.character(years$years)
    years$years <- as.Date(years$years)
    years$years <- as.numeric(years$years)
    
    dataTime$Bescheiddatum <- as.Date(dataTime$Bescheiddatum)
    
    
    ggplot(data = dataTime, 
           mapping = aes(
             xmin = as.Date(paste0(input$timeStart,"-01-01", "%Y-%m-%d")), 
             xmax = as.Date(paste0(input$timeStart,"-12-31",  "%Y-%m-%d")),
             as.Date(dataTime$Bescheiddatum, "%Y-%m-%d"))) +
      geom_bar(stat = "count") +
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
      xlab("Datum") +
      ylab("Anzahl der abgefüllten Weine") +
      ggtitle(paste("Zeitliche Verteilung aller Abfüllungen im Jahr", input$timeStart)) +
      scale_x_date(limits = c(as.Date(paste0(input$timeStart,"-01-01")), as.Date(paste0(input$timeStart,"-12-31"))), 
                   breaks = seq(as.Date(paste0(input$timeStart,"-01-01")), as.Date(paste0(input$timeStart,"-12-31")), "month"),
                   date_labels = "%Y-%m-%d")  
    
  })
  #-------------------------------------
  
  
  formulaText4 <- reactive({
    paste("Abfüllverlauf des Weines",input$weinart2)
  })
  
  # Return the formula text for printing as a caption
  output$caption4 <- renderText({
    formulaText4()
  })
  
  
  reactivedata4 <- reactive({
    
    dataSelection <- data %>%
      filter(as.character(Bezeichnung.Z2) == input$name)
    
  })
  
  
  
  
  output$verlaufEinWein <- renderPlot({
    
    
    dataSelection <- reactivedata4()
    
    
    
    #Hilfe für vertikale Jahresgrenzen im Plot
    years <-  c("2015-01-01","2016-01-01","2017-01-01")
    years <- as.data.frame(years)
    years$years <- as.character(years$years)
    years$years <- as.Date(years$years)
    years$years <- as.numeric(years$years)
    
    
    # name <- names  #???
    
    dataSelection$Bescheiddatum <- as.Date(dataSelection$Bescheiddatum)
    
    
    ggplot(data = dataSelection, 
           mapping = aes(xmin = as.Date("2014-01-01", "%Y-%m-%d"), 
                         xmax = as.Date("2017-12-31",  "%Y-%m-%d"),
                         ymin = 0,
                         ymax = max(dataSelection$Menge.Flasche) + 300,
                         as.Date(dataSelection$Bescheiddatum, "%Y-%m-%d"),
                         dataSelection$Menge.Flasche)) +
      geom_bar(stat = "identity", width = 8) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
      xlab("Datum") +
      ylab("Anzahl der abgefüllten Weine") +
      ggtitle(paste("Abfüllungen für die Sorte", input$name)) +
      scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2017-12-31")),
                   breaks = seq(as.Date("2014-01-01"), as.Date("2017-12-31"), "quarter"),
                   date_labels = "%Y-%m-%d") +
      geom_vline(aes(xintercept = years), color = "darkred") +
      
      
      annotate("text", x= as.Date("2014-03-01"), y= 5600, label= paste("Jahr","2014"), size = 4) +
      annotate("text", x= as.Date("2015-03-01"), y= 5600, label= paste("Jahr", "2015"), size = 4) +
      annotate("text", x= as.Date("2016-03-01"), y= 5600, label= paste("Jahr","2016"), size = 4) +
      annotate("text", x= as.Date("2017-03-01"), y= 5600, label= paste("Jahr","2017"), size = 4) +
      
      
      geom_text(mapping = aes(label = paste("Jahrgang \n",dataSelection$Jahrgang),
                              y = (0.7*(dataSelection$Menge.Flasche)), 
                              angle = 45, hjust = -0.5))
    
  })
  #----------------------------------------------------
  
  #-------#--------------------------------------#
  #---4---#----Entwicklung der Verkaufsmengen----#
  #-------#--------------------------------------#
  
  
  #Ziel: Ist die Menge gefallen, da mehr auf Qualität gesetzt wird? 
  #     Ist der Umsatz gestiegen? Woher bekomme ich die alten, exakten Preise?
  
  
  
  
  # Return the formula text for printing as a caption
  output$caption5 <- renderText({
    "Abgefüllte Flaschen pro Jahrgang"
  })
  
  
  reactivedata5 <- reactive({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    
    
    #Detailgrad von grob nach fein: Weinart -> Rebsorte.1 -> Bezeichnung.Z2
    
    #4.1  Mengenentwicklung über alle Weine 
    dataMenge1 <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen = sum(Menge.Flasche))
    
    
  })
  
  
  
  
  output$uebersicht <- renderPlot({
    
    dataMenge1 <- reactivedata5()
    
    #size preparation
    readable <- ifelse(dataMenge1$SummeFlaschen < 50000, 
                       readable <- 65000*0.0001,
                       0.0001*dataMenge1$SummeFlaschen)
    
    
    
    ggplot(data = dataMenge1, aes(x = Jahrgang, y = dataMenge1$SummeFlaschen,
                                  ymax = max(dataMenge1$SummeFlaschen) + 40000)) +
      geom_point( na.rm = TRUE, aes(size = dataMenge1$SummeFlaschen), alpha = 0.8,
                  color="darkblue",
                  show_guide = FALSE) +
      scale_size(range = c(1, 75))+
      # ggtitle("Entwicklung der Gesamtmenge")  +
      geom_text(aes(label = paste(dataMenge1$SummeFlaschen, "\n Flaschen")), color="white", size = readable) +
      xlab("Jahrgang") +
      ylab("") +
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=14,face="bold"))
    
    
    
  })
  
  #---------------
  #Summary One Year (2017)
  
  
  zielJahr17 <- 2017
  
  
  output$flaschen17 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen17 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen17, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten17 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten17 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine17 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine17 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine17 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine17 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst17 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst17 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose17 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose17 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir17 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir17 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten17 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten17 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage17 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr17) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage17 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage17 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  #-------------------------------------  
  
  
  #---------------
  #Summary One Year (2016)
  
  
  zielJahr16 <- 2016
  
  
  output$flaschen16 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen16 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen16, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten16 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten16 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine16 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine16 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine16 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine16 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst16 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst16 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose16 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose16 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir16 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir16 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten16 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten16 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage16 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr16) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage16 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage16 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  #-------------------------------------  
  
  #---------------
  #Summary One Year (2015)
  
  
  zielJahr15 <- 2015
  
  
  output$flaschen15 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen15 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen15, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten15 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten15 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine15 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine15 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine15 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine15 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst15 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst15 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose15 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose15 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir15 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir15 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten15 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten15 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage15 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr15) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage15 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage15 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  #-------------------------------------  
  #---------------
  #Summary One Year (2014)
  
  
  zielJahr14 <- 2014
  
  
  output$flaschen14 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen14 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen14, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten14 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten14 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine14 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine14 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine14 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine14 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst14 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst14 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose14 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose14 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir14 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir14 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten14 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten14 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage14 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr14) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage14 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage14 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  #-------------------------------------  
  
  #---------------
  #Summary One Year (2013)
  
  
  zielJahr13 <- 2013
  
  
  output$flaschen13 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen13 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen13, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten13 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten13 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine13 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine13 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine13 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine13 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst13 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst13 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose13 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose13 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir13 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir13 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten13 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten13 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage13 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr13) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage13 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage13 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  #-------------------------------------  
  
  #---------------
  #Summary One Year (2012)
  
  
  zielJahr12 <- 2012
  
  
  output$flaschen12 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen12 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen12, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten12 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten12 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine12 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine12 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine12 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine12 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst12 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst12 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose12 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose12 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir12 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir12 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten12 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten12 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage12 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr12) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage12 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage12 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  #-------------------------------------  
  
  #---------------
  #Summary One Year (2011)
  
  
  zielJahr11 <- 2011
  
  
  output$flaschen11 <- renderValueBox({
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Jahrgang, Weinart, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeFlaschen11 = sum(Menge.Flasche))
    
    valueBox(
      value = format(flaschenData$SummeFlaschen11, big.mark = " ") , #reactive function
      subtitle = "Flaschen insgesamt abgefüllt",
      icon = icon("bottle")
    )
  })
  
  
  
  
  output$sorten11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bezeichnung.Z2,Jahrgang, Menge.Flasche) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeSorten11 = sum(length(Bezeichnung.Z2)))
    
    
    
    valueBox(
      value = flaschenData$SummeSorten11 , #reactive function
      subtitle = "Weinsorten im Sortiment",
      icon = icon("bottle")
    )
  })
  
  
  output$weißweine11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißeine11 = sum(Weinart == "Weißwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißeine11 , #reactive function
      subtitle = "Weißweine",
      icon = icon("bottle")
    )
  })
  
  output$rotweine11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRotweine11 = sum(Weinart == "Rotwein"))
    
    
    
    valueBox(
      value = flaschenData$SummeRotweine11 , #reactive function
      subtitle = "Rotweine",
      icon = icon("bottle")
    )
  })
  
  
  output$weißherbst11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeWeißherbst11 = sum(Weinart == "Weißherbst"))
    
    
    
    valueBox(
      value = flaschenData$SummeWeißherbst11 , #reactive function
      subtitle = "Weißherbst",
      icon = icon("bottle")
    )
  })
  
  
  output$rose11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeRose11 = sum(Weinart == "Rosé"))
    
    
    
    valueBox(
      value = flaschenData$SummeRose11 , #reactive function
      subtitle = "Rosé-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  output$blancdenoir11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(Summeblancdenoir11 = sum(Weinart == "blanc de noir"))
    
    
    
    valueBox(
      value = flaschenData$Summeblancdenoir11 , #reactive function
      subtitle = "Blanc-de-noir-Wein(e)",
      icon = icon("bottle")
    )
  })
  
  
  
  output$rebsorten11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum, Weinart, Rebsorte.1) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(AnzahlRebsorten11 = length(unique(Rebsorte.1)))
    
    
    
    valueBox(
      value = flaschenData$AnzahlRebsorten11 , #reactive function
      subtitle = "verschiedene Rebsorten",
      icon = icon("bottle")
    )
  })
  
  output$AnzahlAbfülltage11 <- renderValueBox({
    
    dataMenge <- data %>% 
      select(Jahrgang, Menge.Flasche, Rebsorte.1, Weinart, Bezeichnung.Z2, Bescheiddatum, Antrags.Jahr)
    flaschenData <- dataMenge %>% 
      select(Bescheiddatum, Jahrgang, Bezeichnung.Z2, Bescheiddatum) %>% 
      filter(Jahrgang == zielJahr11) %>% 
      group_by(Jahrgang) %>% 
      summarise(SummeTage11 = length(unique(Bescheiddatum)))
    
    
    
    valueBox(
      value = flaschenData$SummeTage11 , #reactive function
      subtitle = "Tage wurde abgefüllt",
      icon = icon("bottle")
    )
  })
  
  #-----------
  #REGRESSION  
  
  
  #-------------------------------------
  
  
  reactivedataPrediction <- reactive({
    
    eingabe <- c(input$AuswahlJahrgang,input$AuswahlWein,input$AuswahlDatumStart, input$AuswahlDatumEnde)
    
  })
  
  
  output$modelPrediction <- renderText({
    eingabe <-  reactivedataPrediction()
    
    
    # customFunction(2017,"Kerner Qualitätswein ", as.Date("2017-12-01"),as.Date("2018-10-01"))
    
    customFunction(eingabe[1], eingabe[2], as.Date(eingabe[3]), as.Date(eingabe[4]))
    
    
    
  }) 
  
  formulaTextRegression <- reactive({
    
    
    paste("Geschätzte Gesamtmenge an benötigten Flaschen für den angegebenen Zeitraum:")
    
  })
  
  # Return the formula text for printing as a caption
  output$captionregression <- renderText({
    formulaTextRegression()
  })
  
  
  
  
  output$predictionDurchschnitt <- renderText({
    eingabe2 <-  reactivedataPrediction()
    
    functionDurchschnittl(eingabe2[1], eingabe2[2], as.Date(eingabe2[3]), as.Date(eingabe2[4]))
    
  }) 
  
  
  formulaTextRegressionDurchschnitt <- reactive({
    
    
    paste("Durchschnittliche Verkaufsmenge pro Tag für die Sorte ",input$AuswahlWein,":")
    
    
  })
  
  
  # Return the formula text for printing as a caption
  output$captionregressionDurchschnitt <- renderText({
    formulaTextRegressionDurchschnitt()
  })
  
  
  #-------------------------------------  
  
}
#-------------------------------------





# source("ui.R", encoding="UTF-8")

#--------------------------------------------------------
shinyApp(ui, server)
#--------------------------------------------------------


# shiny::runApp()



# 
# 
# #--------------------------------------------------------
# shinyApp(ui, server)
# #--------------------------------------------------------
# 
