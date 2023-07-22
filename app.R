library(shiny)
library(ggplot2)
library(magrittr)
library(DT)
library(dplyr)
library(vcd)
library(tidyr)


ui <- navbarPage(
  
  titlePanel(h1("My title")),
  
  #hier brauch ich noch die häufigkeiten
  tabPanel(title = "Barplots",
           sidebarLayout(position = "right",
                         
                         sidebarPanel(
                           
                           #fare_prices Verteilung genauer untersuchen
                           selectInput(
                             inputId = "x1",
                             label = "X-axis:",
                             choices = c("classes", "gender", "embarked", "age", "fare_prices", "sibling_spouses", "parents_children"),
                             selected = "classes"
                           ),
                           
                           selectInput(
                             inputId = "age_slider",
                             label = "Age Range",
                             selected = 1,
                             choices = c(1,2,4,5,8,10,16,20,40)
                           ),
                           
                           selectInput(
                             inputId = "fare_slider",
                             label = "Fare Range",
                             selected = 1,
                             choices = c(1,2,4,8,16,32,64)
                           )
                           
                           
                         ),
                         
                         mainPanel(
                           plotOutput(outputId = "barplot")
                         )
           )
  ),
  
  tabPanel(title = "multiple Features",
           fluidPage(
             mainPanel(
               width = 13,
               plotOutput(outputId = "multipleStackedBarChart")
             ),
             fluidRow(
               column(width = 12,
                      checkboxInput(
                        inputId = "Pclass",
                        label = "Pclass",
                        value = FALSE
                      ),
                      checkboxInput(
                        inputId = "Sex",
                        label = "Sex",
                        value = TRUE
                      ),
                      checkboxInput(
                        inputId = "Embarked",
                        label = "Embarked",
                        value = FALSE
                      ),
                      checkboxInput(
                        inputId = "SibSp",
                        label = "SibSp",
                        value = FALSE
                      ),
                      checkboxInput(
                        inputId = "Fare",
                        label = "Fare",
                        value = FALSE
                      ),
                      checkboxInput(
                        inputId = "Age",
                        label = "Age",
                        value = FALSE
                      ),
                      selectInput(
                        inputId = "age_slider2",
                        label = "Age Range",
                        selected = 1,
                        choices = c(1,2,4,5,8,10,16,20,40)
                      ),
                      selectInput(
                        inputId = "fare_slider2",
                        label = "Fare Range",
                        selected = 1,
                        choices = c(1,2,4,8,16,32,64)
                      ),
                      
                      sliderInput(
                        inputId = "domainRangeSlider",
                        label = "domain",
                        min = 0,
                        max = 100,
                        value = c(0,100)
                      ),
                      
                      sliderInput(
                        inputId = "survivedSlider",
                        label = "value range of survivability",
                        min = 0,
                        max = 100,
                        value = c(0,100)
                      )
                      
               )
             )
           )
  ),
  
  
  
  tabPanel(title = "Abhängigkeiten der Merkmale",
           
           mainPanel(
             
             tableOutput("dependencies")
             
           )
           
  ),
  
  
  tabPanel(title = "Datensatz",
           
           
           mainPanel(
             
             dataTableOutput("dataTable")
           )
           
  ),
  
  tabPanel(title = "Quellen",
           
           p("https://stackoverflow.com/questions/74043151/geom-text-in-ggplot-gem-bar-geom-text-requires-the-following-missing-aesthet"),
           
           p("https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2"),
           
           p("https://ggplot2.tidyverse.org/reference/geom_text.html"),
           
           
           h3("ChatGPT Prompts:"),
           
           p("was macht die nrow() funktion?"),
           
           p("wie würde ich die relativen Häufigkeiten in einer Zeile berechnen, 
          wenn ich mehrere Merkmale hätte wie hier: 
          ggplot(df, aes(x = paste(first_feature, second_feature, third_feature), fill = factor(titanic_data$Survived)))"),
           
           p("was macht die interaction() methode?"),
           
           p("wie kann ich auf 2 nachkommastellen runden"),
           
           p("was muss ich im UI part der shiny app statt sidebarPanel angeben, dass der Inhalt des ursprünglichen sidebarPanels unter dem MainPanel ist?"),
           
           p("alle Dezimalwerte einer spalte auf nächste tiefere ganze zahl in R runterrunden"),
           
           
  ),
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  titanic_data <- read.csv("titanic_data.csv")
  #-------------------------------------------------------------------------------------------------------------------------------------------------- 
  output$barplot <- renderPlot({
    
    if(input$x1 == "classes"){
      # factor wird hier gefordert
      titanic_data$Survived <- factor(titanic_data$Survived)
      classes <- ggplot(data = titanic_data, aes(x= Pclass, fill = Survived, label = "classes"))
      classes <- classes + geom_bar(stat = "count")
      classes <- classes + geom_text(aes(x = Pclass, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      classes
    }
    else if(input$x1 == "gender") {
      titanic_data$Survived <- factor(titanic_data$Survived)
      gender <- ggplot(data = titanic_data, aes(x= Sex, fill = Survived, label = "gender"))
      gender <- gender + geom_bar(stat = "count")
      gender <- gender + geom_text(aes(x = Sex, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      gender
    }
    else if(input$x1 == "embarked") {
      titanic_data$Survived <- factor(titanic_data$Survived)
      embarked <- ggplot(data = titanic_data, aes(x= Embarked, fill = Survived, label = "embarked"))
      embarked <- embarked + geom_bar(stat = "count")
      embarked <- embarked + geom_text(aes(x = Embarked, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      embarked
    }
    else if(input$x1 == "age") {
      age_breaks <- c(seq(0, 80, (80/as.numeric(input$age_slider))))
      age_labels <- paste(age_breaks[-length(age_breaks)], age_breaks[-1], sep = "-")
      age_class <- cut(titanic_data$Age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)
      titanic_data$Survived <- factor(titanic_data$Survived)
      age <- ggplot(data = titanic_data, aes(x= age_class, fill = Survived, label = "age"))
      age <- age + geom_bar(stat = "count")
      age <- age + geom_text(aes(x = age_class, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      age
    }
    
    else if(input$x1 == "fare_prices"){
      #hier wurden die fare_prices runtergerundet auf die tiefere ganze zahl
      fare_breaks <- c(seq(0, 512, (512/as.numeric(input$fare_slider))))
      fare_labels <- paste(fare_breaks[-length(fare_breaks)], fare_breaks[-1], sep = "-")
      fare_class <- cut(floor(titanic_data$Fare), breaks = fare_breaks, labels = fare_labels, include.lowest = TRUE)
      titanic_data$Survived <- factor(titanic_data$Survived)
      fare <- ggplot(data = titanic_data, aes(x= fare_class, fill = Survived, label = "fare_prices"))
      fare <- fare + geom_bar(stat = "count")
      fare <- fare + geom_text(aes(x = fare_class, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      fare
    }
    
    else if(input$x1 == "sibling_spouses"){
      titanic_data$Survived <- factor(titanic_data$Survived)
      sibsp <- ggplot(data = titanic_data, aes(x= SibSp, fill = Survived, label = "siblings_spouses"))
      sibsp <- sibsp + geom_bar(stat = "count")
      sibsp <- sibsp + geom_text(aes(x = SibSp, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      sibsp
    }
    
    else if(input$x1 == "parents_children"){
      titanic_data$Survived <- factor(titanic_data$Survived)
      Parch <- ggplot(data = titanic_data, aes(x= Parch, fill = Survived, label = "siblings_children"))
      Parch <- Parch + geom_bar(stat = "count")
      Parch <- Parch + geom_text(aes(x = Parch, label = round(..count.. / sum(..count..),2)), size = 5, position = position_stack(vjust = 0.5), stat = "count")
      
      Parch
    }
  })
  
  #---------------------------------------------------------------------------------------------------------------------------------------------#    
  
  
  output$multipleStackedBarChart <- renderPlot({
    
    #interaction() macht kombinationen aus den merkmalen, also Kreuzprodukt
    grouped_data <- interaction(titanic_data$Sex)
    
    if(input$Pclass == TRUE) { grouped_data <- paste(grouped_data, titanic_data$Pclass, sep = ".") }
    
    if(input$Embarked == TRUE) { grouped_data <- paste(grouped_data, titanic_data$Embarked, sep = ".") }
    
    if(input$SibSp == TRUE) { grouped_data <- paste(grouped_data, titanic_data$SibSp, sep = ".") }
    
    if(input$Fare == TRUE){
      
      fare_breaks <- c(seq(0, 512, (512/as.numeric(input$fare_slider2))))
      fare_labels <- paste(fare_breaks[-length(fare_breaks)], fare_breaks[-1], sep = "-")
      fare_class <- cut(floor(titanic_data$Fare), breaks = fare_breaks, labels = fare_labels, include.lowest = TRUE)
      
      grouped_data <- paste(grouped_data, fare_class, sep = ".")
    }
    
    if(input$Age == TRUE){
      
      age_breaks <- c(seq(0, 80, (80/as.numeric(input$age_slider2))))
      age_labels <- paste(age_breaks[-length(age_breaks)], age_breaks[-1], sep = "-")
      age_class <- cut(titanic_data$Age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)
      grouped_data <- paste(grouped_data, age_class, sep = ".")
      
    }
    
    grouped_table <- table(titanic_data$Survived, grouped_data)
    grouped_df <- data.frame(grouped_table)
    
    #mutate fügt neue Spalte hinzu
    df_relative <- grouped_df
    df_relative <- group_by(df_relative, grouped_data)
    df_relative <- mutate(df_relative, round(Freq / sum(Freq), digits = 2))
    
    upperBoundValueRange <- input$survivedSlider[2] / 100
    lowerBoundValueRange <- input$survivedSlider[1] / 100
    
    upperBoundDomain <- input$domainRangeSlider[2] / 100
    lowerBoundDomain <- input$domainRangeSlider[1] / 100
    
    #dadurch wird der Definitionsbereich/domain ausgewählt
    filtered_data <- df_relative[as.integer((nrow(df_relative) * lowerBoundDomain)):as.integer((nrow(df_relative) * upperBoundDomain)), ]
    print(filtered_data)
    #dadurch wird der Wertebreich/value range ausgewählt
    filtered_data <- df_relative %>%
      filter((Var1 == 1 & `round(Freq/sum(Freq), digits = 2)` >= lowerBoundValueRange & `round(Freq/sum(Freq), digits = 2)` <= upperBoundValueRange) 
             | (Var1 == 0 & `round(Freq/sum(Freq), digits = 2)` <= (1- lowerBoundValueRange) & `round(Freq/sum(Freq), digits = 2)` >= (1 - upperBoundValueRange)))
    
    print(filtered_data)
    
    names(filtered_data)[names(filtered_data)== "`round(Freq/sum(Freq), digits = 2)`"] <- "relative frequencies"
    names(filtered_data)[names(filtered_data)== "Var1"] <- "Survived"

    
    #für fill muss Var2 anscheinend ein faktor sein
    plot_bar <- ggplot(data = filtered_data, aes(x = (grouped_data) , y = Freq, fill = factor(Survived))) +
      geom_bar(stat = "identity") + 
      geom_text(aes(label = filtered_data$`round(Freq/sum(Freq), digits = 2)`), position = position_stack(vjust = 0.5), size = 3) +
      labs(x = "Merkmale", y = "absolute frequencies")
    plot_bar
    
    
  })
  
  
  #---------------------------------------------------------------------------------------------------------------------------------------------#    
  passenger_survived <- assocstats(table(titanic_data$PassengerId, titanic_data$Survived))
  passenger_survived$cramer
  
  pclass_survived <- assocstats(table(titanic_data$Pclass, titanic_data$Survived))
  pclass_survived$cramer
  
  cabin_survived <- assocstats(table(titanic_data$Cabin, titanic_data$Survived))
  cabin_survived$cramer
  
  name_survived <- assocstats(table(titanic_data$Name, titanic_data$Survived))
  name_survived$cramer
  
  gender_survived <- assocstats(table(titanic_data$Sex, titanic_data$Survived))
  gender_survived$cramer
  
  age_survived <- assocstats(table(titanic_data$Age, titanic_data$Survived))
  age_survived$cramer
  
  sbsp_survived <- assocstats(table(titanic_data$SibSp, titanic_data$Survived))
  sbsp_survived$cramer
  
  pach_survived <- assocstats(table(titanic_data$SibSp, titanic_data$Survived))
  pach_survived$cramer
  
  ticket_survived <- assocstats(table(titanic_data$Ticket, titanic_data$Survived))
  ticket_survived$cramer
  
  fare_survived <- assocstats(table(titanic_data$Fare, titanic_data$Survived))
  fare_survived$cramer
  
  embarked_survived <- assocstats(table(titanic_data$Embarked, titanic_data$Survived))
  embarked_survived$cramer
  
  output$dependencies <- renderTable({
    
    data <- data.frame(Kombinationen = c("Passenger | Survived:",
                                         "Pclass | Survived:", 
                                         "Cabin | Survived:",
                                         "Name | Survived:",
                                         "Gender | Survived:", 
                                         "Age | Survived:",
                                         "Sibling_Spouses | Survived:", 
                                         "Parents_Children | Survived:",
                                         "Ticket | Survived:",
                                         "Fare_Prices | Survived:",
                                         "Embarked | Survived:"
                                         
    ),
    
    Abhängigkeit = c(passenger_survived$cramer,
                     pclass_survived$cramer,
                     cabin_survived$cramer,
                     name_survived$cramer,
                     gender_survived$cramer,
                     age_survived$cramer,
                     sbsp_survived$cramer,
                     pach_survived$cramer,
                     ticket_survived$cramer,
                     fare_survived$cramer,
                     embarked_survived$cramer
    )
    )
    data
    
  })
  
  #-------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$dataTable <- renderDataTable({
    titanic_data  # Dein gesamter Datensatz
  })
  
  
  #-------------------------------------------------------------------------------------------------------------------------------------------------    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

