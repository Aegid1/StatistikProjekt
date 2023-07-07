#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(magrittr)
library(DT)
library(dplyr)
library(vcd)
library(tidyr)

titanic <- read.csv2("titanic_data.csv")

survived_counts <- table(titanic_data$Survived)
sex_counts <- table(titanic_data$Sex)


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
                        inputId = "werteRangeSlider",
                        label = "Werte",
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
             
    )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    output$barplot <- renderPlot({
      
        if(input$x1 == "classes"){
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
      
      upperBound <- input$werteRangeSlider[2] / 100
      lowerBound <- input$werteRangeSlider[1] / 100
      filtered_data <- df_relative[as.integer((nrow(df_relative) * lowerBound)):as.integer((nrow(df_relative) * upperBound)), ]
      
      #für fill muss Var2 anscheinend ein faktor sein
      plot_bar <- ggplot(data = filtered_data, aes(x = (grouped_data) , y = Freq, fill = factor(Var1))) +
        geom_bar(stat = "identity") + 
        geom_text(aes(label = filtered_data$`round(Freq/sum(Freq), digits = 2)`), position = position_stack(vjust = 0.5), size = 3) 
      
      plot_bar
      
      
    })
    
    
#---------------------------------------------------------------------------------------------------------------------------------------------#    
    pclass_survived <- assocstats(table(titanic_data$Pclass, titanic_data$Survived))
    pclass_survived$cramer
    
    gender_survived <- assocstats(table(titanic_data$Sex, titanic_data$Survived))
    gender_survived$cramer
    
    embarked_survived <- assocstats(table(titanic_data$Embarked, titanic_data$Survived))
    embarked_survived$cramer
    
    age_survived <- assocstats(table(titanic_data$Age, titanic_data$Survived))
    age_survived$cramer
    
    fare_survived <- assocstats(table(titanic_data$Fare, titanic_data$Survived))
    fare_survived$cramer
    
    sbsp_survived <- assocstats(table(titanic_data$SibSp, titanic_data$Survived))
    sbsp_survived$cramer
    
    pach_survived <- assocstats(table(titanic_data$SibSp, titanic_data$Survived))
    pach_survived$cramer
    
#---------------------------------------------------------------------------------------------------------------------------------------------#    
    
    output$dependencies <- renderTable({
      
      data <- data.frame(Kombinationen = c("Pclass | Survived:", 
                                     "Gender | Survived", 
                                     "Embarked | Survived",
                                     "Age | Survived",
                                     "Fare_Prices | Survived",
                                     "Sibling_Spouses | Survived", 
                                     "Parents_Children | Survived"),
                          
                           Abhängigkeit = c(pclass_survived$cramer,
                                    gender_survived$cramer,
                                      embarked_survived$cramer,
                                      age_survived$cramer,
                                      fare_survived$cramer,
                                      sbsp_survived$cramer,
                                      pach_survived$cramer)
      )
      data
      
    })
    
#-------------------------------------------------------------------------------------------------------------------------------------------------

    output$dataTable <- renderDataTable({
      titanic_data  # Dein gesamter Datensatz
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
