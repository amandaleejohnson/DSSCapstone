# DSS Capstone - NextWord
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))
suppressWarnings(library(datasets))

shinyUI(fluidPage(
    # define the theme used for the app
    theme = shinythemes::shinytheme("slate"),
    pageWithSidebar(
        headerPanel("Data Science Specialization Capstone Project - NextWord"),
        sidebarPanel(
            h4("Type at least one English word into the text box below and click Submit."), 
            h4("To the right, you'll see a table of output."),  
            h4("The column labeled PrecedingWords contains the word or phrase used to predict the next word."), 
            h4("The column labeled NextWord contains the most probable next word."), 
            textInput("Tcir", placeholder="Enter text here", label=h3("Type at least one English word here:")),
            submitButton("Submit") #Delays reaction until the user presses this button
            
            # h4('Predicted word :'),
            # verbatimTextOutput("prediction"), 
            # textOutput("prediction")
            
        ),
        mainPanel(
                tabsetPanel(
                    tabPanel('Output Table',
                             dataTableOutput("mytable1"))
                )
        
        )
    )    
))



