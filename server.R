#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#setwd("C:/Users/ajohns34/Desktop/Final Capstone Submission/Shiny App")

#source("C:/Users/ajohns34/Desktop/Final Capstone Submission/FINAL Capstone - Part 4.R") 
source("FINAL Capstone - Part 4.R")

shinyServer(function(input, output) {
    #Output a large table with the first 100 rows.
    # output$prediction = renderText({
    #     nextword(input$Tcir, wordcount, bigramcount, trigramcount, fourgramcount)[1, 2]
    # })
    
    output$mytable1 = renderDataTable({
        nextword(input$Tcir, wordcount, bigramcount, trigramcount, fourgramcount)[1:100, ]
    })
    
    
})



