library(shiny)


source("ui.R")
source("server.R")

shinyApp(ui = ui.ui(), server = server.server())
