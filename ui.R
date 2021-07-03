library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(DT)
library(shinydashboard)
library(EnvStats)




ui <- navbarPage(title = "Verteilungen",
                 theme = shinytheme("cerulean"),
                 
                 ################################Home########################################
                 tabPanel(title = "Home",
                          tags$div(class = "header", checked = NA,
                                   tags$h4("Verteilungsrechner für Dichte, Verteilungsfunktion, Quantile und Zufallszahlen"),
                                   tags$br(),
                                   tags$p("Herzlich Willkommen,"),
                                   tags$p("dies ist meine Applikation  um einfach und bequem die wichtigsten Infos über die wichtigsten Verteilungen zu erhalten.",
                                          tags$br(),
                                          "Wähle einfach oben im Menü deine Verteilung aus, gebe die benötigten Parameter an und lass dir die Dichte, die Verteilungsfunktion oder Zufallszahlen anzeigen.",
                                          tags$br(),
                                          "Unter jeder Grafik ist eine Tabelle mit den am meisten gebrauchten Quantilen. Wenn du jedoch ein bestimmtes Quantil wissen willst, gebe es einfach an der entsprechenden 
                             Stelle ein(nur eine Zahl von 0-1, Punkt anstatt Komma nutzen) und es wird ausgegeben.",
                                          tags$br(),
                                          "Zudem lassen sich nun die ersten zwei Momente von Zufallsvariablen der entsprechenden Verteilung bequem ablesen.", 
                                          tags$br(),
                                          "[Alle Angaben nach bestem Wissen, aber ohne Gewähr ;-) ]"),
                                   tags$p("Made by Alexander Pieper"),
                                   tags$br()
                                   
                          )),
                 
                 
                 
                 
                 
                 
                 ############################################Diskret###################################
                 navbarMenu(title = "Diskret",
                            
                            
                            tabPanel(title = "Binomialverteilung",
                                     titlePanel("Binomialverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("probbinom",label = "Erfolgswahrscheinlichkeit eingeben:",value = 0.5,min = 0.01,max = 0.99),
                                         numericInput("sizebinom",label = "Anzahl von Experimenten eingeben:",value = 10,min = 0,step = 1),
                                         actionButton("gobinomdichte","Dichte anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gobinomvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVbinom",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randombinom", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzbinom", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandbinom", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotbinom"),
                                         tags$b(textOutput("capEVbinom")),
                                         uiOutput("EVbinom2"),
                                         uiOutput("EVbinom"),
                                         tags$b(textOutput("capbinom")),
                                         textOutput("resbinom")
                                       )
                                     )),
                            tabPanel(title = "Negative Binomialverteilung",
                                     titlePanel("Negative Binomialverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("probnegbinom",label = "Erfolgswahrscheinlichkeit eingeben:",value = 0.5,min = 0.01,max = 0.99),
                                         numericInput("sizenegbinom",label = "Anzahl der Erfolge bis Abbruch eingeben:",value = 10,min = 0,step = 1),
                                         actionButton("gonegbinomdichte","Dichte anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gonegbinomvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVnegbinom",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomnegbinom", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trznegbinom", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandnegbinom", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotnegbinom"),
                                         tags$b(textOutput("capEVnegbinom")),
                                         uiOutput("EVnegbinom2"),
                                         uiOutput("EVnegbinom"),
                                         tags$b(textOutput("capnegbinom")),
                                         textOutput("resnegbinom")
                                       )
                                     )),
                            
                            tabPanel(title = "Geometrische Verteilung",
                                     titlePanel("Geometrische Verteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("probgeom",label = "Erfolgswahrscheinlichkeit eingeben:",value = 0.5,min = 0.01,max = 0.99),
                                         #numericInput("endgeom",label = "Ende eingeben:",min = 0,value = 10,step = 1),
                                         actionButton("gogeomdichte","Dichte anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gogeomvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVgeom",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomgeom", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1),
                                         textInput("trzgeom", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandgeom", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotgeom"),
                                         tags$b(textOutput("capEVgeom")),
                                         uiOutput("EVgeom2"),
                                         uiOutput("EVgeom"),
                                         tags$b(textOutput("capgeom")),
                                         textOutput("resgeom")
                                       )
                                       
                                     )),
                            tabPanel(title = "Hypergeometrische Verteilung",
                                     titlePanel("Hypergeometrische Verteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("nhyp",label = "Grundgesamtheit N eingeben:",min = 0,value = 20,step = 1),
                                         numericInput("mhyp",label = "Elemente mit der gewüntschen Eigenschaft M eingeben:",value = 10,min = 0,step = 1),
                                         numericInput("khyp",label = "Größe der Stichprobe n eingeben:", min = 0,step = 1,value = 10),
                                         #numericInput("endhyp",label = "Ende eingeben:",min = 0,value = 10,step = 1),
                                         actionButton("gohypdichte","Dichte anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gohypvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVhyp",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomhyp", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzhyp", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandhyp", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plothyp"),
                                         tags$b(textOutput("capEVhyp")),
                                         uiOutput("EVhyp2"),
                                         uiOutput("EVhyp"),
                                         tags$b(textOutput("caphyp")),
                                         textOutput("reshyp")
                                       ))),
                            tabPanel(title = "Poissonverteilung",
                                     titlePanel("Poissonverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("lambdapois",label = "Lambda eingeben:",min = 0,value = 1),
                                         #numericInput("endpois",label = "Ende eingeben:",min = 0,value = 10,step = 1),
                                         actionButton("gopoisdichte","Dichte anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gopoisvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVpois",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randompois", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1),
                                         textInput("trzpois", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandpois", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotpois"),
                                         tags$b(textOutput("capEVpois")),
                                         uiOutput("EVpois2"),
                                         uiOutput("EVpois"),
                                         tags$b(textOutput("cappois")),
                                         textOutput("respois")
                                       )))
                            
                 ),
                 
                 
                 
                 
                 #################################################Stetig################################    
                 navbarMenu(title = "Stetig",
                            tabPanel(title = "Gleichverteilung",
                                     titlePanel("Gleichverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("startunif",label = "Start eingeben:",value = -4),
                                         numericInput("endunif",label = "Ende eingeben:",value = 4),
                                         numericInput("alphaunif",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         actionButton("gounif","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gounifvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVunif",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomunif", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzunif", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandunif", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotunif"),
                                         verbatimTextOutput("quantunif"),
                                         tableOutput("tableunif"),
                                         tags$b(textOutput("capEVunif")),
                                         uiOutput("EVunif2"),
                                         uiOutput("EVunif"),
                                         tags$b(textOutput("capunif")),
                                         textOutput("resunif")
                                       )
                                     )),
                            
                            
                            
                            tabPanel(title = "Normalverteilung",
                                     titlePanel("Normalverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("meannorm",label = "Erwartungswert eingeben:",value = 0),
                                         numericInput("sdnorm",label = "Standardabweichung eingeben:",value = 1,min = 0),
                                         numericInput("alphanorm",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startnorm",label = "Start der x-Achse eingeben: (optional)",value = NA),
                                         numericInput("endnorm",label = "Ende der x-Achse eingeben: (optional)",value = NA),
                                         actionButton("gonorm","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gonormvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVnorm",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomnorm", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trznorm", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandnorm", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotnorm"),
                                         verbatimTextOutput("quantnorm"),
                                         tableOutput("tablenorm"),
                                         tags$b(textOutput("capEVnorm")),
                                         uiOutput("EVnorm2"),
                                         uiOutput("EVnorm"),
                                         tags$b(textOutput("capnorm")),
                                         textOutput("resnorm")
                                       )
                                     )
                            ),
                            
                            
                            
                            
                            tabPanel(title = "Lognormalverteilung",
                                     titlePanel("Lognormalverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("meanlnorm",label = "Erwartungswert eingeben:",value = 0),
                                         numericInput("sdlnorm",label = "Standardabweichung eingeben:",value = 1,min = 0),
                                         numericInput("alphalnorm",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startlnorm",label = "Start der x-Achse eingeben: (optional)",value = NA, min = 0),
                                         numericInput("endlnorm",label = "Ende der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         actionButton("golnorm","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("golnormvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVlognorm",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomlognorm", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzlognorm", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandlognorm", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("plotlnorm"),
                                         verbatimTextOutput("quantlnorm"),
                                         tableOutput("tablelnorm"),
                                         tags$b(textOutput("capEVlognorm")),
                                         uiOutput("EVlognorm2"),
                                         uiOutput("EVlognorm"),
                                         tags$b(textOutput("caplognorm")),
                                         textOutput("reslognorm")
                                       )
                                     )
                            ),
                            tabPanel(title = "Exponentialverteilung",
                                     titlePanel("Exponentialverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("lambdaexp",label = "Lambda eingeben:",value = 1,min = 0),
                                         numericInput("alphaexp",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startexp",label = "Start der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         numericInput("endexp",label = "Ende der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         actionButton("goexp","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goexpvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVexp",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomexp", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzexp", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandexp", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("plotexp"),
                                         verbatimTextOutput("quantexp"),
                                         tableOutput("tableexp"),
                                         tags$b(textOutput("capEVexp")),
                                         uiOutput("EVexp2"),
                                         uiOutput("EVexp"),
                                         tags$b(textOutput("capexp")),
                                         textOutput("resexp")
                                       )
                                     )
                            ),
                            tabPanel(title = "Chi-Quadrat-Verteilung",
                                     titlePanel("Chi-Quadrat-Verteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("dfchisq",label = "Freiheitsgrad eingeben:",value = 3,min = 1,step = 1),
                                         numericInput("alphachisq",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startchisq",label = "Start der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         numericInput("endchisq",label = "Ende der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         actionButton("gochisq","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gochisqvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVchisq",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomchisq", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzchisq", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandchisq", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("plotchisq"),
                                         verbatimTextOutput("quantchisq"),
                                         tableOutput("tablechisq"),
                                         tags$b(textOutput("capEVchisq")),
                                         uiOutput("EVchisq2"),
                                         uiOutput("EVchisq"),
                                         tags$b(textOutput("capchisq")),
                                         textOutput("reschisq")
                                       )
                                     )
                            ),
                            tabPanel(title = "t-Verteilung",
                                     titlePanel("t-Verteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("dft",label = "Freiheitsgrad eingeben:",value = 3,min = 1,step = 1),
                                         numericInput("alphat",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startt",label = "Start der x-Achse eingeben: (optional)",value = NA),
                                         numericInput("endt",label = "Ende der x-Achse eingeben: (optional)",value = NA),
                                         actionButton("got","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gotvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVt",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomt", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzt", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandt", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("plott"),
                                         verbatimTextOutput("quantt"),
                                         tableOutput("tablet"),
                                         tags$b(textOutput("capEVt")),
                                         uiOutput("EVt2"),
                                         uiOutput("EVt"),
                                         tags$b(textOutput("capt")),
                                         textOutput("rest")
                                       )
                                     )
                            ),
                            tabPanel(title = "F-Veteilung",
                                     titlePanel("F-Verteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("dff1",label = "Freiheitsgrad 1 eingeben:",value = 5,min = 1,step = 1),
                                         numericInput("dff2",label = "Freiheitsgrad 2 eingeben:",value = 10,min = 1,step = 1),
                                         numericInput("alphaf",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startf",label = "Start der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         numericInput("endf",label = "Start der x-Achse eingeben: (optional)",value = NA),
                                         actionButton("gof","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gofvert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVf",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomf", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzf", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandf", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("plotf"),
                                         verbatimTextOutput("quantf"),
                                         tableOutput("tablef"),
                                         tags$b(textOutput("capEVf")),
                                         uiOutput("EVf2"),
                                         uiOutput("EVf"),
                                         tags$b(textOutput("capf")),
                                         textOutput("resf")
                                       )
                                     )
                            ),
                            tabPanel(title = "Gammaverteilung",
                                     titlePanel("Gammaverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("bgamma",label = "Parameter a eingeben:",value = 1),
                                         numericInput("pgamma",label = "Parameter b eingeben:",value = 2),
                                         numericInput("alphagamma",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startgamma",label = "Start der x-Achse eingeben: (optional)",value = NA,min = 0),
                                         numericInput("endgamma",label = "Start der x-Achse eingeben: (optional)",value = NA),
                                         actionButton("gogamma","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gogammavert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVgamma",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomgamma", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzgamma", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandgamma", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotgamma"),
                                         verbatimTextOutput("quantgamma"),
                                         tableOutput("tablegamma"),
                                         tags$b(textOutput("capEVgamma")),
                                         uiOutput("EVgamma2"),
                                         uiOutput("EVgamma"),
                                         tags$b(textOutput("capgamma")),
                                         textOutput("resgamma")
                                       )
                                     )
                            ),
                            tabPanel(title = "Betaverteilung",
                                     titlePanel("Betaverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("alphaparambeta",label = "Parameter alpha eingeben:",value = 2,min = 0),
                                         numericInput("betabeta",label = "Parameter beta eingeben:",value = 2,min = 0),
                                         numericInput("alphabeta",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startbeta",label = "Start der x-Achse eingeben: (optional)",value = NA, min = 0),
                                         numericInput("endbeta",label = "Ende der x-Achse eingeben: (optional)",value = NA),
                                         actionButton("gobeta","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gobetavert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVbeta",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randombeta", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzbeta", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandbeta", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotbeta"),
                                         verbatimTextOutput("quantbeta"),
                                         tableOutput("tablebeta"),
                                         tags$b(textOutput("capEVbeta")),
                                         uiOutput("EVbeta2"),
                                         uiOutput("EVbeta"),
                                         tags$b(textOutput("capbeta")),
                                         textOutput("resbeta")
                                       )
                                     )
                            ),
                            tabPanel(title = "Paretoverteilung",
                                     titlePanel("Paretoverteilung"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("apareto",label = "Parameter a eingeben:",value = 2,min = 0),
                                         numericInput("kpareto",label = "Parameter b eingeben:",value = 3,min = 0),
                                         numericInput("alphapareto",label = "Alpha des Quantils eingeben: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startpareto",label = "Start der x-Achse eingeben: (optional)",value = NA, min = 0),
                                         numericInput("endpareto",label = "Start der x-Achse eingeben: (optional)",value = NA),
                                         actionButton("gopareto","Dichte anzeigen",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goparetovert","Verteilungsfunktion anzeigen",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVpareto",label = "Momente der ZV anzeigen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randompareto", label = "Anzahl der Zufallszahlen eingeben:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzpareto", label = "Trennzeichen eingeben: (optional)", value = " "),
                                         actionButton("gorandpareto", label = "Zufallszahlen erzeugen", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plotpareto"),
                                         verbatimTextOutput("quantpareto"),
                                         tableOutput("tablepareto"),
                                         tags$b(textOutput("capEVpareto")),
                                         uiOutput("EVpareto2"),
                                         uiOutput("EVpareto"),
                                         tags$b(textOutput("cappareto")),
                                         textOutput("respareto")
                                       )
                                     )
                            )
                 ),
                 
                 #################################################### RNG     ############################################################################# 
                 navbarMenu(title = "Zufallsgenerator", 
                            tabPanel(title = "Zahlen", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("urnenumber", label = "Mögliche Zahlen eingeben:", value = "1,2,3,4,5,6"),
                                         textInput("trennznum", label = "verwendetes Trennzeichen eingeben:", value = ","),
                                         numericInput("anzahlnumber", label = "Anzahl der zu wählenden Elemente eingeben:", value = 1, min = 1, max = 100000),
                                         checkboxInput("zurnumber", label = "Mit zurücklegen?", value = FALSE),
                                         actionButton("gonumber", label = "Start",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         textOutput("resultnumber")
                                       )
                                       
                                     )),
                            tabPanel(title = "Worte", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("urneword", label = "Mögliche Worte eingeben:", value = "Hallo,Welt,!"),
                                         textInput("trennzword", label = "verwendetes Trennzeichen eingeben:", value = ","),
                                         numericInput("anzahlword", label = "Anzahl der zu wählenden Elemente eingeben:", value = 1, min = 1, max = 100000),
                                         checkboxInput("zurword", label = "Mit zurücklegen?", value = FALSE),
                                         actionButton("gorandword", label = "Start",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         textOutput("resultword")
                                       )
                                       
                                     ))
                 ),
                 
                 
                 
                 
                 
                 #################################################### Moviepicker ####################################
                 
                 
                 navbarMenu(title = "Movie picker", 
                            tabPanel(title = "Marvel", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$h4("Select all Movies you want in the pool:"),
                                         checkboxInput("select_all_none", label = "Select All/None", value = TRUE),
                                         checkboxInput("ironman1", label = "Iron Man", value = TRUE),
                                         checkboxInput("ironman2", label = "Iron Man 2", value = TRUE),
                                         checkboxInput("ironman3", label = "Iron Man 3", value = TRUE),
                                         checkboxInput("cap1", label = "Captain America: The First Avenger", value = TRUE),
                                         checkboxInput("cap2", label = "Captain America: The Winter Soldier", value = TRUE),
                                         checkboxInput("cap3", label = "Captain America: Civil War", value = TRUE),
                                         checkboxInput("thor1", label = "Thor", value = TRUE),
                                         checkboxInput("thor2", label = "Thor - The Dark World", value = TRUE),
                                         checkboxInput("thor3", label = "Thor - Ragnarok", value = TRUE),
                                         checkboxInput("guards1", label = "Guardians of the Galaxy Vol. 1", value = TRUE),
                                         checkboxInput("guards2", label = "Guardians of the Galaxy Vol. 2", value = TRUE),
                                         checkboxInput("spiderman1", label = "Spider man: Homecoming", value = TRUE),
                                         checkboxInput("spiderman2", label = "Spider man: Far from Home", value = TRUE),
                                         checkboxInput("antman1", label = "Ant-Man", value = TRUE),
                                         checkboxInput("antman2", label = "Ant-Man and the Wasp", value = TRUE),
                                         checkboxInput("blackpanther", label = "Black Panther", value = TRUE),
                                         checkboxInput("drstrange", label = "Doctor Strange", value = TRUE),
                                         checkboxInput("capmarvel", label = "Captain Marvel", value = TRUE),
                                         checkboxInput("avengers1", label = "The Avengers", value = TRUE),
                                         checkboxInput("avengers2", label = "The Avengers: Age of Ultron", value = TRUE),
                                         checkboxInput("avengers3", label = "The Avengers: Infinity War", value = TRUE),
                                         checkboxInput("avengers4", label = "The Avengers: Endgame", value = TRUE),
                                         actionButton("gochoosemarvelmovie", label = "Start",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         tags$h2(textOutput("marvelresulttext")),
                                         uiOutput("marvelresultgif"),
                                         tags$p(textOutput("disclaimer"))
                                         # tags$img(imageOutput("marvelresultgif"))#,
                                         # # img(src = "endgame.gif")
                                       )
                                       
                                     ))#,
                            # tabPanel(title = "Disney", 
                            #          sidebarLayout(
                            #            sidebarPanel(
                            #              textInput("urneword", label = "Mögliche Worte eingeben:", value = "Hallo,Welt,!"),
                            #              textInput("trennzword", label = "verwendetes Trennzeichen eingeben:", value = ","),
                            #              numericInput("anzahlword", label = "Anzahl der zu wählenden Elemente eingeben:", value = 1, min = 1, max = 100000),
                            #              checkboxInput("zurword", label = "Mit zurücklegen?", value = FALSE),
                            #              actionButton("gorandword", label = "Start",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                            #            ),
                            #            mainPanel(
                            #              textOutput("resultword")
                            #            )
                            #            
                            #          ))
                 ))