library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(DT)
library(shinydashboard)
library(EnvStats)




ui <- navbarPage(title = "Pieperprograms",
                 theme = shinytheme("cerulean"),
                 
                 ################################Home########################################
                 # German
                 # tabPanel(title = "Home",
                 #          tags$div(class = "header", checked = NA,
                 #                   tags$h3("Verteilungsrechner für Dichte, Verteilungsfunktion, Quantile und Zufallszahlen"),
                 #                   tags$br(),
                 #                   tags$p("Herzlich Willkommen,"),
                 #                   tags$p("dies ist meine Applikation  um einfach und bequem die wichtigsten Infos über die wichtigsten Verteilungen zu erhalten.",
                 #                          tags$br(),
                 #                          "Wähle einfach oben im Menü deine Verteilung aus, gebe die benötigten Parameter an und lass dir die Dichte, die Verteilungsfunktion oder Zufallszahlen anzeigen.",
                 #                          tags$br(),
                 #                          "Unter jeder Grafik ist eine Tabelle mit den am meisten gebrauchten Quantilen. Wenn du jedoch ein bestimmtes Quantil wissen willst, gebe es einfach an der entsprechenden 
                 #             Stelle ein(nur eine Zahl von 0-1, Punkt anstatt Komma nutzen) und es wird ausgegeben.",
                 #                          tags$br(),
                 #                          "Zudem lassen sich nun die ersten zwei Momente von Zufallsvariablen der entsprechenden Verteilung bequem ablesen.", 
                 #                          tags$br(),
                 #                          "[Alle Angaben nach bestem Wissen, aber ohne Gewähr ;-) ]"),
                 #                   tags$p("Made by Alexander Pieper"),
                 #                   tags$br()
                 #                   
                 #          ))
                 
                 # English
                 tabPanel(title = "Home",
                          tags$div(class = "header", checked = NA,
                                   tags$h2("My Collection"),
                                   tags$br(),
                                   tags$p("Welcome,",
                                          tags$br(),
                                          "This is a small Collection of some useful Tools that you can access via the Menu at the top.",
                                          tags$br()),
                                   tags$h4("Distribution Calculator"),
                                   tags$p("The Menupoints Discrete and Continuous contain various information on the most important Distributions. 
                                          Just choose your Distribution and set the parameters accordingly, so you can have a plot of the Density or Distribution function. 
                                          In addition you can get information of important quantiles, the first and second moment and random numbers, chosen from this distribution",
                                   tags$br(),
                                   "All information to the best of my knowledge, but without guarantee ;-)"),
                                   tags$h4("Random Generator"),
                                   tags$p("The Random Generator helps you to make unimportant decisions like 'whats for dinner?'. 
                                          In this Case, one would enter 'Pizza, Pasta, Bread' and let the Machine decide.
                                          The same applies to the number field."),
                                   tags$h4("Movie Picker"),
                                   tags$p("This application helps you find the next Movie to watch. Just tick the movies you are in the mood for and then a RNG makes a choice. Spoiler: The Output of the Movie Picker has a little Easter Egg"),
                                   tags$p("Made by Alexander Pieper"),
                                   tags$br()

                          )),
                 
                 
                 
                 
                 
                 
                 ############################################Diskret###################################
                 navbarMenu(title = "Discrete",
                            
                            
                            tabPanel(title = "Binomal distribution",
                                     titlePanel("Binomal distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("probbinom",label = "Enter probability of success:",value = 0.5,min = 0.01,max = 0.99),
                                         numericInput("sizebinom",label = "Enter number of experiments:",value = 10,min = 0,step = 1),
                                         actionButton("gobinomdichte","Show density function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gobinomvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVbinom",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randombinom", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzbinom", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandbinom", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            tabPanel(title = "Negative binomal distribution",
                                     titlePanel("Negative binomal distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("probnegbinom",label = "Enter probability of success:",value = 0.5,min = 0.01,max = 0.99),
                                         numericInput("sizenegbinom",label = "Enter amount of successes until termination:",value = 10,min = 0,step = 1),
                                         actionButton("gonegbinomdichte","Show density function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gonegbinomvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVnegbinom",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomnegbinom", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trznegbinom", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandnegbinom", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            
                            tabPanel(title = "Geometric distribution",
                                     titlePanel("Geometric distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("probgeom",label = "Enter probability of success:",value = 0.5,min = 0.01,max = 0.99),
                                         #numericInput("endgeom",label = "Ende eingeben:",min = 0,value = 10,step = 1),
                                         actionButton("gogeomdichte","Show density function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gogeomvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVgeom",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomgeom", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1),
                                         textInput("trzgeom", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandgeom", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            tabPanel(title = "Hypergeometric distribution",
                                     titlePanel("Hypergeometric distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("nhyp",label = "Enter population (N):",min = 0,value = 20,step = 1),
                                         numericInput("mhyp",label = "Enter amount of elements with desired characteristic (M):",value = 10,min = 0,step = 1),
                                         numericInput("khyp",label = "Enter amount of elements to be picked (n):", min = 0,step = 1,value = 10),
                                         #numericInput("endhyp",label = "Ende eingeben:",min = 0,value = 10,step = 1),
                                         actionButton("gohypdichte","Show density function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gohypvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVhyp",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomhyp", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzhyp", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandhyp", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         plotOutput("plothyp"),
                                         tags$b(textOutput("capEVhyp")),
                                         uiOutput("EVhyp2"),
                                         uiOutput("EVhyp"),
                                         tags$b(textOutput("caphyp")),
                                         textOutput("reshyp")
                                       ))),
                            tabPanel(title = "Poisson distribution",
                                     titlePanel("Poisson distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("lambdapois",label = "Enter lambda:",min = 0,value = 1),
                                         #numericInput("endpois",label = "Ende eingeben:",min = 0,value = 10,step = 1),
                                         actionButton("gopoisdichte","Show density function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gopoisvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVpois",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randompois", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1),
                                         textInput("trzpois", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandpois", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                 navbarMenu(title = "Continuous",
                            tabPanel(title = "Uniform distribution",
                                     titlePanel("Uniform distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("startunif",label = "Enter start:",value = -4),
                                         numericInput("endunif",label = "Enter end:",value = 4),
                                         numericInput("alphaunif",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         actionButton("gounif","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gounifvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVunif",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomunif", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzunif", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandunif", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            
                            
                            
                            tabPanel(title = "Normal distribution",
                                     titlePanel("Normal distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("meannorm",label = "Enter expected value:",value = 0),
                                         numericInput("sdnorm",label = "Enter standard deviation:",value = 1,min = 0),
                                         numericInput("alphanorm",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startnorm",label = "Enter start of x-axis: (optional)",value = NA),
                                         numericInput("endnorm",label = "Enter end of x-axis: (optional)",value = NA),
                                         actionButton("gonorm","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gonormvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVnorm",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomnorm", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trznorm", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandnorm", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            
                            
                            
                            
                            tabPanel(title = "Lognormal distribution",
                                     titlePanel("Lognormal distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("meanlnorm",label = "Enter expected value:",value = 0),
                                         numericInput("sdlnorm",label = "Enter standard deviation:",value = 1,min = 0),
                                         numericInput("alphalnorm",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startlnorm",label = "Enter start of x-axis: (optional)",value = NA, min = 0),
                                         numericInput("endlnorm",label = "Enter end of x-axis: (optional)",value = NA,min = 0),
                                         actionButton("golnorm","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("golnormvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVlognorm",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomlognorm", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzlognorm", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandlognorm", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
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
                            tabPanel(title = "Exponential distribution",
                                     titlePanel("Exponential distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("lambdaexp",label = "Enter lambda:",value = 1,min = 0),
                                         numericInput("alphaexp",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startexp",label = "Enter start of x-axis: (optional)",value = NA,min = 0),
                                         numericInput("endexp",label = "Enter end of x-axis: (optional)",value = NA,min = 0),
                                         actionButton("goexp","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goexpvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVexp",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomexp", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzexp", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandexp", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
                                         
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
                            tabPanel(title = "Chi-squared distribution",
                                     titlePanel("Chi-squared distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("dfchisq",label = "Enter Degree of freedom:",value = 3,min = 1,step = 1),
                                         numericInput("alphachisq",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startchisq",label = "Enter start of x-axis: (optional)",value = NA,min = 0),
                                         numericInput("endchisq",label = "Enter end of x-axis: (optional)",value = NA,min = 0),
                                         actionButton("gochisq","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gochisqvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVchisq",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomchisq", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzchisq", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandchisq", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
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
                            tabPanel(title = "t-distribution",
                                     titlePanel("t-distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("dft",label = "Enter Degree of freedom:",value = 3,min = 1,step = 1),
                                         numericInput("alphat",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startt",label = "Enter start of x-axis: (optional)",value = NA),
                                         numericInput("endt",label = "Enter end of x-axis: (optional)",value = NA),
                                         actionButton("got","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gotvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVt",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomt", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzt", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandt", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
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
                            tabPanel(title = "F-distribution",
                                     titlePanel("F-distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("dff1",label = "Enter Degree of freedom 1:",value = 5,min = 1,step = 1),
                                         numericInput("dff2",label = "Enter Degree of freedom 2:",value = 10,min = 1,step = 1),
                                         numericInput("alphaf",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startf",label = "Enter start of x-axis: (optional)",value = NA,min = 0),
                                         numericInput("endf",label = "Enter end of x-axis: (optional)",value = NA),
                                         actionButton("gof","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gofvert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVf",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomf", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzf", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandf", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                         
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
                            tabPanel(title = "Gamma distribution",
                                     titlePanel("Gamma distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("bgamma",label = "Enter parameter a:",value = 1),
                                         numericInput("pgamma",label = "Enter parameter b:",value = 2),
                                         numericInput("alphagamma",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startgamma",label = "Enter start of x-axis: (optional)",value = NA,min = 0),
                                         numericInput("endgamma",label = "Enter end of x-axis: (optional)",value = NA),
                                         actionButton("gogamma","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gogammavert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVgamma",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randomgamma", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzgamma", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandgamma", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            tabPanel(title = "Beta distribution",
                                     titlePanel("Beta distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("alphaparambeta",label = "Enter parameter alpha:",value = 2,min = 0),
                                         numericInput("betabeta",label = "Enter parameter beta:",value = 2,min = 0),
                                         numericInput("alphabeta",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startbeta",label = "Enter start of x-axis: (optional)",value = NA, min = 0),
                                         numericInput("endbeta",label = "Enter end of x-axis: (optional)",value = NA),
                                         actionButton("gobeta","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("gobetavert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVbeta",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randombeta", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzbeta", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandbeta", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                            tabPanel(title = "Pareto distribution",
                                     titlePanel("Pareto distribution"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("apareto",label = "Enter parameter a:",value = 2,min = 0),
                                         numericInput("kpareto",label = "Enter parameter b:",value = 3,min = 0),
                                         numericInput("alphapareto",label = "Enter alpha of the quantile: (optional)",value = NA,step = 0.05,min = 0,max = 1),
                                         numericInput("startpareto",label = "Enter start of x-axis: (optional)",value = NA, min = 0),
                                         numericInput("endpareto",label = "Enter start of x-axis: (optional)",value = NA),
                                         actionButton("gopareto","Show density function",icon = icon("play_circle"),style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goparetovert","Show distribution function",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         actionButton("goEVpareto",label = "Show moments of a random variable", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5"),
                                         numericInput("randompareto", label = "Enter amount of random numbers:", value = 1, min = 1, step = 1, max = 100000),
                                         textInput("trzpareto", label = "Enter seperator: (optional)", value = " "),
                                         actionButton("gorandpareto", label = "Generate random numbers", style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
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
                 navbarMenu(title = "Random Generator", 
                            tabPanel(title = "Numbers", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("urnenumber", label = "Enter possible Numbers:", value = "1,2,3,4,5,6"),
                                         textInput("trennznum", label = "Enter used Seperator:", value = ","),
                                         numericInput("anzahlnumber", label = "Enter amount of wanted Elements:", value = 1, min = 1, max = 100000),
                                         checkboxInput("zurnumber", label = "with possible duplicates?", value = FALSE),
                                         actionButton("gonumber", label = "Start",style = "color: #3ca7e5;border-color:#3ca7e5;background-color:#3ca7e5")
                                       ),
                                       mainPanel(
                                         textOutput("resultnumber")
                                       )
                                       
                                     )),
                            tabPanel(title = "Words", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("urneword", label = "Enter possible Words:", value = "Hello,World,!"),
                                         textInput("trennzword", label = "Enter used Seperator:", value = ","),
                                         numericInput("anzahlword", label = "Enter amount of wanted Elements:", value = 1, min = 1, max = 100000),
                                         checkboxInput("zurword", label = "with possible duplicates?", value = FALSE),
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
                            #              textInput("trennzword", label = "verwendetes Enter seperator:", value = ","),
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