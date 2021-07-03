library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library(shinydashboard)
library(EnvStats)






server <- function(input,output, session){
  marvel_label_map = list("ironman1" = "Iron Man",
                          "ironman2" = "Iron Man 2",
                          "ironman3" = "Iron Man 3",
                          "cap1" = "Captain America: The First Avenger",
                          "cap2" = "Captain America: The Winter Soldier",
                          "cap3" = "Captain America: Civil War",
                          "thor1" = "Thor",
                          "thor2" = "Thor - The Dark World",
                          "thor3" = "Thor - Ragnarok",
                          "guards1" = "Guardians of the Galaxy Vol. 1",
                          "guards2" = "Guardians of the Galaxy Vol. 2",
                          "spiderman1" = "Spider man: Homecoming",
                          "spiderman2" = "Spider man: Far from Home",
                          "antman1" = "Ant-Man",
                          "antman2" = "Ant-Man and the Wasp",
                          "blackpanther" = "Black Panther",
                          "drstrange" = "Doctor Strange",
                          "capmarvel" = "Captain Marvel",
                          "avengers1" = "The Avengers",
                          "avengers2" = "The Avengers: Age of Ultron",
                          "avengers3" = "The Avengers: Infinity War",
                          "avengers4" = "The Avengers: Endgame"
  )
  ################### Movie Picker ###################
  observe({
    if(input$select_all_none){
      for(i in names(marvel_label_map)){
        updateCheckboxInput(session, i, value = TRUE)
      }
      
    }else{
      for(i in names(marvel_label_map)){
        updateCheckboxInput(session, i, value = FALSE)
      }
    }
  })
  
  observeEvent(input$gochoosemarvelmovie,{
    
    pool <- names(marvel_label_map)
    
    for(element in pool){
      if(!input[[element]]){
        pool <- pool[pool != element]
      }
    }
    if(length(pool) == 0){
      print("Here")
      output$marvelresulttext <- renderText(
        "You have to choose at least one Movie!"
      )
      output$marvelresultgif <- renderUI({
        
      })
    }else{
      result_orig = sample(pool, 1)
      result = marvel_label_map[[result_orig]]
      
      output$marvelresulttext <- renderText(
        result
      )
      output$marvelresultgif <- renderUI({
        img(src = paste0(result_orig, ".gif"), width = 480) 
      })
      output$disclaimer <- renderText(
        "All gif's are from giphy.com"
      )
    }
    
    
    
  })
  
  
  ################### RNG ###################
  
  observeEvent(input$gonumber,{
    
    poolnum <- as.numeric(unlist(strsplit(input$urnenumber,input$trennznum)))
    if(input$anzahlnumber > length(poolnum) & input$zurnumber == FALSE){
      solution <- "Fehler: Anzahl der Elemente ist größer als die Länge der möglichen Zahlen. 'Mit zurücklegen' muss ausgewählt werden"
    }else{
      if(input$zurnumber == TRUE){
        solutionnum <- sample(poolnum, size = input$anzahlnumber, replace = TRUE)
      }else{
        solutionnum <- sample(poolnum, size = input$anzahlnumber, replace = FALSE)
      }
    }
    output$resultnumber <- renderText(solutionnum)
  })
  
  
  
  observeEvent(input$gorandword,{
    poolword <- unlist(strsplit(input$urneword,input$trennzword))
    if(input$anzahlword > length(poolword) & input$zurword == FALSE){
      solutionword <- "Fehler: Anzahl der Elemente ist größer als die Länge der möglichen Zahlen. 'Mit zurücklegen' muss ausgewählt werden"
    }else{
      if(input$zurword == TRUE){
        solutionword <- sample(poolword, size = input$anzahlword, replace = TRUE)
      }else{
        solutionword <- sample(poolword, size = input$anzahlword, replace = FALSE)
      }
    }
    output$resultword <- renderText(solutionword)
  })
  
  ###############################################Binomialverteilung##############################
  
  
  
  observeEvent(input$goEVbinom,{
    output$capEVbinom <- renderText("Die ersten Momente:")
    output$EVbinom <- renderUI({
      withMathJax(
        paste0("$$X \\sim B(",input$sizebinom,",",input$probbinom,")\\rightarrow\\begin{cases}
                E[X] = ",input$sizebinom*input$probbinom," \\\\
                Var[X] = ",input$sizebinom*input$probbinom*(1-input$probbinom),"
                \\end{cases}\\!$$")
      )
    })
    output$EVbinom2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim B(n,p)\\rightarrow\\begin{cases}
                E[X] =  n \\cdot p\\\\
                Var[X] = n \\cdot p \\cdot (1-p)
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  observeEvent(input$gobinomvert,{
    output$plotbinom <- renderPlot({
      x <- seq(from = 0,
               to = input$sizebinom,
               by = 1)
      qplot(x,pbinom(x,size = input$sizebinom,prob = input$probbinom),
            main = "Verteilungsfunktion der Binomialverteilung", 
            ylab = "Wahrscheinlichkeit")
      
    })
  })
  
  observeEvent(input$gobinomdichte,{
    output$plotbinom <- renderPlot({
      x <- seq(from = 0,
               to = input$sizebinom,
               by = 1)
      qplot(x,dbinom(x,size = input$sizebinom,prob = input$probbinom),
            main = "Dichte der Binomialverteilung", 
            ylab = "Dichte")
      
      
    })
    
  })
  
  
  observeEvent(input$gorandbinom,{
    output$capbinom <- renderText("Zufallszahlen:")
    output$resbinom <- renderText(paste(rbinom(isolate(input$randombinom), 
                                               size = input$sizebinom, 
                                               prob = input$probbinom), collapse = isolate(input$trzbinom)))
  })
  
  ############################################### Negative Binomialverteilung##############################
  
  
  observeEvent(input$goEVnegbinom,{
    output$capEVnegbinom <- renderText("Die ersten Momente:")
    output$EVnegbinom <- renderUI({
      withMathJax(
        paste0("$$X \\sim NB(",input$sizenegbinom,",",input$probnegbinom,")\\rightarrow\\begin{cases}
                E[X] = ",input$sizenegbinom/input$probnegbinom," \\\\
                Var[X] = ",input$sizenegbinom*(1-input$probnegbinom)/input$probnegbinom^2,"
                \\end{cases}\\!$$")
      )
    })
    output$EVnegbinom2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim NB(n,p)\\rightarrow\\begin{cases}
                E[X] = \\frac{n}{p}\\\\
                Var[X] = n \\cdot \\frac{(1-p)}{p^2}
                \\end{cases}\\!$$")
      )
    })
  })
  
  # Hier aus wikipedia: p ist die erfolgswahrscheinlichkeit, bei 
  # Stat2 skript(bergter) ist \theta die misserfolgswahrscheinlichkeit
  
  
  
  observeEvent(input$gonegbinomvert,{
    output$plotnegbinom <- renderPlot({
      x <- seq(from = input$sizenegbinom,
               to = input$sizenegbinom + qnbinom(p = 0.999, size = input$sizenegbinom, prob = input$probnegbinom),
               by = 1)
      qplot(x,pnbinom(x - input$sizenegbinom,size = input$sizenegbinom,prob = input$probnegbinom),
            main = "Verteilungsfunktion der negativen Binomialverteilung", 
            ylab = "Wahrscheinlichkeit")
      
    })
  })
  
  observeEvent(input$gonegbinomdichte,{
    output$plotnegbinom <- renderPlot({
      x <- seq(from = input$sizenegbinom,
               to = input$sizenegbinom + qnbinom(p = 0.999, size = input$sizenegbinom, prob = input$probnegbinom),
               by = 1)
      qplot(x,dnbinom(x - input$sizenegbinom,size = input$sizenegbinom,prob = input$probnegbinom),
            main = "Dichte der negativen Binomialverteilung", 
            ylab = "Dichte")
      
      
    })
    
  })
  
  observeEvent(input$gorandnegbinom,{
    output$capnegbinom <- renderText("Zufallszahlen:")
    output$resnegbinom <- renderText(paste(rnbinom(isolate(input$randomnegbinom), 
                                                   size = input$sizenegbinom, 
                                                   prob = input$probnegbinom) + input$sizenegbinom, collapse = isolate(input$trznegbinom)))
  })
  
  
  
  ###############################################Geometrische Verteilung##############################
  
  observeEvent(input$goEVgeom,{
    output$capEVgeom <- renderText("Die ersten Momente:")
    output$EVgeom <- renderUI({
      withMathJax(
        paste0("$$X \\sim Geo(",input$probgeom,")\\rightarrow\\begin{cases}
                E[X] = ",1/input$probgeom," \\\\
                Var[X] = ",(1-input$probgeom)/input$probgeom^2,"
                \\end{cases}\\!$$")
      )
    })
    output$EVgeom2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim Geo(p)\\rightarrow\\begin{cases}
                E[X] = \\frac{1}{p} \\\\
                Var[X] = \\frac{1-p}{p^2}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  observeEvent(input$gogeomvert,{
    output$plotgeom <- renderPlot({
      x <- seq(from = 0,
               to = qgeom(0.99,prob = input$probgeom),
               by = 1)
      qplot(x,pgeom(x,prob = input$probgeom),
            main = "Verteilungsfunktion der Geometrischen Verteilung", 
            ylab = "Wahrscheinlichkeit")
      
      
    })})
  
  observeEvent(input$gogeomdichte,{
    output$plotgeom <- renderPlot({
      x <- seq(from = 0,
               to = qgeom(0.99,prob = input$probgeom),
               by = 1)
      qplot(x,dgeom(x,prob = input$probgeom),
            main = "Dichte der Geometrischen Verteilung", 
            ylab = "Dichte")
      
      
    })
    
  })
  
  observeEvent(input$gorandgeom,{
    output$capgeom <- renderText("Zufallszahlen:")
    output$resgeom <- renderText(paste(rgeom(isolate(input$randomgeom), 
                                             prob = input$probgeom), collapse = isolate(input$trzgeom)))
  })
  
  
  
  #############################################Hypergeometrische####################################    
  
  observeEvent(input$goEVhyp,{
    output$capEVhyp <- renderText("Die ersten Momente:")
    output$EVhyp <- renderUI({
      withMathJax(
        paste0("$$X \\sim H(",input$khyp,",",input$mhyp,",",input$nhyp,")\\rightarrow\\begin{cases}
                E[X] = ",input$khyp*input$mhyp/(input$nhyp)," \\\\
                Var[X] = ",input$khyp*(input$mhyp/input$nhyp)*(1-input$mhyp/input$nhyp)*((input$nhyp-input$khyp)/(input$nhyp-1)),"
                \\end{cases}\\!$$")
      )
    })
    output$EVhyp2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim H(n,M,N)\\rightarrow\\begin{cases}
                E[X] =  n \\cdot \\frac{M}{N}\\\\
                Var[X] = n \\cdot \\frac{M}{N} \\cdot \\left(1-\\frac{M}{N}\\right)\\cdot \\left(\\frac{N-n}{N-1}\\right)
                \\end{cases}\\!$$")
      )
    })
  })
  
  # R,wiki
  # n = N
  # m = M
  # k = n
  
  
  observeEvent(input$gohypvert,{
    output$plothyp <- renderPlot({
      x <- seq(from = 0,
               to = input$khyp,
               by = 1)
      qplot(x,phyper(x,
                     m = input$mhyp,
                     n = input$nhyp-input$mhyp,
                     k = input$khyp),
            main = "Verteilungsfunktion der Hypergeometrischen Verteilung", 
            ylab = "Wahrscheinlichkeit")
      
      
    })})
  
  observeEvent(input$gohypdichte,{
    output$plothyp <- renderPlot({
      x <- seq(from = 0,
               to = input$khyp,
               by = 1)
      qplot(x,dhyper(x,
                     m = input$mhyp,
                     n = input$nhyp-input$mhyp,
                     k = input$khyp),
            main = "Dichte der Hypergeometrischen Verteilung", 
            ylab = "Dichte")
      
      
    })
    
  })
  
  observeEvent(input$gorandhyp,{
    output$caphyp <- renderText("Zufallszahlen:")
    output$reshyp <- renderText(paste(rhyper(isolate(input$randomhyp), 
                                             m = input$mhyp,
                                             n = input$nhyp-input$mhyp,
                                             k = input$khyp), collapse = isolate(input$trzhyp)))
  })
  
  
  ###############################################Poissonverteilung##############################
  
  
  observeEvent(input$goEVpois,{
    output$capEVpois <- renderText("Die ersten Momente:")
    output$EVpois <- renderUI({
      withMathJax(
        paste0("$$X \\sim Poi(",input$lambdapois,")\\rightarrow\\begin{cases}
                E[X] = ",input$lambdapois," \\\\
                Var[X] = ",input$lambdapois,"
                \\end{cases}\\!$$")
      )
    })
    output$EVpois2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim Poi(\\lambda)\\rightarrow\\begin{cases}
                E[X] =  \\lambda\\\\
                Var[X] = \\lambda
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  observeEvent(input$gopoisvert,{
    
    
    output$plotpois <- renderPlot({
      x <- seq(from = qpois(0.001,
                            lambda = input$lambdapois),
               to = qpois(0.999,
                          lambda = input$lambdapois),
               by = 1)
      qplot(x,ppois(x,lambda = input$lambdapois),
            main = "Verteilungsfunktion der Poissonverteilung", 
            ylab = "Wahrscheinlichkeit")
      
      
    })})
  
  observeEvent(input$gopoisdichte,{
    output$plotpois <- renderPlot({
      x <- seq(from = qpois(0.001,
                            lambda = input$lambdapois),
               to = qpois(0.999,
                          lambda = input$lambdapois),
               by = 1)
      qplot(x,dpois(x,lambda = input$lambdapois),
            main = "Dichte der Poissonverteilung", 
            ylab = "Dichte")
      
      
    })
    
  })
  
  
  observeEvent(input$gorandpois,{
    output$cappois <- renderText("Zufallszahlen:")
    output$respois <- renderText(paste(rpois(isolate(input$randompois), 
                                             lambda = input$lambdapois), collapse = isolate(input$trzpois)))
  })
  
  
  
  ###########################Gleichverteilung########################
  
  
  
  observeEvent(input$goEVunif,{
    output$capEVunif <- renderText("Die ersten Momente:")
    output$EVunif <- renderUI({
      withMathJax(
        paste0("$$X \\sim U(",input$startunif,",",input$endunif,")\\rightarrow\\begin{cases}
                E[X] = ",(input$startunif+input$endunif)/2," \\\\
                Var[X] = ",((input$endunif-input$startunif)^2)/12,"
                \\end{cases}\\!$$")
      )
    })
    output$EVunif2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim U(a,b)\\rightarrow\\begin{cases}
                E[X] =  \\frac{a+b}{2}\\\\
                Var[X] = \\frac{(b-a)^2}{12} 
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  observeEvent(input$gounifvert,{
    output$plotunif <- renderPlot({
      x <- seq(from = input$startunif,
               to = input$endunif,
               length.out = 1000)
      qplot(x,punif(x,
                    min = input$startunif,
                    max = input$endunif),
            geom = "line",
            main = "Verteilungsfunktion der Gleichverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = c(input$startunif,input$endunif))+
        geom_vline(aes(xintercept = as.numeric(qunif(input$alphaunif,
                                                     min = input$startunif,
                                                     max = input$endunif))),
                   na.rm = T)
      
    })
    output$quantunif <- renderPrint(paste("Das Quantil bei alpha=",
                                          input$alphaunif ,
                                          "ist:",
                                          qunif(input$alphaunif,
                                                min = input$startunif,
                                                max = input$endunif)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tableunif <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qunif(a,
                                                              min = input$startunif,
                                                              max = input$endunif),6))))
    ) 
  })
  
  
  observeEvent(input$gounif,{
    output$plotunif <- renderPlot({
      x <- seq(from = input$startunif,
               to = input$endunif,
               length.out = 1000)
      qplot(x,dunif(x,
                    min = input$startunif,
                    max = input$endunif),
            geom = "line",
            main = "Dichte der Gleichverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = c(input$startunif,input$endunif))+
        geom_vline(aes(xintercept = as.numeric(qunif(input$alphaunif,
                                                     min = input$startunif,
                                                     max = input$endunif))),
                   na.rm = T)
      
    })
    output$quantunif <- renderPrint(paste("Das Quantil bei alpha=",
                                          input$alphaunif ,
                                          "ist:",
                                          qunif(input$alphaunif,
                                                min = input$startunif,
                                                max = input$endunif)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tableunif <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qunif(a,
                                                              min = input$startunif,
                                                              max = input$endunif),6))))
    ) 
    
    
    
  })
  
  
  observeEvent(input$gorandunif,{
    output$capunif <- renderText("Zufallszahlen:")
    output$resunif <- renderText(paste(round(runif(isolate(input$randomunif), 
                                                   min = input$startunif,
                                                   max = input$endunif),6), collapse = isolate(input$trzunif)))
  })
  
  
  #################################################Normalverteilung##############################
  
  observeEvent(input$goEVnorm,{
    output$capEVnorm <- renderText("Die ersten Momente:")
    output$EVnorm <- renderUI({
      withMathJax(
        paste0("$$X \\sim N(",input$meannorm,",",input$sdnorm^2,")\\rightarrow\\begin{cases}
                E[X] = ",input$meannorm," \\\\
                Var[X] = ",input$sdnorm^2,"
                \\end{cases}\\!$$")
      )
    })
    output$EVnorm2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim N(\\mu,\\sigma^2)\\rightarrow\\begin{cases}
                E[X] = \\mu \\\\
                Var[X] = \\sigma^2
                 \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  observeEvent(input$gonormvert,{
    output$plotnorm <- renderPlot({
      x <- if(is.na(input$startnorm)|is.na(input$endnorm)){
        seq(from = qnorm(0.001,
                         mean = input$meannorm,
                         sd = input$sdnorm),
            to = qnorm(0.999,
                       mean = input$meannorm,
                       sd = input$sdnorm),
            length.out = 1000)
      }else{
        seq(from = input$startnorm,
            to = input$endnorm,
            length.out = 1000)
      }
      qplot(x,pnorm(x,
                    mean = input$meannorm,
                    sd = input$sdnorm),
            geom = "line",
            main = "Verteilungsfunktion der Normalverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startnorm)|is.na(input$endnorm)){
          c(qnorm(0.001,
                  mean = input$meannorm,
                  sd = input$sdnorm),qnorm(0.999,
                                           mean = input$meannorm,
                                           sd = input$sdnorm))
        }else{
          c(input$startnorm,input$endnorm)
        })+
        geom_vline(aes(xintercept = as.numeric(qnorm(input$alphanorm,
                                                     mean = input$meannorm,
                                                     sd = input$sdnorm))),
                   na.rm = T)
      
    })
    output$quantnorm <- renderPrint(paste("Das Quantil bei alpha=",
                                          input$alphanorm ,
                                          "ist:",
                                          qnorm(input$alphanorm,
                                                mean = input$meannorm,
                                                sd = input$sdnorm)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablenorm <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qnorm(a,
                                                              mean = input$meannorm,
                                                              sd = input$sdnorm),6))))
    ) 
  })
  
  
  observeEvent(input$gonorm,{
    output$plotnorm <- renderPlot({
      x <- if(is.na(input$startnorm)|is.na(input$endnorm)){
        seq(from = qnorm(0.001,
                         mean = input$meannorm,
                         sd = input$sdnorm),
            to = qnorm(0.999,
                       mean = input$meannorm,
                       sd = input$sdnorm),
            length.out = 1000)
      }else{
        seq(from = input$startnorm,
            to = input$endnorm,
            length.out = 1000)
      }
      qplot(x,dnorm(x,
                    mean = input$meannorm,
                    sd = input$sdnorm),
            geom = "line",
            main = "Dichte der Normalverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startnorm)|is.na(input$endnorm)){
          c(qnorm(0.001,
                  mean = input$meannorm,
                  sd = input$sdnorm),qnorm(0.999,
                                           mean = input$meannorm,
                                           sd = input$sdnorm))
        }else{
          c(input$startnorm,input$endnorm)
        })+
        geom_vline(aes(xintercept = as.numeric(qnorm(input$alphanorm,
                                                     mean = input$meannorm,
                                                     sd = input$sdnorm))),
                   na.rm = T)
      
    })
    output$quantnorm <- renderPrint(paste("Das Quantil bei alpha=",
                                          input$alphanorm ,
                                          "ist:",
                                          qnorm(input$alphanorm,
                                                mean = input$meannorm,
                                                sd = input$sdnorm)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablenorm <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qnorm(a,
                                                              mean = input$meannorm,
                                                              sd = input$sdnorm),6))))
    ) 
    
    
    
  })
  
  
  
  
  observeEvent(input$gorandnorm,{
    output$capnorm <- renderText("Zufallszahlen:")
    output$resnorm <- renderText(paste(round(rnorm(isolate(input$randomnorm), 
                                                   mean = input$meannorm,
                                                   sd = input$sdnorm),6), collapse = isolate(input$trznorm)))
  })
  
  
  
  
  #########################################Lognormalverteilung#####################################
  
  
  
  observeEvent(input$goEVlognorm,{
    output$capEVlognorm <- renderText("Die ersten Momente:")
    output$EVlognorm <- renderUI({
      withMathJax(
        paste0("$$X \\sim LN(",input$meanlnorm,",",input$sdlnorm^2,")\\rightarrow\\begin{cases}
                E[X] = ",exp(input$meanlnorm+((input$sdlnorm^2)/2))," \\\\
                Var[X] = ",exp(2*input$meanlnorm+(input$sdlnorm^2))*(exp(input$sdlnorm^2)-1),"
                \\end{cases}\\!$$")
      )
    })
    output$EVlognorm2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim LN(\\mu,\\sigma^2)\\rightarrow\\begin{cases}
                E[X] =  e^{\\mu + \\frac{\\sigma^2}{2}}\\\\
                Var[X] = e^{2\\mu + \\sigma^2}\\cdot \\left(e^{\\sigma^2}-1\\right)
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  
  observeEvent(input$golnormvert,{
    output$plotlnorm <- renderPlot({
      x <- if(is.na(input$startlnorm)|is.na(input$endlnorm)){
        seq(from = qlnorm(0.001, 
                          mean = input$meanlnorm,
                          sd = input$sdlnorm),
            to = qlnorm(0.999, 
                        mean = input$meanlnorm,
                        sd = input$sdlnorm),
            length.out = 1000)
      }else{
        seq(from = input$startlnorm,
            to = input$endlnorm,
            length.out = 1000) 
      }
      qplot(x,plnorm(x,
                     meanlog = input$meanlnorm,
                     sdlog = input$sdlnorm),
            geom = "line",
            main = "Verteilungsfunktion der Lognormalverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startlnorm)|is.na(input$endlnorm)){
          c(qlnorm(0.001, 
                   mean = input$meanlnorm,
                   sd = input$sdlnorm),qlnorm(0.999, 
                                              mean = input$meanlnorm,
                                              sd = input$sdlnorm))
        }else{
          c(input$startlnorm,input$endlnorm)
        }) +
        geom_vline(aes(xintercept = as.numeric(qlnorm(input$alphalnorm,
                                                      mean = input$meanlnorm,
                                                      sd = input$sdlnorm))),
                   na.rm = T)
      
    })
    output$quantlnorm <- renderPrint(paste("Das Quantil bei alpha=",
                                           input$alphalnorm ,
                                           "ist:",
                                           qlnorm(input$alphalnorm,
                                                  meanlog = input$meanlnorm,
                                                  sdlog = input$sdlnorm)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablelnorm <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qlnorm(a,
                                                               meanlog = input$meanlnorm,
                                                               sdlog = input$sdlnorm),6))))
    )
  })
  
  
  observeEvent(input$golnorm,{
    output$plotlnorm <- renderPlot({
      x <- if(is.na(input$startlnorm)|is.na(input$endlnorm)){
        seq(from = qlnorm(0.001, 
                          mean = input$meanlnorm,
                          sd = input$sdlnorm),
            to = qlnorm(0.999, 
                        mean = input$meanlnorm,
                        sd = input$sdlnorm),
            length.out = 1000)
      }else{
        seq(from = input$startlnorm,
            to = input$endlnorm,
            length.out = 1000) 
      }
      qplot(x,dlnorm(x,
                     meanlog = input$meanlnorm,
                     sdlog = input$sdlnorm),
            geom = "line",
            main = "Dichte der Lognormalverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startlnorm)|is.na(input$endlnorm)){
          c(qlnorm(0.001, 
                   mean = input$meanlnorm,
                   sd = input$sdlnorm),qlnorm(0.999, 
                                              mean = input$meanlnorm,
                                              sd = input$sdlnorm))
        }else{
          c(input$startlnorm,input$endlnorm)
        }) +
        geom_vline(aes(xintercept = as.numeric(qlnorm(input$alphalnorm,
                                                      mean = input$meanlnorm,
                                                      sd = input$sdlnorm))),
                   na.rm = T)
      
    })
    output$quantlnorm <- renderPrint(paste("Das Quantil bei alpha=",
                                           input$alphalnorm ,
                                           "ist:",
                                           qlnorm(input$alphalnorm,
                                                  meanlog = input$meanlnorm,
                                                  sdlog = input$sdlnorm)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablelnorm <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qlnorm(a,
                                                               meanlog = input$meanlnorm,
                                                               sdlog = input$sdlnorm),6))))
    ) 
  })
  
  
  
  observeEvent(input$gorandlognorm,{
    output$caplognorm <- renderText("Zufallszahlen:")
    output$reslognorm <- renderText(paste(round(rlnorm(isolate(input$randomlognorm), 
                                                       meanlog = input$meanlnorm,
                                                       sdlog = input$sdlnorm),6), collapse = isolate(input$trzlognorm)))
  })
  
  
  
  
  ########################################Exponentialverteilung########################################
  
  
  
  observeEvent(input$goEVexp,{
    output$capEVexp <- renderText("Die ersten Momente:")
    output$EVexp <- renderUI({
      withMathJax(
        paste0("$$X \\sim Exp(",input$lambdaexp,")\\rightarrow\\begin{cases}
                E[X] = ",1/input$lambdaexp," \\\\
                Var[X] = ",1/input$lambdaexp^2,"
                \\end{cases}\\!$$")
      )
    })
    output$EVexp2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim Exp(\\lambda)\\rightarrow\\begin{cases}
                E[X] = \\frac{1}{\\lambda} \\\\
                Var[X] = \\frac{1}{\\lambda^2}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  
  observeEvent(input$goexpvert,{
    output$plotexp <- renderPlot({
      x <- if(is.na(input$startexp)|is.na(input$endexp)){
        seq(from = qexp(0.001,
                        rate = input$lambdaexp),
            to = qexp(0.999,
                      rate = input$lambdaexp),
            length.out = 1000)
      }else{
        seq(from = input$startexp,
            to = input$endexp,
            length.out = 1000)
      }
      qplot(x,pexp(x,
                   rate = input$lambdaexp),
            geom = "line",
            main = "Verteilungsfunktion der Exponentialverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startexp)|is.na(input$endexp)){
          c(qexp(0.001,
                 rate = input$lambdaexp),qexp(0.999,
                                              rate = input$lambdaexp))
        }else{
          c(input$startexp,input$endexp)
        }) +
        geom_vline(aes(xintercept = as.numeric(qexp(input$alphaexp,
                                                    rate = input$lambdaexp))),
                   na.rm = T)
      
    })
    output$quantexp <- renderPrint(paste("Das Quantil bei alpha=",
                                         input$alphaexp ,
                                         "ist:",
                                         qexp(input$alphaexp,
                                              rate = input$lambdaexp)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tableexp <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qexp(a,rate = input$lambdaexp),6))))
    ) 
  })
  
  
  observeEvent(input$goexp,{
    output$plotexp <- renderPlot({
      x <- if(is.na(input$startexp)|is.na(input$endexp)){
        seq(from = qexp(0.001,
                        rate = input$lambdaexp),
            to = qexp(0.999,
                      rate = input$lambdaexp),
            length.out = 1000)
      }else{
        seq(from = input$startexp,
            to = input$endexp,
            length.out = 1000)
      }
      qplot(x,dexp(x,
                   rate = input$lambdaexp),
            geom = "line",
            main = "Dichte der Exponentialverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startexp)|is.na(input$endexp)){
          c(qexp(0.001,
                 rate = input$lambdaexp),qexp(0.999,
                                              rate = input$lambdaexp))
        }else{
          c(input$startexp,input$endexp)
        }) +
        geom_vline(aes(xintercept = as.numeric(qexp(input$alphaexp,
                                                    rate = input$lambdaexp))),
                   na.rm = T)
      
    })
    output$quantexp <- renderPrint(paste("Das Quantil bei alpha=",
                                         input$alphaexp ,
                                         "ist:",
                                         qexp(input$alphaexp,
                                              rate = input$lambdaexp)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tableexp <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qexp(a,
                                                             rate = input$lambdaexp),6))))
    ) 
    
  })
  
  
  
  
  observeEvent(input$gorandexp,{
    output$capexp <- renderText("Zufallszahlen:")
    output$resexp <- renderText(paste(round(rexp(isolate(input$randomexp), 
                                                 rate = input$lambdaexp),6), collapse = isolate(input$trzexp)))
  })
  
  
  
  
  
  ########################################Chi-Quadrat-Verteilung########################################
  
  observeEvent(input$goEVchisq,{
    output$capEVchisq <- renderText("Die ersten Momente:")
    output$EVchisq <- renderUI({
      withMathJax(
        paste0("$$X \\sim \\chi^2_{",input$dfchisq,"}\\rightarrow\\begin{cases}
                E[X] = ",input$dfchisq," \\\\
                Var[X] = ",2*input$dfchisq,"
                \\end{cases}\\!$$")
      )
    })
    output$EVchisq2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim \\chi^2_{df}\\rightarrow\\begin{cases}
                E[X] =  df\\\\
                Var[X] = 2\\cdot df
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  observeEvent(input$gochisqvert,{
    output$plotchisq <- renderPlot({
      x <- if(is.na(input$startchisq)|is.na(input$endchisq)){
        seq(from = qchisq(0.0001,
                          df = input$dfchisq),
            to = qchisq(0.9999,
                        df = input$dfchisq),
            length.out = 1000)
      }else{
        seq(from = input$startchisq,
            to = input$endchisq,
            length.out = 1000)
      }
      qplot(x,pchisq(x,
                     df = input$dfchisq),
            geom = "line",
            main = "Verteilungsfunktion der Chi-Quadrat-Verteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startchisq)|is.na(input$endchisq)){
          c(qchisq(0.0001,
                   df = input$dfchisq),qchisq(0.9999,
                                              df = input$dfchisq))
        }else{
          c(input$startchisq,input$endchisq)
        }) +
        geom_vline(aes(xintercept = as.numeric(qchisq(input$alphachisq,
                                                      df = input$dfchisq))),
                   na.rm = T)
      
    })
    output$quantchisq <- renderPrint(paste("Das Quantil bei alpha=",
                                           input$alphachisq ,
                                           "ist:",
                                           qchisq(input$alphachisq,
                                                  df = input$dfchisq)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablechisq <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qchisq(a,df=input$dfchisq),6))))
    ) 
  })
  
  
  
  observeEvent(input$gochisq,{
    output$plotchisq <- renderPlot({
      x <- if(is.na(input$startchisq)|is.na(input$endchisq)){
        seq(from = qchisq(0.0001,
                          df = input$dfchisq),
            to = qchisq(0.9999,
                        df = input$dfchisq),
            length.out = 1000)
      }else{
        seq(from = input$startchisq,
            to = input$endchisq,
            length.out = 1000)
      }
      qplot(x,dchisq(x,
                     df = input$dfchisq),
            geom = "line",
            main = "Dichte der Chi-Quadrat-Verteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startchisq)|is.na(input$endchisq)){
          c(qchisq(0.0001,
                   df = input$dfchisq),qchisq(0.9999,
                                              df = input$dfchisq))
        }else{
          c(input$startchisq,input$endchisq)
        })+
        geom_vline(aes(xintercept = as.numeric(qchisq(input$alphachisq,
                                                      df = input$dfchisq))),
                   na.rm = T)
      
    })
    output$quantchisq <- renderPrint(paste("Das Quantil bei alpha=",
                                           input$alphachisq ,
                                           "ist:",
                                           qchisq(input$alphachisq,
                                                  df = input$dfchisq)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablechisq <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qchisq(a,df=input$dfchisq),6))))
    ) 
    
  })
  
  
  
  
  observeEvent(input$gorandchisq,{
    output$capchisq <- renderText("Zufallszahlen:")
    output$reschisq <- renderText(paste(round(rchisq(isolate(input$randomchisq), 
                                                     df=input$dfchisq),6), collapse = isolate(input$trzchisq)))
  })
  
  
  
  
  
  
  
  ########################################Studentsche t-Verteilung########################################
  
  
  observeEvent(input$goEVt,{
    output$capEVt <- renderText("Die ersten Momente:")
    output$EVt <- renderUI({
      withMathJax(
        paste0("$$X \\sim t_{",input$dft,"}\\rightarrow\\begin{cases}
                E[X] = ",0," \\\\
                Var[X] = ",input$dft/(input$dft-2),"
                \\end{cases}\\!$$")
      )
    })
    output$EVt2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim t_{df}\\rightarrow\\begin{cases}
                E[X] = 0 \\\\
                Var[X] = \\frac{df}{df-2}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  observeEvent(input$gotvert,{
    output$plott <- renderPlot({
      x <- if(is.na(input$startt) | is.na(input$endt)){
        seq(from = qt(0.001, 
                      df = input$dft),
            to = qt(0.999, 
                    df = input$dft),
            length.out = 1000)
      }else{
        seq(from = input$startt,
            to = input$endt,
            length.out = 1000)
      }
      qplot(x,pt(x,
                 df = input$dft),
            geom = "line",
            main = "Verteilungsfunktion der Studentschen t-Verteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startt) | is.na(input$endt)){
          c(qt(0.001,
               df = input$dft),
            qt(0.999, 
               df = input$dft))
        }else{
          c(input$startt,input$endt)
        })+
        geom_vline(aes(xintercept = as.numeric(qt(input$alphat,
                                                  df = input$dft))),
                   na.rm = T)
      
    })
    output$textt <- renderPrint("Das Quantil")
    output$quantt <- renderPrint(paste("Das Quantil bei alpha=",
                                       input$alphat ,
                                       "ist:",
                                       qt(input$alphat,
                                          df = input$dft)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablet <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qt(a,df=input$dft),6))))
    ) 
  })
  
  
  
  observeEvent(input$got,{
    output$plott <- renderPlot({
      x <- if(is.na(input$startt) | is.na(input$endt)){
        seq(from = qt(0.001, 
                      df = input$dft),
            to = qt(0.999, 
                    df = input$dft),
            length.out = 1000)
      }else{
        seq(from = input$startt,
            to = input$endt,
            length.out = 1000)
      }
      qplot(x,dt(x,
                 df = input$dft),
            geom = "line",
            main = "Dichte der Studentschen t-Verteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startt) | is.na(input$endt)){
          c(qt(0.001,
               df = input$dft),
            qt(0.999, 
               df = input$dft))
        }else{
          c(input$startt,input$endt)
        })+
        geom_vline(aes(xintercept = as.numeric(qt(input$alphat,
                                                  df = input$dft))),
                   na.rm = T)
      
    })
    output$quantt <- renderPrint(paste("Das Quantil bei alpha=",
                                       input$alphat ,
                                       "ist:",
                                       qt(input$alphat,
                                          df = input$dft)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablet <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qt(a,df=input$dft),6))))
    ) 
    
  })
  
  
  
  observeEvent(input$gorandt,{
    output$capt <- renderText("Zufallszahlen:")
    output$rest <- renderText(paste(round(rt(isolate(input$randomt), 
                                             df=input$dft),6), collapse = isolate(input$trzt)))
  })
  
  
  
  
  
  ########################################F-Verteilung########################################
  
  
  observeEvent(input$goEVf,{
    output$capEVf <- renderText("Die ersten Momente:")
    output$EVf <- renderUI({
      withMathJax(
        paste0("$$X \\sim F_{",input$dff1,",",input$dff2,"}\\rightarrow\\begin{cases}
                E[X] = ",input$dff2/(input$dff2-2)," \\\\
                Var[X] = ",(2*input$dff2^2*(input$dff1+input$dff2-2))/(input$dff1*(input$dff2-2)^2*(input$dff2-4)),"
                \\end{cases}\\!$$")
      )
    })
    output$EVf2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim F_{df_1,df_2}\\rightarrow\\begin{cases}
                E[X] =  \\frac{df_2}{df_2-2}\\\\
                Var[X] = \\frac{2\\cdot df^2_2\\cdot(df_1+df_2-2)}{df_1 \\cdot (df_2-2)^2\\cdot (df_2-4)}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  
  observeEvent(input$gofvert,{
    output$plotf <- renderPlot({
      x <- if(is.na(input$startf) | is.na(input$endf)){
        seq(from = qf(0.0001, 
                      df1 = input$dff1,
                      df2 = input$dff2),
            to = qf(0.999, 
                    df1 = input$dff1,
                    df2 = input$dff2),
            length.out = 1000)
      }else{
        seq(from = input$startf,
            to = input$endf,
            length.out = 1000)
      }
      qplot(x,pf(x,
                 df1 = input$dff1,
                 df2 = input$dff2),
            geom = "line",
            main = "Verteilungsfunktion der F-Verteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startf) | is.na(input$endf)){
          c(qf(0.0001,
               df1 = input$dff1,
               df2 = input$dff2),
            qf(0.999, 
               df1 = input$dff1,
               df2 = input$dff2))
        }else{
          c(input$startf,input$endf)
        })+
        geom_vline(aes(xintercept = as.numeric(qf(input$alphaf,
                                                  df1 = input$dff1,
                                                  df2 = input$dff2))),
                   na.rm = T)
      
    })
    output$quantf <- renderPrint(paste("Das Quantil bei alpha=",
                                       input$alphaf ,
                                       "ist:",
                                       qf(input$alphaf,
                                          df1 = input$dff1,
                                          df2 = input$dff2)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablef <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qf(a,df1=input$dff1,df2 = input$dff2),6))))
    ) 
  })
  
  
  
  
  observeEvent(input$gof,{
    output$plotf <- renderPlot({
      x <- if(is.na(input$startf) | is.na(input$endf)){
        seq(from = qf(0.0001, 
                      df1 = input$dff1,
                      df2 = input$dff2),
            to = qf(0.999, 
                    df1 = input$dff1,
                    df2 = input$dff2),
            length.out = 1000)
      }else{
        seq(from = input$startf,
            to = input$endf,
            length.out = 1000)
      }
      qplot(x,df(x,
                 df1 = input$dff1,
                 df2 = input$dff2),
            geom = "line",
            main = "Dichte der F-Verteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startf) | is.na(input$endf)){
          c(qf(0.0001,
               df1 = input$dff1,
               df2 = input$dff2),
            qf(0.999, 
               df1 = input$dff1,
               df2 = input$dff2))
        }else{
          c(input$startf,input$endf)
        })+
        geom_vline(aes(xintercept = as.numeric(qf(input$alphaf,
                                                  df1 = input$dff1,
                                                  df2 = input$dff2))),
                   na.rm = T)
      
    })
    output$quantf <- renderPrint(paste("Das Quantil bei alpha=",
                                       input$alphaf ,
                                       "ist:",
                                       qf(input$alphaf,
                                          df1 = input$dff1,
                                          df2 = input$dff2)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablef <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qf(a,df1=input$dff1,df2 = input$dff2),6))))
    ) 
    
  })
  
  
  
  
  observeEvent(input$gorandf,{
    output$capf <- renderText("Zufallszahlen:")
    output$resf <- renderText(paste(round(rf(isolate(input$randomf), 
                                             df1 = input$dff1,
                                             df2 = input$dff2),6), collapse = isolate(input$trzf)))
  })
  
  
  
  
  
  
  #######################################gammaverteilung####################################################
  
  observeEvent(input$goEVgamma,{
    output$capEVgamma <- renderText("Die ersten Momente:")
    output$EVgamma <- renderUI({
      withMathJax(
        paste0("$$X \\sim \\Gamma (",input$bgamma,",",input$pgamma,")\\rightarrow\\begin{cases}
                E[X] = ",input$pgamma/input$bgamma," \\\\
                Var[X] = ",input$pgamma/input$bgamma^2,"
                \\end{cases}\\!$$")
      )
    })
    output$EVgamma2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim \\Gamma (a,b)\\rightarrow\\begin{cases}
                E[X] = \\frac{b}{a} \\\\
                Var[X] = \\frac{b}{a^2}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  observeEvent(input$gogammavert,{
    output$plotgamma <- renderPlot({
      x <- if(is.na(input$startgamma) | is.na(input$endgamma)){
        seq(from = qgamma(0.001, 
                          shape = input$pgamma,
                          rate = input$bgamma),
            to = qgamma(0.999, 
                        shape = input$pgamma,
                        rate = input$bgamma),
            length.out = 1000)
      }else{
        seq(from = input$startgamma,
            to = input$endgamma,
            length.out = 1000)
      }
      qplot(x,pgamma(x,
                     shape = input$pgamma,
                     rate = input$bgamma),
            geom = "line",
            main = "Verteilungsfunktion der Gammaverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startgamma) | is.na(input$endgamma)){
          c(qgamma(0.001,
                   shape = input$pgamma,
                   rate = input$bgamma),
            qgamma(0.999, 
                   shape = input$pgamma,
                   rate = input$bgamma))
        }else{
          c(input$startgamma,input$endgamma)
        })+
        geom_vline(aes(xintercept = as.numeric(qgamma(input$alphagamma,
                                                      shape = input$pgamma,
                                                      rate = input$bgamma))),
                   na.rm = T)
      
    })
    output$quantgamma <- renderPrint(paste("Das Quantil bei alpha=",
                                           input$alphagamma ,
                                           "ist:",
                                           qgamma(input$alphagamma,
                                                  shape = input$pgamma,
                                                  rate = input$bgamma)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablegamma <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qgamma(a,
                                                               shape = input$pgamma,
                                                               rate = input$bgamma),6))))
    ) 
  })
  
  
  
  observeEvent(input$gogamma,{
    output$plotgamma <- renderPlot({
      x <- if(is.na(input$startgamma) | is.na(input$endgamma)){
        seq(from = qgamma(0.001, 
                          shape = input$pgamma,
                          rate = input$bgamma),
            to = qgamma(0.999, 
                        shape = input$pgamma,
                        rate = input$bgamma),
            length.out = 1000)
      }else{
        seq(from = input$startgamma,
            to = input$endgamma,
            length.out = 1000)
      }
      qplot(x,dgamma(x,
                     shape = input$pgamma,
                     rate = input$bgamma),
            geom = "line",
            main = "Dichte der Gammaverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startgamma) | is.na(input$endgamma)){
          c(qgamma(0.001,
                   shape = input$pgamma,
                   rate = input$bgamma),
            qgamma(0.999, 
                   shape = input$pgamma,
                   rate = input$bgamma))
        }else{
          c(input$startgamma,input$endgamma)
        })+
        geom_vline(aes(xintercept = as.numeric(qgamma(input$alphagamma,
                                                      shape = input$pgamma,
                                                      rate = input$bgamma))),
                   na.rm = T)
      
    })
    output$quantgamma <- renderPrint(paste("Das Quantil bei alpha=",
                                           input$alphagamma ,
                                           "ist:",
                                           qgamma(input$alphagamma,
                                                  shape = input$pgamma,
                                                  rate = input$bgamma)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablegamma <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qgamma(a,shape = input$pgamma,
                                                               rate = input$bgamma),6))))
    ) 
    
  })
  
  
  
  observeEvent(input$gorandgamma,{
    output$capgamma <- renderText("Zufallszahlen:")
    output$resgamma <- renderText(paste(round(rgamma(isolate(input$randomgamma), 
                                                     shape = input$pgamma,
                                                     rate = input$bgamma),6), collapse = isolate(input$trzgamma)))
  })
  
  
  
  
  
  
  
  ############################################Betaverteilung############################################
  
  observeEvent(input$goEVbeta,{
    output$capEVbeta <- renderText("Die ersten Momente:")
    output$EVbeta <- renderUI({
      withMathJax(
        paste0("$$X \\sim \\mathcal{B} (",input$alphaparambeta,",",input$betabeta,")\\rightarrow\\begin{cases}
                E[X] = ",input$alphaparambeta/(input$alphaparambeta+input$betabeta)," \\\\
                Var[X] = ",(input$alphaparambeta*input$betabeta)/((input$alphaparambeta+input$betabeta)^2*(input$alphaparambeta+input$betabeta+1)),"
                \\end{cases}\\!$$")
      )
    })
    output$EVbeta2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim \\mathcal{B} (\\alpha,\\beta)\\rightarrow\\begin{cases}
                E[X] =  \\frac{\\alpha}{\\alpha + \\beta}\\\\
                Var[X] = \\frac{\\alpha \\cdot \\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  observeEvent(input$gobetavert,{
    output$plotbeta <- renderPlot({
      x <- if(is.na(input$startbeta) | is.na(input$endbeta)){
        seq(from = qbeta(0.001, 
                         shape1 = input$alphaparambeta,
                         shape2 = input$betabeta),
            to = qbeta(0.999, 
                       shape1 = input$alphaparambeta,
                       shape2 = input$betabeta),
            length.out = 1000)
      }else{
        seq(from = input$startbeta,
            to = input$endbeta,
            length.out = 1000)
      }
      qplot(x,pbeta(x,
                    shape1 = input$alphaparambeta,
                    shape2 = input$betabeta),
            geom = "line",
            main = "Verteilungsfunktion der Betaverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startbeta) | is.na(input$endbeta)){
          c(qbeta(0.001,
                  shape1 = input$alphaparambeta,
                  shape2 = input$betabeta),
            qbeta(0.999, 
                  shape1 = input$alphaparambeta,
                  shape2 = input$betabeta))
        }else{
          c(input$startbeta,input$endbeta)
        }) +
        geom_vline(aes(xintercept = as.numeric(qbeta(input$alphabeta,
                                                     shape1 = input$alphaparambeta,
                                                     shape2 = input$betabeta))),
                   na.rm = T)
      
    })
    output$quantbeta <- renderPrint(paste("Das Quantil bei alpha=",
                                          input$alphabeta ,
                                          "ist:",
                                          qbeta(input$alphabeta,
                                                shape1 = input$alphaparambeta,
                                                shape2 = input$betabeta)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablebeta <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qbeta(a,
                                                              shape1 = input$alphaparambeta,
                                                              shape2 = input$betabeta),6))))
    ) 
  })
  
  
  observeEvent(input$gobeta,{
    output$plotbeta <- renderPlot({
      x <- if(is.na(input$startbeta) | is.na(input$endbeta)){
        seq(from = qbeta(0.001, 
                         shape1 = input$alphaparambeta,
                         shape2 = input$betabeta),
            to = qbeta(0.999, 
                       shape1 = input$alphaparambeta,
                       shape2 = input$betabeta),
            length.out = 1000)
      }else{
        seq(from = input$startbeta,
            to = input$endbeta,
            length.out = 1000)
      }
      qplot(x,dbeta(x,
                    shape1 = input$alphaparambeta,
                    shape2 = input$betabeta),
            geom = "line",
            main = "Dichte der Betaverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startbeta) | is.na(input$endbeta)){
          c(qbeta(0.001,
                  shape1 = input$alphaparambeta,
                  shape2 = input$betabeta),
            qbeta(0.999, 
                  shape1 = input$alphaparambeta,
                  shape2 = input$betabeta))
        }else{
          c(input$startbeta,input$endbeta)
        })+
        geom_vline(aes(xintercept = as.numeric(qbeta(input$alphabeta,
                                                     shape1 = input$alphaparambeta,
                                                     shape2 = input$betabeta))),
                   na.rm = T)
      
    })
    output$quantbeta <- renderPrint(paste("Das Quantil bei alpha=",
                                          input$alphabeta,
                                          "ist:",
                                          qbeta(input$alphabeta,
                                                shape1 = input$alphaparambeta,
                                                shape2 = input$betabeta)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablebeta <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qbeta(a,shape1 = input$alphaparambeta,
                                                              shape2 = input$betabeta),6))))
    ) 
    
  })
  
  
  
  
  
  observeEvent(input$gorandbeta,{
    output$capbeta <- renderText("Zufallszahlen:")
    output$resbeta <- renderText(paste(round(rbeta(isolate(input$randombeta), 
                                                   shape1 = input$alphaparambeta,
                                                   shape2 = input$betabeta),6), collapse = isolate(input$trzbeta)))
  })
  
  
  
  
  
  
  
  ########################################Paretoverteilung########################################
  
  observeEvent(input$goEVpareto,{
    output$capEVpareto <- renderText("Die ersten Momente:")
    output$EVpareto <- renderUI({
      withMathJax(
        paste0("$$X \\sim  Par(",input$apareto,",",input$kpareto,")\\rightarrow\\begin{cases}
                E[X] = ",input$apareto*input$kpareto/(input$kpareto-1)," \\\\
                Var[X] = ",(input$kpareto*input$apareto^2)/((input$kpareto-1)^2*(input$kpareto-2)),"
                \\end{cases}\\!$$")
      )
    })
    output$EVpareto2 <- renderUI({
      withMathJax(
        paste0("$$X \\sim  Par(a,b)\\rightarrow\\begin{cases}
                E[X] = \\frac{ab}{b-1} \\\\
                Var[X] = \\frac{ba^2}{(b-1)^2\\cdot(b-2)}
                \\end{cases}\\!$$")
      )
    })
  })
  
  
  
  
  
  
  observeEvent(input$goparetovert,{
    output$plotpareto <- renderPlot({
      x <- if(is.na(input$startpareto) | is.na(input$endpareto)){
        seq(from = qpareto(0.001, 
                           location = input$apareto,
                           shape = input$kpareto),
            to = qpareto(0.999, 
                         location = input$apareto,
                         shape = input$kpareto),
            length.out = 1000)
      }else{
        seq(from = input$startpareto,
            to = input$endpareto,
            length.out = 1000)
      }
      qplot(x,ppareto(x,
                      location = input$apareto,
                      shape = input$kpareto),
            geom = "line",
            main = "Verteilungsfunktion der Paretoverteilung", 
            ylab = "Wahrscheinlichkeit")+
        scale_x_continuous(limits = if(is.na(input$startpareto) | is.na(input$endpareto)){
          c(qpareto(0.001,
                    location = input$apareto,
                    shape = input$kpareto),
            qpareto(0.999, 
                    location = input$apareto,
                    shape = input$kpareto))
        }else{
          c(input$startpareto,input$endpareto)
        }) +
        geom_vline(aes(xintercept = as.numeric(qpareto(input$alphapareto,
                                                       location = input$apareto,
                                                       shape = input$kpareto))),
                   na.rm = T)
      
    })
    output$quantpareto <- renderPrint(paste("Das Quantil bei alpha=",
                                            input$alphapareto ,
                                            "ist:",
                                            qpareto(input$alphapareto,
                                                    location = input$apareto,
                                                    shape = input$kpareto)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablepareto <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qpareto(a,
                                                                location = input$apareto,
                                                                shape = input$kpareto),6))))
    ) 
  })
  
  
  observeEvent(input$gopareto,{
    output$plotpareto <- renderPlot({
      x <- if(is.na(input$startpareto) | is.na(input$endpareto)){
        seq(from = qpareto(0.001, 
                           location = input$apareto,
                           shape = input$kpareto),
            to = qpareto(0.999, 
                         location = input$apareto,
                         shape = input$kpareto),
            length.out = 1000)
      }else{
        seq(from = input$startpareto,
            to = input$endpareto,
            length.out = 1000)
      }
      qplot(x,dpareto(x,
                      location = input$apareto, 
                      shape = input$kpareto),
            geom = "line",
            main = "Dichte der Paretoverteilung", 
            ylab = "Dichte")+
        scale_x_continuous(limits = if(is.na(input$startpareto) | is.na(input$endpareto)){
          c(qpareto(0.001,
                    location = input$apareto,
                    shape = input$kpareto),
            qpareto(0.999, 
                    location = input$apareto,
                    shape = input$kpareto))
        }else{
          c(input$startpareto,input$endpareto)
        }) +
        geom_vline(aes(xintercept = as.numeric(qpareto(input$alphapareto,
                                                       location = input$apareto, 
                                                       shape = input$kpareto))),
                   na.rm = T)
      
    })
    output$quantpareto <- renderPrint(paste("Das Quantil bei alpha=",
                                            input$alphapareto ,
                                            "ist:",
                                            qpareto(input$alphapareto,
                                                    location = input$apareto, 
                                                    shape = input$kpareto)))
    
    a <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    output$tablepareto <- renderTable(
      cbind(c("Alpha:","Quantil:"),t(data.frame(a,round(qpareto(a, 
                                                                location = input$apareto,
                                                                shape = input$kpareto),6))))
    ) 
    
  })
  
  
  
  
  
  observeEvent(input$gorandpareto,{
    output$cappareto <- renderText("Zufallszahlen:")
    output$respareto <- renderText(paste(round(rpareto(isolate(input$randompareto), 
                                                       location = input$apareto,
                                                       shape = input$kpareto),6), collapse = isolate(input$trzpareto)))
  })
  
  
  
  
  
  
  
  
  
  
}

