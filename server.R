library(shiny)
library(ggplot2)
library(reshape2)
library(fields)
library(robustX)
library(DMwR)
library(depth)
library(MASS)

shinyServer(function(input, output)
{ 
  
  load("demodata.RData")
  colors <- rep(0, times=2200)
  cbPalette <- c("#99CC33", "#CC3300", "#FFCC33")
  convex_result <- NA
  db_result <- NA
  lof_result <- NA
  
  set2.man <- c(901:1900)
  set1.man <- c(1:900)
  set3.man <- c(1901:2100)
  set4.man <- c(2101:2150)
  set5.man <- c(2151:2200)
  set6.man <- which(d$x >= 4.5)
  
  
  output$convex_l = renderUI({
    if (input$algo == "convex")
    {
      sliderInput("convex_l",
                  "Depth",
                  value = 1,
                  min = 1,
                  max = 25)
    }
  })
  
  output$eps = renderUI({
    if (input$algo == "db")
    {
      sliderInput("eps",
                  "Radius",
                  value = 1,
                  min = 0,
                  max = 2, 
                  step = 0.1)
    }
  })
  
  output$ratio = renderUI({
    if (input$algo == "db")
    {
      sliderInput("ratio",
                  "Ratio",
                  value = 0.05,
                  min = 0,
                  max = 0.25, 
                  step = 0.001)
    }
  })
  
  output$alpha = renderUI({
    if (input$algo == "bacon")
    {
      sliderInput("alpha",
                  "alpha",
                  value = 0.95,
                  min = 0.01,
                  max = 0.99, 
                  step = 0.01)
    }
  })
  
  
  output$lof_k = renderUI({
    if (input$algo == "lof")
    {
      sliderInput("lof_k",
                  "k",
                  value = 5,
                  min = 1,
                  max = 30)
    }
  })
  
 # output$animation = renderUI({
#    if (input$algo == "bacon")
#    {
#      max = 10
      #sliderInput("animation", 
       #           "Step counter:", 
      #            value = 1, 
      #            max = 10, 
      #            min = 1, 
      #            step = 1, 
      #            animate=T) 
#    }
#  })
  
  output$threshold = renderUI({
    if (input$algo == "lof")
    {
      print(length(lof_result))
      if(length(lof_result) == 1) {
        sliderInput("threshold",
                    "threshold",
                    value = 1.5,
                    min = 1,
                    max = 3,
                    step = 0.01
        )  
      } else {
        print(length(lof_result))
        min <- min(lof_result)
        max <- max(lof_result)
        
        sliderInput("threshold",
                    "threshold",
                    value = quantile(lof_result, probs=0.95),
                    min = min,
                    max = max
        )
      }
    } 
  })
  
  output$mansel = renderUI({
    if (input$algo == "bacon")
    {
      selectInput(inputId = "mansel",
                  label = "Type:",
                  choices = c("Mahalanobis"    = "mahal",
                              "Manual selection"      = "manual"
                              ),
                  selected = "mahal")
    } 
  })
  
  output$set = renderUI({
    if (input$algo == "bacon")
    {
      if(is.null(input$mansel)) {
        shiny:::flushReact()
        return()
      }
      if (input$mansel == "manual") {
        selectInput(inputId = "set",
                    label = "Initial set:",
                    choices = c("Set 1"    = "set1",
                                "Set 2"      = "set2",
                                "Set 3"      = "set3",
                                "Set 4"      = "set4",
                                "Set 5"      = "set5",
                                "Set 6"      = "set6"
                    ),
                    selected = "set1")  
      }
    } 
  })
  
  
  output$size = renderUI({
    if (input$algo == "bacon")
    {
      if(is.null(input$mansel)) {
        shiny:::flushReact()
        return()
      }
      if(input$mansel == "mahal") {
        sliderInput("size",
                    "size",
                    value = 550,
                    min = 1,
                    max = 1100)  
      }
      
    }
  })
  
##########################



############
# Plotting #
############
output$plot = renderPlot({
  if (input$algo == "lof") {
    c <- calc_lof()
    if(length(c) == 0) {
      shiny:::flushReact()
      return()
    }
    print("input_thresholdra varva...")
    if (is.null(input$threshold)) {
      shiny:::flushReact()
      return()
    }
    print("input_threshold megjott,")
    lof_result <<- c
    d$colors = (c >= input$threshold) 
    base <- ggplot(d)
    base <- base + geom_point(aes(x = x, y = y,  col = colors)) + 
      scale_colour_manual(values=cbPalette) +
      ##xlim(c(-10, 10)) +
      ###ylim(c(-10, 10)) +
      theme(legend.position = "none")
    print(base)
  } 
  ###########################convex###################################
  else if (input$algo == "convex") {
    b <- calc_convex()
    if(length(b) == 0) {
      shiny:::flushReact()
      return()
    }
    convex_result <<- b
    base <- ggplot()
    base <- base + geom_point(data = d, aes(x = x, y = y)) + 
      geom_point(data = b, aes(x = x, y = y, color = as.factor(color))) +
      geom_path(data = b, aes(x = x, y = y,group = color, color = as.factor(color)) ) +
      scale_colour_hue(l=40) +
      #xlim(c(-10, 10)) +
      ##ylim(c(-10, 10)) +
      theme(legend.position = "none")
    print(base)
  } 
  #####################################################################
  ############################### db #################################
  else if (input$algo == "db") {
    c <- calc_db()
    if(length(c) == 0) {
      shiny:::flushReact()
      return()
    }
    print("idaig")
    db_result <<- c
    th <- 0.1
    if(!is.null(input$ratio)) {
      th =  input$ratio 
    }
    d$color <- (c <= th)
    base <- ggplot()
    if (sum(d$color) == length(d[,1])) {
        base <- base + geom_point(data = d, aes(x = x, y = y), color = "#CC3300") +
        scale_colour_manual(values=cbPalette)+
          #xlim(c(-10, 10)) +
          ##ylim(c(-10, 10)) +
        theme(legend.position = "none")  
    } else {
        base <- base + geom_point(data = d, aes(x = x, y = y, color = color)) +
        scale_colour_manual(values=cbPalette)+
          #xlim(c(-10, 10)) +
          #ylim(c(-10, 10)) +
        theme(legend.position = "none")  
    }
    print(base)
  } 
  
  #####################################################################
  
  ################################bacon################################
  
  else if (input$algo == "bacon") {
      c <- calc_bacon()
      if(length(c) == 0) {
        shiny:::flushReact()
        return()
      }
      d$color <- c
      
      if(input$mansel == "manual" & length(unique(d$color)) == 2) {
        print(unique(d$color))
        if(any(unique(d$color) == 0)) {
          base <- ggplot()
          base <- base + geom_point(data = d, aes(x = x, y = y, color = as.factor(color))) +
            scale_colour_manual(values = c("#99CC33", "#FFCC33"))+
            theme(legend.position = "none")
          print(base) 
        } else {
          base <- ggplot()
          base <- base + geom_point(data = d, aes(x = x, y = y, color = as.factor(color))) +
            scale_colour_manual(values = c("#CC3300", "#FFCC33"))+
            theme(legend.position = "none")
          print(base)
        }
      }
      else {
        #cbPalette <- c("#99CC33", "#CC3300", "#FFCC33")
        if (length(unique(d$color)) == 2) {
          print(unique(d$color))
          base <- ggplot()
          base <- base + geom_point(data = d, aes(x = x, y = y, color = as.factor(color))) +
            scale_colour_manual(values = c("#99CC33", "#CC3300"))+
            theme(legend.position = "none")
          print(base) 
        } else {
          base <- ggplot()
          base <- base + geom_point(data = d, aes(x = x, y = y, color = as.factor(color))) +
            scale_colour_manual(values=cbPalette)+
            theme(legend.position = "none")
          print(base) 
        }
      }
  } 
  
  #####################################################################
  else {
    base <- ggplot(d)
    base <- base + geom_point(aes(x = x, y = y)) +
      #xlim(c(-10, 10)) +
      #ylim(c(-10, 10))
    print(base)
    
  }
  })


output$summary <- renderPrint({
  #######################################convex############################
  if (input$algo == "convex" & !is.null(input$convex_l)) {
    print("Depth:")
    print(input$convex_l)
    if(length(convex_result) != 1) {
      print("Ratio of chosen elements: ")
      print(length(convex_result[,1]) / length(d[,1]))
    }
  }
  ########################################################################
  
 # else if (input$algo == "lof" & !is.null(input$lof_k)) { 
#    print("Ratio of chosen elements: ")
#    if(length(lof_result) != 1) {
#      print(sum(lof_result >= input$threshold) / length(d[,1])) 
#    }
#  }
  
  
  ##############################db########################################
  else if (input$algo == "db" & !is.null(input$eps)) { 
    print("Ratio of chosen elements: ")
    if (length(db_result) != 1) {
      print(sum(db_result <= input$ratio) / length(d[,1])) 
    }
  }
  ########################################################################
})


################
# Calculations #
################
calc_lof <- function() {
  print("calc_lof")
  if(is.null(input$lof_k)) {
    shiny:::flushReact()
    print("isnull lof")
    return(numeric(0))
  }
  a <- lofactor(d, input$lof_k)
  print(max(a))
  a
}

calc_convex <- function() {
  print("Calc_convx")
  if(is.null(input$convex_l)) {
      shiny:::flushReact()
      return(numeric(0))
  }
  a <- isodepth(d, dpth = c(1:input$convex_l), output = TRUE)
  b <- matrix(NA, ncol=3)
  for(i in 1:input$convex_l) {
    fs <- cbind(a[[i]], i)
    b <- rbind(b, fs)
    b <- rbind(b, fs[1,]) ## hogy a torott vonal zart legyen!
  }
  b <- as.data.frame(b)
  colnames(b) <- c("x", "y", "color")
  b
}

calc_db <- function() {
  if(is.null(input$eps)) {
    shiny:::flushReact()
    print("isnull db")
    return(numeric(0))
  }
  k <- fields.rdist.near(d, delta = input$eps, max.points=5000000)
  tmp <- as.data.frame(cbind(k[[1]], k[[2]]))
  cc <- dcast(tmp, formula = V1 ~ V2, value.var="V3")
  l <- length(d[,1])
  cc$SUM <- rowSums(!is.na(cc[, 2:l]))
  cc$SUM / l
}

calc_bacon <- function() {
  print("BACON called")
  if(is.null(input$alpha)) {
    shiny:::flushReact()
    print("isnull bacon")
    return(numeric(0))
  }
  
  if (input$mansel == "mahal")  {
    if(is.null(input$size)) {
      shiny:::flushReact()
      print("isnull bacon mahal")
      return(numeric(0))
    }
    a <- mvBACON(x=as.matrix(d), m=input$size, collect = 1, alpha=input$alpha, maxsteps=input$animation)  
    !a$subset
  } else {
    
    if(is.null(input$set)) {
      shiny:::flushReact()
      print("isnull bacon manual")
      return(numeric(0))
    }
    
    if (input$set == "set1") {
      a <- mvBACON(x=as.matrix(d), 
                   init.sel = "manual",
                   man.sel = set1.man,
                   alpha=input$alpha, 
                   maxsteps=input$animation)  
      a$subset <-  1 - a$subset
      a$subset[set1.man] <- 2
      a$subset
    } else if(input$set == "set2") {
      a <- mvBACON(x=as.matrix(d), 
                   init.sel = "manual",
                   man.sel = set2.man,
                   alpha=input$alpha, 
                   maxsteps=input$animation)  
      a$subset <-  1 - a$subset
      a$subset[set2.man] <- 2
      a$subset
    } else if(input$set == "set3") { 
      a <- mvBACON(x=as.matrix(d), 
                   init.sel = "manual",
                   man.sel = set3.man,
                   alpha=input$alpha, 
                   maxsteps=input$animation)  
      a$subset <- 1 - a$subset
      a$subset[set3.man] <- 2
      a$subset
    }
    else if(input$set == "set4") { 
      a <- mvBACON(x=as.matrix(d), 
                   init.sel = "manual",
                   man.sel = set4.man,
                   alpha=input$alpha, 
                   maxsteps=input$animation)  
      a$subset <- 1 - a$subset
      a$subset[set4.man] <- 2
      a$subset
    }
    else if(input$set == "set5") { 
      a <- mvBACON(x=as.matrix(d), 
                   init.sel = "manual",
                   man.sel = set5.man,
                   alpha=input$alpha, 
                   maxsteps=input$animation)  
      a$subset <- 1 - a$subset
      a$subset[set5.man] <- 2
      a$subset
    }
    else if(input$set == "set6") { 
      a <- mvBACON(x=as.matrix(d), 
                   init.sel = "manual",
                   man.sel = set6.man,
                   alpha=input$alpha, 
                   maxsteps=input$animation)  
      a$subset <- 1 - a$subset
      a$subset[set6.man] <- 2
      a$subset
    }
  }
  
}

bacon_steps <- function() {
  a <- mvBACON(x=as.matrix(d), m=input$size, collect = 1, alpha=input$alpha, maxsteps=500)
  a$steps
}

})

