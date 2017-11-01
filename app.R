library(shiny)
library(ggplot2)
library(stringr)
library(plyr)
library(devtools)
library(dplyr)
library(tidyr)
library(shinyjs)

appCSS <- "
#loading-content {
  position: absolute;
  background: #000009;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

ui <-  fluidPage(


  

  titlePanel(title=div(img(src="otago_logo.png"),"CRISPR-Cas Protospacer Distribution Analysis", style = "color: #000066")),
  
  
  
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(
    id = "loading-content",
    h2("Loading...")
  ),
  
  hidden(
    div(
      id = "app-content",
  ##side bar layout
  sidebarLayout(
    
    ##side bar panel
    sidebarPanel( 
                 tags$style(".container-fluid{
                            font-family: 'Arial Bold';
                            background-color: #FFCC33
                            }"),
      
                 tags$style(".row{
                            font-family: 'Arial';
                            background-color: #ffffff
                            }"),
                 
      
      
      selectInput("Subtype", "Choose Subtype", choices = list("I-A", "I-B", "I-C", "I-D", "I-E", "I-F", "II-A", "II-C", "III-A", "III-D"), selected = "I-F")
      ,     
      
      conditionalPanel(condition = "input.tabselected==6",

      sliderInput( inputId = "quadrantRange",
                   label = "Window to look within (in each direction)",
                   min = 0, 
                   max = 30000,
                   value = c(0,5000), 
                   step = 5000
      ),
      

      
      checkboxInput(inputId = "allQuadrandsPlotted", label = "Plot all the quadrants together", value = T),
      helpText("If this box is selected then each of the quadrandts will be shown, otherwise the quadrandt selected by the slider above will be used."),
      
 
 actionButton("QuadrantplotsUpdate", "Update Plot")
      
      
      
      ),     
 conditionalPanel(condition = "input.tabselected==7",
                  
                  sliderInput( inputId = "relativePositionslider",
                               label = "Protospacer numbers",
                               min = 2, 
                               max = 10,
                               value = c(2,3), 
                               step = 1
                  ),
                  sliderInput( inputId = "relativePlotRange",
                                 label = "Window to look within (in each direction)",
                                 min = 1000, 
                                 max = 100000,
                                 value = 10000, 
                                 step = 1000
                  ),
                  
                  
                  
                  
                  actionButton("relativeplotsUpdate", "Update Plot")
                  
                  
                  
 ), 
 conditionalPanel(condition = "input.tabselected==1",
                  
                  ##slider input
                  
                  
                  sliderInput( inputId = "binwidth",
                               label = "Nucleotides per bin",
                               min = 150, 
                               max = 3000,
                               value = 150, 
                               step = 50
                  ),
                  
                  sliderInput( inputId = "smoothing",
                               label = "Overlap of bins",
                               min = 0, 
                               max = 1000,
                               value = 150, 
                               step = 5
                  ),
                  
                  sliderInput( inputId = "range",
                               label = "Window to look within (in each direction)",
                               min = 1000, 
                               max = 100000,
                               value = 10000, 
                               step = 1000
                  ),
                  
                  helpText("Overly the density of the genomes at each position"),
                  checkboxInput(inputId = "keepGenomes", label = "Genome Density", value = F),
                  checkboxInput(inputId = "keepsecondhitonly", label = "Analyse only the second hit", value = F),
                  
                  actionButton("plotsUpdate", "Update Plot")
                  
                  
                  
 ),
      
      
      conditionalPanel(condition = "input.tabselected==3",
                       
                       ##slider input
                       
                       
                      
                       uiOutput("orderbyOutput"),
                       
                       helpText("Keep the Priming Protospacer values when analysing the data"),
                       checkboxInput(inputId = "keepPPS", label = "Priming Protospacer Data", value = T),
                       helpText("Plot a histogram based on the row selected"),
                       
                       actionButton("controller", "Plot Data")
      ),
 
 
 conditionalPanel(condition = "input.tabselected==5",

                  
                  
                  uiOutput(outputId = "binwidth2Slider"),
                  
                  helpText("Select data for plotting to check for any outliers/anomalies"),
                  actionButton("controller2", "Go to Data")
                  
                  
                  
                  
 ),
 
 
 conditionalPanel(condition = "input.tabselected==2",

                  helpText("Select the analysis that should be displayed"),
                  checkboxInput(inputId = "keepKsAll", label = "All Data", value = T),
                  checkboxInput(inputId = "keepKsTarget", label = "Target Strand Data", value = F),
                  checkboxInput(inputId = "keepKsNonTarget", label = "Non-Target Strand Data", value = F),
                  checkboxInput(inputId = "keepKsTarget5", label = "Target Strand 5' Data", value = F),
                  checkboxInput(inputId = "keepKsNonTarget5", label = "Non-Target 5' Strand Data", value = F),
                  checkboxInput(inputId = "keepKsTarget3", label = "Target Strand 3' Data", value = F),
                  checkboxInput(inputId = "keepKsNonTarget3", label = "Non-Target 3' Strand Data", value = F)
          
 )
 
 
 
 #,conditionalPanel(condition = "input.tabselected==2" )
 
  #,    conditionalPanel(condition = "input.tabselected==4" )
      
      
  ),
    
    mainPanel(
      
      uiOutput("datatype")
      
      
    )
    
    
  )
  
    )
  )
)


server <- function(input, output, session){
  # Simulate work being done for 1 second

  # Hide the loading message when the rest of the server function has executed

  
  observeEvent(input$controller, {
    updateTabsetPanel(session, "tabselected",
                      selected = "5"
    )
  })

  
  observeEvent(input$controller2, {
    updateTabsetPanel(session, "tabselected",
                      selected = "3"
    )
  })
  
  
  output$datatype <- renderUI({
    
      

      tabsetPanel(type = "tab", 
                  tabPanel("Standard Plot",value = 1, plotOutput(outputId = "standardPlot"), helpText("Click the download button to download this plot"), downloadButton(outputId = "downloadStandard", label = "Download"), radioButtons("filetype1","Select the file type", choices = list("png", "pdf"), selected = "pdf")), 
                  tabPanel("Quadrant Plots",value = 6, plotOutput("quadrantPlots")), 
                  tabPanel("Adjusted Distribution Plot", value = 1,  plotOutput(outputId = "adjustedPlot"), helpText("Click the download button to download this plot"), downloadButton(outputId = "downloadAdjusted", label = "Download"), radioButtons("filetype2","Select the file type", choices = list("png", "pdf"), selected = "pdf")), 
                  tabPanel("Relative Postition Plots",value = 7, plotOutput("relativePositions")), 
                  tabPanel("Statistics", value = 2, 
                           uiOutput("ksResultsAllHelpText"), uiOutput("ksResultsAll"), 
                           uiOutput("ksResultsTargetHelpText"), uiOutput("ksResultsTarget"), 
                           uiOutput("ksResultsNonTargetHelpText"), uiOutput("ksResultsNonTarget"), 
                           uiOutput("ksResultsTarget5HelpText"), uiOutput("ksResultsTarget5"), 
                           uiOutput("ksResultsNonTarget5HelpText"), uiOutput("ksResultsNonTarget5"), 
                           uiOutput("ksResultsTarget3HelpText"), uiOutput("ksResultsTarget3"), 
                           uiOutput("ksResultsNonTarget3HelpText"), uiOutput("ksResultsNonTarget3")),
                  tabPanel("Data",value = 3, tableOutput(outputId = "data")),
                  tabPanel("Data Histogram", value = 5, plotOutput(outputId = "dataPlot")),
                  tabPanel("Summary", value = 4, tableOutput(outputId = "summary")),
                  
                  id = "tabselected"
                  
      )
       
    
    
  })
  
  
    
  targets.dat <- reactive({
    x <- read.table("refseq_79.swipe.one_target_genome.no.arrays.in.targets", comment.char = "", fill = T, sep = "\t", header = T)
    x <- x%>%mutate(strand.plus.direction = paste(target.strand, five.three.prime.dir, sep = "_"))
    x <- x%>%mutate(legend.labels = ifelse(strand.plus.direction == "n_3", "Non-target 3' direction", ifelse(strand.plus.direction == "n_5", "Non-target 5' direction",ifelse(strand.plus.direction == "t_3", "Target 3' direction", "Target 5' direction"))))
    
    for(i in 1:20){
    x <- x%>%filter(protospacer.distance.num != i)
    x <- x%>%filter(protospacer.distance.num != -i)
    }
    x
    
  })
  
  columnNames <- reactive({
    colnames(targets.dat()%>%select(target.acc., Genome, target.start.num, host.target.pair, array.num, spacer.num, spacer_order.num, target.strand,five.three.prime.dir, protospacer.distance.num))
  })
  
  output$orderbyOutput <- renderUI({
    selectInput("orderby", "Sort Data by Column:", choices = columnNames())
    
  })
  
  
  output$ksResultsAll <- renderUI({
    if(input$keepKsAll ==T){
    verbatimTextOutput(outputId = "statisticalAnalysisAll")
}
  })
  output$ksResultsTarget <- renderUI({
    if(input$keepKsTarget ==T){
      
    verbatimTextOutput(outputId = "statisticalAnalysisTarget")
    }
  })
  output$ksResultsNonTarget <- renderUI({
    if(input$keepKsNonTarget ==T){
      
    verbatimTextOutput(outputId = "statisticalAnalysisNonTarget")
    }
  })
  output$ksResultsTarget5 <- renderUI({
    if(input$keepKsTarget5 ==T){
      
    verbatimTextOutput(outputId = "statisticalAnalysisTarget5")
    }
  })
  output$ksResultsNonTarget5 <- renderUI({
    if(input$keepKsNonTarget5 ==T){
      
    verbatimTextOutput(outputId = "statisticalAnalysisNonTarget5")
    }
  })
  output$ksResultsTarget3 <- renderUI({
    if(input$keepKsTarget3 ==T){
      
    verbatimTextOutput(outputId = "statisticalAnalysisTarget3")
    }
  })
  output$ksResultsNonTarget3 <- renderUI({
    if(input$keepKsNonTarget3 ==T){
      
    verbatimTextOutput(outputId = "statisticalAnalysisNonTarget3")
    }
  })
  
  
  
  
  output$ksResultsAllHelpText <- renderUI({
    if(input$keepKsAll ==T){
      helpText("KS test results for all the data in the window")    }
  })
  output$ksResultsTargetHelpText <- renderUI({
    if(input$keepKsTarget ==T){
      
      helpText("KS test results for the target strand data in the window")    }
  })
  output$ksResultsNonTargetHelpText <- renderUI({
    if(input$keepKsNonTarget ==T){
      
      helpText("KS test results for the non-target strand data in the window")    }
  })
  output$ksResultsTarget5HelpText <- renderUI({
    if(input$keepKsTarget5 ==T){
      
      helpText("KS test results for the target strand 5' data in the window")    }
  })
  output$ksResultsNonTarget5HelpText <- renderUI({
    if(input$keepKsNonTarget5 ==T){
      
      helpText("KS test results for the non-target strand 5' data in the window")    }
  })
  output$ksResultsTarget3HelpText <- renderUI({
    if(input$keepKsTarget3 ==T){
      
      helpText("KS test results for the target strand 3' data in the window")    }
  })
  output$ksResultsNonTarget3HelpText <- renderUI({
    if(input$keepKsNonTarget3 ==T){
      
      helpText("KS test results for the non-target strand 3' data in the window")    }
  })
  

  
  

  rh <- reactive({
    set.seed(100)
    targets.dat.replicated <- targets.dat()[rep(seq_len(nrow(targets.dat())), 100), ]
    #targets.dat.replicated%>%targets.dat.replicated%>%filter(spacer.order_num > 1)
    rh <- targets.dat.replicated%>%
      mutate(protospacer.distance.num = runif(min = -0.5, max = 0.5, n = nrow(targets.dat.replicated)))%>%
      mutate(protospacer.distance.num = round(protospacer.distance.num*genome.length.num, 0))%>%
      mutate(protospacer.distance.num = ifelse(spacer_order.num == 1, 0, protospacer.distance.num))%>%
      mutate(target.strand = round(runif(min = 0, max = 1, n = nrow(targets.dat.replicated)), 0))%>%
      mutate(target.strand = ifelse(target.strand == 1, "t", "n"))%>%
      mutate(five.three.prime.dir = ifelse(protospacer.distance.num < 0, ifelse(target.strand == "t", 5, 3), ifelse(target.strand == "t", 3, 5)))%>%
      mutate(strand.plus.direction = paste(target.strand, five.three.prime.dir, sep = "_"))%>%
      mutate(target.start.num = runif(min = 0, max = 1, n = nrow(targets.dat.replicated)))%>%
      mutate(target.start.num = round(target.start.num*genome.length.num, 0))
    rm(targets.dat.replicated)
    
    rh <- rh%>%arrange(unique.spacer.target.match)%>%mutate(group.number = rep(1:10, 10*nrow(targets.dat())))
    rh
    
  })
    
  Subtype.label <- reactive({
    input$Subtype
  })
  
  
  binwidth <- reactive({
    input$binwidth
  })
  
  
  smoothing.val <- reactive({
    
    
    if(input$smoothing == 0){
      x <- 1
    }else{
    x <- input$smoothing
    }
    x
  })
  
  xlim.num <- reactive({
    input$range
  })
  
  
  sr <- reactive({
    
    if(input$keepsecondhitonly == T){
      x <- rh()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num == 2)
      
    }else{
      x <- rh()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num > 1)
      
    }
    

    x <- x%>%mutate(data.type = paste("random", group.number, sep = "_"))%>%
      mutate(data.type = ifelse(data.type == "random_1", "random_01", data.type))
    
    
    x%>%select(protospacer.distance.num, data.type, strand.plus.direction)
    
    
  })
  
  
  st <- reactive({
    
    if(input$keepsecondhitonly == T){
      x <- targets.dat()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num == 2)
      
    }else{
      x <- targets.dat()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num > 1)
      
    } 
    
    
    
    x <- x%>%mutate(data.type = "targets")%>%
      mutate(strand = ifelse(strand.plus.direction == "n_5", "n", ifelse(strand.plus.direction == "n_3", "n", "t")))%>%
      mutate(protospacer.distance.num = ifelse(strand == "n", -protospacer.distance.num, protospacer.distance.num))%>%
      select(-strand)
    
    x%>%select(protospacer.distance.num, data.type, strand.plus.direction)
    
    
  })
  
  
  den <- reactive({
    den <- rbind(sr(), st())
    
    
    D = rbind(sr(), st()) 

    n_3.den <- D%>%filter(grepl("n_", strand.plus.direction))  %>% 
      group_by(data.type) %>% 
      # calculate densities for each group over same range; store in list column
      summarise(d = list(density(protospacer.distance.num, from = -xlim.num(), to = 0, n = xlim.num()/binwidth(), bw = smoothing.val()))) %>% 
      # make a new data.frame from two density objects
      do(data.frame(distance.breaks.short = .$d[[1]]$x,    # grab one set of x values (which are the same)
                    density.values.random_1 = .$d[[1]]$y,
                    density.values.random_10 = .$d[[2]]$y,
                    density.values.random_2 = .$d[[3]]$y,
                    density.values.random_3 = .$d[[4]]$y,
                    density.values.random_4 = .$d[[5]]$y,
                    density.values.random_5 = .$d[[6]]$y,
                    density.values.random_6 = .$d[[7]]$y,
                    density.values.random_7 = .$d[[8]]$y,
                    density.values.random_8 = .$d[[9]]$y,
                    density.values.random_9 = .$d[[10]]$y,
                    density.values.targets = .$d[[11]]$y))# %>%    # and subtract the y values
    
    n_5.den <- D%>%filter(grepl("n_", strand.plus.direction))%>% 
      group_by(data.type) %>% 
      # calculate densities for each group over same range; store in list column
      summarise(d = list(density(protospacer.distance.num, from = 0, to = xlim.num(), n = xlim.num()/binwidth(), bw = smoothing.val()))) %>% 
      # make a new data.frame from two density objects
      do(data.frame(distance.breaks.short = .$d[[1]]$x,    # grab one set of x values (which are the same)
                    density.values.random_1 = .$d[[1]]$y,
                    density.values.random_10 = .$d[[2]]$y,
                    density.values.random_2 = .$d[[3]]$y,
                    density.values.random_3 = .$d[[4]]$y,
                    density.values.random_4 = .$d[[5]]$y,
                    density.values.random_5 = .$d[[6]]$y,
                    density.values.random_6 = .$d[[7]]$y,
                    density.values.random_7 = .$d[[8]]$y,
                    density.values.random_8 = .$d[[9]]$y,
                    density.values.random_9 = .$d[[10]]$y,
                    density.values.targets = .$d[[11]]$y))# %>%    # and subtract the y values
    t_3.den <- D%>%filter(grepl("t_", strand.plus.direction))%>% 
      group_by(data.type) %>% 
      # calculate densities for each group over same range; store in list column
      summarise(d = list(density(protospacer.distance.num, from = 0, to = xlim.num(), n = xlim.num()/binwidth(), bw = smoothing.val()))) %>% 
      # make a new data.frame from two density objects
      do(data.frame(distance.breaks.short = .$d[[1]]$x,    # grab one set of x values (which are the same)
                    density.values.random_1 = .$d[[1]]$y,
                    density.values.random_10 = .$d[[2]]$y,
                    density.values.random_2 = .$d[[3]]$y,
                    density.values.random_3 = .$d[[4]]$y,
                    density.values.random_4 = .$d[[5]]$y,
                    density.values.random_5 = .$d[[6]]$y,
                    density.values.random_6 = .$d[[7]]$y,
                    density.values.random_7 = .$d[[8]]$y,
                    density.values.random_8 = .$d[[9]]$y,
                    density.values.random_9 = .$d[[10]]$y,
                    density.values.targets = .$d[[11]]$y))# %>%    # and subtract the y values
    
    
    t_5.den <- D%>%filter(grepl("t_", strand.plus.direction))%>% 
      group_by(data.type) %>% 
      # calculate densities for each group over same range; store in list column
      summarise(d = list(density(protospacer.distance.num, from = -xlim.num(), to = 0, n = xlim.num()/binwidth(), bw = smoothing.val()))) %>% 
      # make a new data.frame from two density objects
      do(data.frame(distance.breaks.short = .$d[[1]]$x,    # grab one set of x values (which are the same)
                    density.values.random_1 = .$d[[1]]$y,
                    density.values.random_10 = .$d[[2]]$y,
                    density.values.random_2 = .$d[[3]]$y,
                    density.values.random_3 = .$d[[4]]$y,
                    density.values.random_4 = .$d[[5]]$y,
                    density.values.random_5 = .$d[[6]]$y,
                    density.values.random_6 = .$d[[7]]$y,
                    density.values.random_7 = .$d[[8]]$y,
                    density.values.random_8 = .$d[[9]]$y,
                    density.values.random_9 = .$d[[10]]$y,
                    density.values.targets = .$d[[11]]$y))# %>%    # and subtract the y values
    
    n_3.den <- n_3.den%>%mutate(strand.plus.direction = "n_3")
    n_5.den <- n_5.den%>%mutate(strand.plus.direction = "n_5")
    t_3.den <- t_3.den%>%mutate(strand.plus.direction = "t_3")
    t_5.den <- t_5.den%>%mutate(strand.plus.direction = "t_5")
    
    #  colnames(n_3.den) <- colnames(n_3.den)
    #  colnames(n_5.den) <- colnames(n_3.den)
    #  colnames(t_3.den) <- colnames(n_3.den)
    #  colnames(t_5.den) <- colnames(n_3.den)
    den <- rbind(n_3.den, n_5.den, t_3.den, t_5.den)
    #  den <- rbind(n_5.den, t_5.den)
    
    
    rDen1 <- den%>%mutate(density.values = density.values.random_1)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_1")%>%mutate(group.main =  "random")
    
    rDen2 <- den%>%mutate(density.values = density.values.random_2)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_2")%>%mutate(group.main =  "random")
    
    rDen3 <- den%>%mutate(density.values = density.values.random_3)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_3")%>%mutate(group.main =  "random")
    
    rDen4 <- den%>%mutate(density.values = density.values.random_4)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_4")%>%mutate(group.main =  "random")
    
    rDen5 <- den%>%mutate(density.values = density.values.random_5)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_5")%>%mutate(group.main =  "random")
    
    rDen6 <- den%>%mutate(density.values = density.values.random_6)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_6")%>%mutate(group.main =  "random")
    
    rDen7 <- den%>%mutate(density.values = density.values.random_7)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_7")%>%mutate(group.main =  "random")
    
    rDen8 <- den%>%mutate(density.values = density.values.random_8)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_8")%>%mutate(group.main =  "random")
    
    rDen9 <- den%>%mutate(density.values = density.values.random_9)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_9")%>%mutate(group.main =  "random")
    
    rDen10 <- den%>%mutate(density.values = density.values.random_10)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "random_10")%>%mutate(group.main =  "random")
    
    
    tDen <- den%>%mutate(density.values = density.values.targets)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group =  "targets")%>%mutate(group.main =  "targets")
    
    den <- rbind(rDen1,rDen2,rDen3,rDen4,rDen5,rDen6,rDen7,rDen8,rDen9,rDen10, tDen)
    
    den <- den%>%arrange(distance.breaks.short)
    
    
    sdrandomDensity <- den%>%filter(group.main == "random")%>%group_by(distance.breaks.short, strand.plus.direction)%>%summarise(sdDensity = sd(density.values))%>%mutate(breaksStrandDirection = paste(distance.breaks.short, strand.plus.direction, sep = "$"))%>%ungroup()%>%select(-strand.plus.direction, - distance.breaks.short)
    meanrandomDensity <- den%>%filter(group.main == "random")%>%group_by(distance.breaks.short, strand.plus.direction)%>%summarise(meanDensity = mean(density.values))%>%mutate(breaksStrandDirection = paste(distance.breaks.short, strand.plus.direction, sep = "$"))
    randomDensity <- left_join(sdrandomDensity, meanrandomDensity, by = "breaksStrandDirection")
    
    randomDensity <- randomDensity%>%mutate(upperRandomDensity = meanDensity + 2*sdDensity)%>%mutate(lowerRandomDensity = meanDensity - 2*sdDensity)
    targetDensity <- den%>%filter(group == "targets")
    
    upperRandomDensityValues <- randomDensity%>%mutate(density.values = upperRandomDensity)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group = "upperRandomDensity")%>%mutate(group.main = "random")
    lowerRandomDensityValues <- randomDensity%>%mutate(density.values = lowerRandomDensity)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group = "lowerRandomDensity")%>%mutate(group.main = "random")
    meanRandomDensityValues <- randomDensity%>%mutate(density.values = meanDensity)%>%select(distance.breaks.short, density.values, strand.plus.direction)%>%mutate(group = "meanDensity")%>%mutate(group.main = "random")        
    
    rbind(targetDensity, upperRandomDensityValues, lowerRandomDensityValues, meanRandomDensityValues)
  })
  
  
  
  randomQuadrantRange <- reactive({
    if(input$keepsecondhitonly == T){
      random.hits <- rh()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num == 2)
      
    }else{
      random.hits <- rh()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num > 1)
      
    }
    
    
    if(input$allQuadrandsPlotted == T){
      
      random.hits <- random.hits%>%filter(spacer_order.num != 1)%>%filter(Subtype == Subtype.label())%>%
        filter(protospacer.distance.num > -30000)%>%
        filter(protospacer.distance.num < 30000)
      
      
      
      ##30000
      
      rt530 <- random.hits%>%
        filter(protospacer.distance.num > -30000)%>%
        filter(protospacer.distance.num < -10000)%>%filter(target.strand == "t")
      
      
      rt330 <- random.hits%>%
        filter(protospacer.distance.num > 10000)%>%
        filter(protospacer.distance.num < 30000)%>%filter(target.strand == "t")
      
      rn530 <- random.hits%>%
        filter(protospacer.distance.num > 10000)%>%
        filter(protospacer.distance.num < 30000)%>%filter(target.strand == "n")
      
      rn330 <- random.hits%>%
        filter(protospacer.distance.num > -30000)%>%
        filter(protospacer.distance.num < -10000)%>%filter(target.strand == "n")
      
      
      
      
      
      
      countrt530 <- rt530%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanRT530 <- mean(countrt530$numberOfOccurances)
      lowerRT530 <- meanRT530 - sd(countrt530$numberOfOccurances)*2
      upperRT530 <- meanRT530 + sd(countrt530$numberOfOccurances)*2
      
      
      
      countrn530 <- rn530%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanNRT530 <- mean(countrn530$numberOfOccurances)
      lowerNRT530 <- meanNRT530 - sd(countrn530$numberOfOccurances)*2
      upperNRT530 <- meanNRT530 + sd(countrn530$numberOfOccurances)*2
      
      
      countrt330 <- rt330%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanRT330 <- mean(countrt330$numberOfOccurances)
      lowerRT330 <- meanRT330 - sd(countrt330$numberOfOccurances)*2
      upperRT330 <- meanRT330 + sd(countrt330$numberOfOccurances)*2
      
      
      countrn330 <- rn330%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanNRT330 <- mean(countrn330$numberOfOccurances)
      lowerNRT330 <- meanNRT330 - sd(countrn330$numberOfOccurances)*2
      upperNRT330 <- meanNRT330 + sd(countrn330$numberOfOccurances)*2
      
      
      
      ##10000
      
      rt510 <- random.hits%>%
        filter(protospacer.distance.num > -10000)%>%
        filter(protospacer.distance.num < -5000)%>%filter(target.strand == "t")
      
      
      rt310 <- random.hits%>%
        filter(protospacer.distance.num > 5000)%>%
        filter(protospacer.distance.num < 10000)%>%filter(target.strand == "t")
      
      rn510 <- random.hits%>%
        filter(protospacer.distance.num > 5000)%>%
        filter(protospacer.distance.num < 10000)%>%filter(target.strand == "n")
      
      rn310 <- random.hits%>%
        filter(protospacer.distance.num > -10000)%>%
        filter(protospacer.distance.num < -5000)%>%filter(target.strand == "n")
      
      
      
      
      
      
      countrt510 <- rt510%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanRT510 <- mean(countrt510$numberOfOccurances)
      lowerRT510 <- meanRT510 - sd(countrt510$numberOfOccurances)*2
      upperRT510 <- meanRT510 + sd(countrt510$numberOfOccurances)*2
      
      
      
      countrn510 <- rn510%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanNRT510 <- mean(countrn510$numberOfOccurances)
      lowerNRT510 <- meanNRT510 - sd(countrn510$numberOfOccurances)*2
      upperNRT510 <- meanNRT510 + sd(countrn510$numberOfOccurances)*2
      
      
      countrt310 <- rt310%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanRT310 <- mean(countrt310$numberOfOccurances)
      lowerRT310 <- meanRT310 - sd(countrt310$numberOfOccurances)*2
      upperRT310 <- meanRT310 + sd(countrt310$numberOfOccurances)*2
      
      
      countrn310 <- rn310%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanNRT310 <- mean(countrn310$numberOfOccurances)
      lowerNRT310 <- meanNRT310 - sd(countrn310$numberOfOccurances)*2
      upperNRT310 <- meanNRT310 + sd(countrn310$numberOfOccurances)*2
      
      
      
      ##5000
      
      rt55 <- random.hits%>%
        filter(protospacer.distance.num > -5000)%>%
        filter(protospacer.distance.num < -0)%>%filter(target.strand == "t")
      
      
      rt35 <- random.hits%>%
        filter(protospacer.distance.num > 0)%>%
        filter(protospacer.distance.num < 5000)%>%filter(target.strand == "t")
      
      rn55 <- random.hits%>%
        filter(protospacer.distance.num > 0)%>%
        filter(protospacer.distance.num < 5000)%>%filter(target.strand == "n")
      
      rn35 <- random.hits%>%
        filter(protospacer.distance.num > -5000)%>%
        filter(protospacer.distance.num < -0)%>%filter(target.strand == "n")
      
      
      
      
      
      
      countrt55 <- rt55%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanRT55 <- mean(countrt55$numberOfOccurances)
      lowerRT55 <- meanRT55 - sd(countrt55$numberOfOccurances)*2
      upperRT55 <- meanRT55 + sd(countrt55$numberOfOccurances)*2
      
      
      
      countrn55 <- rn55%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanNRT55 <- mean(countrn55$numberOfOccurances)
      lowerNRT55 <- meanNRT55 - sd(countrn55$numberOfOccurances)*2
      upperNRT55 <- meanNRT55 + sd(countrn55$numberOfOccurances)*2
      
      
      countrt35 <- rt35%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanRT35 <- mean(countrt35$numberOfOccurances)
      lowerRT35 <- meanRT35 - sd(countrt35$numberOfOccurances)*2
      upperRT35 <- meanRT35 + sd(countrt35$numberOfOccurances)*2
      
      
      countrn35 <- rn35%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
      meanNRT35 <- mean(countrn35$numberOfOccurances)
      lowerNRT35 <- meanNRT35 - sd(countrn35$numberOfOccurances)*2
      upperNRT35 <- meanNRT35 + sd(countrn35$numberOfOccurances)*2
      
      
      
      
      
      quadrantTable  <- data.frame(y = c(meanRT530/6, meanRT530/6, meanRT510, meanRT510, meanRT55, meanRT55, meanRT35, meanRT35, meanRT310, meanRT310, meanRT330/6, meanRT330/6,
                                         lowerRT530/6, lowerRT530/6, lowerRT510, lowerRT510, lowerRT55, lowerRT55, lowerRT35, lowerRT35, lowerRT310, lowerRT310, lowerRT330/6, lowerRT330/6,
                                         upperRT530/6, upperRT530/6, upperRT510, upperRT510, upperRT55, upperRT55, upperRT35, upperRT35, upperRT310, upperRT310, upperRT330/6, upperRT330/6,
                                         meanNRT530/6, meanNRT530/6, meanNRT510, meanNRT510, meanNRT55, meanNRT55, meanNRT35, meanNRT35, meanNRT310, meanNRT310, meanNRT330/6, meanNRT330/6,
                                         lowerNRT530/6, lowerNRT530/6, lowerNRT510, lowerNRT510, lowerNRT55, lowerNRT55, lowerNRT35, lowerNRT35, lowerNRT310, lowerNRT310, lowerNRT330/6, lowerNRT330/6,
                                         upperNRT530/6, upperNRT530/6, upperNRT510, upperNRT510, upperNRT55, upperNRT55, upperNRT35, upperNRT35, upperNRT310, upperNRT310, upperNRT330/6, upperNRT330/6),
                                   x = c(-30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000,
                                         -30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000,
                                         -30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000,
                                         -30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000,
                                         -30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000,
                                         -30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000),
                                   group = c("targets_random_mean", "targets_random_mean", "targets_random_mean", "targets_random_mean", 
                                             "targets_random_mean", "targets_random_mean", "targets_random_mean", "targets_random_mean",
                                             "targets_random_mean", "targets_random_mean", "targets_random_mean", "targets_random_mean",
                                             "targets_random_lower", "targets_random_lower", "targets_random_lower", "targets_random_lower", 
                                             "targets_random_lower", "targets_random_lower", "targets_random_lower", "targets_random_lower",
                                             "targets_random_lower", "targets_random_lower", "targets_random_lower", "targets_random_lower",
                                             "targets_random_upper", "targets_random_upper", "targets_random_upper", "targets_random_upper", 
                                             "targets_random_upper", "targets_random_upper", "targets_random_upper", "targets_random_upper",
                                             "targets_random_upper", "targets_random_upper", "targets_random_upper", "targets_random_upper",
                                             "nontargets_random_mean", "nontargets_random_mean", "nontargets_random_mean", "nontargets_random_mean", 
                                             "nontargets_random_mean", "nontargets_random_mean", "nontargets_random_mean", "nontargets_random_mean",
                                             "nontargets_random_mean", "nontargets_random_mean", "nontargets_random_mean", "nontargets_random_mean",
                                             "nontargets_random_lower", "nontargets_random_lower", "nontargets_random_lower", "nontargets_random_lower", 
                                             "nontargets_random_lower", "nontargets_random_lower", "nontargets_random_lower", "nontargets_random_lower",
                                             "nontargets_random_lower", "nontargets_random_lower", "nontargets_random_lower", "nontargets_random_lower",
                                             "nontargets_random_upper", "nontargets_random_upper", "nontargets_random_upper", "nontargets_random_upper", 
                                             "nontargets_random_upper", "nontargets_random_upper", "nontargets_random_upper", "nontargets_random_upper",
                                             "nontargets_random_upper", "nontargets_random_upper", "nontargets_random_upper", "nontargets_random_upper"))
      
      
      
      
    }else{
      
    random.hits <- random.hits%>%filter(spacer_order.num != 1)%>%filter(Subtype == Subtype.label())%>%
      filter(protospacer.distance.num > -input$quadrantRange[2])%>%
      filter(protospacer.distance.num < input$quadrantRange[2])
    
    randomtarget5PrimeQuadrant <- random.hits%>%
      filter(protospacer.distance.num > -input$quadrantRange[2])%>%
      filter(protospacer.distance.num < -input$quadrantRange[1])%>%filter(target.strand == "t")
    
    
    randomtarget3PrimeQuadrant <- random.hits%>%
      filter(protospacer.distance.num > input$quadrantRange[1])%>%
      filter(protospacer.distance.num < input$quadrantRange[2])%>%filter(target.strand == "t")
    
    randomnonTarget5PrimeQuadrant <- random.hits%>%
      filter(protospacer.distance.num > input$quadrantRange[1])%>%
      filter(protospacer.distance.num < input$quadrantRange[2])%>%filter(target.strand == "n")
    
    randomnonTarget3PrimeQuadrant <- random.hits%>%
      filter(protospacer.distance.num > -input$quadrantRange[2])%>%
      filter(protospacer.distance.num < -input$quadrantRange[1])%>%filter(target.strand == "n")
    
    
    
    
    
    
    numberOfHitsRT5 <- randomtarget5PrimeQuadrant%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
    meanRT5 <- mean(numberOfHitsRT5$numberOfOccurances)
    lowerRT5 <- meanRT5 - sd(numberOfHitsRT5$numberOfOccurances)*2
    upperRT5 <- meanRT5 + sd(numberOfHitsRT5$numberOfOccurances)*2
    
    
    
    numberOfHitsNRT5 <- randomnonTarget5PrimeQuadrant%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
    meanNRT5 <- mean(numberOfHitsNRT5$numberOfOccurances)
    lowerNRT5 <- meanNRT5 - sd(numberOfHitsNRT5$numberOfOccurances)*2
    upperNRT5 <- meanNRT5 + sd(numberOfHitsNRT5$numberOfOccurances)*2
    
    
    numberOfHitsRT3 <- randomtarget3PrimeQuadrant%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
    meanRT3 <- mean(numberOfHitsRT3$numberOfOccurances)
    lowerRT3 <- meanRT3 - sd(numberOfHitsRT3$numberOfOccurances)*2
    upperRT3 <- meanRT3 + sd(numberOfHitsRT3$numberOfOccurances)*2
    
    
    numberOfHitsNRT3 <- randomnonTarget3PrimeQuadrant%>%group_by(group.number)%>%summarise(numberOfOccurances = n())
    meanNRT3 <- mean(numberOfHitsNRT3$numberOfOccurances)
    lowerNRT3 <- meanNRT3 - sd(numberOfHitsNRT3$numberOfOccurances)*2
    upperNRT3 <- meanNRT3 + sd(numberOfHitsNRT3$numberOfOccurances)*2
    
    
    
    
    
    total_density <-     meanRT5 + meanNRT5 + meanRT3 + meanNRT3

    
    
    
    meanRT5 <- mean(numberOfHitsRT5$numberOfOccurances)/total_density
    lowerRT5 <- meanRT5 - sd(numberOfHitsRT5$numberOfOccurances)*2/total_density
    upperRT5 <- meanRT5 + sd(numberOfHitsRT5$numberOfOccurances)*2/total_density
    meanNRT5 <- mean(numberOfHitsNRT5$numberOfOccurances)/total_density
    lowerNRT5 <- meanNRT5 - sd(numberOfHitsNRT5$numberOfOccurances)*2/total_density
    upperNRT5 <- meanNRT5 + sd(numberOfHitsNRT5$numberOfOccurances)*2/total_density
    meanRT3 <- mean(numberOfHitsRT3$numberOfOccurances)/total_density
    lowerRT3 <- meanRT3 - sd(numberOfHitsRT3$numberOfOccurances)*2/total_density
    upperRT3 <- meanRT3 + sd(numberOfHitsRT3$numberOfOccurances)*2/total_density
    meanNRT3 <- mean(numberOfHitsNRT3$numberOfOccurances)/total_density
    lowerNRT3 <- meanNRT3 - sd(numberOfHitsNRT3$numberOfOccurances)*2/total_density
    upperNRT3 <- meanNRT3 + sd(numberOfHitsNRT3$numberOfOccurances)*2/total_density
    
    quadrantTable  <- data.frame(quadrant = c("Target 5'", "Target 3'", "Non Target 5'", "Non Target 3'"),
               meanValue = c(meanRT5, meanRT3, -meanNRT5, -meanNRT3), 
               lowerValue = c(lowerRT5, lowerRT3, -lowerNRT5, -lowerNRT3),
               upperValue = c(upperRT5, upperRT3, -upperNRT5, -upperNRT3))
    }
    quadrantTable  
  })
  
  targetQuadrantTable <- reactive({
    if(input$keepsecondhitonly == T){
      dat <- targets.dat()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num == 2)
      
    }else{
      dat <- targets.dat()%>%filter(Subtype == Subtype.label())%>%filter(spacer_order.num > 1)
      
    } 
    
    
    if(input$allQuadrandsPlotted == T){
      
      dat <- dat%>%filter(spacer_order.num != 1)%>%filter(Subtype == Subtype.label())%>%
        filter(protospacer.distance.num > -30000)%>%
        filter(protospacer.distance.num < 30000)
      
      
      t530 <- nrow(dat%>%
                                     filter(protospacer.distance.num > -30000)%>%
                                     filter(protospacer.distance.num < -10000)%>%
                                     filter(target.strand == "t")
      )
      t330 <- nrow(dat%>%
                                     filter(protospacer.distance.num > 10000)%>%
                                     filter(protospacer.distance.num < 30000) %>%filter(target.strand == "t")
      )
      n530 <- -nrow(dat%>%
                                         filter(protospacer.distance.num > 10000)%>%
                                         filter(protospacer.distance.num < 30000)%>%filter(target.strand == "n")
      )
      n330 <- -nrow(dat%>%
                                         filter(protospacer.distance.num > -30000)%>%
                                         filter(protospacer.distance.num < -10000) %>%filter(target.strand == "n")
      )
      
      
      
      t510 <- nrow(dat%>%
                     filter(protospacer.distance.num > -10000)%>%
                     filter(protospacer.distance.num < -5000)%>%
                     filter(target.strand == "t")
      )
      t310 <- nrow(dat%>%
                     filter(protospacer.distance.num > 5000)%>%
                     filter(protospacer.distance.num < 10000) %>%filter(target.strand == "t")
      )
      n510 <- -nrow(dat%>%
                      filter(protospacer.distance.num > 5000)%>%
                      filter(protospacer.distance.num < 10000)%>%filter(target.strand == "n")
      )
      n310 <- -nrow(dat%>%
                      filter(protospacer.distance.num > -10000)%>%
                      filter(protospacer.distance.num < -5000) %>%filter(target.strand == "n")
      )
      
      
      t55 <- nrow(dat%>%
                     filter(protospacer.distance.num > -5000)%>%
                     filter(protospacer.distance.num < -0)%>%
                     filter(target.strand == "t")
      )
      t35 <- nrow(dat%>%
                     filter(protospacer.distance.num > 0)%>%
                     filter(protospacer.distance.num < 5000) %>%filter(target.strand == "t")
      )
      n55 <- -nrow(dat%>%
                      filter(protospacer.distance.num > 0)%>%
                      filter(protospacer.distance.num < 5000)%>%filter(target.strand == "n")
      )
      n35 <- -nrow(dat%>%
                      filter(protospacer.distance.num > -5000)%>%
                      filter(protospacer.distance.num < -0) %>%filter(target.strand == "n")
      )
      
      
      
      quadrantTable  <- data.frame(y = c(t530/6, t530/6, t510, t510, t55, t55, t35, t35, t310, t310, t330/6, t330/6,
                       n330/6, n330/6, n310, n310, n35, n35, n55, n55, n510, n510, n530/6, n530/6),
                 x = c(-30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000,
                        -30000, -10000, -10000, -5000, -5000, 0, 0, 5000, 5000, 10000, 10000, 30000),
                 group = c("targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual", "targets_actual",
                           "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual", "nontargets_actual"))
      
      

    }else{
    
    dat <- dat%>%filter(spacer_order.num != 1)%>%filter(Subtype == Subtype.label())%>%
      filter(protospacer.distance.num > -input$quadrantRange[2])%>%
      filter(protospacer.distance.num < input$quadrantRange[2])
    
    
    target5PrimeQuadrant <- nrow(dat%>%
                                   filter(protospacer.distance.num > -input$quadrantRange[2])%>%
                                   filter(protospacer.distance.num < -input$quadrantRange[1])%>%
                                   filter(target.strand == "t")
    )
    target3PrimeQuadrant <- nrow(dat%>%
                                   filter(protospacer.distance.num > input$quadrantRange[1])%>%
                                   filter(protospacer.distance.num < input$quadrantRange[2]) %>%filter(target.strand == "t")
    )
    nonTarget5PrimeQuadrant <- -nrow(dat%>%
                                       filter(protospacer.distance.num > input$quadrantRange[1])%>%
                                       filter(protospacer.distance.num < input$quadrantRange[2])%>%filter(target.strand == "n")
    )
    nonTarget3PrimeQuadrant <- -nrow(dat%>%
                                       filter(protospacer.distance.num > -input$quadrantRange[2])%>%
                                       filter(protospacer.distance.num < -input$quadrantRange[1]) %>%filter(target.strand == "n")
    )
    
    
    
    
    total_density <- abs(target5PrimeQuadrant) + abs(target3PrimeQuadrant) + abs(nonTarget5PrimeQuadrant) + abs(nonTarget3PrimeQuadrant)
    
    target5PrimeQuadrant <- target5PrimeQuadrant/total_density
    target3PrimeQuadrant <- target3PrimeQuadrant/total_density
    nonTarget5PrimeQuadrant <- nonTarget5PrimeQuadrant/total_density
    nonTarget3PrimeQuadrant <- nonTarget3PrimeQuadrant/total_density
    
    quadrantTable <- data.frame(plotPosition = c(-10, -5,10,5,10,5,-10,-5), 
                                quadrantData = c(0,target5PrimeQuadrant,
                                                 0,nonTarget5PrimeQuadrant,
                                                 0, target3PrimeQuadrant,
                                                 0,nonTarget3PrimeQuadrant),
                                strand = c("Target","Target", 
                                           "Non-Target","Non-Target",
                                           "Target","Target", 
                                           "Non-Target","Non-Target"))
    
    }
    quadrantTable
  })
  
  output$binwidth2Slider <- renderUI({
    
   
    x <- targets.dat()%>%
      filter(Subtype == input$Subtype)%>%
      filter(protospacer.distance.num > -input$range)%>%
      filter(protospacer.distance.num < input$range)%>%
      #filter(spacer_order.num != 0)%>%
      select(target.acc., Genome, target.start.num, host.target.pair, array.num, spacer.num, spacer_order.num, target.strand,five.three.prime.dir, protospacer.distance.num)
    
    if(input$keepPPS == F){
      x <- x%>%filter(spacer_order.num != 1)
    }
    
    colm <- input$orderby
    colm <- which(colnames(x) == colm)
    y <- ifelse(is.numeric(x[,colm]), T, ifelse(is.integer(x[,colm]), T, F))
    
    if(y == T){ 
    max_binwidth = max(x[,colm])
    
    sliderInput( inputId = "binwidth2",
                 label = "Size of the bins",
                 min = ifelse(max_binwidth < 100, 1, ifelse(max_binwidth < 1000, round(max_binwidth/100), round(max_binwidth/1000))), 
                 max = max_binwidth,
                 value = max_binwidth/10, 
                 step = ifelse(max_binwidth < 100, 1, ifelse(max_binwidth < 1000, round(max_binwidth/100), round(max_binwidth/1000)))

    )
    }else{
      helpText("")
    }
  })
  

  GenomeEndDensities <- reactive({
    
    
    sdNumbers <- 2
    
    datGenome <- targets.dat()%>%filter(Subtype == Subtype.label())%>%mutate(ppsToGenomeEnds = genome.length.num - target.start.num)
    pps.datGenome <- datGenome%>%filter(spacer_order.num == 1)
    
    DGenome <- pps.datGenome%>%select(target.start.num, ppsToGenomeEnds)%>%mutate(data.type = "genome lengths")
    
    
    GenomeStart <- DGenome %>% group_by(data.type) %>% 
      # calculate densities for each group over same range; store in list column
      summarise(d = list(density(target.start.num, from = min(.$target.start.num), to = max(.$target.start.num), n = xlim.num()/binwidth()*2))) %>% 
      # make a new data.frame from two density objects
      do(data.frame(distance.breaks.short = .$d[[1]]$x,    # grab one set of x values (which are the same)
                    density.values = .$d[[1]]$y))# %>%    # and subtract the y values
    GenomeStart <- GenomeStart%>%mutate(distance.breaks.short = -distance.breaks.short)
    
    GenomeEnd <- DGenome %>% group_by(data.type) %>% 
      # calculate densities for each group over same range; store in list column
      summarise(d = list(density(ppsToGenomeEnds, from = min(.$ppsToGenomeEnds), to = max(.$ppsToGenomeEnds), n = xlim.num()/binwidth()*2))) %>% 
      # make a new data.frame from two density objects
      do(data.frame(distance.breaks.short = .$d[[1]]$x,    # grab one set of x values (which are the same)
                    density.values = .$d[[1]]$y))# %>%    # and subtract the y values
    
    GenomeEndDensities <- rbind(GenomeStart, GenomeEnd)
    
    
    
    GenomeEndDensities%>%arrange(distance.breaks.short)
    
    
    
  })
  

  
  
  output$standardPlot <- renderPlot({
    
    
    input$plotsUpdate
    
    isolate({
    dat = targets.dat()
    random.hits = rh()
    den <- den()
    
    
    plot.title1 <- paste("Subtype",Subtype.label())
    plot.subtitle <- paste("Number of hits:", nrow(st()))
    
    
    #den <- den%>%mutate(density.values = as.numeric(density.values)*nrow(st()))
    
    graph.height <- max(den$density.values)
    
    
    #GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = as.numeric(density.values)*nrow(st))
    
    genomeEndMax <- max(GenomeEndDensities()$density.values)
    
    
    GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = density.values*graph.height/genomeEndMax)
    
    
    
    
    if(input$keepGenomes == T){
    p <- suppressWarnings(ggplot() +
                            geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                            geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                            geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                            geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "targets"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                            geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                            geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                            geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5)+
                            geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "targets"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                            geom_path(data = GenomeEndDensities, aes(x = distance.breaks.short, y = density.values),size=1.5, linetype ="dotted") +
                            ggtitle(label = plot.title1, subtitle = plot.subtitle) +
                            coord_cartesian(ylim = c(-graph.height,graph.height),xlim = c(-xlim.num()*1.1, xlim.num()*1.1)) +
                            labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
                            theme_bw() +
                            theme(axis.text.x=element_text(size=14),
                                  axis.text.y=element_text(size=14),
                                  plot.title=element_text(size=12, face="bold", color="black"))
     )
    
    }else{
      p <- suppressWarnings(ggplot() +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "targets"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5)+
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "targets"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                              ggtitle(label = plot.title1, subtitle = plot.subtitle) +
                              coord_cartesian(ylim = c(-graph.height,graph.height),xlim = c(-xlim.num()*1.1, xlim.num()*1.1)) +
                              labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
                              theme_bw() +
                              theme(axis.text.x=element_text(size=14),
                                    axis.text.y=element_text(size=14),
                                    plot.title=element_text(size=12, face="bold", color="black"))
      )
    }
    
    
 
    
    
    p
  
    
    })
    
  })
  
  
  output$quadrantPlots <- renderPlot({
    input$QuadrantplotsUpdate
    
    isolate({    
    if(input$allQuadrandsPlotted == T){
      

        targetQuadrantTable <-  targetQuadrantTable()
        randomQuadrantRange <- randomQuadrantRange()
       
        maxTarget <- max(targetQuadrantTable$y)
        maxRandom <- max(randomQuadrantRange$y)
        
        
        targetQuadrantTable <- targetQuadrantTable%>%mutate(y = y/maxTarget)%>%mutate(group.main = "targets")

                
        randomQuadrantRange <- randomQuadrantRange%>%mutate(y = y/maxRandom)%>%mutate(group.main = "random")
        
        
        p <- suppressWarnings(ggplot() +
          geom_path(data = targetQuadrantTable%>%filter(group == "targets_actual"), aes(x = x, y = y, colour = group.main, group = group), size = 1.5)+
          geom_path(data = targetQuadrantTable%>%filter(group == "nontargets_actual"), aes(x = x, y = y, colour = group.main, group = group), size = 1.5)+
            geom_path(data = randomQuadrantRange%>%filter(group == "targets_random_mean"), aes(x = x, y = y, colour = group.main, group = group), size = 1.5)+
            geom_path(data = randomQuadrantRange%>%filter(group == "targets_random_lower"), aes(x = x, y = y, colour = group.main, group = group), size = 1.5, linetype="dotted")+
            geom_path(data = randomQuadrantRange%>%filter(group == "targets_random_upper"), aes(x = x, y = y, colour = group.main, group = group), size = 1.5, linetype="dotted")+
            geom_path(data = randomQuadrantRange%>%filter(group == "nontargets_random_mean"), aes(x = x, y = -y, colour = group.main, group = group), size = 1.5)+
            geom_path(data = randomQuadrantRange%>%filter(group == "nontargets_random_lower"), aes(x = x, y = -y, colour = group.main, group = group), size = 1.5, linetype="dotted")+
            geom_path(data = randomQuadrantRange%>%filter(group == "nontargets_random_upper"), aes(x = x, y = -y, colour = group.main, group = group), size = 1.5, linetype="dotted")+
           # ggtitle(label = plot.title1, subtitle = plot.subtitle) +
            labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
            theme_bw() +
            theme(axis.text.x=element_text(size=14),
                  axis.text.y=element_text(size=14),
                  plot.title=element_text(size=12, face="bold", color="black"))
       
       )
      
    }else{
    
 
      dat = targets.dat()

      randomQuadrantRange <- randomQuadrantRange()
      targetQuadrantTable <- targetQuadrantTable()

                                             
      p <- ggplot(targetQuadrantTable, aes(x=plotPosition, y=quadrantData, fill = strand)) + 
        geom_bar(stat="identity", position="identity") +
        geom_segment(aes(x=-7.5,xend=-2.5,y=randomQuadrantRange[1,2],yend=randomQuadrantRange[1,2]))+
        geom_segment(aes(x=-7.5,xend=-2.5,y=randomQuadrantRange[1,3],yend=randomQuadrantRange[1,3]))+
        geom_segment(aes(x=-7.5,xend=-2.5,y=randomQuadrantRange[1,4],yend=randomQuadrantRange[1,4]))+
        geom_segment(aes(x=2.5,xend=7.5,y=randomQuadrantRange[2,2],yend=randomQuadrantRange[2,2]))+
        geom_segment(aes(x=2.5,xend=7.5,y=randomQuadrantRange[2,3],yend=randomQuadrantRange[2,3]))+
        geom_segment(aes(x=2.5,xend=7.5,y=randomQuadrantRange[2,4],yend=randomQuadrantRange[2,4]))+
        geom_segment(aes(x=2.5,xend=7.5,y=randomQuadrantRange[3,2],yend=randomQuadrantRange[3,2]))+
        geom_segment(aes(x=2.5,xend=7.5,y=randomQuadrantRange[3,3],yend=randomQuadrantRange[3,3]))+
        geom_segment(aes(x=2.5,xend=7.5,y=randomQuadrantRange[3,4],yend=randomQuadrantRange[3,4]))+
        geom_segment(aes(x=-7.5,xend=-2.5,y=randomQuadrantRange[4,2],yend=randomQuadrantRange[4,2]))+
        geom_segment(aes(x=-7.5,xend=-2.5,y=randomQuadrantRange[4,3],yend=randomQuadrantRange[4,3]))+
        geom_segment(aes(x=-7.5,xend=-2.5,y=randomQuadrantRange[4,4],yend=randomQuadrantRange[4,4]))+
        # ggtitle(label = plot.title1, subtitle = plot.subtitle) +
        labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
        theme_bw() +
        theme(axis.text.x=element_text(size=14),
              axis.text.y=element_text(size=14),
              plot.title=element_text(size=12, face="bold", color="black"))

      
      

  
    
    }
    
    
    
    
    
    })
    p
  })
  
  
  output$adjustedPlot <- renderPlot({
    
    
    input$plotsUpdate
    
    isolate({
    dat = targets.dat()
    random.hits = rh()
    den <- den()
    

      
      targetDensities <- den%>%filter(group.main == "targets")%>%mutate(break.id = paste(distance.breaks.short, strand.plus.direction))%>%select(density.values, break.id)
      randomDensities <- den%>%filter(group.main == "random")%>%mutate(break.id = paste(distance.breaks.short, strand.plus.direction))
      
      den <- left_join(randomDensities, targetDensities, by = "break.id")
      
      den <- den%>%mutate(density.values = density.values.y/density.values.x)
      den <- den%>%mutate(group.main = "Adjusted Target Densities")
      
      
      den <- den%>%arrange(distance.breaks.short)
      
      
      
      
      
      
      
      dat = targets.dat()%>%filter(Subtype == Subtype.label())
      #xlim.num() <- input$range
      ##store input variable for use later

      plot.subtitle <- ifelse(xlim.num() != binwidth(), paste("Window size = ", xlim.num(), " nucleotides. \nBinwdith = ", binwidth(), " nucleotides.", sep = ""),  paste("Window size = ", xlim.num(), " nucleotides.", sep = ""))    
      

      
      
      
     # den <- den%>%mutate(density.values = density.values*nrow(st()))
      
      graph.height <- max(den$density.values)
      
      
      #GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = density.values*nrow(st))
      
      genomeEndMax <- max(GenomeEndDensities()$density.values)
      
      
      GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = density.values*graph.height/genomeEndMax)
      
      
      
      plot.title1 <- paste("Subtype",Subtype.label())
      plot.subtitle <- paste("Number of hits:", nrow(st()))
      
      if(input$keepGenomes == T){
      
      p <-  suppressWarnings(ggplot() +
                               geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                               geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                               geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                               geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                               geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                               geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1)+
                               geom_path(data = GenomeEndDensities, aes(x = distance.breaks.short, y = density.values),size=1.5, linetype ="dotted") +
                               geom_hline(yintercept = 1) +
                               geom_hline(yintercept = -1) +
                               geom_hline(yintercept = 0.5, linetype="dashed") +
                               geom_hline(yintercept = -0.5, linetype="dashed") +
                               geom_hline(yintercept = 2, linetype="dashed") +
                               geom_hline(yintercept = -2, linetype="dashed") +    
                               scale_fill_hue("Group") +
                               ggtitle(label = plot.title1, subtitle = plot.subtitle) +
                               labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
                               coord_cartesian(ylim = c(-graph.height,graph.height),xlim = c(-xlim.num()*1.1, xlim.num()*1.1)) +
                               theme_bw() +
                               theme(axis.text.x=element_text(size=14),
                                     axis.text.y=element_text(size=14),
                                     plot.title=element_text(size=12, face="bold", color="black"))
                             
      )
      
      
      }else{
        p <-  suppressWarnings(ggplot() +
                                 geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                                 geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                                 geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                                 geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                                 geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                                 geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1)+
                                 geom_hline(yintercept = 1) +
                                 geom_hline(yintercept = -1) +
                                 geom_hline(yintercept = 0.5, linetype="dashed") +
                                 geom_hline(yintercept = -0.5, linetype="dashed") +
                                 geom_hline(yintercept = 2, linetype="dashed") +
                                 geom_hline(yintercept = -2, linetype="dashed") +    
                                 scale_fill_hue("Group") +
                                 ggtitle(label = plot.title1, subtitle = plot.subtitle) +
                                 labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
                                 coord_cartesian(ylim = c(-graph.height,graph.height),xlim = c(-xlim.num()*1.1, xlim.num()*1.1)) +
                                 theme_bw() +
                                 theme(axis.text.x=element_text(size=14),
                                       axis.text.y=element_text(size=14),
                                       plot.title=element_text(size=12, face="bold", color="black"))
                               
        )
      }
      
      
    
    

    p
    
    })
    
  })

  
  output$relativePositions <- renderPlot({
    lower.num <- input$relativePositionslider[1]
    upper.num <- input$relativePositionslider[2]
    
    input$relativeplotsUpdate
    
    isolate({
    
    dat <- targets.dat()%>%filter(Subtype == Subtype.label())%>%
      filter(hits.count.num >= upper.num)

        
  lowerData <- dat%>%filter(spacer_order.num == lower.num)%>%select(host.target.pair, protospacer.distance.num, target.start.num)
    
  upperData <- dat%>%filter(spacer_order.num == upper.num)%>%select(host.target.pair, target.start.num)

  if(nrow(lowerData) & nrow(upperData) > 1){
    
  dat <- left_join(lowerData, upperData, by = "host.target.pair")
  
  dat <- dat%>%mutate(distanceToUpper = target.start.num.y - target.start.num.x)
  
  p <- suppressWarnings(ggplot() +
    geom_point(data = dat, aes(x = protospacer.distance.num, y = distanceToUpper))+
    # ggtitle(label = plot.title1, subtitle = plot.subtitle) +
    labs(x=paste("Distance from the priming protospacer to protospacer number", lower.num),y=paste("Distanec from protospacer number", lower.num, "to protospacer number", upper.num)) +
      coord_cartesian(ylim = c(-input$relativePlotRange, input$relativePlotRange),
                      xlim = c(-input$relativePlotRange, input$relativePlotRange)) +
      theme_bw() +
    theme(axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          plot.title=element_text(size=12, face="bold", color="black"))
  )
  p
    }
  
    })

    })
  
  output$statisticalAnalysisAll <- renderPrint({
    sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())
    st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())
    ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
    
  })
  
  output$statisticalAnalysisTarget <- renderPrint({
  sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("t_", strand.plus.direction))
  st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("t_", strand.plus.direction))
  ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
     
  })
  output$statisticalAnalysisNonTarget <- renderPrint({
    sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("n_", strand.plus.direction))
    st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("n_", strand.plus.direction))
    ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
    
  })
  
  output$statisticalAnalysisTarget5 <- renderPrint({
    sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("t_5", strand.plus.direction))
    st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("t_5", strand.plus.direction))
    ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
    
  })
  output$statisticalAnalysisNonTarget5 <- renderPrint({
    sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("n_5", strand.plus.direction))
    st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("n_5", strand.plus.direction))
    ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
    
  })
  output$statisticalAnalysisTarget3 <- renderPrint({
    sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("t_3", strand.plus.direction))
    st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("t_3", strand.plus.direction))
    ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
    
  })
  output$statisticalAnalysisNonTarget3 <- renderPrint({
    sr <- sr()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("n_3", strand.plus.direction))
    st <- st()%>%filter(protospacer.distance.num > -xlim.num())%>%filter(protospacer.distance.num < xlim.num())%>%filter(grepl("n_3", strand.plus.direction))
    ks.res <- suppressWarnings(ks.test(sr$protospacer.distance.num, st$protospacer.distance.num))
    ks.res
    
    
  })
  
  output$summary <- renderTable({
    summaryData <- read.table("summary_table_all_data_83.txt", header = T, sep = "\t")
    summaryData <- summaryData%>%mutate(Category = rownames(summaryData))
  })
  
  
  output$dataPlot <- renderPlot({
    x <- targets.dat()%>%
      filter(Subtype == input$Subtype)%>%
      filter(protospacer.distance.num > -input$range)%>%
      filter(protospacer.distance.num < input$range)%>%
      #filter(spacer_order.num != 0)%>%
      select(target.acc., Genome, target.start.num, host.target.pair, array.num, spacer.num, spacer_order.num, target.strand,five.three.prime.dir, protospacer.distance.num)
    
    if(input$keepPPS == F){
      x <- x%>%filter(spacer_order.num != 1)
    }
    
    
    colm <- input$orderby
    colm <- which(colnames(x) == colm)
    x <- x[order(x[,colm]),]

    y <- ifelse(is.numeric(x[,colm]), T, ifelse(is.integer(x[,colm]), T, F))
    
    if(y == T){
    
    p <- suppressWarnings(ggplot() +
      geom_histogram(data = x, aes(x = x[,colm], y = ..count..), binwidth = input$binwidth2)  +
      ggtitle(label = paste("Number of hits categorised by:", input$orderby)) +
      labs(x= input$orderby,y="Number of hits") +
      theme_bw() +
      theme(axis.text.x=element_text(size=14),
            axis.text.y=element_text(size=14),
            plot.title=element_text(size=12, face="bold", color="black"))
    
    )
    }else{
      

    p <-suppressWarnings(ggplot() +
        geom_bar(data = x, aes(x[,colm])) +
      ggtitle(label = paste("Number of hits categorised by:", input$orderby)) +
      labs(x= input$orderby,y="Number of hits") +
      theme_bw() +
      theme(axis.text.x=element_text(size=14),
            axis.text.y=element_text(size=14),
            plot.title=element_text(size=12, face="bold", color="black"))
    )
      
    }
    p
  })
  
  
 
  
  
  output$data <- renderTable({
    
    x <- targets.dat()%>%
      filter(Subtype == input$Subtype)%>%
      filter(protospacer.distance.num > -input$range)%>%
      filter(protospacer.distance.num < input$range)%>%
      #filter(spacer_order.num != 0)%>%
      select(target.acc., Genome, target.start.num, host.target.pair, array.num, spacer.num, spacer_order.num, target.strand,five.three.prime.dir, protospacer.distance.num)
  
    
    if(input$keepPPS == F){
      x <- x%>%filter(spacer_order.num != 1)
    }
     colm <- input$orderby
   colm <- which(colnames(x) == colm)
    
   
   
   
   x <- x[order(x[,colm]),]
    
    x
    
  })


  
  output$downloadStandard  <- downloadHandler(
    
    filename = function(){
      paste(input$Subtype, "_", input$range, "_", input$binwidth, "_", input$smoothing, "_standard.", input$filetype1, sep = "")
    },
    
    content = function(file){
      
      if(input$filetype1 == "png"){
        png(file)
      }
      if(input$filetype1 == "pdf"){
        pdf(file)
      }
      
      dat = targets.dat()
      random.hits = rh()
      den <- den()
      
      
      plot.title1 <- paste("Subtype",Subtype.label())
      plot.subtitle <- paste("Number of hits:", nrow(st))
      
      
      #den <- den%>%mutate(density.values = as.numeric(density.values)*nrow(st()))
      
      graph.height <- max(den$density.values)
      
      
      #GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = as.numeric(density.values)*nrow(st))
      
      genomeEndMax <- max(GenomeEndDensities()$density.values)
      
      
      GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = density.values*graph.height/genomeEndMax)
      
      
      
      
      
      p <- suppressWarnings(ggplot() +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                              geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "targets"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5)+
                              geom_path(data = den%>%mutate(density.values= -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "targets"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                              geom_path(data = GenomeEndDensities, aes(x = distance.breaks.short, y = density.values),size=1.5, linetype ="dotted") +
                              ggtitle(label = plot.title1, subtitle = plot.subtitle) +
                              coord_cartesian(ylim = c(-graph.height,graph.height),xlim = c(-xlim.num()*1.1, xlim.num()*1.1)) +
                              labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
                              theme_bw() +
                              theme(axis.text.x=element_text(size=14),
                                    axis.text.y=element_text(size=14),
                                    plot.title=element_text(size=12, face="bold", color="black"))
      )
      
      
      
      
      
      
      
      p
          
        
      
      
      
    }
    
  )
  
  
  output$downloadAdjusted  <- downloadHandler(
    
    filename = function(){
      paste(Subtype.label(), "_", xlim.num(), "_", binwidth(), "_", smoothing.val(), "_adjusted.", input$filetype2, sep = "")
    },
    
    content = function(file){
      
      if(input$filetype2 == "png"){
        png(file)
      }
      if(input$filetype2 == "pdf"){
        pdf(file)
      }
      dat = targets.dat()
      random.hits = rh()
      den <- den()
      
      
      
      targetDensities <- den%>%filter(group.main == "targets")%>%mutate(break.id = paste(distance.breaks.short, strand.plus.direction))%>%select(density.values, break.id)
      randomDensities <- den%>%filter(group.main == "random")%>%mutate(break.id = paste(distance.breaks.short, strand.plus.direction))
      
      den <- left_join(randomDensities, targetDensities, by = "break.id")
      
      den <- den%>%mutate(density.values = density.values.y/density.values.x)
      den <- den%>%mutate(group.main = "Adjusted Target Densities")
      
      
      den <- den%>%arrange(distance.breaks.short)
      
      
      
      
      
      
      
      dat = targets.dat()%>%filter(Subtype == Subtype.label())
      #xlim.num() <- input$range
      ##store input variable for use later
      
      plot.subtitle <- ifelse(xlim.num() != binwidth(), paste("Window size = ", xlim.num(), " nucleotides. \nBinwdith = ", binwidth(), " nucleotides.", sep = ""),  paste("Window size = ", xlim.num(), " nucleotides.", sep = ""))    
      
      
      
      
      
      # den <- den%>%mutate(density.values = density.values*nrow(st()))
      
      graph.height <- max(den$density.values)
      
      
      #GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = density.values*nrow(st))
      
      genomeEndMax <- max(GenomeEndDensities()$density.values)
      
      
      GenomeEndDensities <- GenomeEndDensities()%>%mutate(density.values = density.values*graph.height/genomeEndMax)
      
      
      
      plot.title1 <- paste("Subtype",Subtype.label())
      plot.subtitle <- paste("Number of hits:", nrow(st()))
      
      p <-  suppressWarnings(ggplot() +
                               geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                               geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5) +
                               geom_path(data = den%>%filter(grepl("t_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1.5) +
                               geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "upperRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                               geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "lowerRandomDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group), linetype="dotted",size=1.5)+
                               geom_path(data = den%>%mutate(density.values = -density.values)%>%filter(grepl("n_", strand.plus.direction))%>%filter(group == "meanDensity"), aes(x = distance.breaks.short, y = density.values, fill = strand.plus.direction, colour = group.main, group = group),size=1)+
                               geom_path(data = GenomeEndDensities, aes(x = distance.breaks.short, y = density.values),size=1.5, linetype ="dotted") +
                               geom_hline(yintercept = 1) +
                               geom_hline(yintercept = -1) +
                               geom_hline(yintercept = 0.5, linetype="dashed") +
                               geom_hline(yintercept = -0.5, linetype="dashed") +
                               geom_hline(yintercept = 2, linetype="dashed") +
                               geom_hline(yintercept = -2, linetype="dashed") +    
                               scale_fill_hue("Group") +
                               ggtitle(label = plot.title1, subtitle = plot.subtitle) +
                               labs(x="Distance from oldest protospacer (nucleotides)",y="Density of hits") +
                               coord_cartesian(ylim = c(-graph.height,graph.height),xlim = c(-xlim.num()*1.1, xlim.num()*1.1)) +
                               theme_bw() +
                               theme(axis.text.x=element_text(size=14),
                                     axis.text.y=element_text(size=14),
                                     plot.title=element_text(size=12, face="bold", color="black"))
                             
      )
      
      
      
      
      
      
      
      
      p
      
      
      
    }
    
  )
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content") 
}

shinyApp(ui = ui, server = server)
