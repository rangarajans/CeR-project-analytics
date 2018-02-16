library(RMySQL)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(shinydashboard)
library(networkD3)
library(htmlwidgets)
library(RColorBrewer)
source("functions.r")

#Queries
qInst <- "select distinct(institution) from researcher"
qFaci <- "select distinct(facility) from facilityusage"
qInstitution <- " institution in ("
qFacility <- " facility in ("
qFacdiv_1 <- "select * from facilityusage" 
qResearch_1 <- "select distinct projectID as \'Project ID\', pName as \'Project Name\', rDescription as \'Description\', researcher as \'Primary Researcher\', type as \'Type\' from researchgraph where type is not NULL"
qResearch_2 <- ' order by projectID'
qSankey_1 <- "select distinct institution, division, facility, type from researchgraph"
qSankey_2 <- " order by institution, division, facility, type"
qSantab_1 <- "select distinct projectID as \'Project ID\', pName as \'Project Name\', researcher as \'Primary Researcher\', rDescription as \'Description\' from researchgraph"
qSantab_2 <- " order by projectID, researcher"
#Institutions list, sorted ASC
inst <- sort(unlist(sqlQuery(qInst), use.names = FALSE))
faci <- sort(unlist(sqlQuery(qFaci), use.names = FALSE))
# colorset <- c("#2980b9","#2c3e50", "#9b59b6", "#2ecc71", "#1abc9c", "#d35400", "#f1c40f", "#bdc3c7", "#16a085")
clickJS <- 'function () {
d3.selectAll(".node").on("mousedown.drag", null);
d3.selectAll(".node").on("click",function(d) { Shiny.onInputChange("id", d.name); })
}'
cerFacilities <- c("CeR Machine Learning", "CeR Research Data", "CeR Visualisation Services", "CeR VM-Farm", "NeCTAR", "Visualisation Cluster")
#### End - Global functions and variables

#### Begin color schemes
set.seed(008)
colorset <- colorRampPalette(brewer.pal(8, "Set2"))(10)
colorsan <- sample(colorRampPalette(brewer.pal(9, "Set1"))(200))
colorsan <- c('#00467F', '#CC4729', '#A71930', '#7D0063','#448517', '#4F2D7F', '#008075', '#0039A6', '#006990','#FFC286', colorsan)
linkcolors <- colorRampPalette(brewer.pal(9, "Purples"))(200)
#### End color schemes
ui <- fluidPage(
  tags$style("#institution {font-size:12px;}"),
  tags$style("#bar {font-size:10px;}"),
  titlePanel(title = "Research Outcomes"),
  sidebarLayout(
    sidebarPanel(checkboxGroupInput(inputId = "facility", label = "Select Institution(s):",faci),
                 checkboxGroupInput(inputId = "institution", label = "Select Facility(s):",inst, selected = 'University of Auckland'),
                 tableOutput("data"), width = '2'),
    mainPanel(
      plotlyOutput("bar", height = 500, width = "100%"),
      verbatimTextOutput("text1"),
      verbatimTextOutput("click")
    )
  )
)

ui_1 <- dashboardPage(skin = "blue",
                      dashboardHeader(title = "Project Analytics"),
                      dashboardSidebar(
                        sidebarMenu (
                          menuItem ("Facilities",
                                    checkboxGroupInput(inputId = "facility", label = NULL, choices = faci, selected = cerFacilities), startExpanded = TRUE, selected = TRUE),
                          menuItem ("Institutions", 
                                    checkboxGroupInput(inputId = "institution", label = NULL, choices = inst, selected = 'University of Auckland'))
                        ),
                        width = "15vw", collapsed = FALSE
                      ),
                      dashboardBody(
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "theme_2.css")),
                        tabBox(
                          tabPanel("By Facility", 
                                   plotlyOutput("bar", width = "100%", height = "50vh"),
                                   dataTableOutput('table', width = "100%")
                                   # verbatimTextOutput("click1")
                          ),
                          tabPanel("By Research Type", 
                                   sankeyNetworkOutput("sankey", width = "100%", height = "50vh"), #50vh initially
                                   dataTableOutput('table1', width = "100%"),
                                   # verbatimTextOutput("click"),
                                   tags$script(clickJS)
                          ),
                          width = "98vw", height = "96vh"
                        )
                      )
)

server <- function(input, output) {
  #dynamic SQL querying from checkboxinput filter
  instituteList <- reactive({
    text <- input$institution
    # text <- paste(text, collapse = '\',\'')
    return(text)
  })
  instOutput <- reactive({
    text <- instituteList()
    # print (text)
    text <- paste(text, collapse = '\',\'')
    instFilter <- quoteString(text)
  })
  
  faciOutput <- reactive ({
    text <- input$facility
    text <- paste(text, collapse = '\',\'')
    faciFilter <- quoteString(text)
  })
  
  facOutput <- reactive ({
    instFilter <- instOutput()
    faciFilter <- faciOutput()
    if (instFilter != "''" && faciFilter != "''") {
      qFacdiv <- paste(qFacdiv_1, " where" , qInstitution, instFilter,')', " and", qFacility, faciFilter, ')',sep = "")
    }
    else if (instFilter != "''" && faciFilter == "''") {
      qFacdiv <- paste(qFacdiv_1, " where",qInstitution, instFilter, ')', sep = "")
    }
    else if (instFilter == "''" && faciFilter != "''") {
      qFacdiv <- paste(qFacdiv_1, " where", qFacility, faciFilter, ')', sep = "")
    }
    else {
      qFacdiv <- qFacdiv_1
    }
    fac <- sqlQuery(qFacdiv)
    output <- fac[,c(2,3)]
    output <- output[order(output$facility, output$division, decreasing = c(F,T)), ]
    return(output)
    # return (qFacdiv)
  })
  #switched to plotly to allow mouse events
  output$bar <- renderPlotly({
    facdiv <- facOutput() 
    facdiv <- as.data.frame(table(facdiv))
    colnames(facdiv) <- c("Facility", "Division", "Projects")
    font <- list(
      size = 12, color = '#636F74', family = 'Calibri'
    )
    margin <- list(
      t = 30, r = 0, b = 60, l = 40, unit = "pt"
    )
    title <- paste(instituteList(), collapse = ', ')
    title <- paste(title, ' - Facility usage by Division', collapse = '\\n')
    titlefont <- list(
      size = 14
    )
    legend <- list(
      x = 100, y = 0.5
    )
    p <- plot_ly(facdiv, x = ~Facility, y = ~Projects, type = 'bar', color = ~Division, colors = colorset) %>% 
      layout(barmode = 'stack', margin = margin,font = font, title = title, titlefont = titlefont, legend = legend)
  })
  
  chartDrill <- reactive({
    eventData <- event_data("plotly_click")
    xPos <- eventData$pointNumber + 1
    yPos <- eventData$y
    qryData <- facOutput()
    facList <- unique(qryData$facility)
    if (length(xPos) == 0) {
      return(0)
    }
    else {
      if (length(facList) < xPos) {
        return (facList)
      }
      else {
        facName <- facList[[xPos]]
        divList <- qryData[qryData$facility == facName,]
        divList <- divList[,2] 
        divList <- sort(divList, decreasing = TRUE)
        divName <- divList[[yPos]]
        filters <- c(facName, divName)
        return(filters)
      }
    }
    # return(event_data("plotly_click"))
  })
  
  resInput <- reactive({
    facdiv <- chartDrill()
    instList <- instOutput()
    if (facdiv == 0) {
      if (instList == "''") {
        qResearch <- paste(qResearch_1, qResearch_2, sep = '')
      }
      else {
        qResearch <- paste(qResearch_1, ' and institution in (', instList, ')', qResearch_2, sep = '')
      }
    }
    else {
      facName <- quoteString(facdiv[1])
      divName <- quoteString(facdiv[2])
      if (instList == "''") {
        qResearch <- paste(qResearch_1, ' and facility = ',facName, ' and division = ',divName, qResearch_2, sep = '')
      }
      else {
        qResearch <- paste(qResearch_1, ' and institution in (',instList, ')', ' and facility = ',facName, ' and division = ',divName, qResearch_2, sep = '')
      }
    }
  })
  
  resOutput <- reactive({
    qResearch <- resInput()
    res <- sqlQuery(qResearch)
    res
  })
  
  tableInfo <- reactive ({
    selInst <- instOutput()
    selFacDiv <- chartDrill()
    if (selFacDiv == 0) {
      selFac <- 'All'
      selDiv <- 'All'
    }
    else {
      selFac <- selFacDiv[1]
      selDiv <- selFacDiv[2] 
    }
    info <- paste("Facility:",selFac, "  |  ", "Division:", selDiv, sep = " ")
    return (info)
  })
  
  output$table <- renderDataTable(
    datatable(
      resOutput(),
      options = list (pageLength = 30,
                      lengthMenu = c(30,50,100)),
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
      caption = tableInfo()
    )
  )
  
  visData <- reactive({
    instFilter <- instOutput()
    faciFilter <- faciOutput()
    if (instFilter != "''" && faciFilter != "''") {
      qSankey <- paste(qSankey_1, ' where', qInstitution, instFilter, ') and', qFacility, faciFilter, ')', qSankey_2, sep = "")
    }
    else if (instFilter != "''" && faciFilter == "''") {
      qSankey <- paste(qSankey_1, ' where', qInstitution, instFilter, ')', qSankey_2, sep = "")
    }
    else if (instFilter == "''" && faciFilter != "''") {
      qSankey <- paste(qSankey_1, ' where', qFacility, faciFilter, ')', qSankey_2, sep = "")
    }
    else {
      qSankey <- paste(qSankey_1, qSankey_2, sep = "")
    }
    
    rawData <- sqlQuery(qSankey)
    nameList <- c(unique(rawData$institution), unique(rawData$division), unique(rawData$facility), unique(rawData$type))
    numList <- seq(from = 0, to = length(nameList)-1)
    nameFrame <- data.frame(nameList, numList)
    newData <- rawData
    newData[] <- nameFrame$numList[match(unlist(rawData), nameFrame$nameList)]
    #temporary column names for rbind
    names(newData) <- c('a','a','a','a')
    sanData <-rbind(newData[c(1,2)], newData[c(2,3)], newData[c(3,4)])
    sanData <- as.data.frame(table(sanData))
    names(sanData) <- c("source", "target", "value")
    sanData <- sanData[!(sanData$value == 0),]
    rownames(sanData) <- 1:nrow(sanData)
    ind <- sapply(sanData, is.factor)
    sanData[ind] <- lapply(sanData[ind], function(x) as.numeric(as.character(x)))
    nodes = data.frame(nameList)
    nodes$nodeid <- c(1:nrow(nodes))
    nodes$colors <- head(colorsan, nrow(nodes))
    sanData$colors <- head(linkcolors, nrow(sanData))
    write.csv(nodes, "F:\\Masters\\Internship\\CER\\Project Analytics\\Shiny\\TestData\\nodes.csv")
    return (list(sanData, nodes, rawData))
    # return(qSankey)
  })
  
  # Draw sankey plot using Networkd3 package
  output$sankey <- renderSankeyNetwork({
    visData <- visData()
    sanData <- visData[[1]]
    nodes <- visData[[2]]
    colors <- paste(colorsan, collapse = '","')
    colors <- paste('d3.scaleOrdinal(["', colors, '"])', sep = "")
    # colors <- 
    # colors <- JS("d3.scaleOrdinal().domain(['Faculty of Science', 'Faculty of Engineering']).range(['#FF0000', '#00FF00'])")
    san <- sankeyNetwork(Links = sanData, Nodes = nodes, Source = "source", Target = "target", Value = "value", fontSize = 10,
                         NodeGroup = "colors", NodeID = 1, colourScale = colors, LinkGroup = NULL) 
    # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))
    onRender(san, clickJS)
  })
  # Check TestSankey.R. Create a list with colors, sort by faculty, assign colors. Remaining could be anything...
  # On click events for sankey nodes
  sanClick <- reactive ({
    selNode <- input$id
    rawData <- visData()[[3]]
    if (is.null(selNode)) {
      return (NULL)
    }
    else if (any(rawData == selNode) == FALSE) {
      return (NULL)
    }
    else {
      rawData <- visData()[[3]]
      colName <- colName(selNode, rawData)
      return (list(colName, selNode))
    }
    return(selNode)
  })
  
  ###Sankey onClick
  sanClickData <- reactive ({
    instFilter <- instOutput()
    faciFilter <- faciOutput()
    if (instFilter != "''" && faciFilter != "''") {
      qSantab <- paste(qSantab_1, ' where', qInstitution, instFilter, ') and', qFacility, faciFilter, ')', sep = "")
    }
    else if (instFilter != "''" && faciFilter == "''") {
      qSantab <- paste(qSantab_1, ' where', qInstitution, instFilter, ')', sep = "")
    }
    else if (instFilter == "''" && faciFilter != "''") {
      qSantab <- paste(qSantab_1, ' where', qFacility, faciFilter, ')', sep = "")
    }
    else {
      qSantab <- paste(qSantab_1, sep = "")
    }
    if (is.null(sanClick())) {
      return (qSantab)
    }
    else {
      colName <- sanClick()[[1]]
      selNode <- quoteString(sanClick()[[2]])
      if (identical(colName, character(0))) {
        qSantab
      }
      else {
        if (instFilter == "''" && faciFilter == "''") {
          qSantab <- paste(qSantab, ' where ', colName, ' = ', selNode, sep = "")
        }
        else {
          qSantab <- paste(qSantab, ' and ', colName, ' = ', selNode, sep = "")
        }  
      }
    }
    qSantab <- paste(qSantab, qSantab_2, sep = "")
    return (qSantab)
  })
  
  ###Sankey chart table - begin###
  sanTable <- reactive({
    qSantab <- sanClickData()
    res <- sqlQuery(qSantab)
    res
  })
  
  
  sanTableInfo <- reactive ({
    if (is.null(sanClick())) {
      info <- ""
    }
    else {
      colName <- sanClick()[[1]]
      colName <- paste(toupper(substring(colName, 1,1)), substring(colName, 2), sep = "")
      selNode <- sanClick()[[2]]
      info <- paste(colName, ': ', selNode, sep = "")
    }
    return (info)
  })
  
  output$table1 <- renderDataTable(
    datatable(
      sanTable(),
      options = list (pageLength = 30,
                      lengthMenu = c(30,50,100)),
      filter = 'top',
      caption = sanTableInfo()
    )
  )
  ###Sankey chart table - end###
  
  ###Test Outputs - begin###
  output$click <- renderPrint({
    print (visData())
  })
  
  output$click1 <- renderPrint({
    print (instituteList())
    print (class(instituteList()))
  })
  ###Test Outputs - end###
}

shinyApp(ui = ui_1, server = server)