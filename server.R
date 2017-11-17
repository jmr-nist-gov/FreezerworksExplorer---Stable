#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shiny)
require(dplyr)
require(DT)
require(ggplot2)
require(lubridate)

# Loading data from an RData doesn't work, load manually or specify based on read.csv

# Custom functions (these are preloaded and only presented here for transparency)
## Returns an integer vector of all lengths of levels in each column in dataframe 'x' 
colsUsed <- function(x) {
  sapply(1:dim(x)[2],
    function(y) {
      length(levels(as.factor(x[,y])))
    }
  )
}
getDateRange <- function(x) {
  c(
    min(x, na.rm=T),
    max(x, na.rm=T)
  )
}
datadown <- function(x, y, z){
   if (!is.null(y)){
     x[y,] %>% select(one_of(z))
   } else {
     x %>% select(one_of(z))
   }
}


# Define server logic
shinyServer(function(session, input, output) {
  
  #Reactive datasets and variables
  ChosenProjects <- reactive({
    if (length(input$ProjectChoice)==0) levels(MESBdata$'Project ID') else input$ProjectChoice
  })
  ChosenSpecies <- reactive({
    if (length(input$SpeciesChoice)==0) levels(MESBdata$Species) else input$SpeciesChoice
  })
  ChosenColumns <- reactive({
    input$ShowVars
  })
  ChosenSummary <- reactive({
    input$SummariseBy
  })
  ChosenDates <- reactive({
    input$DateRange
  })
  ChosenAnalyteCategories <- reactive({
    input$ShowChemGroup
  })
  dataout <- reactive({
    MESBdata %>%
      #ifelse(input$StrictDates,
      #  filter(`Date Collected` >= input$DateRange[1] & `Date Collected` <= input$DateRange[2])
        filter(`Date Collected` >= input$DateRange[1] & `Date Collected` <= input$DateRange[2] | is.na(`Date Collected`)) %>%
      #) %>%
      filter(`Project ID` %in% ChosenProjects()) %>%
      filter(Species %in% ChosenSpecies())
  })
  chemdata <- reactive({
    chem %>% 
      filter(GUAID %in% dataout()$'Globally Unique Aliquot ID') %>% 
      filter(category %in% ChosenAnalyteCategories()) %>%
      select(-c(analyte_order,dl,value))
  })
  summary <- reactive({
    if (length(ChosenSummary())>0){
      dataout() %>%
        group_by_(.dots=lapply(ChosenSummary(), as.symbol)) %>% 
        summarise('Aliquot Count'=n()) %>%
        arrange(desc(`Aliquot Count`))
    } else {
      data.frame(
        'Aliquots'=c("Total Number","Checked Out","Not Used"),
        'Aliquot Count'=c(
          dataout() %>% select(`Globally Unique Aliquot ID`) %>% count() %>% unlist(use.names=FALSE),
          dataout() %>% select(`Date Out`) %>% filter(!is.na(`Date Out`)) %>% count() %>% unlist(use.names=FALSE),
          dataout() %>% select(`Date Out`) %>% filter(is.na(`Date Out`)) %>% count() %>% unlist(use.names=FALSE)
        )
      )
    }
  })
  
  #Outputs
  output$AliquotCount <- DT::renderDataTable({
    DT::datatable(summary(),
                  filter=list(position='top',clear=TRUE),
                  options=list(stateSave=TRUE,
                               caseInsensitive=TRUE,
                               autoWidth=TRUE,
                               pageLength=25
                  ),
                  rownames=FALSE
    )
  })
  output$AllData <- DT::renderDataTable({
    DT::datatable(dataout() %>% select(one_of(ChosenColumns())),
                  filter=list(position='top',clear=TRUE),
                  extensions='ColReorder',
                  options=list(stateSave=TRUE,
                               caseInsensitive=TRUE,
                               autoWidth=TRUE,
                               pageLength=25,
                               colReorder=TRUE
                  ),
                  rownames=FALSE
    )
  })
  output$AllChemData <- DT::renderDataTable({
    DT::datatable(chemdata(),
                  filter=list(position='top',clear=TRUE),
                  extensions='ColReorder',
                  options=list(stateSave=TRUE,
                               caseInsensitive=TRUE,
                               autoWidth=TRUE,
                               pageLength=25,
                               colReorder=TRUE
                  ),
                  rownames=FALSE
    )
  })
  output$Chemistry <- renderPlot({
      points <- ggplot(chemdata(),
        aes(x=analyte, y=statvals, colour=as.factor(detected)))+
        geom_point(alpha=0.2)+
        facet_grid(units~category, shrink=TRUE, drop=TRUE, scales='free', space='free_x')+
        theme_classic()+
        labs(x='Analyte', y='Mass Fraction')+
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
        theme(legend.position='bottom')+guides(colour=guide_legend(nrow=2,byrow=TRUE))+
        scale_colour_discrete(name=element_blank(), breaks=c(FALSE,TRUE), labels=c("Not Detected","Detected"))
      #p <- ggplotly(points)
      #p
      points
  })
  
  # Button actions
  
  observeEvent(input$GetAliquotData, updateNavbarPage(session, 'DataTabs', selected='All Data'))
  output$saveAsCSV <- downloadHandler(
    filename = function() {paste('MESB Aliquot List ', Sys.time(), '.csv', sep='')},
    content = function(file) {write.csv(datadown(dataout(), input$AllData_rows_selected, input$ShowVars), file)}
  )
  output$saveAsXLS <- downloadHandler(
    filename = function() {paste('MESB Aliquot List ', Sys.time(), '.xls', sep='')},
    content = function(file) {write.table(datadown(dataout(), input$AllData_rows_selected, input$ShowVars), file, sep='\t')}
  )
  
  # Reactive UI elements
  
  observe({
    projects <- input$ProjectChoice
    species <- input$SpeciesChoice
    dateRange <- input$DateRange
    
    #Subset metadata for restricted input choices
    temp <- MESBdata 
    if (length(projects)>0) {
      temp <- temp %>% filter(`Project ID` %in% projects)
      sppList <- MESBdata %>% filter(`Project ID` %in% projects) %>% droplevels()
      sppList <- levels(sppList$Species)
    } else {
      sppList <- levels(MESBdata$Species)
    }
    if (length(species)>0) {
      temp <- temp %>% filter(`Species` %in% species)
      projList <- MESBdata %>% filter(`Species` %in% species) %>% droplevels()
      projList <- levels(projList$'Project ID')
    } else {
      projList <- levels(MESBdata$'Project ID')
    }
    #temp <- temp %>% filter(between(`Date Collected`,dateRange[1],dateRange[2]))
    temp <- temp %>% droplevels()
    vars <- temp %>% colsUsed()
    metaList <- names(MESBdata)[which(vars>1)]
    collectionDates <- getDateRange(temp$'Date Collected')
    
    #Update inputs based on selection
    updateSelectInput(session,'ShowVars',
                      choices=metaList,
                      selected=c('Project ID',
                                 'Species',
                                 'Globally Unique Sample ID',
                                 'Globally Unique Aliquot ID')
                      )
    updateSelectInput(session,'SpeciesChoice',
                      choices=sppList,
                      selected=species
                      )
    updateSelectInput(session, 'ProjectChoice',
                      choices=projList,
                      selected=projects)
    if (!is.na(collectionDates[1])) {
      updateDateRangeInput(session,'DateRange',
                           min=collectionDates[1],
                           max=collectionDates[2],
                           start=collectionDates[1],
                           end=collectionDates[2]
                           )
    }
    output$log <- renderPrint(class(input$RestrictDates))
  })
})
