#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shinythemes)
require(dplyr)

# Loading data from an RData doesn't work, load manually or specify based on read.csv

# Establish page variables
intro <- "Placeholder for introductory language"
namList <- names(MESBdata)
minDate <- min(MESBdata$`Date Collected`[!is.na(MESBdata$`Date Collected`)])
maxDate <- max(MESBdata$`Date Collected`[!is.na(MESBdata$`Date Collected`)])
# dateRange <- as.Date(as.vector(dateRange$'Date Collected'), format="%m/%d/%Y")


shinyUI(
  navbarPage("Marine STAR",theme = shinytheme("yeti"),
      tabPanel("Home",
        fluidRow(
          column(12,
            titlePanel("Marine Environmental Specimen Bank Explorer v0.1"),
            hr(),
            intro
          )
        )
      ),
    #navbarMenu("Navigation",
      tabPanel("Sample Inventory",
               
        # Heading row with selections for Project and Species
        fluidRow(column(12,h3("Explore samples housed at the NIST Marine ESB"))),
        fluidRow(column(12,h4("1. Select the broadest categories of interest"))),
        fluidRow(
          column(4,
            selectInput('ProjectChoice',
              label=h6('Project'),
              choices=levels(MESBdata$'Project ID'),
              multiple=TRUE,
              selectize=TRUE
            )
          ),
          column(4,
            selectInput('SpeciesChoice',
              label=h6('Species'),
              choices=levels(MESBdata$Species),
              multiple=TRUE,
              selectize=TRUE
            )
          ),
          column(4,
            dateRangeInput('DateRange',
              label=h6('Collected from'),
              start=minDate,
              end=maxDate,
              min=minDate,
              max=maxDate,
              format="d M yyyy",
              startview="decade"
            )
          #),
          #column(1,
          #       checkboxInput('StrictDates',
          #                   label='Strict Dates',
          #                   value=TRUE
          #       )
          )
        ),
        hr(),
        fluidRow(column(12,h4("2. Refine your search"))),
        fluidRow(
          column(12,
            tabsetPanel(id='DataTabs',
              tabPanel(id='SummaryCounts', 'Summary Counts',
                selectInput('SummariseBy',
                  label=h6('Count Aliquots by:'),
                  choices=namList,
                  selected=c('Species','Tissue Type'),
                  multiple=TRUE,
                  selectize=TRUE
                ),
                DT::dataTableOutput('AliquotCount'),
                hr()#,
                #fluidRow(
                #  column(4, offset=4, 
                #    actionButton('GetAliquotData','Restrict aliquot data to selected rows')
                #  )
                #)
              ),
              tabPanel(id='AllData', 'All Data',
                fluidRow(
                  column(12,
                    selectInput('ShowVars',
                      label=h6('Show data for:'),
                      choices=namList,
                      multiple=TRUE,
                      selectize=TRUE,
                      selected=c('Project ID',
                                 'Species',
                                 'Globally Unique Sample ID',
                                 'Age Class',
                                 'Tissue Type',
                                 'Globally Unique Aliquot ID')
                    )
                  )
                ),
                fluidRow(
                  column(12,
                    DT::dataTableOutput('AllData'),
                    hr()
                  )
                ),
                fluidRow(
                  column(4, offset=8,
                    'Save this aliquot set as:',
                    downloadButton('saveAsCSV','CSV'),
                    downloadButton('saveAsXLS','XLS')
                  )
                )
              ),
              tabPanel('Freezers',
                plotOutput('Freezers')
              ),
              tabPanel('Testing',
                textOutput('log')
              )
            )
          )
        )
      ),
      tabPanel(id='chemistry', 'Environmental Chemistry (experimental)',
        fluidRow(
          column(12,
            selectInput('ShowChemGroup',
               label=h2('Select Chemical Class(es) of Interest'),
               choices=levels(chem$category),
               multiple=TRUE,
               selectize=TRUE,
               selected=levels(chem$category)
            )
          )
        ),
        hr(),
        fluidRow(
          column(12,
            plotOutput('Chemistry',height="800px")
          )
        ),
        hr(),
        fluidRow(
          column(12,
            DT::dataTableOutput('AllChemData')
          )
        )
      ),
      tabPanel(ie='request','Request an Aliquot Set (unavailable)'
      )
    #) #<- navBarMenu end
  )
)
