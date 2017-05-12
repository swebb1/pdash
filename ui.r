library(shiny)
library(shinydashboard)
library(shinyFiles)

shinyUI(dashboardPage(
  title="pdash",
  skin="blue",
  dashboardHeader(title = "PDash", titleWidth = 220),
  dashboardSidebar(width=220,
                   sidebarMenu(
                     menuItem("Scatter plot",tabName="vp",icon=shiny::icon("line-chart")),
                     menuItem("Peptide plots",tabName="pep",icon=shiny::icon("line-chart"))
                   )
  ),
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
    tabItem(tabName="vp",
              fluidRow(
                tabBox(width=9,
                  tabPanel("Table",
                    div(style = 'overflow-x: scroll', dataTableOutput('vtable'))
                  ),
                  tabPanel("Scatter Plot",
                    plotlyOutput("vplot",height = "600px"),
                    textInput("VplotName","Save as:","plot1"),
                    downloadButton('downloadVplot', 'Save plot as pdf'),
                    p(strong("Selected Points:")),
                    div(style = 'overflow-x: scroll', dataTableOutput('vseltable')),
                    uiOutput("downloadFile")
                  )
                ),
                tabBox(
                  width = 3,
                  tabPanel("File Input",
                            fileInput("vfile", "Input File",multiple = FALSE),
                            checkboxInput("vheader", "File has column headers", TRUE),
                            checkboxInput("vxls", "Excel file", FALSE),
                            selectInput("vsep","Column delimeter",choices = c("comma","tab","space"))
                  ),
                  tabPanel("Plot Controls",
                          uiOutput("vplot_cols")
                          #actionButton("vgo",label = "Plot",icon=shiny::icon("arrow-circle-right"))
                  ),
                  tabPanel("Filters",
                           uiOutput("vplot_fils")
                           #actionButton("vfil",label = "Filter",icon=shiny::icon("arrow-circle-right"))
                  )
                )
              )
    ),
    tabItem(tabName="pep",
            fluidRow(
              tabBox(width = 9,
                tabPanel("Table",
                  div(style = 'overflow-x: scroll', dataTableOutput('ptable'))
                ),
                tabPanel("Bar Plot",
                  plotOutput("pplot1",height = "600px")
                ),
                tabPanel("Stacked Plot",
                  plotlyOutput("pplot2",height = "600px")
                ),
                tabPanel("Tiled Plot",
                         plotlyOutput("pplot3",height = "600px")
                )
              ),
              tabBox(
                width = 3,
                tabPanel("File Input",
                          fileInput("pfile", "Input File",multiple = FALSE),
                          checkboxInput("pheader", "File has column headers", TRUE),
                          checkboxInput("pxls", "Excel file", FALSE),
                          selectInput("psep","Column delimeter",choices = c("comma","tab","space"))
                ),
                tabPanel("Plot Controls",
                          uiOutput("pplot_cols"),
                          actionButton("pgo",label = "Plot",icon=shiny::icon("arrow-circle-right")),
                          textInput("PplotName","Save plots as:","plot2"),
                          downloadButton('downloadPplot', 'Save plots as pdf')
                )
              )
            )
    )
    )
  )
  )
)

