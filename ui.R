
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(plotly)
library(markdown)

shinyUI(fluidPage(

  # Application title
    #titlePanel("Automatic Grader"),
    
    navbarPage("Automatic Grader",
        tabPanel("Build the Grades",
  # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    
                    radioButtons('sep', 'Separator',
                                    c(CSV_File =',',
                                      XLSX =';',
                                      TSV ='\t'),
                                  ','),
      
                    hr(),
      
                    fileInput('file1', 'Choose file to upload',
                                    accept = c('text/csv',
                                                'text/comma-separated-values',
                                                '.csv',
                                                '.xls',
                                                '.xlsx'
                                              )
                              ),
      
      #              hr(),
      
       #             numericInput("grades", 
        #                      label = "How many Grade cluster do you want to have",  
         #                     value = 8),


                    hr(),
                    
                    numericInput("ab", 
                                 label = "What marks do you want to give to Absentees",  
                                 value = 0, max = 0),                          
      
                    sliderInput("de", "Factor that should multipy with sigma", 
                              min = 0.5, max = 2.5, value = 1, step= 0.1),
      
                    hr(),
      
                    radioButtons("dataset", "How do you want to grade", 
                                      choices = c( "Relative", "Absolute")
                                 ),
      
                    hr(),
      
                    tags$em("(Enter maximum marks for calculating Absolute grading)"),
      
                    numericInput("num", label = "Maximum Marks",  value = 100),
      
                    hr(),
      
                    radioButtons("print_data", "Want to print data",
                                    c("No", "Yes")
                                  ),
      
                    hr(),
            
                    downloadButton('downloadData', 'Download Grade File')
      
                ),

              # Show a plot of the generated distribution
                mainPanel(
                  tabsetPanel(type = "tabs", 
                          tabPanel("Plot", plotlyOutput("marks"),
                                   uiOutput("summary"),
                                   hr(),
                                  h3("Statistics of Final marks"),
                                  tableOutput("stats"),
                                  column(6,plotlyOutput("counter")),
                                  column(6,plotlyOutput("counter2")),
                                  list(tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                        })"))),
                                   DT::dataTableOutput("table")), 
                          tabPanel("Summary", plotlyOutput("mas2"),
                                   hr(),
                                   column(6,uiOutput("plots")),
                                   column(6,uiOutput("plots22")),
                                   hr()
                                   
                          ),
                          tabPanel("Bar Plot",  plotlyOutput("table22"), hr(),
                                   plotlyOutput("table223"))
                  )
                )
            )
        ),

###############################################################  
###############################################################
###############################################################

        tabPanel("Make Grade File",
            sidebarLayout(
                sidebarPanel(                 
                  checkboxGroupInput("checkGroup", label = h3("Select atleast one"), 
                                     choices = list("Roll no." = "Roll_no", "Name" = "Name"),
                                     selected = 1),     
                  
                  numericInput("num2", label = "How many Quiz",  value = 1, min=1),
                  hr(),
                  
                  uiOutput("mm"),
                  
                  selectInput("asgn", "No. of Assignments:",
                              c("0"=0, "1" = 1, "2" = 2,"3" = 3, "4"=4, "5"=5)),
                  uiOutput("ass"),
                  
                  radioButtons("midterm", "Will you be taking midterm exam",
                               c("No", "Yes")),
                  uiOutput("mid"),
                  
                  radioButtons("endterm", "Will you be taking endterm exam",
                               c("No", "Yes")),
                  uiOutput("end"),
                  
                  hr(),
                  hr(),
                  checkboxInput("checkbox", 
                                label = "Check to original new Table", 
                                value = FALSE),
                
                  downloadButton('downloadfile', 'Download the CSV File')
                  
              ),
                mainPanel(
                  
                  DT::dataTableOutput("table2")
                )
            )
        ),
navbarMenu("More",
           tabPanel("How to use & About",
                    fluidRow(
                      column(6,
                             h3("How to Use"),
                        tags$ol(
                          tags$li(" Go to \"Make Grade File\" tab."), 
                          tags$li(" Select at least one of Radio button(either Roll 
                           no. or Name or both) and then select from UI."), 
                          tags$li("Now check radio button \"Check to original new Table\"  
                           to view original table that will be downloaded."),
                          tags$li("Hit Download button to download the table that you had just ceated(
                            which can be seen in right side. File will be downloaded with the name
                            'Enter_the_Entries.csv'."),
                        
                          tags$br(),
                        
                          tags$b("Once filling out enteries in the above downloaded file."),
                        
                        tags$br(),
                        
                          tags$li("Go to \"Build the Grades\" tab."),
                          tags$li("Choose the above file by clicking on Browse button."),
                          tags$li("Check what marks do you want to give to the absentees(maximum is 0)."),
                          tags$li("Select wether you want to do the Absolute grading or Relative grading."),
                          tags$li("Enter input in 'maximum marks' if you have selected Absolute grading(by default 
                                  it is 100, which is the standard one)."),
                          tags$li("Once completed, Click on 'Yes' to view the table in which now two column 
                                  will be added(Total marks and Grades Respectively)."),
                          tags$li("Once this is done, click on Download button to download the Graded File.")
                        ),
                        tags$h3("Thanks "),
                               tags$h4("Sakthi sir, Nirmal sir  and Pushkal sir",
                               "for encouraging me to contribute something to College."),
                             tags$h4("-Lokesh Todwal")
                        
                    )
                  )
            )
    ),tags$footer("ALL CONTENT COPYRIGHT LOKESH TODWAL © 2017 • ALL RIGHTS RESERVED", align = "center", style = "
              position:relative;
                  bottom:0;
                  width:100%;
                  height:50px;   /* Height of the footer */
                  color: white;
                  padding: 10px;
                  background-color: black;
                  z-index: 3;")
)
    
))
