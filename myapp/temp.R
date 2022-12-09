library(shiny)
library(shinydashboard)

# Define UI ----

sidebar<-dashboardSidebar(
  width = 280,
  sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Widgets", tabName = "widgets"),
    menuItem("extra", tabname = 'extra')
  )
)



home<-tabItem(tabName = "home", 
              h1("Indian Economy Through Pixels", 
                 style = "text-align:center; color:blue; font-weight: bold;"),
              p("India’s economy has one of the fastest growth rates in the world. 
              Therefore, it is reasonable to believe that firm profits have only 
              increased over time."), 
              p("It is also reasonable to assume that more companies 
              opened during this decade than during the preceding one. 
              Sales and overall assets should have increased year after year."), 
              p("However, are these speculations true?"),
              p("In this project, we will try to shed light on these assumptions using data
                collected from the CMIE prowess database."),
              
              
              tags$figure(
                align = "center",
                img(src="economy_pic.jpeg", width = 600)
              ),
              hr(style="border-color: black;"), 
              p("This dataset includes financial information about firms from 2001 through 2022. 
                We won’t look at every type of company. We will instead focus on big businesses 
                that aren’t substantially influenced by the government. To this end, this dataset
                excludes:"),
              HTML("<ul>
                   <li>businesses that are government-owned</li>
                   <li>firms that have either sales or total assets less than 10 crore</li>
                   <li>mining and construction firms</li>
                   <li>financial firms( Are excluded due to capital controls imposed on these firms by the government )</li>
                   <li>firms that are not listed on either the NSE or the BSE</li>
                   </ul>"),
              p("In order to describe firm specific characteristics, we will use the 
                following seven attributes from the dataset."),
              HTML("<ul>
                   <li>company name</li>
                   <li>incorporation year: The age and birth cohort of a firms are proxied by the year of incorporation</li>
                   <li>financial year:The year for which the company’s financial information is recorded</li>
                   <li>sales: The revenue of the firm</li>
                   <li>total assets: Is a proxy for the size of the firm</li>
                   <li>retained profits/losses for the year</li>
                   <li>industry sector</li>
                   </ul>")
)


body <- dashboardBody(
  tabItems(
    # First tab content
    home
    ,
    
    # Second tab content
    tabItem(tabName = "widgets",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    # third tab content
    tabItem(tabName = "extra",
            fluidRow(
              tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
              ),
              tabBox(
                side = "right", height = "250px",
                selected = "Tab3",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
              )
            )
    )
    
  )
)



ui <- dashboardPage(
  ## the name on top left
  dashboardHeader(title = " Indian Economy in Pixels ", titleWidth = 280),
  ## Sidebar content
  sidebar, 
  ## things to be displayed in the white area. The actual graphs to be shown.
  body
  
)


# Define server logic ----
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.jpeg')
    
    # Generate a png
    png(outfile, width=400, height=400)
    img(src="economy_pic.jpeg", align = "center")
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}

# Run the app ----
#shinyApp(ui = ui, server = server)
runApp("app.R")