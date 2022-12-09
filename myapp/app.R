library(shiny)
library(shinydashboard)

# Define UI ----

sidebar<-dashboardSidebar(
  width = 320,
  sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Explore Dataset", tabName = "explore_dataset"),
    menuItem("More Firms each year?", tabName = 'counts'),
    menuItem("Exponential increase in sales per year?", tabName = 'sales'),
    menuItem("what about profits?", tabName = 'profits')

  )
)



home<-tabItem(tabName = "home", 
              h1("Indian Economy Through Pixels", 
                 style = "font-family:courier;text-align:center; color:blue; font-weight: bold;"),
              
              p("India’s economy has one of the fastest growth rates in the world. 
              Therefore, it is reasonable to believe that firm profits have only 
              increased over time.",  style="font-family:verdana;font-size:150%;"), 
              
              p("It is also reasonable to assume that more companies 
              opened during this decade than during the preceding one. 
              Sales and overall assets should have increased year after year.", 
                style="font-family:verdana;font-size:150%;"), 
              
              p("However, are these speculations true?",
                  style="font-family:verdana;font-size:150%;"),
              
              p("In this project, we will try to shed light on these assumptions using data
                collected from the CMIE prowess database.", 
                style="font-family:verdana;font-size:150%;"),

              
              tags$figure(
                align = "center",
                img(src="economy.jpg", width = 600)
              ),
              hr(style="border-color: black;"), 
              
              p("This dataset includes financial information about firms from 2001 through 2022. 
                We won’t look at every type of company. We will instead focus on big businesses 
                that aren’t substantially influenced by the government. To this end, this dataset
                excludes:",   style="font-family:verdana;font-size:150%;"),
              HTML("<ul style=\"font-family:verdana;font-size:150%;\">
                   <li>businesses that are government-owned</li>
                   <li>firms that have either sales or total assets less than 10 crore</li>
                   <li>mining and construction firms</li>
                   <li>financial firms( Are excluded due to capital controls imposed on these firms by the government )</li>
                   <li>firms that are not listed on either the NSE or the BSE</li>
                   </ul>"),
              
              
              
              tags$figure(
                align = "center",
                img(src="dataset.jpg", width = 600)
              ),
              
              hr(style="border-color: black;"), 
              
              p("In order to describe firm specific characteristics, we will use the 
                following seven attributes from the dataset.",   style="font-family:verdana;font-size:150%;"),
              HTML("<ul style=\"font-family:verdana;font-size:150%;\">
                   <li>company name</li>
                   <li>incorporation year: The age and birth cohort of a firms are proxied by the year of incorporation</li>
                   <li>financial year:The year for which the company’s financial information is recorded</li>
                   <li>sales: The revenue of the firm</li>
                   <li>total assets: Is a proxy for the size of the firm</li>
                   <li>retained profits/losses for the year</li>
                   <li>industry sector</li>
                   </ul>")
)

explore_dataset<- tabItem(tabName = "explore_dataset",
                              
                          box(
                            width = 8, status = "primary", solidHeader = TRUE,
                            textInput("nse", h3("Enter nse code of company"),
                                      value = 'RELIANCE')
                          ),
                          box(
                            title = paste("SALES ","(in million rupees)"),
                            width = 6, status = "primary", solidHeader = TRUE,
                            tags$figure(
                              align = "center",
                              plotOutput("plot1", height = 550, width = 650)
                              )
                          ),
                        
                          box(
                            title = "RETAINED PROFITS  (in million rupees)", 
                            width = 6, status = "primary", 
                            solidHeader = TRUE,
                            tags$figure(
                              align = "center",
                              plotOutput("plot2", height = 550, width = 650)
                            )
                          )
                            
                            )

counts<-tabItem(tabName = "counts",
                
                box(
                  title = "According to our dataset, the number of firms in each year 
                  has remained almost constant.", 
                  width = 6, status = "primary", 
                  solidHeader = TRUE,
                  tags$figure(
                    align = "center",
                    plotOutput("plot3", height = 550, width = 650)
                  )
                ),
                box(
                  title = "According to our dataset, most companies were founded between 
                  1975 and 2000. Moreover, more companies were founded in the last decade 
                  than in the current decade.", 
                  width = 6, status = "primary", 
                  solidHeader = TRUE,
                  tags$figure(
                    align = "center",
                    plotOutput("plot4", height = 550, width = 650)
                  )
                )

                )
sales = tabItem(tabName = "sales",
                
                box(
                  title = "Mean sales of firms per year has increased constantly over the years.", 
                  width = 6, status = "primary", 
                  solidHeader = TRUE,
                  tags$figure(
                    align = "center",
                    plotOutput("plot5", height = 550, width = 650)
                    )
                  ),
                box(
                  title = "Median sales of firms per year has tapered off over the years.", 
                  width = 6, status = "primary", 
                  solidHeader = TRUE,
                  tags$figure(
                    align = "center",
                    plotOutput("plot6", height = 550, width = 650)
                  )
                )
                )
profits = tabItem(tabName = "profits",
                
                box(
                  title = "Mean retained profits of firms each year saw a huge increase after the COVID pandemic.", 
                  width = 8, status = "primary", 
                  solidHeader = TRUE,
                  tags$figure(
                    align = "center",
                    plotOutput("plot7", height = 550, width = 650)
                  )
                )         
)
              


body <- dashboardBody(
  tabItems(
    # First tab content
    home,
    explore_dataset,
    counts,
    sales,  
    profits
    
  )
)



ui <- dashboardPage(
  ## the name on top left
  dashboardHeader(title = "Indian Economy Through Pixels", titleWidth = 320),
  ## Sidebar content
  sidebar, 
  ## things to be displayed in the white area. The actual graphs to be shown.
  body
    
)


# Define server logic ----
server <- function(input, output) {
  
#  general_wd='/home/abhishek/Documents/visualization/myapp'
  library(testthat)
  library(assertive, warn.conflicts = FALSE)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(gridExtra)
#  setwd(general_wd)
  ds = read.csv(file='dataset.csv',header = TRUE )
  
  output$plot1 <- renderPlot({
    df = ds[(ds$nse==input$nse),]
    
    sales = ggplot( df, aes(y=sales, x=year) )+ 
      ggtitle( paste( "nse code of company is ", input$nse ) )+
      geom_line(color='#efb810')+geom_point(color='#efb810')+
      ylab('')+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
            axis.text=element_text(angle=0, hjust=0.5, size = 15),
            axis.title.x = element_text(angle=0, hjust=0.5, size=20)
      )+
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
    sales
  })
  
  output$plot2 <- renderPlot({
    df = ds[(ds$nse==input$nse),]
    
    retained_profits = ggplot( df, aes(y=retained_profits, x=year) )+
      ggtitle( paste( "nse code of company is ", input$nse ) )+
      geom_line(color='#efb810')+geom_point(color='#efb810')+
      ylab('')+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
            axis.text=element_text(angle=0, hjust=0.5, size=15),
            axis.title.x = element_text(angle=0, hjust=0.5, size=20)
      )+
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
    retained_profits
  })
  
  output$plot3 <- renderPlot({
    df = ds %>% group_by(year) %>% 
      summarise( 
        no_of_firms = n_distinct(co_code)
      )
    
  no_of_firms =   ggplot(data=df, aes(x=year, y=no_of_firms)) +
      geom_bar(stat="identity")+
      ggtitle( 'Total number of firms in each year')+
      ylab('')+
      xlab('Year')+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
          plot.title = element_text(face="bold", hjust=0.5, size=20),
          axis.text=element_text(angle=0, hjust=0.5, size=20),
          axis.title.x = element_text(angle=0, hjust=0.5, size=20) 
          
    )
  no_of_firms
  })
  
  output$plot4 <- renderPlot({
    x = ds$incorporation_year
    y = data.frame(x)
    colnames(y)=c('year')
    histgrm = ggplot(y, aes(x=year))+
      geom_histogram(bins=50, color="#00bfc4", fill = '#12b7bc')+
      ylab('')+
      ggtitle('Number of new firms in each year')+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
            plot.title = element_text(face="bold", hjust=0.5, size=20),
            axis.text=element_text(angle=0, hjust=0.5, size=20),
            axis.title.x = element_text(angle=0, hjust=0.5, size=20)
            
      )
    histgrm
  })
  
  output$plot5 <- renderPlot({
    df = ds %>% group_by(year) %>% 
      summarise( mean_sales = mean(sales),
                 median_sales = median(sales),
                 sum_sales = sum(sales),
                 mean_total_assets = mean(total_assets),
                 median_total_assets = median(total_assets)
      )
    
    reg_mean = ggplot( df, aes(y=mean_sales, x=year) )+geom_line()+
      ggtitle( 'Mean Sales Per Year')+
      ylab('')+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
            plot.title = element_text(face="bold", hjust=0.5, size=20),
            axis.text=element_text(angle=0, hjust=0.5, size=20),
            axis.title.x = element_text(angle=0, hjust=0.5, size=20)
            
      )
    reg_mean
  })

  output$plot6 <- renderPlot({
    df = ds %>% group_by(year) %>% 
      summarise( mean_sales = mean(sales),
                 median_sales = median(sales),
                 sum_sales = sum(sales),
                 mean_total_assets = mean(total_assets),
                 median_total_assets = median(total_assets)
      )
    
    reg_median = ggplot( df, aes(y=median_sales, x=year) )+geom_line()+
      ggtitle( 'Median Sales Per Year')+
      ylab('')+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
            plot.title = element_text(face="bold", hjust=0.5, size=20),
            axis.text=element_text(angle=0, hjust=0.5, size=20),
            axis.title.x = element_text(angle=0, hjust=0.5, size=20)
            
      )
    reg_median
  })
  
  output$plot7 <- renderPlot({
    df = ds %>% group_by(year) %>% 
      summarise( 
        mean_retained_profits = mean(retained_profits),
        median_retained_profits = median(retained_profits)
      )
    
    profits = ggplot(data=df, aes(x=year, y=mean_retained_profits)) +
      geom_bar(stat="identity")+
      ggtitle( 'Mean Retained Profits each year(in Mln rupees)')+
      ylab('')+
      xlab('Year')+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
            plot.title = element_text(face="bold", hjust=0.5, size=20),
            axis.text=element_text(angle=0, hjust=0.5, size=20),
            axis.title.x = element_text(angle=0, hjust=0.5, size=20)
            
      )
    profits
  })
  

  
}

# Run the app ----
shinyApp(ui = ui, server = server)
#runApp("app.R")