library(shiny)
library(tidyverse)
library(lubridate)

# Load the data.
orders_raw <- read.csv("items.csv")
# Drop cols and parse.
orders <- orders_raw %>%
  select(Order.Date, Item.Total, Item.Subtotal.Tax, Shipping.Address.Name) %>%
  mutate(Order.Date = as.Date(Order.Date, "%m/%d/%y"),
         Item.Total = parse_number(as.character(Item.Total)),
         Item.Subtotal.Tax = parse_number(as.character(Item.Subtotal.Tax)))

# Shiny UI.
ui <- fluidPage(
  titlePanel("Amazon Order Analysis"),
  helpText("Amazon allows you to download .csv reports on your order history, 
            available from the following link ",
           code('https://www.amazon.com/gp/b2b/reports?ref_=ya_d_l_order_reports&'),
           ". This Shiny based app uses the \"Items\" table to summarize and hopefully
            help analyze purchasing patterns. Also, this dashboard models a simple
            cost-benefit analysis of getting an Amazon Prime subscription to avail
            free-shipping on small orders and such. The data set being used to run
            the dashboard has been anonymized. To run this dashboard against your own
            order history, checkout the code from my gitbub and change the .csv
            path accordingly. Alternatively you can checkout my Kaggle Notebook: ",
           code('https://www.kaggle.com/anupamamanju/amazon-order-history-analysis')),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      fluidRow(
        column(5,
               h4("Total Spend"),
               h1("$",
                  textOutput("total_spend", inline = T)),
               hr(),
               h5("Total Tax Paid"),
               h2("$",
                  textOutput("total_tax", inline = T)),
               hr(),
               h6("Number of orders: ",
                  textOutput("num_orders", inline = T)),
               h6("Most expensive order: $",
                  textOutput("most_expensive", inline = T)),
        ),
        column(7,
               plotOutput(outputId = "plot_1", height = 220),
               selectInput("select_1", "", 
                           choices = list("By Month" = 1, "By Buyer" = 2, "By Order Size" = 3),
                           selected = 1),
        )
      )
    ),
    
    mainPanel(
      width = 8, 
      
      fluidRow(
        # Margin.
        column(1),
        
        column(5,
               plotOutput(outputId = "distPlot", height = 500),
              ),
        
        column(5,
               h4("Amazon Prime Cost-Benefit Analysis."),
               helpText("This chart visualizes a historical view of the months of the
                         year in which it would have been viable to get a Prime Subscription.
                         A month is considered worhty if at least 'num_order_threshold' orders
                         were placed or if at least 'num_small_order_threshold' orders under 35$,
                         (i.e. paid shipping orders) were placed or if the monthly spend was
                         over 'spend_threshold'."),
               hr(),
               numericInput("num_order_threshold", p("num_order_threshold"), value = 4),
               numericInput("num_small_order_threshold", p("num_small_order_threshold"), value = 2),
               numericInput("spend_threshold", p("spend_threshold"), value = 500),
        )
      ),
      
      hr(),
      
      fluidRow(
        # Margin.
        column(1),
        
        column(5,
               fluidRow(
                 column(9,
                        h4("Worth getting Amazon Prime Yearly?"),
                        helpText("Based on the last 12 months, whether it would be
                                  economical to get a yearly membeership instead of
                                  going month to month based on need.")
                 ),
                 column(3, align="center",
                        h2(textOutput("should_get_yearly", inline = T)),
                        h6(textOutput("num_prime_months", inline = T), "/ 12 Months"),
                 )
               )
        ),
        
        column(5,
               fluidRow(
                 column(5, align="right",
                   tableOutput('prime_top_months')
                 ),
                 column(7,
                   h4("Historically active months."),
                   helpText("Months that have been Prime worthy the most number of 
                             times over the time duration of the dataset.")
                 )
               ),
        )
        
      )
      
      
    ),
    
  )
)

# Shiny Server.
server <- function(input, output) {
  
  get_monthly_summary <- reactive({
    
    num_small_order_threshold <- input$num_small_order_threshold  # Assuming a small order charges around 5$ to ship, we need atleast 2 of these to break even
    num_order_threshold <- input$num_order_threshold        # If we order more than this, maybe it's worthwhile to get the product faster (free 2-day shipping)
    amt_spent_threshold <- input$spend_threshold      # If we are spending more than this, maybe it's worthwhile to get the product faster (free 2-day shipping)
    
    orders %>%
      group_by(date_rounded = floor_date(Order.Date, "month")) %>%
      summarize(spend = sum(Item.Total),
                num_small_orders = sum(Item.Total < 35),
                num_orders = n()) %>%
      mutate(need_prime = as.numeric(num_small_orders > num_small_order_threshold |
                                       num_orders > num_order_threshold |
                                       spend > amt_spent_threshold))
    
  })
  
  output$distPlot <- renderPlot({
    
    monthly_summary <- get_monthly_summary()
    
    # Plot Prime worthy months.
    monthly_summary %>%
      mutate(year = year(date_rounded),
             month = month(date_rounded, label = TRUE)) %>%
      select(year, month, need_prime) %>%
      ggplot(aes(x = month, y = need_prime)) +
      geom_bar(stat = "identity", fill = "chartreuse3") +
      facet_wrap( ~ year, ncol = 1) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y= element_blank(),
            axis.title.y = element_blank())
    
  })
  
  output$plot_1 <- renderPlot({
    
    if (input$select_1 == 1) {
      # Pie chart: orders/month
      orders %>%
        group_by(month = month(Order.Date, label=TRUE)) %>%
        summarize(value = sum(Item.Total)) %>%
        ggplot(aes(x="", y=value, fill=month)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()
    }
    
    else if (input$select_1 == 2) {
      # Pie chart: orders/buyer
      orders %>%
        group_by(buyer = Shipping.Address.Name) %>%
        summarize(value = sum(Item.Total)) %>%
        ggplot(aes(x="", y=value, fill=buyer)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()
    } 
    
    else if (input$select_1 == 3) {
      # Pie chart: orders/order-size
      orders %>%
        mutate(small_order = (Item.Total < 35)) %>%
        group_by(small_order) %>%
        summarize(value = n()) %>%
        ggplot(aes(x="", y=value, fill=small_order)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()
    }
    
  })
  
  output$total_spend <- renderText({
    sum(orders$Item.Total)
  })
  
  output$total_tax <- renderText({
    sum(orders$Item.Subtotal.Tax)
  })
  
  output$num_orders <- renderText({
    nrow(orders)
  })
  
  output$most_expensive <- renderText({
    max(orders$Item.Total)
  })
  
  output$should_get_yearly <- renderText({
    
    monthly_summary <- get_monthly_summary()
    # Number of prime worthy months in the last 1 year.
    last_year <- monthly_summary %>%
      filter(date_rounded >= (today() - years(1)) & date_rounded <= today())
    num_good_months <- sum(last_year$need_prime)
    
    # If we make use of prime for at least yearly_cutoff number of months, we are better off taking a yearly subscription.
    yearly_subscription_cost <- 119.0
    monthly_subscription_cost <- 12.99 
    yearly_cutoff <- (yearly_subscription_cost / monthly_subscription_cost)
    
    # Number of prime worhty months in the last 1 year.
    last_year <- monthly_summary %>%
      filter(date_rounded >= (today() - years(1)) & date_rounded <= today())
    num_good_months <- sum(last_year$need_prime)
    
    should_get_yearly <- num_good_months > yearly_cutoff
    if (should_get_yearly) {
      paste("YES")
    } else {
      paste("NO")
    }
    
  })
  
  
  output$num_prime_months <- renderText({
    
    monthly_summary <- get_monthly_summary()
    # Number of prime worthy months in the last 1 year.
    last_year <- monthly_summary %>%
      filter(date_rounded >= (today() - years(1)) & date_rounded <= today())
    num_good_months <- sum(last_year$need_prime)
    paste(num_good_months)
    
  })
  
  output$prime_top_months <- renderTable(digits=0, {
    # Top 3 prime worthy months.
    monthly_summary <- get_monthly_summary()
    monthly_summary %>%
      group_by(Month = month(date_rounded, label = TRUE)) %>%
      summarize(Count = sum(need_prime)) %>%
      arrange(desc(Count)) %>%
      slice(1:3)
  })
  
}

shinyApp(ui = ui, server = server)
