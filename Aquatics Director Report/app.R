library(shiny)
library(googlesheets4)
library(tidyr)
library(lubridate)
library(dplyr)
library(gridExtra)
library(forecast)
library(shinydashboard)
library(ggplot2)
library(prophet)
library(Amelia)
library(padr)
library(plotly)

# Define Ui
ui <- dashboardPage(
  dashboardHeader(title = "Report Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Report", tabName = "data_report"),
      menuItem("Future Patrons Est", tabName = "future_patrons_est"),
      menuItem("Chemical Report", tabName = "Chemical Report")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data_report",
        fluidPage(
          titlePanel("Aquatics Center Report Data"),
          sidebarLayout(
            sidebarPanel(
              selectizeInput("x1", "Data Report", choices = list(
                `Type Of Report` = list(
                  `Last Week` = "Last Week",
                  Weekly = "Weekly",
                  `Last Month` = "Last Month",
                  Monthly = "Monthly",
                  `Last Year` = "Last Year",
                  Yearly = "Yearly",
                  `Complete Historical Report` = "Complete Historical Report"
                )
              ), multiple = FALSE),
              actionButton("Update", "Create Report")
            ),
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Aquatics Center Report",
                  fluidRow(
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("Aquatics Center Use"),
                        plotOutput(outputId = "Plot1")
                      )
                    )
                  ),
                  fluidRow(
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("Aquatics Center use by Shift Time"),
                        plotOutput(outputId = "Plot2")
                      )
                    ),
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("Aquatics Center Use by Day"),
                        plotOutput(outputId = "Plot4")
                      )
                    )
                  ),
                  fluidRow(
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("Spa Use by Shift Time"),
                        plotOutput(outputId = "Plot5")
                      )
                    ),
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("Dive Well use by Shift Time"),
                        plotOutput(outputId = "Plot6")
                      )
                    )
                  ),
                  fluidRow(
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("West Competition Pool Use by Shift Time"),
                        plotOutput(outputId = "Plot7")
                      )
                    ),
                    div(
                      class = "title-container",
                      column(
                        width = 12,
                        h3("East Competition Pool Use By Shift Time"),
                        plotOutput(outputId = "PlotComp")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "future_patrons_est",
        fluidPage(
          titlePanel("Future Patrons Estimation"),
          sidebarLayout(
            sidebarPanel(
              selectizeInput(
                "x2",
                "Future Patrons Estimation Option",
                choices = c("1 Week", "1 Month", "1 Semester","1 Year"),
                multiple = FALSE
              ),
              actionButton("Generate", "Generate Report")
            ),
            mainPanel(
              h3("Prediced Total Aquatics Center Patrons"),
              div(
                style = "overflow-y: auto; max-height: 400px; background-color: white;",
                tableOutput(outputId = "Table1")
              ),
              h3("Predicted Total Aquatics Center Patrons"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotlyOutput("Plot8")
              ),
              h3("Total Aquatic Center Trends"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotOutput(outputId = "PlotStats")
              ),
              h3("Predicted Spa Center Patrons"),
              div(
                style = "overflow-y: auto; max-height: 400px; background-color: white;",
                tableOutput(outputId = "Table2")
              ),
              h3("Predicted Spa Patrons"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotlyOutput("Plot9")
              ),
              h3("Spa Trends"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotOutput(outputId = "PlotStatsSpa")
              ),
              h3("Predicted Dive Well Patrons"),
              div(
                style = "overflow-y: auto; max-height: 400px; background-color: white;",
                tableOutput(outputId = "Table3")
              ),
              h3("Predicted Dive Well Patrons"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotlyOutput("Plot10")
              ),
              h3("Dive Well Trends"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotOutput(outputId = "PlotStatsDive")
              ),
              h3("Predicted East Comp Pool Patrons"),
              div(
                style = "overflow-y: auto; max-height: 400px; background-color: white;",
                tableOutput(outputId = "Table4")
              ),
              h3("Predicted East Comp Pool Patrons"),
              div(
                style = "overflow-y: auto; background-color:white;",
                plotlyOutput("Plot11")
              ),
              h3("East Comp Trends"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotOutput(outputId = "PlotStatsComp")
              ),
              h3("Predicted West Comp Pool Patrons"),
              div(
                style = "overflow-y: auto; max-height: 400px; background-color: white;",
                tableOutput(outputId = "Table5")
              ),
              h3("Predicted West Comp Pool Patrons"),
              div(
                style = "overflow-y: auto; background-color:white;",
                plotlyOutput("Plot12")
              ),
              h3("West Comp Trends"),
              div(
                style = "overflow-y: auto; background-color: white;",
                plotOutput(outputId = "PlotStatsComp2")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'Chemical Report',
        fluidPage(
          titlePanel("Chemicals")
        )
      )
    )
  )
)




  

# Define server logic 
server <- function(input, output) {
  
  #Define start date variable and gooogle sheet link
  start_date = 0
  sheet_id <- "https://docs.google.com/spreadsheets/d/1EDRwPS3qhDyuGaU2eH-EeZpOmJEEKG5I3Je19uPywvs/edit#gid=0"
  
  #Authenticate with googlesheets allowing the following email to access the google sheet.
  gs4_auth(email = "dy_o1@denison.edu")
  
  #run the following code when a button is pressed
  observeEvent(input$Update,{
    
  #Read in the google sheet
  data <-range_speedread(sheet_id)
  
  #based of the user input defined by x1 filter and summarize the data accordingly
  if (input$x1 == "Last Week") {
    start_date <- Sys.Date() - 7
    end_date <- 7
    data <- data[as.Date(data$Date) >=start_date, , drop=FALSE]
    
  } 
  else if (input$x1 == "Weekly") {
    data <- data %>%
      mutate(Date = floor_date(Date, "week")) %>%
      group_by(Date, `Shift Time`) %>%
      summarize(`East Competition Pool Patrons` = (sum(`East Competition Pool Patrons`)/7),
                `Dive Well Patrons` = (sum(`Dive Well Patrons`)/7),
                `Hot Tub Patrons` = (sum(`Hot Tub Patrons`)/7),
                `Proportion_Over_Time` = (sum(`Total Patrons`)/7)) %>%
      distinct(Date, `Shift Time`, .keep_all = TRUE)
    end_date <- 7
  }
  else if (input$x1 == "Last Month") {
    start_date <- Sys.Date() - 30.437
    end_date <- 30.437
    data <- data[as.Date(data$Date) >=start_date, , drop=FALSE]
    
  }
  else if (input$x1 == "Monthly") {
    data <- data %>%
      mutate(Date = floor_date(Date, "month")) %>%
      group_by(Date, `Shift Time`) %>%
      summarize(`East Competition Pool Patrons` = (sum(`East Competition Pool Patrons`)/30.437
),
                `Dive Well Patrons` = (sum(`Dive Well Patrons`)/30.437
),
                `Hot Tub Patrons` = (sum(`Hot Tub Patrons`)/30.437
),
                `Proportion_Over_Time` = (sum(`Total Patrons`)/30.437
)) %>%
      distinct(Date, `Shift Time`, .keep_all = TRUE)
    end_date <- 30}
  else if (input$x1 == "Last Year") {
    start_date <- Sys.Date() - 365
    end_date <- 365
    data <- data[as.Date(data$Date) >=start_date, , drop=FALSE]
    
  } 
  else if (input$x1 == "Yearly") {
    data <- data %>%
      mutate(Date = floor_date(Date, "year")) %>%
      group_by(Date, `Shift Time`) %>%
      summarize(`East Competition Pool Patrons` = (sum(`East Competition Pool Patrons`)/365),
                `Dive Well Patrons` = (sum(`Dive Well Patrons`)/365),
                `Hot Tub Patrons` = (sum(`Hot Tub Patrons`)/365),
                `Proportion_Over_Time` = (sum(`Total Patrons`)/365)) %>%
      distinct(Date, `Shift Time`, .keep_all = TRUE)
    end_date <- 365}
  #This returns the data from the entirety of the sheet.
  else {
    start_date <-  ("2013-1-1")  # Complete Historical Report
    end_date<- "Unknown amount of"
    data <- data[as.Date(data$Date) >=start_date, , drop=FALSE]
    data<- na.omit(data)
    
  }
  
  
  

  {
    #Calculate additional proportion columns
    data$Proportion_Over_Time <- data$`Total Patrons` / sum(data$`Total Patrons`)
    data$Proportion_Spa <- data$`Hot Tub Patrons` / sum(data$`Hot Tub Patrons`)
    data$Proportion_Dive <- data$`Dive Well Patrons` / sum(data$`Dive Well Patrons`)
    data$Proportion_EastComp <- data$`East Competition Pool Patrons` / sum(data$`East Competition Pool Patrons`)
    data$Proportion_WestComp <- data$`West Competition Pool Patrons` / sum(data$`West Competition Pool Patrons`)
    
    # Summarize the data further for the proportion plots
    df_proportion <- data[,-3:-6]
    
    df_proportion <- df_proportion %>%
      mutate_if(is.numeric, ~ ifelse(is.nan(.), 0, .))
    
    df_proportion <- df_proportion %>%
      group_by(`Shift Time`) %>%
      summarize(Total = sum(Proportion_Spa, Proportion_Dive, Proportion_EastComp)) %>%
      distinct(`Shift Time`, .keep_all = TRUE) }
  
  #Prepare a long format dataframe for plotting
  df_long <- gather(data, key = "group", value = "value", -Date)
  df_long <- df_long[-(1:11),]
  df_long$value <- as.integer(df_long$value)
  
  # Compute the means of the columns
  means <- aggregate(data[, c("East Competition Pool Patrons", "West Competition Pool Patrons", "Dive Well Patrons", "Hot Tub Patrons", "Total Patrons")], by = list(Date = data$Date), mean)
  
  # Create a new data frame with the means and date column
  new_df <- data.frame(Date = means$Date, `West Comp Pool Patrons`=means$`West Competition Pool Patrons`,`East Comp Pool Patrons` = means$`East Competition Pool Patrons`, `Dive Well Patrons` = means$`Dive Well Patrons`, `Spa Patrons` = means$`Hot Tub Patrons`,`Total Patrons` = means$`Total Patrons`)    # View the new data frame
  df_long2 <- gather(new_df, key = "group", value = "value", -Date)
  df_long2$value <- as.integer(df_long2$value) 
  

  #Prepare filtered dataframe for plotting based on selected time range
  df_long2_filtered <- df_long2[as.Date(df_long2$Date) >= start_date, , drop = FALSE]
  data$Date <- as.Date(data$Date)
  data$Weekday <- weekdays(data$Date)
  
  #Render all plots
  output$Plot1 <- renderPlot({
    df_long2_filtered <- df_long2_filtered %>%
      mutate(value = ifelse(is.na(value), 0, value))
    
    ggplot(df_long2_filtered, aes(x = as.Date(Date), y = value, color = group, shape = group)) +
      geom_smooth(method = "lm", se = FALSE) +
      ylim(0, NA) +
      labs(x = "Date", y = "Mean Patrons") +
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8)
      ) +
      ggtitle("Patrons Over Time")
  })
  output$Plot2<-renderPlot({
    data<- na.omit(data)
    ggplot(data, aes(x = `Shift Time`, y = (`Total Patrons` / sum(`Total Patrons`)), fill = as.factor(`Shift Time`))) +
      geom_col() +
      ylab("% Patrons") +
      xlab("Shift Time") +
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 8)
      ) +
      guides(fill = FALSE) +
      ggtitle("Aquatics Center use by Shift Time")
    
    
  })
  output$Plot4 <- renderPlot({
    if (!(input$x1 %in% c("Weekly", "Monthly", "Yearly"))) {
      data<- na.omit(data)
      ggplot(data, aes(x = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                       y = `Proportion_Over_Time`, 
                       fill = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))) +
        geom_col() +
        scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 8)
        ) +
        labs(title = "Aquatics Center use by Day", x = "Day", y = "% Visitors", fill = "Day of Week")
    } else {
      NULL
    }
  })
  output$Plot5<-renderPlot({
    data<- na.omit(data)
    
    ggplot(data, aes(x = `Shift Time`, y = (`Hot Tub Patrons`/sum(`Hot Tub Patrons`)), fill = as.factor(`Shift Time`))) +
      geom_col() +
      ylab('% Patrons') +
      xlab("Shift Time")+
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 8)
      ) +
      guides(fill = FALSE)+ ggtitle("Spa use by Shift Time")
  })
  output$Plot6<-renderPlot({
    data<- na.omit(data)
    
    ggplot(data, aes(x = `Shift Time`, y = (`Dive Well Patrons`/sum(`Dive Well Patrons`)), fill = as.factor(`Shift Time`))) +
      geom_col() +
      ylab('% Patrons') +
      xlab("Shift Time")+
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 8)
      ) +
      guides(fill = FALSE)+ ggtitle("Dive Well use by Shift Time")
  })
  output$Plot7<-renderPlot({
    data<- na.omit(data)
    
    ggplot(data, aes(x = `Shift Time`, y = `West Competition Pool Patrons`/sum(`West Competition Pool Patrons`), fill = as.factor(`Shift Time`))) +
      geom_col() +
      ylab('% Patrons') +
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 8)
      ) +
      guides(fill = FALSE)+ ggtitle(" West Competition Pool Use by Shift Time")
  })
  output$PlotComp<-renderPlot({
    data<- na.omit(data)
    
    ggplot(data, aes(x = `Shift Time`, y = `East Competition Pool Patrons`/sum(`East Competition Pool Patrons`), fill = as.factor(`Shift Time`))) +
      geom_col() +
      ylab('% Patrons') +
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 8)
      ) +
      guides(fill = FALSE)+ ggtitle(" East Competition Pool Use by Shift Time")
  })
  })
  
  #Load in data from google sheet
  sheet<-range_speedread(sheet_id)
  observeEvent(input$Generate,{
      #Load in data from google sheet
      data <-range_speedread(sheet_id)
      
      #transform data into long format
      df_long <- gather(data, key = "group", value = "value", -Date)
      df_long <- df_long[-(1:11),]
      df_long$value <- as.integer(df_long$value) 
      
      
      {
        # Compute the means of the columns
        means <- aggregate(data[, c("West Competition Pool Patrons","East Competition Pool Patrons", "Dive Well Patrons", "Hot Tub Patrons", "Total Patrons")], by = list(Date = data$Date), mean)
        
        # Create a new data frame with the means and date column
        new_df <- data.frame(Date = means$Date, `Comp Pool Patrons` = means$`East Competition Pool Patrons`, `Dive Well Patrons` = means$`Dive Well Patrons`, `Spa Patrons` = means$`Hot Tub Patrons`,`Total Patrons` = means$`Total Patrons`)  
        
        #Get new data into long format
        df_long2 <- gather(new_df, key = "group", value = "value", -Date)
        df_long2$value <- as.integer(df_long2$value) 
      }
      {
        
        # Convert the 'Date' column in the 'data' dataframe to Date format
        data$Date <- as.Date(data$Date)
        
        # Extract the weekday from the 'Date' column and store it in a new column 'Weekday'
        data$Weekday <- weekdays(data$Date)
        
        # Determine the prediction length based on the user input 'x2' using conditional statements
        if (input$x2 == "1 Week") {
          Pred_Length <- 7
        } else if (input$x2 == "1 Month") {
          Pred_Length <- 30
        } else if (input$x2 == "1 Semester") {
          Pred_Length <- 75
        } else if (input$x2 == "1 Year") {
          Pred_Length <- 365
        }
        
        # Combine 'Date' and 'Shift Time' columns into a single 'Date' column in the 'sheet' dataframe
        UnitedSheet <- unite(sheet, Date, c(Date, `Shift Time`))
        
        # Remove rows with NA values in the 'Date' column from 'UnitedSheet'
        UnitedSheet$Date <- na.omit(UnitedSheet$Date)
        
        # Convert the 'Date' column in 'UnitedSheet' to POSIXct format with timezone as "UTC"
        UnitedSheet$Date <- ymd_hms(UnitedSheet$Date, tz = "UTC")
        
        # Pad the time intervals in 'UnitedSheet' to have hourly time points
        PaddedSheet <- pad(UnitedSheet, "hour")
        
        # Convert the 'Date' column in 'PaddedSheet' to POSIXct format with format "%Y-%m-%d %H:%M:%S"
        PaddedSheet$Date <- as.POSIXct(PaddedSheet$Date, format = "%Y-%m-%d %H:%M:%S")
        
        # Define a vector 'time_to_remove' containing specific times to be removed from the 'data' dataframe
        time_to_remove <- c("22:00:00", "23:00:00", "00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00")
        
        # Remove rows with the specified times from the 'data' dataframe
        data <- PaddedSheet[!(format(PaddedSheet$Date, "%H:%M:%S") %in% time_to_remove), ]
        
        # Perform data manipulation using dplyr to create the 'filtered_dataset' dataframe
        filtered_dataset <- data %>%
          mutate(shift_time = sprintf("%02d:00:00", hour(Date)),   # Formats the hour part with leading zeros
                 Date = as.Date(Date)) %>%                        # Extracts only the date part (year-month-day)
          select(shift_time, Date, `East Competition Pool Patrons`, `West Competition Pool Patrons`, `Dive Well Patrons`, `Hot Tub Patrons`, `Total Patrons`)
        
        # Group the 'filtered_dataset' dataframe by 'Date' and calculate the sum of 'Total Patrons' for each date
        data3 <- filtered_dataset[, c(2, 7)] %>%
          group_by(Date) %>%
          summarise(`Total Patrons` = sum(`Total Patrons`))
        
        # Convert 'data3' to a data frame
        data3 <- as.data.frame(data3)
        
        # Perform multiple imputation on 'data3' using the 'amelia' function from the amelia package
        aq_imp <- amelia(data3)
        aq_imp_data <- aq_imp$imputations
        aq_imp_data_list <- lapply(aq_imp_data, as.data.frame)
        
        # Extract the fifth imputed dataset from the list of imputations and round the 'Total Patrons' values to integers
        data4 <- aq_imp_data$imp5
        data4$`Total Patrons` <- round(data4$`Total Patrons`, digits = 0)
        
        # Group the 'data4' dataframe by 'Date' and calculate the sum of 'Total Patrons' for each date
        result <- data4 %>%
          group_by(Date) %>%
          summarise(`Total Patrons` = sum(`Total Patrons`))
        
        # Create new columns 'ds' and 'y' in the 'result' dataframe using the 'mutate' function
        result <- mutate(
          result,
          ds = Date,          # Create a new column 'ds' using the 'Date' column
          y = `Total Patrons` # Create a new column 'y' using the 'Total Patrons' column
        )
        
        # Perform time series forecasting using the 'prophet' function from the prophet package
        m <- prophet(result)
        future <- make_future_dataframe(m, periods = Pred_Length, include_history = TRUE)
        forecast <- predict(m, future)
        
        # Convert 'forecast$ds' to Date format and format it as "YYYY-MM-DD"
        forecast$ds <- as.Date(forecast$ds)
        forecast$ds <- format(forecast$ds, "%Y-%m-%d")
        
        # Render the forecast results as a table and interactive plot 
        output$Table1 <- renderTable({
          tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], Pred_Length)
        })
        output$Plot8 <- renderPlotly({
          plot_ly(forecast, x = ~ds, y = ~yhat, type = "scatter", mode = "lines")
        })
        output$PlotStats <- renderPlot({
          prophet_plot <- prophet::prophet_plot_components(m, forecast)
        })
        
      
      #Spa
        #see above annotations for explanation of code
      time_to_remove2 <- c("22:00:00", "23:00:00", "00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00")
      
      data <- PaddedSheet[!(format(PaddedSheet$Date, "%H:%M:%S") %in% time_to_remove2), ]
      
      
      data32 <- filtered_dataset[, c(2, 6)] %>%
        group_by(Date) %>%
        summarise(`Hot Tub Patrons` = sum(`Hot Tub Patrons`))
      
      data32<-as.data.frame(data32)
      aq_imp2 <- amelia(data32)
      aq_imp_data2 <- aq_imp2$imputations
      aq_imp_data_list2 <- lapply(aq_imp_data2, as.data.frame)
      data42<-(aq_imp_data2$imp5)
      data42$`Hot Tub Patrons`<- round(data42$`Hot Tub Patrons`, digits = 0)
      
      #end of imputations
      
      
      result2 <- data42 %>%
        group_by(Date) %>%
        summarise(`Hot Tub Patrons` = sum(`Hot Tub Patrons`))
      
      result2 <- mutate (
        result2,
        ds = Date,  # Create new ds column from date using mutate
        y = `Hot Tub Patrons`   # Create new y column from value using mutate
      )
      
      m2 <- prophet(result2)
      future2 <- make_future_dataframe(m2, periods = Pred_Length, include_history = TRUE)
      
      forecast2 <- predict(m2, future2)
      
      # Render the forecast results as a table and interactive plot 
      output$Table2 <- renderTable({
        forecast2$ds <- as.Date(forecast2$ds)
        forecast2$ds <- format(forecast2$ds, "%Y-%m-%d") 
        tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],Pred_Length)
      })
      output$Plot9 <- renderPlotly({
        plot(m2, forecast2, ylim = c(0, max(forecast2$y)))
        
  })
      output$PlotStatsSpa <- renderPlot({
        prophet_plot <- prophet::prophet_plot_components(m2, forecast2)        
        
      })
      
      #Dive
      #see #total for annotations for explanation of code
      
      time_to_remove3 <- c("22:00:00", "23:00:00", "00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00")
      
      # Remove rows with the specified times
      data3 <- PaddedSheet[!(format(PaddedSheet$Date, "%H:%M:%S") %in% time_to_remove3), ]
      
      filtered_dataset3 <- data3 %>%
        mutate(shift_time = sprintf("%02d:00:00", hour(Date)), # Formats the hour part with leading zeros
               Date = as.Date(Date)) %>%                      # Extracts only the date part (year-month-day)
        select(shift_time, Date, `East Competition Pool Patrons`, `West Competition Pool Patrons`,`Dive Well Patrons`,`Hot Tub Patrons`,`Total Patrons`)   
      
      data33 <- filtered_dataset3[, c(2, 5)] %>%
        group_by(Date) %>%
        summarise(`Dive Well Patrons` = sum(`Dive Well Patrons`))
      
      # Display the resulting summarized data frame
      data33<-as.data.frame(data33)
      
      aq_imp3 <- amelia(data33)
      aq_imp_data3 <- aq_imp3$imputations
      aq_imp_data_list3 <- lapply(aq_imp_data3, as.data.frame)
      data43<-(aq_imp_data3$imp5)
      data43$`Dive Well Patrons3`<- round(data43$`Dive Well Patrons`, digits = 0)
      
      #end of imputations
      
      
      result3 <- data43 %>%
        group_by(Date) %>%
        summarise(`Dive Well Patrons` = sum(`Dive Well Patrons`))
      
      result3 <- mutate (
        result3,
        ds = Date,  # Create new ds column from date using mutate
        y = `Dive Well Patrons`   # Create new y column from value using mutate
      )
      
      m3 <- prophet(result3)
      future3 <- make_future_dataframe(m3, periods = Pred_Length, include_history = TRUE)
      
      forecast3 <- predict(m3, future3)
      
      # Render the forecast results as a table and interactive plot 
      output$Table3 <- renderTable({
        forecast3$ds <- as.Date(forecast3$ds)
        forecast3$ds <- format(forecast3$ds, "%Y-%m-%d") 
        tail(forecast3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],Pred_Length)
      })
      output$Plot10 <- renderPlotly({
        plot(m3, forecast3, ylim = c(0, max(forecast3$y)))
        
      })
      output$PlotStatsDive <- renderPlot({
        prophet_plot <- prophet::prophet_plot_components(m3, forecast3)        
        
      })
      
      #East
      #see #total's annotations for explanation of code
      
      time_to_remove <- c("22:00:00", "23:00:00", "00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00")
      
      # Remove rows with the specified times
      data4 <- PaddedSheet[!(format(PaddedSheet$Date, "%H:%M:%S") %in% time_to_remove), ]
      
      filtered_dataset4 <- data4 %>%
        mutate(shift_time = sprintf("%02d:00:00", hour(Date)), # Formats the hour part with leading zeros
               Date = as.Date(Date)) %>%                      # Extracts only the date part (year-month-day)
        select(shift_time, Date, `East Competition Pool Patrons`, `West Competition Pool Patrons`,`Dive Well Patrons`,`Hot Tub Patrons`,`Total Patrons`)   
      
      data3_4 <- filtered_dataset4[2:3] %>%
        group_by(Date) %>%
        summarise(`East Competition Pool Patrons` = sum(`East Competition Pool Patrons`))
      
      # Display the resulting summarized data frame
      data3_4<-as.data.frame(data3_4)
      
      aq_imp4 <- amelia(data3_4)
      aq_imp_data4 <- aq_imp4$imputations
      aq_imp_data_list4 <- lapply(aq_imp_data4, as.data.frame)
      data44<-(aq_imp_data4$imp5)
      data44$`East Competition Pool Patrons`<- round(data44$`East Competition Pool Patrons`, digits = 0)
      
      #end of imputations
      
      
      result4 <- data44 %>%
        group_by(Date) %>%
        summarise(`East Competition Pool Patrons` = sum(`East Competition Pool Patrons`))
      
      result4 <- mutate (
        result4,
        ds = Date,  # Create new ds column from date using mutate
        y = `East Competition Pool Patrons`   # Create new y column from value using mutate
      )
      
      m4 <- prophet(result4)
      future4 <- make_future_dataframe(m4, periods = Pred_Length, include_history = TRUE)
      
      forecast4 <- predict(m4, future4)
      
      # Render the forecast results as a table and interactive plot 
      output$Table4 <- renderTable({
        forecast4$ds <- as.Date(forecast4$ds, format = "%Y-%m-%d")
        
        tail(forecast4[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],Pred_Length)
        
        
        tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],Pred_Length)
      })
      output$Plot11 <- renderPlotly({
        plot(m4, forecast4, ylim = c(0, max(forecast4$y)))
        
      })
      output$PlotStatsComp <- renderPlot({
        prophet_plot <- prophet::prophet_plot_components(m4, forecast4)        
        
      })
      
      #West
      #see #total's annotations for explanation of code
      
      # Times to remove
      time_to_remove <- c("22:00:00", "23:00:00", "00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00")
      
      # Remove rows with the specified times
      data5 <- PaddedSheet[!(format(PaddedSheet$Date, "%H:%M:%S") %in% time_to_remove), ]
      
      filtered_dataset5 <- data5 %>%
        mutate(shift_time = sprintf("%02d:00:00", hour(Date)), # Formats the hour part with leading zeros
               Date = as.Date(Date)) %>%                      # Extracts only the date part (year-month-day)
        select(shift_time, Date, `East Competition Pool Patrons`, `West Competition Pool Patrons`,`Dive Well Patrons`,`Hot Tub Patrons`,`Total Patrons`)   
      
      data35 <- filtered_dataset5[, c(2, 4)] %>%
        group_by(Date) %>%
        summarise(`West Competition Pool Patrons` = sum(`West Competition Pool Patrons`))
      
      # Display the resulting summarized data frame
      data35<-as.data.frame(data35)
      
      aq_imp5 <- amelia(data35)
      aq_imp_data5 <- aq_imp5$imputations
      aq_imp_data_list5 <- lapply(aq_imp_data5, as.data.frame)
      data45<-(aq_imp_data5$imp5)
      data45$`West Competition Pool Patrons`<- round(data45$`West Competition Pool Patrons`, digits = 0)
      
      #end of imputations
      
      
      result5 <- data45 %>%
        group_by(Date) %>%
        summarise(`West Competition Pool Patrons` = sum(`West Competition Pool Patrons`))
      
      result5 <- mutate (
        result5,
        ds = Date,  # Create new ds column from date using mutate
        y = `West Competition Pool Patrons`   # Create new y column from value using mutate
      )
      
      m5 <- prophet(result5)
      future5 <- make_future_dataframe(m5, periods = Pred_Length, include_history = TRUE)
      
      # Render the forecast results as a table and interactive plot 
      forecast5 <- predict(m5, future5)
      output$Table5<-renderTable({
        forecast5$ds <- as.Date(forecast5$ds)
        forecast5$ds <- format(forecast5$ds, "%Y-%m-%d") 
        tail(forecast5[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],Pred_Length)
      })
      output$Plot12 <- renderPlotly({
        plot(m5, forecast5, ylim = c(0, max(forecast5$y)))
        
      })
      output$PlotStatsComp2 <- renderPlot({
        prophet_plot <- prophet::prophet_plot_components(m5, forecast5)        
        
      })
      
  
}})}
# Run the application 
shinyApp(ui = ui, server = server)
