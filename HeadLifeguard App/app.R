#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(stringr)
library(shinyjs)
library(shiny)
library(googlesheets4)
library(googledrive)
library(gargle)
library(readr)

sheet_id <- "https://docs.google.com/spreadsheets/d/1EDRwPS3qhDyuGaU2eH-EeZpOmJEEKG5I3Je19uPywvs/edit#gid=0"

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .custom-background {
          background-color: #f5f5f5;
          padding: 10px;
          border-radius: 5px;
        }
        "
      )
    )
  ),
  titlePanel("Headguard Report"),
  sidebarLayout(
    sidebarPanel(
        class = "custom-background",
        selectInput("check_time", "Shift Time", choices = list(
          `School Year Times` = list(
            `5:30 AM - 7:45 AM` = c("05:00 AM","06:00 AM", "07:00 AM","08:00 AM"),
            `7:15 AM - 8:45 AM` = c("07:00 AM", "08:00 AM","09:00 AM"),
            `11:45 AM - 1:15 PM` = c("12:00 PM", "01:00 PM"),
            `12:15 PM - 4:45 PM` = c("12:00 PM", "01:00 PM", "02:00 PM", "03:00 PM", "04:00 PM"),
            `4:15 PM - 7:15 PM` = c("04:00 PM", "05:00 PM", "06:00 PM","07:00 PM"),
            `6:45 PM - 9:15 PM` = "07:00 PM, 08:00 PM, 09:00 PM"
          ),
          `Summer Times` = list(
            `7:15 AM - 9:45 AM` = c("07:00 AM", "08:00 AM", "09:00 AM","10:00 AM"),
            `9:15 AM - 12:15 PM` = c("09:00 AM","10:00 AM","11:00 AM"),
            `11:45 AM - 1:15 PM` = c("12:00 PM"),
            `2:45 PM - 6:15 PM` = c("03:00 PM","04:00 PM", "05:00 PM", "06:00 PM")
            
          )
        ), multiple = FALSE),
        numericInput("Comp1", "Number of Patrons in the East Competition Pool", value = 0),
        numericInput("Comp2", "Number of Patrons in the West Competition Pool", value = 0),
        numericInput("Dive", "Number of Patrons in the Dive Well", value = 0),
        numericInput("Spa", "Number of Patrons in the Spa", value = 0),
        actionButton("Update", "Submit Data"),
        textOutput("output")
      
    ),
    mainPanel(strong("Chemicals"),
      fluidRow(
        column(
          width = 3,
          class = "custom-background",
          checkboxInput("Alarm1", "Comp Pool In Alarm",value=FALSE),
          numericInput("Free Chlorine Comp", "Free Chlorine Comp", value = 2.0),
          numericInput("Combined Chlorine Comp", "Combined Chlorine Comp", value = 0),
          numericInput("pH Comp", "pH Comp", value = 7.4),
          numericInput("Temperature Comp", "Temperature Comp", value = 78),
          numericInput("Flow Rate Comp", "Flow Rate Comp", value = 2400),
          numericInput("ORP Comp", "ORP Comp", value = 842),
          
        ),
        column(
          width = 3,
          class = "custom-background",
          checkboxInput("Alarm2", "Dive Well In Alarm",value = FALSE),
          numericInput("Free Chlorine Dive", "Free Chlorine Dive", value = 1.8),
          numericInput("Combined Chlorine Dive", "Combined Chlorine Dive", value = 0),
          numericInput("pH Dive", "pH Dive", value = 7.4),
          numericInput("Temperature Dive", "Temperature Dive", value = 82),
          numericInput("Flow Rate Dive", "Flow Rate Dive", value = 580),
          numericInput("ORP Dive", "ORP Dive", value = 824)
        ),
        column(
          width = 3,
          class = "custom-background",
          checkboxInput("Alarm3", "Spa In Alarm",value = FALSE),
          numericInput("Free Chlorine Spa", "Free Chlorine Spa", value = 3.8),
          numericInput("Combined Chlorine Spa", "Combined Chlorine Spa", value = 0),
          numericInput("pH Spa", "pH Spa", value = 7.4),
          numericInput("Temperature Spa", "Temperature Spa", value = 98),
          numericInput("Flow Rate Spa", "Flow Rate Spa", value = 134),
          numericInput("ORP Spa", "ORP Spa", value = 780)
        )
      ),
      dataTableOutput("data_table")
    )
  )
)



server <- function(input, output,session) {
  
  options(
    gargle_oauth_cache = ".secrets",
    gargle_oauth_email = TRUE
  )
  
  drive_find(n_max = 5)
  data <- range_speedread(sheet_id)
  if (data[["Date"]][nrow(data)] != as.Date(Sys.Date())) {
    
    # Create a data frame with the desired values
    current_date <- Sys.Date()
    shift_time <- seq(from = as.POSIXct(paste(current_date, "05:00:00")), 
                      to = as.POSIXct(paste(current_date, "21:00:00")), 
                      by = "hour")
    shift_time <- format(shift_time, "%I:%M %p")  # Format time as 09:00 AM
    shift_time <- gsub("AM$", " AM", shift_time)  # Add space before AM
    shift_time <- gsub("PM$", " PM", shift_time)  # Add space before PM
    `East Competition Pool Patrons` <- rep(0, length(shift_time))
    `West Competition Pool Patrons` <- rep(0, length(shift_time))
    `Dive Well Patrons` <- rep(0, length(shift_time))
    `Hot Tub Patrons` <- rep(0, length(shift_time))
    `Total Patrons` <- rep(0, length(shift_time))
    `Free Chlorine Comp` <- rep(0, length(shift_time))
    `Combined Chlorine Comp` <- rep(0, length(shift_time))
    `pH Comp` <- rep(0, length(shift_time))
    `Temperature Comp` <- rep(0, length(shift_time))
    `Flow Rate Comp` <- rep(0, length(shift_time))
    `ORP Comp` <- rep(0, length(shift_time))
    `Free Chlorine Dive` <- rep(0, length(shift_time))
    `Combined Chlorine Dive` <- rep(0, length(shift_time))
    `pH Dive` <- rep(0, length(shift_time))
    `Temperature Dive` <- rep(0, length(shift_time))
    `Flow Rate Dive` <- rep(0, length(shift_time))
    `ORP Dive` <- rep(0, length(shift_time))
    `Free Chlorine Spa` <- rep(0, length(shift_time))
    `Combined Chlorine Spa` <- rep(0, length(shift_time))
    `pH Spa` <- rep(0, length(shift_time))
    `Temperature Spa` <- rep(0, length(shift_time))
    `Flow Rate Spa` <- rep(0, length(shift_time))
    `ORP Spa` <- rep(0, length(shift_time))
    `Comp Alarm` <- FALSE
    `Dive Alarm` <- FALSE
    `Spa Alarm` <- FALSE
    sheet <- data.frame(shift_time, Date = current_date, 
                       `East Competition Pool Patrons`, 
                       `West Competition Pool Patrons`, 
                       `Dive Well Patrons`, 
                       `Hot Tub Patrons`,
                       `Total Patrons`,
                       `Free Chlorine Comp`,
                       `Combined Chlorine Comp`,
                       `pH Comp`,
                       `Temperature Comp`,
                       `Flow Rate Comp`,
                       `ORP Comp`,
                       `Free Chlorine Dive`,
                       `Combined Chlorine Dive`,
                       `pH Dive`,
                       `Temperature Dive`,
                       `Flow Rate Dive`,
                       `ORP Dive`,
                       `Free Chlorine Spa`,
                       `Combined Chlorine Spa`,
                       `pH Spa`,
                       `Temperature Spa`,
                       `Flow Rate Spa`,
                       `ORP Spa`,
                       `Comp Alarm`,
                       `Dive Alarm`,
                       `Spa Alarm`
    )
    # Append the data frame to the existing Google Sheet
    sheet_append(sheet_id, sheet)
    
  } 
  sheet<-range_speedread(sheet_id)
  n<- as.numeric(Sys.Date())-19550
  last_row_index <- max(which(sheet[[1]] != ""))
  last_row_index<-last_row_index+1
  DateFormula<-last_row_index-(16)
  DateFormulaSolved<-(paste('A', DateFormula, sep = ''))
  observeEvent(input$Update, {
    check_date <- Sys.Date()
    user_date <- as.Date(Sys.Date())
    num_rows <- nrow(sheet)
    sheet <- sheet[((num_rows - 16):num_rows), ]

    sheet$`Shift Time` <- format(as.POSIXct(sheet$`Shift Time`, format = "%I:%M %p"), format = "%I:%M %p")

    
    cleaned_string <- gsub("[c\\(\\)\"]", "", input$check_time)
    split_strings <- strsplit(cleaned_string, ", ")[[1]]

    
    count <-0
    for (value in (sheet$`Shift Time`)) {
      shift_times <- split_strings
      count <-count + 1
      if (value %in% shift_times) {

        sheet$`West Competition Pool Patrons`[count] <- (input$Comp2/length(shift_times))
        sheet$`East Competition Pool Patrons`[count] <- input$Comp1/length(shift_times)
        sheet$`Dive Well Patrons`[count] <- input$Dive/length(shift_times)
        sheet$`Hot Tub Patrons`[count] <- input$Spa/length(shift_times)
        if ((input$Dive + input$Spa + input$Comp2 + input$Comp1) <= 1) {
          sheet$`Total Patrons`[count] <- ceiling(sum(input$Dive, input$Spa, input$Comp2, input$Comp1) / length(shift_times))
        } else {
          sheet$`Total Patrons`[count] <- round(sum(input$Dive, input$Spa, input$Comp2, input$Comp1) / length(shift_times))
        }
        sheet$`Free Chlorine Comp`[count] <- input$`Free Chlorine Comp`
        sheet$`Combined Chlorine Comp`[count] <- input$`Combined Chlorine Comp`
        sheet$`pH Comp`[count] <- input$`pH Comp`
        sheet$`Temperature Comp`[count] <- input$`Temperature Comp`
        sheet$`Flow Rate Comp`[count] <- input$`Flow Rate Comp`
        sheet$`ORP Comp`[count] <- input$`ORP Comp`
        sheet$`Free Chlorine Dive`[count] <- input$`Free Chlorine Dive`
        sheet$`Combined Chlorine Dive`[count] <- input$`Combined Chlorine Dive`
        sheet$`pH Dive`[count] <- input$`pH Dive`
        sheet$`Temperature Dive`[count] <- input$`Temperature Dive`
        sheet$`Flow Rate Dive`[count] <- input$`Flow Rate Dive`
        sheet$`ORP Dive`[count] <- input$`ORP Dive`
        sheet$`Free Chlorine Spa`[count] <- input$`Free Chlorine Spa`
        sheet$`Combined Chlorine Spa`[count] <- input$`Combined Chlorine Spa`
        sheet$`pH Spa`[count] <- input$`pH Spa`
        sheet$`Temperature Spa`[count] <- input$`Temperature Spa`
        sheet$`Flow Rate Spa`[count] <- input$`Flow Rate Spa`
        sheet$`ORP Spa`[count] <- input$`ORP Spa`
        sheet$`Comp Alarm`[count] <- input$Alarm1
        sheet$`Dive Alarm`[count] <- input$Alarm2
        sheet$`Spa Alarm`[count] <- input$Alarm3
      }
    }

    #output$data_table <- renderDataTable({sheet})
  
    #output$output <- renderText({DateFormulaSolved})
    range_write(sheet_id, sheet,sheet='DATA', range = DateFormulaSolved, col_names = FALSE)
    output$output <- renderText({'Submitted'})
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)
