#### Reference Guides ####
#
# https://r4ds.had.co.nz/
# https://cprobe.army.mil/rsconnect/CAA_Intro_to_R/
# https://mastering-shiny.org/
#

# This code is broken into 5 sections:
# 1. Libraries and Reading the data - moved to Import.R file
# 2. Tidying the data
# 3. User Interface (UI)
# 4. Server 
# 5. Plotting Data

# The DISA GitLab URL to pull from is: https://gitlab.devforce.disa.mil/rhowell/t2t.git


#### 1. Libraries and Reading the Data ####
# Libraries and Reading in data are moved to Import.R file #

source("Import.R") ### This is a separate file which Imports the Libraries and Files ###


#### 2. Tidying the data ####

T2T <- T2T_test7 %>%
  full_join(T2T_Input_Historic_FY21) %>%
  left_join(Roster, by ="Name") %>%
  filter(!is.na(Name)) %>%
  mutate(`Start Date` = Deploy,
         `End Date` = Redeploy) %>%
  mutate(as.Date(`Start Date`)) %>%
  mutate(as.Date(`End Date`)) %>%
  mutate(Duration = difftime(`End Date`, `Start Date`, units="days") + 1)

T2T$Name <- factor(T2T$Name) %>%    # this sorts the data by Name for the OPTEMPO chart 
  fct_reorder(T2T$Duration)


#### 3. UI ####

ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard",
     dropdownMenu(type = "messages",
         messageItem(
         from = "Content Manager POC:",
         message = "ryan.l.howell7.mil@army.mil")
  )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
      menuItem("List", tabName = "tab_list", icon = icon("th")),
      menuItem("Troop To Task", tabName = "tab_T2T", icon = icon("address-card")),
      menuItem("Support Map", tabName = "tab_map", icon = icon("globe")),
      hr(),     # horizontal rule - creates a line break
      checkboxGroupInput("tab_division", "Division", choices = unique(Roster$Division), selected = "Exercise"),
      dateRangeInput("tab_daterange", "Date Range", start = today(), format = "mm-dd-yy", end = today() + ddays(90), startview = "month"),
      selectInput("tab_team", "Team", choices = unique(T2T$Team), selected = unique(T2T$Team), multiple = TRUE),      
      selectInput("tab_task", "Task", choices = unique(T2T$Task), selected = unique(T2T$Task), multiple = TRUE),
      h5(str_wrap(paste("Last update was", format(file.mtime("T2T_Input.xlsx"), "%b %d, %Y at %H:%m"), sep = " ")))
  )),
  
  dashboardBody(
    
    fluidRow(box("CUI", width = 12, align="center", background = "purple")),   # Mandatory for RMF
      
     tabItems(
       
      # First tab 
      tabItem(tabName = "tab_dashboard",
            fluidRow(
              box(tableOutput("people_count"), width = 3),
              tabBox(title = "OPTEMPO", side = "right", width = 6, selected = "Total Days",
                tabPanel("Weekend Days", plotOutput("people_weekend")),
                tabPanel("TDY Days", plotOutput("people_tdy")),
                tabPanel("Total Days", plotOutput("people_bar"))),
              box(tableOutput("location_count"), width = 3)),
              fluidRow(box(plotOutput("gantt"), width = 12))
            ),
      
      # Second tab
      tabItem(tabName = "tab_list",
              fluidRow(
              autoWaiter(),
              waiterPreloader(),
                checkboxInput("show_participants", "Show Participants", value = TRUE)),
              fluidRow(    
                box(dataTableOutput("dt"), width = 12))
          ),
      
      # Third tab
      tabItem(tabName = "tab_T2T",
              autoWaiter(),
              waiterPreloader(),
              fluidRow(plotOutput("troop_to_task")),
              fluidRow(
                autoWaiter(),
                waiterPreloader(),
                box(tableOutput("troop_to_task_avail")),
                column(6, 
                       numericInput("no_months", "Number of Months to Print", 3),
                       checkboxInput("check_div", "Check to print Division Events", value = TRUE),
                       downloadButton("download_pg3", "Generate printable Event Calendar"),
                       br(),
                       ("Click once and practice digital patience")))
              ),
       
      # Forth tab       
      tabItem(tabName = "tab_map",
              fluidRow(
                box(leafletOutput("map1", height = 700), width = 12)),
      )),
      
      fluidRow(box("CUI", width = 12, align="center", background = "purple"))     # Mandatory for RMF
))

#### 4. Server ####

server <- function(input, output, session) { 

# Mandatory for RMF

  observe({ shinyalert(
  
"You are accessing a U.S. Government (USG) Information System (IS) that is provided for USG-authorized use only.", "Mandatory DoD Notice and Consent

By using this IS (which includes any device attached to this IS), you consent to the following conditions:

The USG routinely intercepts and monitors communications on this IS for purposes including, but not limited to, penetration testing, COMSEC monitoring, network operations and defense, personnel misconduct (PM), law enforcement (LE), and counterintelligence (CI) investigations.
At any time, the USG may inspect and seize data stored on this IS.
Communications using, or data stored on, this IS are not private, are subject to routine monitoring, interception, and search, and may be disclosed or used for any USG authorized purpose.
This IS includes security measures (e.g., authentication and access controls) to protect USG interests - not for your personal benefit or privacy.
Notwithstanding the above, using this IS does not constitute consent to PM, LE or CI investigative searching or monitoring of the content of privileged communications, or work product, related to personal representation or services by attorneys, psychotherapists, or clergy, and their assistants. Such communications and work product are private and confidential. See User Agreement for details.
Acknowledgement Of Responsibilities Of Receiving And Maintaining Privacy Act Data
DATA YOU ARE ABOUT TO ACCESS COULD POTENTIALLY BE PROTECTED BY THE PRIVACY ACT OF 1974. You must:

Have completed the necessary training with regards to Security Awareness and safe-guarding Personally Identifiable Information.
Ensure that data is not posted, stored or available in any way for uncontrolled access on any media.
Ensure that data is protected at all times as required by the Privacy Act of 1974 (5 USC 552a(I)(3)) as amended and other applicable DOD regulatory and statutory authority; data will not be shared with offshore contractors; data from the application, or any information derived from the application, shall not be published, disclosed, released, revealed, shown, sold, rented, leased or loaned to anyone outside of the performance of official duties without prior DMDC approval.
Delete or destroy data from downloaded reports upon completion of the requirement for their use on individual projects.
Ensure data will not be used for marketing purposes.
Ensure distribution of data from a DMDC application is restricted to those with a need-to-know. In no case shall data be shared with persons or entities that do not provide documented proof of a need-to-know.
Be aware that criminal penalties under section 1106(a) of the Social Security Act (42 USC 1306(a)), including possible imprisonment, may apply with respect to any disclosure of information in the application(s) that is inconsistent with the terms of application access. The user further acknowledges that criminal penalties under the Privacy Act (5 USC 552a(I)(3)) may apply if it is determined that the user has knowingly and willfully obtained access to the application(s) under false pretenses.", 
    type = "warning")
  })
  

  # Updates the task after the user changes the date range
  observeEvent(input$tab_daterange, {
    updateSelectInput(inputId = "tab_task",
                      choices = unique(T2T$Task[T2T$`Start Date` <= input$tab_daterange[2] &
                                                T2T$`End Date` >= input$tab_daterange[1]]), selected = unique(T2T$Task))
  })
  
  # Updates the Team when the user updates the Division
  observeEvent(input$tab_division, {
    updateSelectInput(inputId = "tab_team", 
                      choices = unique(T2T$Team[T2T$Division %in% input$tab_division]), selected = unique(T2T$Team)) 
  })

  # The primary variable which all charts branch   
  T2T_master <-  reactive({
    T2T %>%
      filter(Task %in% c(input$tab_task))  %>%
      filter(Division %in% input$tab_division) %>%
      filter(`Start Date` <= c(input$tab_daterange[2]) & `End Date` >= c(input$tab_daterange[1])) %>%
      filter(Team %in% c(input$tab_team)) %>%
      filter(!is.na(Event)) %>%
      dplyr::select(Event, Task, `Duty Location`, `Start Date`, `End Date`, Name, `Pay System`, Duration, Division) %>%
      distinct()
  }) 

  # The primary variable which all tables branch    
  T2T_table <- reactive({
    if (input$show_participants == TRUE) {
      T2T_list <- T2T_master() %>%
        dplyr::select(Event, Task, `Duty Location`, `Start Date`, `End Date`, Name, `Pay System`, Duration) %>%
        arrange(`Start Date`) %>%
        mutate(`End Date` = str_remove(`End Date`, "\\.T*$")) %>%
        mutate(`Start Date` = str_remove(`Start Date`, "\\.T*$")) 
    } else {
      T2T_list <- dplyr::select(T2T_master(), Event, Task, `Duty Location`, `Start Date`, `End Date`) %>%
        arrange(`Start Date`) %>%
        mutate(`End Date` = str_remove(`End Date`, "\\.T*$")) %>%
        mutate(`Start Date` = str_remove(`Start Date`, "\\.T*$")) 
    }
  })

  # The variable for the T2T, later used to plot in the plot section
  troop_to_task_fxn <- reactive({
    T2T_master() %>%
      full_join(Roster) %>%
      filter(Division %in% input$tab_division) %>%
      mutate(`Start Date` = as.Date(`Start Date`)) %>%
      mutate(`End Date` = as.Date(`End Date`)) %>%
      ggplot(aes(x = `Start Date`, xend = `End Date`, y = reorder(Name, desc(Name)), yend = Name, color = Event)) +
      theme_bw(base_size = 14, base_family = "Helvetica") +
      geom_segment(size = 4, show.legend = FALSE) +
      geom_label(aes(label = str_wrap(paste(Event, Task), 24)), color = "black", check_overlap = TRUE) +
      scale_x_date(position = "top", date_labels = "%b %y", date_breaks = "1 months", minor_breaks = "1 weeks") +
      labs(title = "Troop To Task", y = "Participants", x = "Date") +
      facet_grid(Team ~ ., scale = "free_y", space="free")
  })
  
  # Filtering the location data 
  location_data <- reactive(
    T2T_master() %>% 
    left_join(location)
  )
  

#### 5. Plot Data ####
  
###  The Dashboard tab plots
   
  # Plot for the table for the count of exercises per person
  output$people_count <- renderTable({
    T2T_master() %>%
      filter(Task == "Exercise") %>%
      filter(Name != "Contractor TBD") %>%
      group_by(Name) %>%
      summarise(Exercises = n()) %>%
      arrange(-Exercises) %>%
      dplyr::select(Name, Exercises)
  })
  
  # Plot for the OPTEMPO
  output$people_bar <- renderPlot({ 
    T2T_master() %>%
      filter(Task != "Leave/Pass") %>%
      filter(Task != "School") %>%
      filter(!is.na(Task)) %>%
      filter(Name != "Contractor TBD") %>%
      count(Name, wt = Duration) %>%
      left_join(Roster) %>%
      ggplot(mapping = aes(reorder(Name, n), n, fill = Team), show.legend = TRUE) +
      geom_col(stat = "identity") +
      geom_text(aes(label = n), hjust = "right") +
      labs(x = NULL, y = "Cumulative Days", caption = "Does not include Leave, Pass, or Schools") +
      theme(legend.position = "bottom") +
      theme_bw(base_size = 14, base_family = "Helvetica") +
      coord_flip()
  })  
  
  # Plot for the TDY
  output$people_tdy <- renderPlot({
    T2T_master() %>%
      filter(`Duty Location` != "Fort Lee, VA") %>%
      filter(`Duty Location` != "MS Teams") %>%
      filter(!is.na(`Duty Location`)) %>%
      filter(Task != "Leave/Pass") %>%
      filter(Task != "School") %>%
      filter(Name != "Contractor TBD") %>%
      count(Name, wt = Duration) %>%
      left_join(Roster) %>%
      ggplot(mapping = aes(reorder(Name, n), n, fill = Team), show.legend = TRUE) +
      geom_col(stat = "identity") +
      geom_text(aes(label = n), hjust = "right") +
      labs(x = NULL, y = "Cumulative Days", caption = "TDY: Does not include Leave, Pass, Schools, Fort Lee, or MS Teams") +
      theme(legend.position = "bottom") +
      theme_bw(base_size = 14, base_family = "Helvetica") +
      coord_flip()  
  })  
  
  # Function calculates the number of weekends. Vectorizing makes it much faster...
  Nweekdays <- Vectorize(function(a, b) 
    sum(weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))
  
  # Plot for the Weekend in the OPTEMPO
  output$people_weekend <- renderPlot({
    T2T_master() %>%
      filter(Task != "Leave/Pass") %>%
      filter(Task != "School") %>%
      filter(Name != "Contractor TBD") %>%
      mutate(Weekends = Nweekdays(`Start Date`, `End Date`)) %>%
      count(Name, wt = Weekends) %>%
      left_join(Roster) %>%
      ggplot(mapping = aes(reorder(Name, n), n, fill = Team), show.legend = TRUE) +
      geom_col(stat = "identity") +
      geom_text(aes(label = n), hjust = "right") +
      labs(x = NULL, y = "Cumulative Days", caption = "Does not include Leave, Pass, or Schools") +
      theme(legend.position = "bottom") +
      theme_bw(base_size = 14, base_family = "Helvetica") +
      coord_flip()  
  }) 
  
  # Plot for the exercise gantt 
  output$gantt <- renderPlot({
    ggplot(T2T_master(), aes(x = as.Date(`Start Date`), xend = as.Date(`End Date`), y = reorder(Event, desc(`Start Date`)), yend = Event, color = Task), show.legend = FALSE) +
      theme_linedraw(base_size = 14, base_family = "Helvetica") +
      geom_segment(size = 4) +
      geom_label(aes(label = str_wrap(Task, 12)), color = "black", check_overlap = TRUE) +
      scale_x_date(position = "bottom", date_labels = "%b %y", date_breaks = "1 months", minor_breaks = "1 weeks") +
      labs(title = "Exercise Calendar", y = "Events", x = "Date")
  }) 
  
  # Plot for the location table
  output$location_count <- renderTable({
    T2T_master() %>%
      filter(!is.na(`Duty Location`)) %>%
      dplyr::select(`Duty Location`, Name) %>%
      distinct() %>% 
      group_by(`Duty Location`) %>%
      summarise(`Max Pax Count` = n()) #%>%
  })


###  The list tab
  
  # Table for the list tab
  output[["dt"]] <- renderDataTable({
    DT::datatable(T2T_table(), 
                  editable = "cell", 
                  caption = str_glue({paste0(format(file.mtime("T2T_Input.xlsx"), "%b %d, %Y at %H:%m"))}),
                  extensions = c("RowGroup", "Buttons"), 
                    options = list(
                    dom = "Bfrtip",
                    buttons = c('excel', 'print'),
                    pageLength = 1000
                    ))
  })


###  The Troop-to-task tab  

  # Plots the T2T variable from above
  output$troop_to_task <- renderPlot({
    troop_to_task_fxn()
  }) 
  
  # Download button for the T2T
  output$download_pg3 <- downloadHandler(
        filename = "report.html",
        content = function(file) {  
          shinyalert("Success!", "Patience, your calendar is downloading.", type = "success")
          shiny::withProgress(
            message = paste0("Downloading", input$dataset, " Data"),
            value = 0,
            {
              shiny::incProgress(1/10)
              Sys.sleep(1)
              shiny::incProgress(5/10)
          tempReport <- file.path(tempdir(), "Calendar printable.Rmd")
          file.copy("Calendar printable.Rmd", tempReport, overwrite = TRUE)     # this calls a separate file to run and export 
          params <- list(n = input$tab_division,                                # these parameters are passed to the file
                         m = input$tab_team, 
                         o = input$no_months, 
                         p = input$check_div)
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv()))
            })
  })
  
  # Table for troops available in the T2T
  output$troop_to_task_avail <- renderTable(
    Roster %>% 
      filter(Division %in% input$tab_division) %>%
      anti_join(T2T_table()) %>%
      dplyr::select(Name) %>%
      rename(Available = Name) %>%
      distinct()
  )


###  The map tab
  
  # Map plot
  output$map1 <- renderLeaflet({
    leaflet(data = location_data()) %>% 
      addTiles() %>%
      addCircleMarkers(data = distinct(location_data()), label = ~htmlEscape(`Duty Location`), labelOptions = labelOptions(noHide = T, offset = c(0, 9),interactive = TRUE)) %>%
      addCircleMarkers(data = location_data(), label = ~htmlEscape(Name), clusterOptions = markerClusterOptions())
  })
  #, clusterOptions = markerClusterOptions() , popup = location_data(data[3]) clusterOptions = markerClusterOptions()

}

shinyApp(ui, server)
