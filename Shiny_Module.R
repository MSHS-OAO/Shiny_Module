

# Create Shiny Modules for Filters 

# Import Libraries
library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(shiny)
library(shinyWidgets)
library(shinydashboard)


# Import data
poolcon <- dbConnect(odbc(), "OAO Cloud DB", timeout = 15)

filters <- tbl(poolcon, "AMBULATORY_FILTERS")
historical.data <- tbl(poolcon,  "AMBULATORY_ACCESS")
historical.data <- historical.data %>% mutate(NEW_PT3 = ifelse(is.na(NEW_PT3), "ESTABLISHED", NEW_PT3))


default_campus_choices <- filters %>% select(CAMPUS) %>% distinct() %>% pull()

# Define UI for Campus
CampusInput <- function(id) {
  box(
    title = "Select Campus:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    pickerInput(NS(id, "selectedCampus"),
                label=NULL,
                choices=  default_campus_choices,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = FALSE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = paste0("{0}/{1}", "Campus"), 
                  dropupAuto = FALSE),
                selected = "MSUS"))
}


# Define Server for Campus
CampusServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$selectedCampus
    })
  })
}


# Define UI for Specialty
SpecialtyInput <- function(id) {
  
  box(
    title = "Select Specialty:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    pickerInput(NS(id,"selectedSpecialty"),
                label=NULL,
                choices= NULL,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0}/{1} Specialties",
                  dropupAuto = FALSE),
                selected = NULL))
}





# Define Server for Specialty
SpecialtyServer <- function(id, data, campus) {
  moduleServer(id, function(input, output, session) {
    observeEvent(campus(), {
      if(!is.null(campus())) {
        
        selected_campus <- campus()
        
        specailty_choices <- data %>% filter(CAMPUS %in% selected_campus) %>%
          select(CAMPUS_SPECIALTY) %>% distinct() %>% pull()
        
        updatePickerInput(session,
                          inputId = id,
                          choices = specailty_choices,
                          selected = specailty_choices)
      }
    }, 
    ignoreNULL = FALSE)
    
    return(reactive(input[[paste0("selectedSpecialty")]]))
  })
  
}



# Define UI for Department
DepartmentInput <- function(id) {
  box(
    title = "Select Department:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    pickerInput(NS(id, "selectedDepartment"),
                label=NULL,
                choices= NULL,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0}/{1} Departments",
                  dropupAuto = FALSE),
                selected = NULL))
}


# Define Server for Department
DepartmentServer <- function(id, data, campus, specialty) {
  moduleServer(id, function(input, output, session) {
    observeEvent(specialty(), {
      # print(specialty())
      if(!is.null(specialty())) {
        
        selected_campus <- campus()
        selected_specialty <- specialty()
        
        department_choices <-  data %>% dplyr::filter(CAMPUS %in% selected_campus, CAMPUS_SPECIALTY %in% selected_specialty) %>%
          select(DEPARTMENT) %>% distinct() %>% pull()
        
        # print(department_choices)
        updatePickerInput(session,
                          inputId = id,
                          choices = department_choices,
                          selected = department_choices)
      }
    }, 
    ignoreNULL = FALSE)
    
    return(reactive(input[["selectedDepartment"]]))
    
  })
  
}


ResourceInput <- function(id) {
  box(
    title = "Select Resource Type:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    checkboxGroupButtons(
      inputId = NS(id, "selectedResource"),
      label = NULL, 
      choices = c("Provider","Resource"),
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon")),
      selected = c("Provider","Resource"))
  )}


ResourceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    return(reactive(input[["selectedResource"]]))
    #reactive({input$selectedResource})
  })
  
}



ProviderInput <- function(id) {
  box(
    title = "Select Provider:",
    width = 12,
    height = "100px",
    solidHeader = FALSE, 
    pickerInput(NS(id, "selectedProvider"), 
                label=NULL,
                choices= NULL,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Providers", 
                  dropupAuto = FALSE),
                selected = NULL))
}






# Define Server for Provider
ProviderServer <- function(id, data, campus, specialty, department, resource) {
  moduleServer(id, function(input, output, session) {
    observeEvent(department(), {
      if(!is.null(department())) {
       
        selected_campus <- campus()
        selected_specialty <- specialty()
        selected_department <- department()
        selected_resource <- resource()
        print(resource())
        
        provider_choices <-  data %>% dplyr::filter(CAMPUS %in% selected_campus, 
                                                    CAMPUS_SPECIALTY %in% selected_specialty,
                                                    DEPARTMENT %in% selected_department,
                                                    RESOURCES %in% selected_resource
                                                    ) %>%
                                               select(PROVIDER) %>% distinct() %>% pull()
        
        updatePickerInput(session,
                          inputId = id,
                          choices = provider_choices,
                          selected = provider_choices)
      }
    }, 
    ignoreNULL = FALSE)
    
    return(reactive(input[["selectedProvider"]]))
    
  })
  
}


#Define UI for Visit Method
VisitMethodInput <- function(id) {
  
  box(
    title = "Select Visit Method:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    pickerInput(NS(id, "selectedVisitMethod"),
                label=NULL,
                choices= NULL,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Visit Methods", 
                  dropupAuto = FALSE),
                selected = NULL))
  
}


# Define Server for Visit Method
VisitMethodServer <- function(id, data, campus, specialty, department, resource, provider) {
  moduleServer(id, function(input, output, session) {
    observeEvent(provider(), {
      if(!is.null(provider())) {
        
        selected_campus <- campus()
        selected_specialty <- specialty()
        selected_department <- department()
        selected_resource <- resource()
        selected_provider <- provider()
        
        if(length(selected_provider) >1000){

          visit_choices <-  data %>% dplyr::filter(CAMPUS %in% selected_campus, 
                                                    CAMPUS_SPECIALTY %in% selected_specialty,
                                                    DEPARTMENT %in% selected_department,
                                                    RESOURCES %in% selected_resource) %>%
                                            select(VISIT_METHOD) %>% distinct() %>% pull()
        
        }else{
          
          visit_choices <-  data %>% dplyr::filter(CAMPUS %in% selected_campus, 
                                                      CAMPUS_SPECIALTY %in% selected_specialty,
                                                      DEPARTMENT %in% selected_department,
                                                      RESOURCES %in% selected_resource,
                                                      PROVIDER %in% selected_provider) %>%
            select(VISIT_METHOD) %>% distinct() %>% pull()
          
        }
        
        updatePickerInput(session,
                          inputId = id,
                          choices = visit_choices,
                          selected = visit_choices)
      }
    }, 
    ignoreNULL = FALSE)
    
    return(reactive(input[["selectedVisitMethod"]]))
    
  })
  
}


#Define UI for Visit Type
VisitTypeInput <- function(id) {
  box(
    title = "Select Visit Type:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    pickerInput(NS(id, "selectedPRCName"),
                label=NULL,
                choices= NULL,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Visit Types", 
                  dropupAuto = FALSE),
                selected = NULL))
  
}


# Define Server for Visit Method
VisitTypeServer <- function(id, data, campus, specialty, department, resource, provider, visit_method) {
  moduleServer(id, function(input, output, session) {
    observeEvent(visit_method(), {
      if(!is.null(visit_method())) {
        
        selected_campus <- campus()
        selected_specialty <- specialty()
        selected_department <- department()
        selected_resource <- resource()
        selected_provider <- provider()
        selected_visit_method <- visit_method()
        
        if(length(selected_provider) >1000){
          
          prc_choices <-  data %>% dplyr::filter(CAMPUS %in% selected_campus, 
                                                   CAMPUS_SPECIALTY %in% selected_specialty,
                                                   DEPARTMENT %in% selected_department,
                                                   RESOURCES %in% selected_resource,
                                                   VISIT_METHOD %in% selected_visit_method) %>%
            select(APPT_TYPE) %>% distinct() %>% pull()
          
        }else{
          
          prc_choices <-  data %>% dplyr::filter(CAMPUS %in% selected_campus, 
                                                   CAMPUS_SPECIALTY %in% selected_specialty,
                                                   DEPARTMENT %in% selected_department,
                                                   RESOURCES %in% selected_resource,
                                                   PROVIDER %in% selected_provider,
                                                   VISIT_METHOD %in% selected_visit_method) %>%
                                                   select(APPT_TYPE) %>% distinct() %>% pull()
          
        }
        
        updatePickerInput(session,
                          inputId = id,
                          choices =  prc_choices,
                          selected =  prc_choices)
      }
    }, 
    ignoreNULL = FALSE)
    
    return(reactive(input[["selectedPRCName"]]))
    
  })
  
}


#Define UI for the app
ui <- fluidPage(
  
  dropdown(
    actionButton("filter", "Update"),  
    CampusInput("selectedCampus"),
    SpecialtyInput("selectedSpecialty"),
    DepartmentInput("selectedDepartment"),
    ResourceInput("selectedResource"),
    ProviderInput("selectedProvider"),
    VisitMethodInput("selectedVisitMethod"),
    VisitTypeInput("selectedPRCName")
    
  ),
  
  plotOutput("newPtWaitTimeByDept", height = "550px")
  
)

#Define Server for the app
server <- function(input, output, session) {
  selected_campus <- CampusServer("selectedCampus")
  selected_specialty <- SpecialtyServer("selectedSpecialty", data = filters, campus = selected_campus)
  selected_department <- DepartmentServer("selectedDepartment", data = filters,
                                          campus = selected_campus, specialty = selected_specialty)

  selected_resource <- ResourceServer("selectedResource")
  selected_provider <- ProviderServer("selectedProvider", data = filters,
                                      campus = selected_campus, 
                                      specialty = selected_specialty,
                                      department = selected_department, 
                                      resource = selected_resource)
  
  selected_visitmethod <- VisitMethodServer("selectedVisitMethod", data = filters,
                                            campus = selected_campus, 
                                            specialty = selected_specialty,
                                            department = selected_department, 
                                            resource = selected_resource,
                                            provider = selected_provider)
  
  selected_visittype <- VisitTypeServer("selectedPRCName", data = filters,
                                        campus = selected_campus, 
                                        specialty = selected_specialty,
                                        department = selected_department, 
                                        resource = selected_resource,
                                        provider = selected_provider,
                                        visit_method = selected_visitmethod
                                        )

  data_all <- eventReactive(input$filter, {
    
   
    
    selected_campus <- selected_campus()
    selected_specialty <- selected_specialty()
    selected_department <-  selected_department()
    selected_resource <- selected_resource()
    selected_provider <- selected_provider()
    selected_visitmethod <- selected_visitmethod()
    selected_visittype <- selected_visittype()
    
    
    if(length(selected_provider) >= 1000){
    
    data <- historical.data %>% filter(CAMPUS %in%  selected_campus, 
                                       CAMPUS_SPECIALTY %in% selected_specialty,
                                       DEPARTMENT %in% selected_department,
                                       RESOURCES %in% selected_resource,
                                       #PROVIDER %in% selected_provider
                                       )
    
    }
    else {
      
      data <- historical.data %>% filter(CAMPUS %in%  selected_campus, 
                                         CAMPUS_SPECIALTY %in% selected_specialty,
                                         DEPARTMENT %in% selected_department,
                                         RESOURCES %in% selected_resource,
                                         PROVIDER %in% selected_provider
      )
    }
  })
  
  
  
  
  # New Patient Wait Time
  output$newPtWaitTimeByDept <- renderPlot({
    
    data <- data_all()
    
   # test <<- data
    
    
    
    # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
    
    #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(WAIT_TIME >= 0) %>%
      group_by(APPT_MADE_MONTH_YEAR, NEW_PT2) %>%
      dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
      filter(NEW_PT2 %in% c("NEW","ESTABLISHED")) %>% collect()
    
    
    waitTime$NEW_PT2 <- ifelse(waitTime$NEW_PT2 == "NEW", "New","Established")
    #waitTime$Appt.MonthYear <- as.Date(waitTime$Appt.MonthYear, format="%Y-%m-%d") ## Create date-year column
    
    waitTime <- waitTime %>% spread(NEW_PT2, medWaitTime) 
    #waitTime$`New Patient Target <= 14` <- 14
    waitTime[is.na(waitTime)] <- 0
    waitTime <- waitTime %>% gather(variable, value, 2:3)
    target <- 14
    
    
    ggplot(waitTime, aes(x=APPT_MADE_MONTH_YEAR, y=value, group = variable, color=variable))+
      # geom_bar(stat = "identity", position = 'dodge')+
      geom_line(size=1) +
      geom_abline(slope=0, intercept=14,  col = "red",lty=2, size = 1) +
      #geom_line(aes(linetype = variable))+
      #scale_linetype_manual(values=c("solid", "solid", "dashed"))+
      #scale_size_manual(values=c(1, 1, 1.3))+
      # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
      #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
      labs(x=NULL, y=NULL,
           #title = "Median Wait Time to New and Established Appointment Over Time",
           title = "Monthly Median Wait Time to New and Established Appointment",
           #subtitle = paste0("Based on scheduled data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
           #caption = "*New patients defined by CPT codes (level of service)."
      )+
      #theme_new_line()+
      #theme_bw()+
      #graph_theme("top")+
      geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
      #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
      scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_color_manual(values=c('#212070','#d80b8c')) +
      geom_point(size = 3.2)
    # stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..)), group = value), geom="text", color="black", 
    #              size=5, fontface="bold.italic")
    
  })
  
}

shinyApp(ui, server)
