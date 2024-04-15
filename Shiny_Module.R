
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
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = FALSE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = paste0("{0}/{1}", "Campus"), 
                  dropupAuto = FALSE),
                choices=  default_campus_choices,
                multiple=TRUE,
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
                          options = pickerOptions(
                            liveSearch = TRUE,
                            actionsBox = FALSE,
                            selectedTextFormat = "count > 1", 
                            countSelectedText = paste0("{0}/{1}", "Specialty"), 
                            dropupAuto = FALSE),
                          choices = specailty_choices,
                          selected = specailty_choices)
      }
    }, ignoreNULL = FALSE)
    
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
    pickerInput(NS(id,"selectedDepartment"),
                label=NULL,
                choices= NULL,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = FALSE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = paste0("{0}/{1}", "Department"), 
                  dropupAuto = FALSE),
                multiple=TRUE,
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
    }, ignoreNULL = FALSE)
    
    return(reactive(input[["selectedDepartment"]]))
  })
  
}


#Define UI for the app
ui <- fluidPage(
  CampusInput("selectedCampus"),
  SpecialtyInput("selectedSpecialty"),
  DepartmentInput("selectedDepartment"),
  #textOutput("result")
  
)

#Define Server for the app
server <- function(input, output, session) {
  selected_campus <- CampusServer("selectedCampus")
  
  selected_specialty <- SpecialtyServer("selectedSpecialty", data = filters, campus = selected_campus)
  
  selected_department <- DepartmentServer("selectedDepartment", data = filters,
                                          campus = selected_campus, specialty = selected_specialty)
  #output$result <- renderText(selected_department())
}

shinyApp(ui, server)
