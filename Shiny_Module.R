
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


# Define UI for Campus
CampusInput <- function(id, data) {
  default_campus_choices <- data %>% 
    select(CAMPUS) %>% distinct() %>% pull()
  
  box(
    title = "Select Campus:",
    width = 12,
    height = "100px",
    solidHeader = FALSE,
    pickerInput(NS(id, "selectedCampus"),
                label=NULL,
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
    observeEvent(campus, {
      if(!is.null(campus)) {
        
        specailty_choices <-  data %>% filter(CAMPUS %in% campus) %>%
            select(CAMPUS_SPECIALTY) %>% distinct() %>% pull()

        updatePickerInput(session,
                          inputId = id,
                          choices = specailty_choices,
                          selected = specailty_choices)
      }
    })
    reactive(input$selectedSpecialty)
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
                multiple=TRUE,
                selected = NULL))
}


# Define Server for Department
DepartmentServer <- function(id, data, campus, specialty) {
  moduleServer(id, function(input, output, session) {
    observeEvent(specialty, {
      if(!is.null(specialty)) {
        
        department_choices <-  data %>% filter(CAMPUS %in% campus, CAMPUS_SPECIALTY %in% specialty ) %>%
          select(DEPARTMENT) %>% distinct() %>% pull()
        
        updatePickerInput(session,
                          inputId = id,
                          choices = department_choices,
                          selected = department_choices)
      }
    })
  })
  
}


#Define UI for the app
ui <- fluidPage(
  CampusInput("selectedCampus", data = filters),
  SpecialtyInput("selectedSpecialty"),
  DepartmentInput("selectedDepartment")
  
)

#Define Server for the app
server <- function(input, output, session) {
  selected_campus <- CampusServer("selectedCampus")
  selected_specialty <- SpecialtyServer("selectedSpecialty", data = filters, campus = selected_campus())
  test <<- selected_specialty 
  selected_department <- DepartmentServer("selectedDepartment", data = filters, 
                                          campus = selected_campus(), specialty = selected_specialty())
}

shinyApp(ui, server)
