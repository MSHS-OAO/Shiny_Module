
#Shiny Module Input
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
                selected = default_campus))
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
                choices= default_specialty_choices,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0}/{1} Specialties",
                  dropupAuto = FALSE),
                selected = default_specialty))
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
                choices= default_departments,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0}/{1} Departments",
                  dropupAuto = FALSE),
                selected = default_departments))
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
      choices = default_resource_type,
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon")),
      selected = default_resource_type)
  )}


ProviderInput <- function(id) {
  box(
    title = "Select Provider:",
    width = 12,
    height = "100px",
    solidHeader = FALSE, 
    pickerInput(NS(id, "selectedProvider"), 
                label=NULL,
                choices= default_provider,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Providers", 
                  dropupAuto = FALSE),
                selected = default_provider))
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
                choices= default_visit_method,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Visit Methods", 
                  dropupAuto = FALSE),
                selected = default_visit_method))
  
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
                choices= default_PRC_name,
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Visit Types", 
                  dropupAuto = FALSE),
                selected = default_PRC_name))
  
}

DateInput <- function(id) {
  box(
    title = "Select Date Range:",
    width = 12, 
    height = "100px",
    solidHeader = FALSE, 
    dateRangeInput(NS(id,"dateRange"), label = NULL,
                   start = dateRange_start, end = dateRange_max,
                   min = dateRange_min, max = dateRange_max))
}

weekInput <- function(id){
  box(
    title = "Select Days of Week:",
    width = 12, 
    solidHeader = FALSE, 
    pickerInput(NS(id, "daysOfWeek"),
                label = NULL,
                choices = daysOfWeek.options,
                selected = daysOfWeek.options,
                multiple=TRUE,                        
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 1", 
                  countSelectedText = "{0}/{1} Days", 
                  dropupAuto = FALSE))
  )
}


HolidayInput <- function(id){
  box(
    title = "Select Holidays to Exclude:",
    width = 12,
    solidHeader = FALSE,
    pickerInput(NS(id, "excludeHolidays"),label=NULL,
                choices= unique(holid$holiday),
                multiple=TRUE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  actionsBox = TRUE,
                  dropupAuto = FALSE),
                selected = unique(holid$holiday)
    )
  )
}



# Define UI for Campus
ui <- dashboardPage(header,
                    dashboardSidebar(sidebarMenu(id = "sbm",
                                menuItem("Access", tabName = "newPatients", icon = icon("plus-circle"))
                                
                                
                                
                    )),
                    dashboardBody(fluidPage(        tabItem(tabName = "newPatients",
                                                            #profvis_ui("profiler"),
                                                            column(11,
                                                                   div("Access | New Patients", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                                                                   
                                                                   fluidRow(
                                                                     boxPlus(
                                                                       title = "New Patient Wait Time", width = 12, status = "primary",
                                                                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                                                       tabBox(
                                                                         title = NULL, type = "pills",
                                                                         id = "tabset5", width = "100%",
                                                                         tabPanel("Total", 
                                                                                  plotOutput("newPtWaitTimeByDept", height = "550px") %>% 
                                                                                    withSpinner(type = 5, color = "#d80b8c"),
                                                                                  br(),
                                                                                  plotOutput("newPtWaitTimeByDeptPercent", height = "550px") %>% 
                                                                                    withSpinner(type = 5, color = "#d80b8c")
                                                                         ),
                                                                         tabPanel("By Provider",
                                                                                  "*Select Fewer Providers for Better Visibility",
                                                                                  plotOutput("newPtWaitTimeByProv", height = "550px") %>% 
                                                                                    withSpinner(type = 5, color = "#d80b8c"),
                                                                                  plotOutput("newPtWaitTimeByProvPercent", height = "550px") %>% 
                                                                                    withSpinner(type = 5, color = "#d80b8c")
                                                                         )
                                                                       ))),
                                                                   fluidRow(
                                                                     boxPlus(
                                                                       title = "New Patient Visit Ratio", width = 12, status = "primary",
                                                                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                                                       tabBox(
                                                                         title = NULL, type = "pills",
                                                                         id = "tabset4", width = "100%",
                                                                         tabPanel("Total", 
                                                                                  plotOutput("newPtRatioByDept", height = "550px") %>% 
                                                                                    withSpinner(type = 5, color = "#d80b8c")),
                                                                         tabPanel("By Provider",
                                                                                  "*Select fewer providers for better visibility",
                                                                                  plotOutput("newPtRatioByProv", height = "550px") %>% 
                                                                                    withSpinner(type = 5, color = "#d80b8c"))))),
                                                                   fluidRow(
                                                                     boxPlus(
                                                                       title = "New Patient Source", width = 12, status = "primary",
                                                                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                                                       plotOutput("newPtApptSourceByDept", height = "550px") %>% 
                                                                         withSpinner(type = 5, color = "#d80b8c")))
                                                            )),
                                                    column(1,
                                                           dropdown(
                                                             conditionalPanel(condition = "input.sbm == 'newPatients'",
                                                                              
                                                                              br(),
                                                                              actionButton("update_filter_access", "CLICK TO UPDATE", width = "80%"),
                                                                              br(),
                                                                              br(),
                                                                              
                                                                              CampusInput("selectedCampus"),
                                                                              SpecialtyInput("selectedSpecialty"),
                                                                              DepartmentInput("selectedDepartment"),
                                                                              ResourceInput("selectedResource"),
                                                                              ProviderInput("selectedProvider"),
                                                                              VisitMethodInput("selectedVisitMethod"),
                                                                              VisitTypeInput("selectedPRCName"),
                                                                              DateInput("dateRange"),
                                                                              weekInput("daysOfWeek"),
                                                                              HolidayInput("excludeHolidays")
                                                                              
                                                             )
                                                           ))))# Close sidebarMenu
                    
) # Close dashboardSidebar)