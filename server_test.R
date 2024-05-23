
## Shiny Module Function
print("Shiny Module")


# Define Server for Campus
CampusServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      print("reactive")
      input$selectedCampus
    })
  #   print("reactive")
  #   observeEvent(input$selectedCampus, {
  #     campus_value(input$selectedCampus)
  #   })
  # return(campus_value(input$selectedCampus))
  })
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
    ignoreNULL = TRUE,
    ignoreInit = TRUE)
    
    return(reactive(input[[paste0("selectedSpecialty")]]))
  })
  
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
    
    return(reactive(isolate(input[["selectedDepartment"]])))
    
  })
  
}


ResourceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    return(reactive(input[["selectedResource"]]))
    #reactive({input$selectedResource})
  })
  
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


DateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    return(reactive(input[["dateRange"]]))
  })
  
}



WeekServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    return(reactive(input[["daysOfWeek"]]))
  })
  
}


HolidayServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    return(reactive(input[["excludeHolidays"]]))
  })
  
}




server <- function(input, output, session) {
dataAll_access_new <- eventReactive(list(input$update_filter_access), {
  selected_campus <- selected_campus()
  selected_specialty <- selected_specialty()
  selected_department <-  selected_department()
  selected_resource <- selected_resource()
  selected_provider <- selected_provider()
  selected_visitmethod <- selected_visitmethod()
  selected_visittype <- selected_visittype()
  selected_dateRange <- selected_dateRange()
  min_date <- selected_dateRange[1]
  end_date <- selected_dateRange[2]
  selected_days <- selected_daysOfWeek()
  selected_holiday <- selected_holiday()
  
  validate(
    need(selected_campus != "", "Please select a Campus"),
    need(selected_specialty != "", "Please select a Specialty"),
    need(selected_department != "", "Please select a Department"),
    need(selected_resource != "", "Please select a Resource"),
    need(selected_provider != "", "Please select a Provider"),
    need(selected_visitmethod != "", "Please select a Visit Method"),
    need(selected_visittype != "", "Please select a Visit Type")
  )
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  
  if(length(selected_provider) >= 1000){
    
    data <- historical.data %>% filter(CAMPUS %in%  selected_campus, 
                                       CAMPUS_SPECIALTY %in% selected_specialty,
                                       DEPARTMENT %in% selected_department,
                                       RESOURCES %in% selected_resource,
                                       VISIT_METHOD %in% selected_visitmethod,
                                       #PROVIDER %in% selected_provider,
                                       TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                       TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                       APPT_DAY %in% selected_days
                                       #HOLIDAY %in% selected_holiday
    )
    
  }
  else {
    
    data <- historical.data %>% filter(CAMPUS %in%  selected_campus, 
                                       CAMPUS_SPECIALTY %in% selected_specialty,
                                       DEPARTMENT %in% selected_department,
                                       RESOURCES %in% selected_resource,
                                       PROVIDER %in% selected_provider,
                                       VISIT_METHOD %in% selected_visitmethod,
                                       TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                       TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                       APPT_DAY %in% selected_days
                                       #HOLIDAY %in% selected_holiday
    )
  }
  
  if(length(selected_visittype) < 1000){
    
    data <- data %>% filter(APPT_TYPE %in% selected_visittype)
  }
})

dataArrived_access_npr_new <- eventReactive(list(input$update_filter_access),{
  
  selected_campus <- selected_campus()
  selected_specialty <- selected_specialty()
  selected_department <-  selected_department()
  selected_resource <- selected_resource()
  selected_provider <- selected_provider()
  selected_visitmethod <- selected_visitmethod()
  selected_visittype <- selected_visittype()
  selected_dateRange <- selected_dateRange()
  min_date <- selected_dateRange[1]
  end_date <- selected_dateRange[2]
  selected_days <- selected_daysOfWeek()
  selected_holiday <- selected_holiday()
  
  validate(
    need(selected_campus != "", "Please select a Campus"),
    need(selected_specialty != "", "Please select a Specialty"),
    need(selected_department != "", "Please select a Department"),
    need(selected_resource != "", "Please select a Resource"),
    need(selected_provider != "", "Please select a Provider"),
    need(selected_visitmethod != "", "Please select a Visit Method"),
    need(selected_visittype != "", "Please select a Visit Type")
  )
  
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  
  if(length(selected_provider) >= 1000){
    
    data <- arrived.data.rows.npr %>% filter(CAMPUS %in%  selected_campus, 
                                             CAMPUS_SPECIALTY %in% selected_specialty,
                                             DEPARTMENT %in% selected_department,
                                             RESOURCES %in% selected_resource,
                                             VISIT_METHOD %in% selected_visitmethod,
                                             #PROVIDER %in% selected_provider,
                                             TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                             TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                             APPT_DAY %in% selected_days
                                             #HOLIDAY %in% selected_holiday
    )
    
  }
  else {
    
    data <- arrived.data.rows.npr %>% filter(CAMPUS %in%  selected_campus, 
                                             CAMPUS_SPECIALTY %in% selected_specialty,
                                             DEPARTMENT %in% selected_department,
                                             RESOURCES %in% selected_resource,
                                             PROVIDER %in% selected_provider,
                                             VISIT_METHOD %in% selected_visitmethod,
                                             TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                             TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                             APPT_DAY %in% selected_days
                                             #HOLIDAY %in% selected_holiday
    )
  }
  
  if(length(selected_visittype) < 1000){
    
    data <- data %>% filter(APPT_TYPE %in% selected_visittype)
  }
})

dataArrived_access_new <- eventReactive(list(input$update_filter_access),{
  
  selected_campus <- selected_campus()
  selected_specialty <- selected_specialty()
  selected_department <-  selected_department()
  selected_resource <- selected_resource()
  selected_provider <- selected_provider()
  selected_visitmethod <- selected_visitmethod()
  selected_visittype <- selected_visittype()
  selected_dateRange <- selected_dateRange()
  min_date <- selected_dateRange[1]
  end_date <- selected_dateRange[2]
  selected_days <- selected_daysOfWeek()
  selected_holiday <- selected_holiday()
  
  validate(
    need(selected_campus != "", "Please select a Campus"),
    need(selected_specialty != "", "Please select a Specialty"),
    need(selected_department != "", "Please select a Department"),
    need(selected_resource != "", "Please select a Resource"),
    need(selected_provider != "", "Please select a Provider"),
    need(selected_visitmethod != "", "Please select a Visit Method"),
    need(selected_visittype != "", "Please select a Visit Type")
  )
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  
  if(length(selected_provider) >= 1000){
    
    data <- arrived.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                         CAMPUS_SPECIALTY %in% selected_specialty,
                                         DEPARTMENT %in% selected_department,
                                         RESOURCES %in% selected_resource,
                                         VISIT_METHOD %in% selected_visitmethod,
                                         #PROVIDER %in% selected_provider,
                                         TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                         TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                         APPT_DAY %in% selected_days
                                         #HOLIDAY %in% selected_holiday
    )
    
  }
  else {
    
    data <- arrived.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                         CAMPUS_SPECIALTY %in% selected_specialty,
                                         DEPARTMENT %in% selected_department,
                                         RESOURCES %in% selected_resource,
                                         PROVIDER %in% selected_provider,
                                         VISIT_METHOD %in% selected_visitmethod,
                                         TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                         TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                         APPT_DAY %in% selected_days
                                         #HOLIDAY %in% selected_holiday
    )
  }
  
  if(length(selected_visittype) < 1000){
    
    data <- data %>% filter(APPT_TYPE %in% selected_visittype)
  }
})

dataArrivedNoShow_new <- eventReactive(list(input$update_filter_access),{
  
  selected_campus <- selected_campus()
  selected_specialty <- selected_specialty()
  selected_department <-  selected_department()
  selected_resource <- selected_resource()
  selected_provider <- selected_provider()
  selected_visitmethod <- selected_visitmethod()
  selected_visittype <- selected_visittype()
  selected_dateRange <- selected_dateRange()
  min_date <- selected_dateRange[1]
  end_date <- selected_dateRange[2]
  selected_days <- selected_daysOfWeek()
  selected_holiday <- selected_holiday()
  
  validate(
    need(selected_campus != "", "Please select a Campus"),
    need(selected_specialty != "", "Please select a Specialty"),
    need(selected_department != "", "Please select a Department"),
    need(selected_resource != "", "Please select a Resource"),
    need(selected_provider != "", "Please select a Provider"),
    need(selected_visitmethod != "", "Please select a Visit Method"),
    need(selected_visittype != "", "Please select a Visit Type")
  )
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  
  if(length(selected_provider) >= 1000){
    
    data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                               CAMPUS_SPECIALTY %in% selected_specialty,
                                               DEPARTMENT %in% selected_department,
                                               RESOURCES %in% selected_resource,
                                               VISIT_METHOD %in% selected_visitmethod,
                                               #PROVIDER %in% selected_provider,
                                               TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                               TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                               APPT_DAY %in% selected_days
                                               #HOLIDAY %in% selected_holiday
    )
    
  }
  else {
    
    data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                               CAMPUS_SPECIALTY %in% selected_specialty,
                                               DEPARTMENT %in% selected_department,
                                               RESOURCES %in% selected_resource,
                                               PROVIDER %in% selected_provider,
                                               VISIT_METHOD %in% selected_visitmethod,
                                               TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                               TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                               APPT_DAY %in% selected_days
                                               #HOLIDAY %in% selected_holiday
    )
  }
  
  if(length(selected_visittype) < 1000){
    
    data <- data %>% filter(APPT_TYPE %in% selected_visittype)
  }
})

## Access Module
selected_campus <-   reactiveVal('MSUS')

observeEvent(input$selectedCampus, {
  print("pbserver")
  selected_campus(input$selectedCampus)
}, ignoreInit = TRUE,
    ignoreNULL = TRUE)

# selected_campus <- CampusServer("selectedCampus")
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
                                      visit_method = selected_visitmethod)


selected_dateRange <- DateServer("dateRange")
selected_daysOfWeek <- WeekServer("daysOfWeek")
selected_holiday <- HolidayServer("excludeHolidays")



# New Patient Wait Time
output$newPtWaitTimeByDept <- renderPlot({
  #data <- dataAll_access()
  data <-   dataAll_access_new()
  
  print("new pat wait time")
  
  test_new <<- data
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
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
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
         subtitle = paste0("Based on scheduled data from ",start_date," to ",end_date)#,
         #caption = "*New patients defined by CPT codes (level of service)."
    )+
    theme_new_line()+
    theme_bw()+
    graph_theme("top")+
    geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
    #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
    scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    scale_color_manual(values=c('#212070','#d80b8c')) +
    geom_point(size = 3.2)
  # stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..)), group = value), geom="text", color="black", 
  #              size=5, fontface="bold.italic")
  
})


output$newPtWaitTimeByDeptPercent <- renderPlot({
  #data <- dataAll_access()
  data <- dataAll_access_new()
  # data_test <<- data
  # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
  
  #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
  
  waitTime_total <- data %>%
    filter(WAIT_TIME >= 0) %>%
    filter(NEW_PT2 == "NEW") %>%
    group_by(APPT_MADE_MONTH_YEAR) %>%
    dplyr::summarise(total_all = n()) %>%
    collect()
  
  waitTime_within_14_days <- data %>%
    filter(WAIT_TIME >= 0, 
           WAIT_TIME < 14.0001, 
           NEW_PT2 == "NEW") %>%
    group_by(APPT_MADE_MONTH_YEAR) %>%
    dplyr::summarise(total = n()) %>%
    collect()
  
  join_data <- inner_join(waitTime_total, waitTime_within_14_days)
  
  percent_within_14_days <- join_data %>% group_by(APPT_MADE_MONTH_YEAR) %>%
    summarise(percent = round((total/total_all),2))
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
  ggplot(percent_within_14_days, aes(x=APPT_MADE_MONTH_YEAR, y=percent, group = 1))+
    # geom_bar(stat = "identity", position = 'dodge')+
    #geom_line(aes(linetype = variable))+
    scale_linetype_manual(values=c("solid"))+
    scale_size_manual(values=c(1))+
    # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
    #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
    labs(x=NULL, y=NULL,
         #title = "Median Wait Time to New and Established Appointment Over Time",
         title = "Percent of New Patients Scheduled Within 14 Days",
         #subtitle = paste0("Based on scheduled data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
         subtitle = paste0("Based on scheduled data from ",start_date," to ",end_date)#,
         #caption = "*New patients defined by CPT codes (level of service)."
    )+
    theme_new_line()+
    theme_bw()+
    graph_theme("none")+
    #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
    #scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    scale_color_manual(values=c('#212070')) +
    geom_point(size = 3.2, color = '#d80b8c') +
    geom_line(size=1, color = '#d80b8c') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(percent_within_14_days$percent)*1.5)
    ) +
    stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = APPT_MADE_MONTH_YEAR), geom="text", color="black", 
                 size=5, fontface="bold.italic")
  
  
})

# New Patient Wait Time
output$newPtWaitTimeByProv <- renderPlot({
  #data <- dataAll_access()
  data <- dataAll_access_new()
  # data <- all.data %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
  
  #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
  
  waitTime <- data %>%
    filter(WAIT_TIME >= 0) %>%
    group_by(PROVIDER, APPT_MADE_MONTH_YEAR, NEW_PT2) %>%
    dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>% collect()
  
  waitTime$NEW_PT2 <- ifelse(waitTime$NEW_PT2 == "NEW", "New","Established")
  #waitTime$Appt.MonthYear <- as.Date(paste0(waitTime$Appt.MonthYear, "-01"), format="%Y-%m-%d") ## Create date-year column
  
  waitTime <- waitTime %>% spread(NEW_PT2, medWaitTime)
  waitTime[is.na(waitTime)] <- 0
  waitTime <- waitTime %>% gather(variable, value, 3:4)
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
  ggplot(waitTime %>% filter(variable == "Established"), aes(x=APPT_MADE_MONTH_YEAR, y=value, group=PROVIDER)) +
    geom_line(aes(color=PROVIDER), size=1) +
    # geom_hline(yintercept=14, linetype="dashed", color = "red", size=1)+
    scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
    labs(x=NULL, y=NULL, 
         title = "Median Wait Time to New and Established Appointment Over Time by Provider",
         #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
         subtitle = paste0("Based on data from ", start_date," to ",end_date)#,
         #caption = "*New patients defined by CPT codes (level of service)."
    )+
    theme_new_line()+
    theme_bw()+
    graph_theme("bottom")+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
    scale_y_continuous(expand = c(0,0), limits = c(0,max(waitTime$value)*1.5))+
    # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
    #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
    geom_point(aes(color=PROVIDER), size = 3.2)
})


output$newPtWaitTimeByProvPercent <- renderPlot({
  #data <- dataAll_access()
  data <- dataAll_access_new()
  # data_test <<- data
  # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
  
  #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
  
  waitTime_total <- data %>%
    filter(WAIT_TIME >= 0) %>%
    filter(NEW_PT2 == "NEW") %>%
    group_by(APPT_MADE_MONTH_YEAR, PROVIDER) %>%
    dplyr::summarise(total_all = n()) %>%
    collect()
  
  waitTime_within_14_days <- data %>%
    filter(WAIT_TIME >= 0, 
           WAIT_TIME < 14.0001, 
           NEW_PT2 == "NEW") %>%
    group_by(APPT_MADE_MONTH_YEAR, PROVIDER) %>%
    dplyr::summarise(total = n()) %>%
    collect()
  
  join_data <- inner_join(waitTime_total, waitTime_within_14_days)
  
  percent_within_14_days <- join_data %>% group_by(APPT_MADE_MONTH_YEAR, PROVIDER) %>%
    summarise(percent = round((total/total_all),2))
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
  ggplot(percent_within_14_days, aes(x=APPT_MADE_MONTH_YEAR, y=percent, group = PROVIDER))+
    # geom_bar(stat = "identity", position = 'dodge')+
    #geom_line(aes(linetype = variable))+
    scale_linetype_manual(values=c("solid"))+
    scale_size_manual(values=c(1))+
    # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
    #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
    labs(x=NULL, y=NULL,
         #title = "Median Wait Time to New and Established Appointment Over Time",
         title = "Percent of New Patients Scheduled Within 14 Days",
         #subtitle = paste0("Based on scheduled data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
         subtitle = paste0("Based on scheduled data from ", start_date," to ", end_date)#,
         #caption = "*New patients defined by CPT codes (level of service)."
    )+
    theme_new_line()+
    theme_bw()+
    graph_theme("bottom")+
    #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
    #scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    geom_point(aes(color=PROVIDER), size = 3.2) +
    geom_line(aes(color=PROVIDER), size=1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(percent_within_14_days$percent)*1.5)
    ) +
    stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = APPT_MADE_MONTH_YEAR), geom="text", color="black", 
                 size=5, fontface="bold.italic")
  
  
  
  
})

# New Patient Ratio by Department
output$newPtRatioByDept <- renderPlot({
  #data <- dataArrived_access_npr()
  data <- dataArrived_access_npr_new()
  
  print("npr")
  
  test_npr <<- data
  # data <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy"  )
  print("1")
  
  
  newpatients.ratio <- data %>%
    group_by(APPT_MADE_MONTH_YEAR,NEW_PT3) %>%
    dplyr::summarise(Total = sum(TOTAL_APPTS, na.rm = TRUE)) %>% collect() %>%
    mutate(NEW_PT3 = ifelse(is.na(NEW_PT3), "ESTABLISHED", NEW_PT3)) %>%
    spread(NEW_PT3, Total) 
  
  newpatients.ratio[is.na(newpatients.ratio)] <- 0
  
  
  print("1.5")
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
  
  newpatients.ratio <- newpatients.ratio %>% mutate(ratio = round(`NEW` / (`ESTABLISHED` + `NEW`),2))
  #newpatients.ratio$Appt.MonthYear <- as.Date(newpatients.ratio$Appt.MonthYear, format="%Y-%m") ## Create date-year column
  #newpatients.ratio[is.na(newpatients.ratio)] <- 0
  ggplot(newpatients.ratio, aes(x=APPT_MADE_MONTH_YEAR, y=ratio, group=1)) +
    # geom_bar(stat = "identity", width = 0.8, fill = "#221f72") +
    geom_line(size=1) +
    geom_line(color = "#221f72", size=1) +
    geom_point(color = "#221f72", size = 3.2) +
    labs(x=NULL, y=NULL,
         #title = "New Patient Ratio Trending over Time",
         title = "Monthly New Patient Ratio",
         subtitle = paste0("Based on arrived data from ", start_date," to ", end_date))+
    theme_new_line()+
    theme_bw()+
    graph_theme("none")+
    theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(newpatients.ratio$ratio)*1.5)
    ) +
    stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = APPT_MADE_MONTH_YEAR), geom="text", color="black", 
                 size=5, fontface="bold.italic")+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    geom_point(size = 3.2)+
    geom_point(color = "#221f72", size = 3.2) +
    scale_color_manual(values = c("#221f72", "#d80b8c"))
  # scale_x_date(breaks = "day", date_labels = "%Y-%m", date_breaks = "1 week",
  #              date_minor_breaks = "1 day", expand = c(0, 0.6))
  
})


# New Patient Ratio by Provideer
output$newPtRatioByProv <- renderPlot({
  #data <- dataArrived_access_npr()
  data <- dataArrived_access_npr_new()
  #data <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS", CAMPUS_SPECIALTY %in% "Allergy")
  
  print("prov_running")
  newpatients.ratio <- data %>%
    group_by(PROVIDER, APPT_MADE_MONTH_YEAR,NEW_PT2) %>%
    dplyr::summarise(Total = sum(TOTAL_APPTS, na.rm = TRUE)) %>% collect() %>%
    mutate(NEW_PT2 = ifelse(is.na(NEW_PT2), "ESTABLISHED", NEW_PT2)) %>%
    group_by(PROVIDER,APPT_MADE_MONTH_YEAR,NEW_PT2) %>%
    dplyr::summarise(Total = n()) %>%
    spread(NEW_PT2, Total)
  
  newpatients.ratio[is.na(newpatients.ratio)] <- 0
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
  newpatients.ratio <-  newpatients.ratio %>% mutate(ratio = round(`NEW` / (`ESTABLISHED` + `NEW`), 2))
  #newpatients.ratio$Appt.MonthYear <- as.Date(paste0(newpatients.ratio$Appt.MonthYear, "-01"), format="%Y-%m-%d") ## Create date-year column
  
  ggplot(newpatients.ratio, aes(x=APPT_MADE_MONTH_YEAR, y=ratio, group = PROVIDER)) +
    geom_line(aes(color=PROVIDER), size=1) +
    # scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
    scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
    labs(x=NULL, y=NULL, 
         title = "New Patient Ratio Over Time by Provider",
         subtitle = paste0("Based on data from ", start_date," to ", end_date))+
    theme_new_line()+
    theme_bw()+
    graph_theme("bottom")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,max(newpatients.ratio$ratio)*1.5))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
    geom_point(aes(color=PROVIDER), size = 3.2)
  # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
  #              date_minor_breaks = "1 day", expand = c(0, 0.6))
  
})

# New Patient Wait Time
output$newPtApptSourceByDept <- renderPlot({
  data <- dataArrived_access_new()
  # data <- kpi.all.data[arrivedNoShow.data.rows,]
  
  print("2")
  newpatients.ratio <- data %>%
    filter(NEW_PT2 == "NEW") %>%
    group_by(SCHEDULE_GROUPING_MAPPED, NEW_PT2) %>%
    dplyr::summarise(Total = n()) %>% collect()
  
  start_date <- isolate(selected_dateRange()[1])
  end_date <- isolate(selected_dateRange()[2])
  
  #newpatients.ratio$APPT_SOURCE_NEW[which(newpatients.ratio$APPT_SOURCE_NEW == "Other")] <- "Practice"
  
  newpatients.ratio$ratio <- round(newpatients.ratio$Total / sum(newpatients.ratio$Total), 2)
  
  newRatio <-
    ggplot(newpatients.ratio, aes(x=factor(SCHEDULE_GROUPING_MAPPED
                                           #, levels = c("Practice","Access Center","My MountSinai/MyChart","StayWell","Zocdoc", "FindADoc")
    ), 
    y=ratio, group=SCHEDULE_GROUPING_MAPPED, fill=SCHEDULE_GROUPING_MAPPED)) +
    geom_bar(stat="identity", width = 0.8) +
    coord_flip() +
    scale_fill_MountSinai('purple')+
    labs(x=NULL, y=NULL,
         title = "New Patient Source",
         subtitle = paste0("Based on scheduled data from ", start_date," to ", end_date,
                           "\nTotal New Patients = ",prettyNum(sum(newpatients.ratio$Total), big.mark = ','))
    )+
    theme_new_line()+
    theme_bw()+
    theme(
      plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
      plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
      axis.text.y = element_text(size = "14"))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(newpatients.ratio$ratio)*1.3))+
    geom_text(aes(label=paste0(ratio*100,"%")), color="black", 
              size=5, position = position_dodge(1), hjust=-.5)
  
  
  #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
  
  waitTime <- dataAll_access_new() %>%
    filter(WAIT_TIME >= 0) %>%
    group_by(SCHEDULE_GROUPING_MAPPED, NEW_PT2) %>%
    dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
    filter(NEW_PT2 == "NEW") %>% collect()
  waitTime$target <- 14
  
  #waitTime$SCHEDULE_GROUPING_MAPPED[which(waitTime$SCHEDULE_GROUPING_MAPPED == "Other")] <- "Practice"
  
  newWaitTime <-
    ggplot(waitTime, aes(x=factor(SCHEDULE_GROUPING_MAPPED#, levels = c("Practice","Access Center","My MountSinai/MyChart","StayWell","Zocdoc", "FindADoc")
    ), 
    y=medWaitTime, group=SCHEDULE_GROUPING_MAPPED, fill=SCHEDULE_GROUPING_MAPPED)) +
    geom_bar(stat="identity", width = 0.8) +
    geom_hline(aes(yintercept=target), linetype="dashed", color = "red", size=1)+
    scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.3))+
    coord_flip() +
    scale_fill_MountSinai('pink')+
    labs(x=NULL, y=NULL, 
         title = "Median Wait Time to New Appointment",
         subtitle = paste0("Based scheduled on data from ", start_date," to ", end_date,
                           "\nWait Time = (Scheduled Appt Date - Appt Made Date)"),
         # caption = "*Based on all of scheduled patients\n**New patients defined by CPT codes (level of service)."
         #caption = "*New patients defined by CPT codes (level of service)."
    )+
    theme_new_line()+
    theme_bw()+
    theme(
      plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
      plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
      axis.text.y = element_text(size = "14"))+
    geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
    geom_text(aes(label=paste0(medWaitTime," days")), color="black", 
              size=5, position = position_dodge(1), hjust=-.5)
  
  
  
  # No Show Rate
  
  data.noShow <- dataArrivedNoShow_new() %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled"))
  # data.noShow <- arrivedNoShow.data
  
  print("3")
  noShows <- data.noShow %>%
    filter(NEW_PT2 == "NEW") %>%
    group_by(SCHEDULE_GROUPING_MAPPED, APPT_STATUS) %>%
    dplyr::summarise(Total = n()) %>% collect() %>%
    spread(APPT_STATUS, Total)
  
  noShows[is.na(noShows)] <- 0
  
  noShows$`No Show Perc` <- round((noShows$`No Show` + noShows$`Canceled`)/(noShows$Arrived + noShows$`No Show` + noShows$`Canceled`),2)
  noShows$SCHEDULE_GROUPING_MAPPED[which(noShows$SCHEDULE_GROUPING_MAPPED == "Other")] <- "Practice"
  
  
  #noShows$SCHEDULE_GROUPING_MAPPED <- ifelse(noShows$SCHEDULE_GROUPING_MAPPED == "Other", "Practice", noShows$SCHEDULE_GROUPING_MAPPED)
  
  
  
  newNoShow <-
    
    ggplot(noShows, aes(x=factor(SCHEDULE_GROUPING_MAPPED#, levels =  c("Practice","Access Center","My MountSinai/MyChart","StayWell","Zocdoc", "FindADoc")
    ), 
    y=`No Show Perc`, group=SCHEDULE_GROUPING_MAPPED, fill=SCHEDULE_GROUPING_MAPPED)) +
    geom_bar(stat="identity", width = 0.8) +
    scale_y_continuous(limits=c(0,max(noShows$`No Show Perc`))*1.3)+
    coord_flip() +
    scale_fill_MountSinai('blue')+
    labs(x=NULL, y=NULL,
         title = "New Patient No Show Rate*",
         subtitle = paste0("Based on scheduled data from ", start_date," to ", end_date),
         caption = "*No Show Rate = (No Show + Same-day Canceled) / (Arrived + No Show + Same-day Canceled)"
    )+
    theme_new_line()+
    theme_bw()+
    theme(
      plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
      plot.caption = element_text(hjust = 0.95, size = 10, face = "italic"),
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
      axis.text.y = element_text(size = "14"))+
    geom_text(aes(label=paste0(`No Show Perc`*100,"%")), color="black", 
              size=5, position = position_dodge(1), hjust=-.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(newpatients.ratio$ratio)*1.3))
  
  
  grid.arrange(newRatio, newWaitTime, newNoShow, ncol=3)
  
})
}