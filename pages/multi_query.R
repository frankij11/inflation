library(rhandsontable)
jic.raw <- jic
multi_query_ui <- function(){ 
  calculator <- shinydashboard::box(title = "Quick Inflation Calculator", width = 12,
                                    rHandsontableOutput("hot_calc")
                                    )
  page <-  tabPanel(
    "Multi Indice Query",
    fluidRow(
      column(3,
             selectInput(
               "sel_ver",
               "JIC Version:",
               c("Joint Inflation Calculator April 2019")
             )),
      column(
        3,
        selectInput(
          "sel_service",
          "Service:",
          c("All", jic_service),
          selected = jic_service[[1]],
          multiple = T
        )
      ),
      column(
        3,
        selectInput(
          "sel_indices",
          "Indices:",
          c("All", jic_indices),
          selected = "APN",
          multiple = T
        )
      ),
      column(
        2,
        numericInput(
          "num_by",
          "Base Year:",
          min = 1970,
          max = 2060,
          step = 1,
          value = 2020
        )
      )
      
      
    ),
    # Create a new row for the table.
    column(1,downloadButton("dwn_infl")) ,column(2,checkboxInput("chk_wide", "Wide or Long?", T)),
    DT::dataTableOutput("tbl_jic")
    
  )
  
  return(page)
}
multi_query_serv <- function(input, output, session){
  #update Base Year
  
  values <- reactiveValues(data=jic)
  observeEvent(input$num_by,{
    values$data <-  change_BY(input$num_by, jic.raw)
  })
  
  
  #update available indices
  observe({
    still_selected <- input$sel_indices
    data <- values$data
    if (input$sel_ver != "All") {
      data <- data %>% filter(Version == input$sel_ver)
    }
    if(is.null(input$sel_service)){ 
        # do nothing
    }
    else if(input$sel_service != "All"| input$sel_service =="") {
        data <- data %>% filter(Service %in% input$sel_service)
    }
    
    indices <- c("All", data %>% select(Indice) %>% unique())
    updateSelectInput(session,"sel_indices", "Indices:", choices = indices, selected <- still_selected )
  })
  # Filter data based on selections
  output$tbl_jic <-
    #print(input$sel)
    try({
      DT::renderDataTable(DT::datatable({
        data <- values$data
        # if (input$sel_ver != "All") {
        #   data <- data %>% filter(Version == input$sel_ver)
        # }
        # if (input$sel_service != "All") {
        #   data <- data %>% filter(Service %in% input$sel_service)
        # }
        
        if (is.null(input$sel_indices)){
          # do nothing
          if(!is.null(input$sel_service)){
            data <- data %>% filter(Service %in% input$sel_service)
          }
        }
        else if (input$sel_indices != "All") {
          data <- data %>% filter(Indice %in% input$sel_indices)
        }
        
        data <- data %>% mutate(Raw=round(Raw,4), Weighted= round(Weighted,4))
        if(input$chk_wide){
          data %>% select(-Service, -tags,-Long.Title) %>%
			pivot_longer(cols=c(Raw, Weighted), names_to="Type") %>%
			pivot_wider(names_from=Year, values_from=value)
        }else{
	    data
          #data %>% select(-Service,-tags, -Long.Title) %>% spread(key=Year, value=Value)
        }
        
      }))
    })
  
  output$dwn_infl <- downloadHandler(
       filename = function() {
         paste('inflation', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(values$data, con, row.names = F)
       }
     )
  
}
