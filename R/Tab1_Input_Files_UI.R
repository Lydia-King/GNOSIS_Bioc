Tab1_Input_Files_Manual_UI <- function(tab1_input_manual) {
    ns <- NS(tab1_input_manual)
    tabPanel("Manual Upload",
             fluidRow(box(title = "Input Patient Data File", width = 3, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          fileInput(ns("Input_Patient_File"), "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                          prettyToggle(inputId = ns("Tab1_Clin_Header_Yes_or_No"), label_on = "Header", label_off = "Header", icon_on = fa_i(name ="check", verify_fa = FALSE), icon_off = fa_i(name ="times", verify_fa = FALSE), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                          fluidRow(column(7, awesomeRadio(inputId = ns("Tab1_Clin_Separator"), label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                   column(5, awesomeRadio(ns("Tab1_Clin_Quote"), "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                          numericInput(inputId = ns("Tab1_Clin_Skip_Lines"), label = "Number of Lines to Skip:", value = 4, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                          h5(strong("Total Number of Clinical Variables:")), verbatimTextOutput('TotalC', placeholder = T),  # Space to display number of rows/columns
                          h5(strong("Total Number of Patients:")),
                          tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: rgba(44,222,235) !important;}')),
                          verbatimTextOutput('TotalR', placeholder = T)),

                      box(collapsible = T, title = "Input Sample Data File", width = 3, status = "primary", solidHeader = TRUE,
                          fileInput(ns("Input_Sample_File"), "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                          prettyToggle(inputId = ns("Tab1_Sample_Header_Yes_or_No"), label_on = "Header", label_off = "Header", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                          fluidRow(column(7, awesomeRadio(inputId = ns("Tab1_Sample_Separator"), label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                   column(5, awesomeRadio(ns("Tab1_Sample_Quote"), "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                          numericInput(inputId = ns("Tab1_Sample_Skip_Lines"), label = "Number of Lines to Skip:", value = 4, min = 0, max = 10),  tags$hr(), # Input: Choose number of lines to skip
                          h5(strong("Total Number of Clinical Variables:")), verbatimTextOutput('TotalC1', placeholder = T), # Space to display number of rows/columns
                          h5(strong("Total Number of Patients:")), verbatimTextOutput('TotalR2', placeholder = T)),

                      box(title = "Input CNA File", status = "primary", width = 3, solidHeader = TRUE, collapsible = T,
                          fileInput(ns("Input_CNA_File"), "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),  tags$hr(),
                          prettyToggle(inputId = ns("Tab1_CNA_Header_Yes_or_No"), label_on = "Header", label_off = "Header", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                          fluidRow(column(7, awesomeRadio(inputId = ns("Tab1_CNA_Separator"), label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                   column(5, awesomeRadio(ns("Tab1_CNA_Quote"), "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                          numericInput(inputId = ns("Tab1_CNA_Skip_Lines"), label = "Number of Lines to Skip:", value = 0, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                          h5(strong("Total Number of Columns:")), verbatimTextOutput('TotalCCNA', placeholder = T), # Space to display number of rows/columns
                          h5(strong("Total Number of Genes:")), verbatimTextOutput('TotalRCNA', placeholder = T)),

                      box(collapsible = T, title = "Input MAF File", width = 3, status = "primary", solidHeader = TRUE,
                          fileInput(ns("Input_MAF_File"), "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                          prettyToggle(inputId = ns("Tab1_MAF_Header_Yes_or_No"), label_on = "Header", label_off = "Header", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                          fluidRow(column(7, awesomeRadio(inputId = ns("Tab1_MAF_Separator"), label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                   column(5, awesomeRadio(ns("Tab1_MAF_Quote"), "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                          numericInput(inputId = ns("Tab1_MAF_Skip_Lines"), label = "Number of Lines to Skip:", value = 1, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                          h5(strong("Total Number of Columns:")), verbatimTextOutput('TotalCMAF', placeholder = T), # Space to display number of rows/columns
                          h5(strong("Total Number of Mutations:")), verbatimTextOutput('TotalRMAF', placeholder = T))))
}

Tab1_Input_Files_API_UI <- function(tab1_input_API_ui) {
    ns <- NS(tab1_input_API_ui)
    # API upload tab
    tabPanel("API Upload", fluidRow(box(collapsible = T, title = "cBioPortal Datasets", width = 12, status = "primary", solidHeader = TRUE,
                                        withSpinner(DT::dataTableOutput(ns("cBioData"))), style = "height:800px")))

}

Tab1_Input_Files_Preview_Clin_UI <- function(tab1_input_preview_clin_ui) {
    ns <- NS(tab1_input_preview_clin_ui)
    # Preview Clinical Data
    tabPanel("Clinical Data",
             fluidRow(box(collapsible = T, title = "Preview", width = 12, status = "primary", solidHeader = TRUE,  shinyjs::useShinyjs(),
                          textOutput(ns("text")), withSpinner(DT::dataTableOutput(ns("Preview"))), style = "height:800px")))
}

Tab1_Input_Files_Preview_CNA_UI <- function(tab1_input_preview_CNA_ui) {
    ns <- NS(tab1_input_preview_CNA_ui)
    # Preview CNA Data
    tabPanel("CNA Data",
             fluidRow(box(collapsible = T, title = "Preview", width = 12, status = "primary", solidHeader = TRUE,  shinyjs::useShinyjs(), textOutput(ns("text")),
                          withSpinner(DT::dataTableOutput(ns("Preview"))), style = "height:800px")))
}

Tab1_Input_Files_Preview_MAF_UI <- function(tab1_input_preview_MAF_ui) {
    ns <- NS(tab1_input_preview_MAF_ui)
    # Preview Mutation Data
    tabPanel("Mutation Data",
             fluidRow(box(collapsible = T, title = "Preview", width = 12, status = "primary", solidHeader = TRUE, shinyjs::useShinyjs(),
                          textOutput(ns("text")), withSpinner(DT::dataTableOutput(ns("Preview"))), style = "height:800px")))

}

