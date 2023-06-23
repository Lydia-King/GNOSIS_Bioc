Tab3_Factor_Levels_UI <- function(id) {
    ns <- NS(id)
    # 1) Convert Variables (Numeric - Factor etc.)
        fluidRow(box(collapsible = T, height = "500px", width = 12, solidHeader = TRUE, status = "primary", title = ("Clinical Variable Types"), verbatimTextOutput(ns("DataTypes")),
                     sidebar = boxSidebar(id= "Tab3_Convert_Type_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("rectangle-list"),
                                          h4(strong("Convert to Numeric")), selectInput(inputId = ns("Tab3_Variables_to_Numeric"), label = "Select Variables:",  choices = "", multiple = TRUE,  width = "97%"),
                                          h4(strong("Convert to Factor")), selectInput(inputId = ns("Tab3_Variables_to_Factor"), label = "Select Variables:",  choices = "", multiple = TRUE,  width = "97%"))),
                 box(collapsible = T, style = "height:300px; overflow-y: scroll;overflow-x: hidden", solidHeader = TRUE, width = 12, status = "primary", title = ("Clinical Variable Levels"), withSpinner(verbatimTextOutput(ns("DataLevels")), proxy.height = "280px")))
}

Tab3_Subset_UI <- function(id) {
    ns <- NS(id)
    # 2) Subset Data (Can do based on 3 factor variables)
    fluidRow(box(collapsible = T, title =  "Filter and Preview Clinical Data", height = "500px", solidHeader = T, status = "primary", width = 12, withSpinner(DT::dataTableOutput(ns("TableRecode1")), proxy.height = "460px"), # Make sure survival recoding has been implemented
        sidebar = boxSidebar(id="Tab3_Subset_Sidebar", width = 25,  background = "#599740",  icon = shiny::icon("rectangle-list"), h4(strong("Options")),
                             selectInput(ns("Tab3_Subset_Variable_1"), "1) Filter Based on Variable:", choices = "PATIENT_ID"), selectInput(ns("Tab3_Subset_Variable_Levels_1"), label = "Choose Variable Level:", choices = "None Selected", selected = "None Selected", multiple = TRUE),
                             selectInput(ns("Tab3_Subset_Variable_2"), "2) Filter Based on Variable:", choices = "PATIENT_ID"), selectInput(ns("Tab3_Subset_Variable_Levels_2"), label = "Choose Variable Level:", choices = "None Selected", selected = "None Selected", multiple = TRUE),
                             selectInput(ns("Tab3_Subset_Variable_3"), "3) Filter Based on Variable:", choices = "PATIENT_ID"), selectInput(ns("Tab3_Subset_Variable_Levels_3"), label = "Choose Variable Level:", choices = "None Selected", selected = "None Selected", multiple = TRUE))),
    box(collapsible = T, solidHeader = TRUE, height = "300px", title = "Check Selected Variable Levels", width = 12, status = "primary", style ="height:300px; overflow-y: scroll;", withSpinner(verbatimTextOutput(ns("TableLevels")), proxy.height = "180px")))

}

Tab3_Recode_UI <- function(id) {
    ns <- NS(id)
    # 3) Survival Recoding
    fluidRow(box(collapsible = T, solidHeader = TRUE, width = 3, status = "primary", title = ("Select Survival Columns"), # Select Options for recoding survival, CNA calculations start column and number of splits in data desired
                 awesomeRadio(inputId = "Tab3_Recode_Survival_Yes_or_No", label = "Recode Survival Columns:",  choices = c("No", "Yes"), selected = "No", inline = F, status = "primary"), tags$hr(),
                 selectInput("Tab3_Select_OS_Column", "Select OS Column:", choices = ""), selectInput("Tab3_Select_DSS_Column", "Select DSS Column:", choices = ""), tags$hr(),
                 textInput("Tab3_OS_Event", "OS Event:", "DECEASED"), verbatimTextOutput("value"), textInput("Tab3_DSS_Event", "DSS Event:", "Died of Disease"), verbatimTextOutput("Died of Disease")),
             box(collapsible = T, solidHeader = TRUE, title = "Check Survival Recoding", width = 9, status = "primary", style = "height:500px", withSpinner(dataTableOutput("TableRecode"), proxy.height = "480px")))
}

Tab3_CNACalc_UI <- function(id) {
    ns <- NS(id)
    # 4) CNA Calculation
    fluidRow(box(style = "height:550px", sidebar = boxSidebar(id = "Tab3_CNA_Calculation_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("rectangle-list"), h4(strong("Options")),
                                                              numericInput(inputId = "Tab3_CNA_Start_Column", label = "Indicate CNA Start Column:", value = 3,  width = "95%", min = 1), tags$hr(width = "93%"),
                                                              awesomeRadio(inputId = "Tab3_CNA_of_Interest", label = "CNA of Interest:", choices = c("None", "Single Gene", "CNA Score"), selected = "None", inline = F, status = "primary"), tags$hr(width = "93%"),
                                                              awesomeRadio(inputId = "Tab3_Merge_Column", label = "Merge on:", choices = c("PATIENT_ID", "SAMPLE_ID"), selected = "PATIENT_ID", inline = T, status = "primary"), tags$hr(width = "93%"),
                                                              conditionalPanel(condition = "input.Tab3_CNA_of_Interest == 'None'",  h5(strong("Options:"))),
                                                              conditionalPanel(condition = "input.Tab3_CNA_of_Interest== 'Single Gene'", h5(strong("Options:")), textInput("Tab3_Select_Genes", "Select Gene of Interest:", "TP53, PTEN, BRCA1, A1BG", placeholder = T,  width = "95%")),
                                                              conditionalPanel(condition = "input.Tab3_CNA_of_Interest == 'CNA Score'", h5(strong("Options:")), prettyToggle(inputId = "Tab3_CNA_Remove_NAs_Yes_or_No", label_on = "Remove NAs", label_off = "Remove NAs", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                                                               prettyToggle(inputId = "Tab3_Segment_CNA_Yes_or_No", label_on = "Segment Data", label_off = "Segment Data", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                                                               numericInput(inputId = "Tab3_Number_of_Segments", label = "Number of Segments:", value = 4, width = "95%"))),
                 collapsible = T, solidHeader = TRUE, width = 12, status = "primary", title = ("CNA Score Exploration"), withSpinner(dataTableOutput("TableCNACalc"), proxy.height = "510px")))
}

Tab3_Data_Down_UI <- function(id) {
    ns <- NS(id)
    # 5) CNA Calculation Download
    fluidRow(box(style = "height:530px", sidebar = boxSidebar(id = "Tab3_CNA_Download_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("rectangle-list"), h4(strong("Options")),
                                                              awesomeRadio(inputId = "Tab3_Download_File_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary"), tags$hr(width = "93%"), # Input: Select Separator:
                                                              prettyToggle(inputId = "Tab3_Download_File_Quote", label_on = "Include Quotes", label_off = "Include Quotes", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                                              prettyToggle(inputId = "Tab3_Download_File_Row_Names", label_on = "Include Row Names", label_off = "Include Row Names", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                                              tags$hr(width = "93%"), downloadButton('Tab3_Download_File', 'Download Data', style = "width:95%;")),
                 collapsible = T, solidHeader = TRUE, width = 12, status = "primary", title = ("Preview and Download Processed Data"), withSpinner(dataTableOutput("TableData"), proxy.height = "510px")))
}
