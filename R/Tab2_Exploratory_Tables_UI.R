Tab2_Exploaratory_Tables_Clin_UI <- function(tab2_exploaratory_tables_clin_ui) {
    ns <- NS(tab2_exploaratory_tables_clin_ui)

    # Clinical Data Table
    tabPanel("All Clinical Data",
             fluidRow(shinydashboardPlus::box(title = "Data Table - Clinical Data", collapsible = T, width = 12, status = "primary", solidHeader = TRUE,  withSpinner(DT::dataTableOutput(ns("Table")), proxy.height = "560px"), style = "height:580px; padding:10px", # Display Data Table
                          sidebar = boxSidebar(id="Tab2_Clin_Sidebar", width = 25, h4(strong("Select Columns to Display")), background = "#599740",  icon = shiny::icon("rectangle-list"),
                                               selectizeInput(ns("Tab2_Column1_Variable"), "Select Column 1:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column2_Variable"), "Select Column 2:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column3_Variable"), "Select Column 3:", choices = "None Selected", width = "95%", selected = "None Selected"),
                                               selectizeInput(ns("Tab2_Column4_Variable"), "Select Column 4:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column5_Variable"), "Select Column 5:", choices = "None Selected", width = "95%", selected = "None Selected"))))) # Select Input: Display desired columns
}


Tab2_Exploaratory_Tables_CNA_UI <- function(tab2_exploaratory_tables_cna_ui) {
    ns <- NS(tab2_exploaratory_tables_cna_ui)

    # CNA Data Table
    tabPanel("CNA Data",
             fluidRow(shinydashboardPlus::box(title = "Data Table - CNA Data", collapsible = T,  width = 12, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput(ns("Table")), proxy.height = "560px"), style = "height:580px; padding:10px", # Display Data Table
                          sidebar = boxSidebar(id="Tab2_CNA_Sidebar", width = 25, h4(strong("Select Columns to Display")), background = "#599740",  icon = shiny::icon("rectangle-list"),
                                               selectizeInput(ns("Tab2_Column1_Variable"), "Select Column 1:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column2_Variable"), "Select Column 2:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column3_Variable"), "Select Column 3:", choices = "None Selected", width = "95%", selected = "None Selected"),
                                               selectizeInput(ns("Tab2_Column4_Variable"), "Select Column 4:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column5_Variable"), "Select Column 5:", choices = "None Selected", width = "95%", selected = "None Selected"))))) # Select Input: Display desired columns
}


Tab2_Exploaratory_Tables_MAF_UI <- function(tab2_exploaratory_tables_maf_ui) {
    ns <- NS(tab2_exploaratory_tables_maf_ui)

    # Mutation (MAF) Table
    tabPanel("Mutation Data",
             fluidRow(shinydashboardPlus::box(title = "Data Table - Mutation Data", width = 12, collapsible = T, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput(ns("Table")), proxy.height = "560px"), style = "height:580px", # Display Data Table
                          sidebar = boxSidebar(id="Tab2_MAF_Sidebar", width = 25, h4(strong("Select Columns to Display")), background = "#599740",  icon = shiny::icon("rectangle-list"),
                                               selectizeInput(ns("Tab2_Column1_Variable"), "Select Column 1:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column2_Variable"), "Select Column 2:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column3_Variable"), "Select Column 3:", choices = "None Selected", width = "95%", selected = "None Selected"),
                                               selectizeInput(ns("Tab2_Column4_Variable"), "Select Column 4:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput(ns("Tab2_Column5_Variable"), "Select Column 5:", choices = "None Selected", width = "95%", selected = "None Selected"))))) # Select Input: Display desired columns
}
