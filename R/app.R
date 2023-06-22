#' GNOSIS Shiny App
#'
#' @description
#' GNOSIS
#' @import shiny shinydashboard shinydashboardPlus dashboardthemes fontawesome shinyWidgets shinycssloaders shinymeta tidyverse DT
#' @export

GNOSIS <- function(...) {

  #  source("/home/lydia/PhD_Insync/PhD_Project/Shiny_App_Code/GNOSIS/R/Setup.R")
    setup()

    ui <- dashboardPage(

        # Set title, title width, logo
        header = dashboardHeader(title = tagList(span(class = "logo-lg", "GNOSIS"))),

        # Create side-bar menu with all tab options:
        sidebar = dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 60px}"), width = 195,
                                  GNOSIS_sidebarUI("sidebar")),

        # Set up theme - use custom theme and import css file
        body = dashboardBody(
            shinyDashboardThemes(theme = "blue_gradient"),
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Style_File.css")),
            tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
            ## Tab 1) Input Data Files
            # Input File Tab: Space to upload desired files (Clinical Patient and Sample, CNA Scores and MAF) or use cBioPortal API to select study
            tabItems(tabItem(tabName = "input_files_1", tabBox(width = "1000px", Tab1_Input_Files_Manual_UI("tab1_input"),
                                                               Tab1_Input_Files_API_UI("tab1_inputAPI"))),
                     tabItem(tabName = "input_files_2", tabBox(width = "1000px",
                                                               Tab1_Input_Files_Preview_Clin_UI("tab1_input_preview_1"),
                                                               Tab1_Input_Files_Preview_CNA_UI("tab1_input_preview_2"),
                                                               Tab1_Input_Files_Preview_MAF_UI("tab1_input_preview_3"))),
            ## Tab 2 -  Tables
            # Exploratory Table Tab: Space to explore uploaded data -> choose 5 columns to study
            tabItem(tabName = "tables",
                    tabBox(width = "1000px",
                           Tab2_Exploaratory_Tables_Clin_UI("tab2_clin_table"),
                           Tab2_Exploaratory_Tables_CNA_UI("tab2_cna_table"),
                           Tab2_Exploaratory_Tables_MAF_UI("tab2_maf_table"))))

    ))

    server <- function(input, output, session) {

        ## For Input Log
        #  track_usage(store_null(console = FALSE))

        ## Tab 1 - Input Files (Upload ourselves or use API)
        # Function to select row from cBioPortal Data
        selectedLine <- reactive({
            if (is.null(input$`tab1_inputAPI-cBioData_rows_selected`)) {
                return(NULL)
            } else {
                rows_selected <- as.numeric(input$`tab1_inputAPI-cBioData_rows_selected`)
            }
        })

        datalist <- Tab1_Input_Files_Manual_Server("tab1_input", rowselect = selectedLine)

        # Tab 1 - Input Data Files
        ## Row/Column
        output$TotalC <- metaRender(renderPrint, {Count_Col(..(datalist[["patient_manual_data"]]()))})
        output$TotalR <- metaRender(renderPrint, {Count_Row(..(datalist[["patient_manual_data"]]()))})

        output$TotalC1 <- metaRender(renderPrint, {Count_Col(..(datalist[["sample_manual_data"]]()))})
        output$TotalR2 <- metaRender(renderPrint, {Count_Row(..(datalist[["sample_manual_data"]]()))})

        output$TotalCCNA <- metaRender(renderPrint, {Count_Col(..(datalist[["CNA_manual_data"]]()))})
        output$TotalRCNA <-  metaRender(renderPrint, {Count_Row(..(datalist[["CNA_manual_data"]]()))})

        output$TotalCMAF <- metaRender(renderPrint, {Count_Col(..(datalist[["MAF_manual_data"]]()))})
        output$TotalRMAF <- metaRender(renderPrint, {Count_Row(..(datalist[["MAF_manual_data"]]()))})

         ## API study dataframe
         Tab1_API_Files_Server("tab1_inputAPI")

        ## Data Preview
         Tab1_Input_Files_Preview_Server("tab1_input_preview_1", datalist, "Combined_clin", length_px = "650px", select_dt = "multiple")
         Tab1_Input_Files_Preview_Server("tab1_input_preview_2", datalist, "CNA_Val", length_px = "650px", select_dt = "multiple")
         Tab1_Input_Files_Preview_Server("tab1_input_preview_3", datalist, "MAF_Val", length_px = "650px", select_dt = "multiple")

        # Tab 2 - Exploratory Tables Tab
        ## Tables
         Tab2_Exploratory_Tables_Server("tab2_clin_table", datalist, "Combined_clin", length_px = "450px", select_dt = "multiple")
         Tab2_Exploratory_Tables_Server("tab2_cna_table", datalist, "CNA_Val", length_px = "450px", select_dt = "multiple")
         Tab2_Exploratory_Tables_Server("tab2_maf_table", datalist, "MAF_Val", length_px = "450px", select_dt = "multiple")

    }

    shinyApp(ui, server, ...)
}

GNOSIS()
