## Tab 5 - Survival Analysis
# 1) Survival Analysis KM plots -> 1) KM Survival curves and log rank tests (Clinical Variables)

Tab5_KM_Clin_UI  <- function(id) {
    ns <- NS(id)
    tagList(
    box(title = ("Kaplan-Meier Plot for Clinical Variables"),
        dropdownMenu = boxDropdown(icon = fa_i(name = "info-circle", verify_fa = FALSE), boxDropdownItem(
            HTML(paste("Useful Resources for Survival Analysis:", "<br/>")),
            tags$a(href="http://www.sthda.com/english/wiki/survival-analysis-basics", "Survival Analysis Basics in R"),
            tags$a(href="https://www.datacamp.com/tutorial/survival-analysis-R", "Survival Analysis in R Tutorial For Beginners"),
            tags$a(href="https://www.statology.org/log-rank-test-in-r/", "How to Perform a Log Rank Test in R"))),
        collapsible = T, solidHeader=T, width = 12, style = "height:520px; overflow-y: hidden", height = "520px", status = "primary", withSpinner(plotOutput(ns("Plot"), height="500px")),
        sidebar = boxSidebar(width = 25, id = "Tab5_KM_Clinical_Sidebar",  background = "#599740",  icon = shiny::icon("rectangle-list"), selectizeInput(ns("Tab5_KM_Clinical_Survival_Time"), "Survival Time:", choices = ""), selectizeInput(ns("Tab5_KM_Clinical_Event_Status"), "Event Status:", choices = ""),
                             selectizeInput(ns("Tab5_KM_Clinical_Select_Variable"), "Select Variable:", choices = ""), tags$hr(), # Select survival variables i.e. time to event, event status Aand variable of interest
                             prettyToggle(inputId = ns("Tab5_KM_Clinical_Display_CI"), label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                             prettyToggle(inputId = ns("Tab5_KM_Clinical_Display_Risk_Table"), label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                             prettyToggle(inputId = ns("Tab5_KM_Clinical_Display_Pval"), label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                             selectInput(ns("Tab5_KM_Clinical_Legend_Position"), "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), tags$hr(),
                             textInput(ns("Tab5_KM_Clinical_Plot_Title"), "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                             textInput(ns("Tab5_KM_Clinical_X_Axis_Title"), "X-axis Title:", "Survival Time", placeholder = T), textInput(ns("Tab5_KM_Clinical_Y_Axis_Title"), "Y-axis Title:", "Survival Probability", placeholder = T),
                             textInput(ns("Tab5_KM_Clinical_Legend_Title"), "Legend Title:", "Legend", placeholder = T), tags$hr(), numericInput(ns("Plot_Width"), "Plot Width (inches):", value=8, min=1, max=50),
                             numericInput(ns("Plot_Height"), "Plot Height (inches):", value=5, min=1, max=50), tags$hr(), downloadButton(ns('Download_PNG'),'Download Plot (PNG)', style = "width:100%;"),
                             br(), br(), downloadButton(ns('Download_SVG'),'Download Plot (SVG)', style = "width:100%;"))),
    box(title = ("Logrank Test"), collapsible = T, style="overflow-y: hidden", solidHeader = T, width = 12, status = "primary", withSpinner(verbatimTextOutput(ns("KMlogrank")))))
}

# 2) Segmented (Quartiled) Survival Plots
Tab5_KM_Quart_UI  <- function(id) {
    ns <- NS(id)
    tagList(
        box(title = ("Kaplan-Meier Plot for CNA Scores and Quartiles"),
            dropdownMenu = boxDropdown(icon = fa_i(name = "info-circle", verify_fa = FALSE), boxDropdownItem(
                HTML(paste("Useful Resources for Survival Analysis:", "<br/>")),
                tags$a(href="http://www.sthda.com/english/wiki/survival-analysis-basics", "Survival Analysis Basics in R"),
                tags$a(href="https://www.datacamp.com/tutorial/survival-analysis-R", "Survival Analysis in R Tutorial For Beginners"),
                tags$a(href="https://www.statology.org/log-rank-test-in-r/", "How to Perform a Log Rank Test in R"))),
            width = 12, status = "primary", solidHeader = T,  style = "height:520px; overflow-y: hidden", height = "520px", collapsible = T, withSpinner(plotOutput(ns("Plot"), height = "500px")),
            sidebar = boxSidebar(width = 25, id = "Tab5_KM_CNA_Quartile_Sidebar",  background = "#599740",  icon = shiny::icon("rectangle-list"), selectizeInput(ns("Tab5_KM_CNA_Survival_Time"), "Survival Time Column:", choices = ""), selectizeInput(ns("Tab5_KM_CNA_Event_Status"), "Event Status Column:", choices = ""), selectizeInput(ns("Tab5_KM_CNA_Select_Variable"), "Select Variable:", choices = ""), tags$hr(), # Variables of Interest
                                 prettyToggle(inputId = ns("Tab5_KM_CNA_Display_CI"), label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 prettyToggle(inputId = ns("Tab5_KM_CNA_Display_Risk_Table"), label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 prettyToggle(inputId = ns("Tab5_KM_CNA_Display_Pval"), label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 selectInput(ns("Tab5_KM_CNA_Legend_Position"), "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), tags$hr(),
                                 textInput(ns("Tab5_KM_CNA_Plot_Title"), "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                 textInput(ns("Tab5_KM_CNA_X_Axis_Title"), "X-axis Title:", "Survival Time", placeholder = T), textInput(ns("Tab5_KM_CNA_Y_Axis_Title"), "Y-axis Title:", "Survival Probability", placeholder = T),
                                 textInput(ns("Tab5_KM_CNA_Legend_Title"), "Legend Title:", "Legend", placeholder = T),
                                 tags$hr(), numericInput(ns("Plot_Width"), "Plot Width (inches):", value=8, min=1, max=50),
                                 numericInput(ns("Plot_Height"), "Plot Height (inches):", value=5, min=1, max=50), tags$hr(),
                                 downloadButton(ns('Download_PNG'),'Download Plot (PNG)', style = "width:100%;"),  br(), br(), downloadButton(ns('Download_SVG'),'Download Plot (SVG)', style = "width:100%;"))),
        box(title = ("Logrank Test"), collapsible = T, style = "overflow-y: hidden", width = 12, status = "primary", solidHeader = T, withSpinner(verbatimTextOutput(ns("KMlogrank1"))))) # Logrank Test
}

## 3) Survival Analysis- Split on Specific Variable (Allows comparison between groups (only binary outcomes))

Tab5_KM_Treatment_1_UI  <- function(id) {
    ns <- NS(id)
        box(title = ("Kaplan-Meier Plot for Treatment - Yes"),
            style = "height:500px", height = "500px", solidHeader=T, width = 12, collapsible = T, status = "primary", withSpinner(plotOutput(ns("Plot"), height = "465px")),
            dropdownMenu = boxDropdown(icon = fa_i(name = "info-circle", verify_fa = FALSE), boxDropdownItem(
                HTML(paste("Note: Chosen treatment variable must be coded as YES/NO i.e. either got treatment or did not.",
                           "Useful Resources for Survival Analysis:", sep="<br/> <br/>")),
                tags$a(href="http://www.sthda.com/english/wiki/survival-analysis-basics", "Survival Analysis Basics in R"),
                tags$a(href="https://www.datacamp.com/tutorial/survival-analysis-R", "Survival Analysis in R Tutorial For Beginners"),
                tags$a(href="https://www.statology.org/log-rank-test-in-r/", "How to Perform a Log Rank Test in R"),id = "dropdownItem1")),
            sidebar = boxSidebar(width = 25, id = "Tab5_KM_Treatment_Sidebar_Yes",  background = "#599740",  icon = shiny::icon("rectangle-list"), selectizeInput(ns("Tab5_KM_Treatment_Survival_Time"), "Survival Time:", choices = ""), selectizeInput(ns("Tab5_KM_Treatment_Event_Status"), "Event Status:", choices = ""),
                                 selectizeInput(ns("Tab5_KM_Treatment_Select_Variable"), "Select Variable:", choices = ""), selectizeInput(ns("Tab5_KM_Treatment_Variable"), "Treatment Variable:", choices = ""), tags$hr(),
                                 prettyToggle(inputId = ns("Tab5_KM_Treatment_Yes_Display_CI"), label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 prettyToggle(inputId = ns("Tab5_KM_Treatment_Yes_Display_Risk_Table"), label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 prettyToggle(inputId = ns("Tab5_KM_Treatment_Yes_Display_Pval"), label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 selectInput(ns("Tab5_KM_Treatment_Yes_Legend_Position"), "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), # Selection Option
                                 tags$hr(), textInput(ns("Tab5_KM_Treatment_Yes_Title"), "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                 textInput(ns("Tab5_KM_Treatment_Yes_X_Axis_Title"), "X-axis Title:", "Survival Time", placeholder = T), textInput(ns("Tab5_KM_Treatment_Yes_Y_Axis_Title"), "Y-axis Title:", "Survival Probability", placeholder = T),
                                 textInput(ns("Tab5_KM_Treatment_Yes_Legend_Title"), "Legend Title:", "Legend", placeholder = T),
                                 tags$hr(), numericInput(ns("Tab5_KM_Treatment_Yes_Width"), "Plot Width (inches):", value=8, min=1, max=50),
                                 numericInput(ns("Tab5_KM_Treatment_Yes_Height"), "Plot Height (inches):", value=5, min=1, max=50), tags$hr(),
                                 downloadButton(ns('Tab5_Download_KM_Treatment_Yes_PNG'),'Download Plot (PNG)', style = "width:100%;"),  br(), br(), downloadButton(ns('Tab5_Download_KM_Treatment_Yes_SVG'),'Download Plot (SVG)', style = "width:100%;")))
}

Tab5_KM_Treatment_2_UI  <- function(id) {
    ns <- NS(id)
    tagList(
        box(title = ("Kaplan-Meier Plot for Treatment - No"), style = "height:500px", height = "500px", solidHeader=T, width = 12, collapsible = T, status = "primary", withSpinner(plotOutput(ns("Plot"), height = "465px")),
            sidebar = boxSidebar(width = 25, id = "Tab5_KM_Treatment_Sidebar_No",  background = "#599740",  icon = shiny::icon("rectangle-list"),
                                 prettyToggle(inputId = ns("Tab5_KM_Treatment_No_Display_CI"), label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 prettyToggle(inputId = ns("Tab5_KM_Treatment_No_Display_Risk_Table"), label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 prettyToggle(inputId = ns("Tab5_KM_Treatment_No_Display_Pval"), label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                 selectInput(ns("Tab5_KM_Treatment_No_Legend_Position"), "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), # Selection Option
                                 tags$hr(), textInput(ns("Tab5_KM_Treatment_No_Title"), "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                 textInput(ns("Tab5_KM_Treatment_No_X_Axis_Title"), "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab5_KM_Treatment_No_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T),
                                 textInput(ns("Tab5_KM_Treatment_No_Legend_Title"), "Legend Title:", "Legend", placeholder = T),
                                 tags$hr(), numericInput(ns("Tab5_KM_Treatment_No_Width"), "Plot Width (inches):", value=8, min=1, max=50),
                                 numericInput(ns("Tab5_KM_Treatment_No_Height"), "Plot Height (inches):", value=5, min=1, max=50), tags$hr(),
                                 downloadButton(ns('Tab5_Download_KM_Treatment_No_PNG'),'Download Plot (PNG)', style = "width:100%;"),  br(), br(), downloadButton(ns('Tab5_Download_KM_Treatment_No_SVG'),'Download Plot (SVG)', style = "width:100%;"))),

        box(title = ("Logrank Test for Treatment - Yes"), solidHeader=T, width = 6, style = "overflow-y: hidden", status = "primary", collapsible = T, withSpinner(verbatimTextOutput(ns("KMlogrankYes")))), # LRT Group 1
        box(title = ("Logrank Test for Treatment - No"), solidHeader=T, width = 6,  style = "overflow-y: hidden",  status = "primary", collapsible = T, withSpinner(verbatimTextOutput(ns("KMlogrankNo"))))) #LRT Group 2 # KM Plot options
}

