## Tab 7 - Cox PH models
# Univariate and Multivariate Cox Tab with Assumptions:
# 1) Univariate Cox
Tab7_Univariate_CoxPH <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(title = ("Univariate Cox Proportional Hazards Model"), dropdownMenu = boxDropdown(icon = fa_i(name = "info-circle", verify_fa = FALSE), boxDropdownItem(
            HTML(paste("Useful Resources for Cox Models:", "<br/>")),
            tags$a(href="http://www.sthda.com/english/wiki/cox-proportional-hazards-model", "Cox Proportional-Hazards Model Basics in R") ,
            tags$a(href="http://www.sthda.com/english/wiki/cox-model-assumptions", "Cox Proportional-Hazards Model Assumptions in R"))), collapsible = T, solidHeader = T,  width = 12, status = "primary", withSpinner(verbatimTextOutput(ns("CoxModelOut"), placeholder = T)), # Cox models OS
            br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput(ns("UniAdjusted"))),
            sidebar = boxSidebar(id="Tab7_Univariate_Cox_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("rectangle-list"), selectizeInput(ns("Tab7_Univariate_Cox_Survival_Time"), "Survival Time:", choices = "", width = "95%"), selectizeInput(ns("Tab7_Univariate_Cox_Event_Status"), "Event Status:", choices = "", width = "95%"),
                                 selectizeInput(ns("Tab7_Univariate_Cox_Select_Variables"), "Select Variable:", choices = "", multiple = T, width = "95%"))))

}

# 2) Multivariable Cox Models
Tab7_Multivariate_CoxPH <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(title = ("Multivariable Cox Proportional Hazards Model"), height = "500px", collapsible = T, solidHeader = T,  width = 12, status = "primary",
            dropdownMenu = boxDropdown(icon = fa_i(name = "info-circle", verify_fa = FALSE), boxDropdownItem(
                HTML(paste("Useful Resources for Cox Models:", "<br/>")),
                tags$a(href="http://www.sthda.com/english/wiki/cox-proportional-hazards-model", "Cox Proportional-Hazards Model Basics in R") ,
                tags$a(href="http://www.sthda.com/english/wiki/cox-model-assumptions", "Cox Proportional-Hazards Model Assumptions in R"))),
            withSpinner(verbatimTextOutput(ns("CoxModelMultiOut"), placeholder = T)), # Cox models OS
            h5(strong("Likelihood Ratio Test:")), verbatimTextOutput(ns("LRTid"), placeholder = T),
            h5(strong("Wald Test:")), verbatimTextOutput(ns("Waldtestid"), placeholder = T),
            h5(strong("Logrank Test:")), verbatimTextOutput(ns("Logrid"), placeholder = T),
            sidebar = boxSidebar(id="Tab7_Multivariable_Cox_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("rectangle-list"), selectizeInput(ns("Tab7_Multivariable_Cox_Survival_Time"), "Survival Time:", choices = "", width = "95%"), selectizeInput(ns("Tab7_Multivariable_Cox_Event_Status"), "Event Status:", choices = "", width = "95%"),
                                 selectizeInput(ns("Tab7_Multivariable_Cox_Select_Variables"), "Select Variable:", choices = "", multiple = T, width = "95%"), selectizeInput(ns("Tab7_Multivariable_Cox_Select_Interaction_Variables_1"), "Select 2-Way Interaction 1:", choices = "", multiple = T, width = "95%"),
                                 selectizeInput(ns("Tab7_Multivariable_Cox_Select_Interaction_Variables_2"), "Select 2-Way Interaction 2:", choices = "", multiple = T, width = "95%"), selectizeInput(ns("Tab7_Multivariable_Cox_Select_Interaction_Variables_3"), "Select 3-way Interaction:", choices = "", multiple = T, width = "95%"))))
}

# 3) Model  Assumptions
Tab7_Assumption_CoxPH <- function(id) {
    ns <- NS(id)

    fluidRow(box(title = ("Multivariable Cox PH Model Assumptions"), collapsible = T, solidHeader = T, width = 12, status = "primary",
                 dropdownMenu = boxDropdown(icon = fa_i(name = "info-circle", verify_fa = FALSE), boxDropdownItem(
                     HTML(paste("Useful Resources for Cox Models:", "<br/>")),
                     tags$a(href="http://www.sthda.com/english/wiki/cox-proportional-hazards-model", "Cox Proportional-Hazards Model Basics in R") ,
                     tags$a(href="http://www.sthda.com/english/wiki/cox-model-assumptions", "Cox Proportional-Hazards Model Assumptions in R"))),
                 sidebar = boxSidebar(id="Tab7_Cox_Assumptions_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("rectangle-list"),
                                      h5(strong("Options:")), prettyToggle(inputId = ns("Tab7_Cox_Assumptions_Display_by_Variable"), label_on = "Display by Variable", label_off = "Display by Variable", icon_on = fa_i(name = "check", verify_fa = FALSE), icon_off = fa_i(name = "times", verify_fa = FALSE)),
                                      tags$hr(width = "96%"), numericInput(ns("Tab7_Cox_Assumptions_Plot_Width"), "Plot Width (inches):", value=8, min=1, max=50, width = "95%"),
                                      numericInput(ns("Tab7_Cox_Assumptions_Plot_Height"), "Plot Height (inches):", value=5, min=1, max=50,  width = "95%"),
                                      tags$hr(width = "96%"),
                                      downloadButton(ns('Tab7_Download_Cox_Assumptions_Plot_PNG'),'Download Plot (PNG)', style = "width:96%;"),  br(), br(), downloadButton(ns('Tab7_Download_Cox_Assumptions_Plot_SVG'),'Download Plot (SVG)', style = "width:96%;")),

                 withSpinner(plotOutput(ns("AssumptionsCox"), height = "600px"))))

}
