## Tab 2 - Exploratory Tables
# Tables to further explore and compare Input Files
Tab2_Exploratory_Tables_Server <- function(id, dataset, length_px, select_dt) {
    moduleServer(id, function(input, output, session) {
        observe({vchoices <- c(names(dataset), "None Selected")
        updateSelectizeInput(session, "Tab2_Column1_Variable", choices = vchoices, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab2_Column2_Variable", choices = vchoices, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab2_Column3_Variable", choices = vchoices, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab2_Column4_Variable", choices = vchoices, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab2_Column5_Variable", choices = vchoices, selected = "None Selected", server = TRUE)})

        Create_Table <- metaReactive2({
            if(input$Tab2_Column1_Variable == "None Selected" & input$Tab2_Column2_Variable == "None Selected" & input$Tab2_Column3_Variable == "None Selected" & input$Tab2_Column4_Variable == "None Selected" & input$Tab2_Column5_Variable == "None Selected"){
                return(validate("Please select data columns to analyse in sequential order i.e. select column 1 to column 5."))
            } else if(input$Tab2_Column1_Variable!= "None Selected" & input$Tab2_Column2_Variable == "None Selected" & input$Tab2_Column3_Variable== "None Selected" & input$Tab2_Column4_Variable == "None Selected" & input$Tab2_Column5_Variable == "None Selected"){
                metaExpr({as_tibble(..(dataset)[,c(..(input$Tab2_Column1_Variable))]) %>% mutate(!!..(input$Tab2_Column1_Variable) := value) %>% select(!!..(input$Tab2_Column1_Variable))})
            } else if(input$Tab2_Column1_Variable != "None Selected" & input$Tab2_Column2_Variable != "None Selected" & input$Tab2_Column3_Variable== "None Selected" & input$Tab2_Column4_Variable == "None Selected" & input$Tab2_Column5_Variable == "None Selected"){
                metaExpr({ ..(dataset)[,c(..(input$Tab2_Column1_Variable), ..(input$Tab2_Column2_Variable))]})
            } else if(input$Tab2_Column1_Variable != "None Selected" & input$Tab2_Column2_Variable != "None Selected" & input$Tab2_Column3_Variable != "None Selected" & input$Tab2_Column4_Variable == "None Selected" & input$Tab2_Column5_Variable == "None Selected"){
                metaExpr({ ..(dataset)[,c(..(input$Tab2_Column1_Variable), ..(input$Tab2_Column2_Variable), ..(input$Tab2_Column3_Variable))]})
            } else if(input$Tab2_Column1_Variable != "None Selected" & input$Tab2_Column2_Variable != "None Selected" & input$Tab2_Column3_Variable != "None Selected" & input$Tab2_Column4_Variable != "None Selected" & input$Tab2_Column5_Variable == "None Selected"){
                metaExpr({ ..(dataset)[,c(..(input$Tab2_Column1_Variable), ..(input$Tab2_Column2_Variable), ..(input$Tab2_Column3_Variable), ..(input$Tab2_Column4_Variable))]})
            } else if(input$Tab2_Column1_Variable != "None Selected" & input$Tab2_Column2_Variable != "None Selected" & input$Tab2_Column3_Variable != "None Selected" & input$Tab2_Column4_Variable != "None Selected" & input$Tab2_Column5_Variable != "None Selected"){
                metaExpr({ ..(dataset)[,c(..(input$Tab2_Column1_Variable), ..(input$Tab2_Column2_Variable), ..(input$Tab2_Column3_Variable), ..(input$Tab2_Column4_Variable), ..(input$Tab2_Column5_Variable))]})
            } else{validate("Please select data columns in sequential order i.e. column 1 to column 5.")}
        })


        output$Table <- metaRender(renderDataTable, {
            DT::datatable(..(Create_Table()), selection = select_dt, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = length_px))
            })

    })
}





