## Tab 3 - Subset/Recode/Format Data
# 1) Recode numeric/categorical variables
Tab3_Factor_Levels_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({vchoices <- c(names(datalist[[data]]()), "None Selected")
        updateSelectInput(session, "Tab3_Variables_to_Numeric", choices = vchoices)
        updateSelectInput(session, "Tab3_Variables_to_Factor", choices = vchoices)})

        Formatted_Data <- metaReactive2({
            metaExpr({..(datalist[[data]]()) %>% mutate_at(..(input$Tab3_Variables_to_Numeric), list(as.numeric)) %>% mutate_at(..(input$Tab3_Variables_to_Factor), list(as.factor))  # Convert columns to numeric/factor
            })
        })

        return(list(formatted_data = Formatted_Data))
    })

}

Tab3_Render_Print_Server <- function(id, datalist, data, expression_text){
    moduleServer(id, function(input, output, session) {
        res <- try(eval(parse(text=expression_text)), silent = TRUE)
     #   validate(need(class(res) != "try-error", "Error: Please make sure arguements are selected carefully"))
        output$DataTypes = metaRender(renderPrint, {eval(parse(text=expression_text))})
        output$DataLevels = metaRender(renderPrint, {sapply(..(datalist[[data]]()), levels)})
    })
}

# 2) Subset Based on 3 categorical variables and 2 numerical values?
# Categorical
# Subset data based on variable factor levels

Tab3_Subset_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
   #  observe({vchoicesSub <- names(datalist[["Combined_clin"]]())
   #  updateSelectInput(session, "Tab3_Subset_Variable_1", choices = vchoicesSub, selected = vchoicesSub[1])
   #  updateSelectInput(session, "Tab3_Subset_Variable_2", choices = vchoicesSub, selected = vchoicesSub[2])
   #  updateSelectInput(session, "Tab3_Subset_Variable_3", choices = vchoicesSub, selected = vchoicesSub[3])})
#
   #  observe({vchoicesSub1 <- c(levels(datalist[[data]]()[,input$Tab3_Subset_Variable_1]), "None Selected")
   #  updateSelectInput(session, "Tab3_Subset_Variable_Levels_1", choices = vchoicesSub1, selected = "None Selected")})
#
   #  observe({vchoicesSub2 <- c(levels(datalist[[data]]()[,input$Tab3_Subset_Variable_2]), "None Selected")
   #  updateSelectInput(session, "Tab3_Subset_Variable_Levels_2", choices = vchoicesSub2, selected = "None Selected")})
#
   #  observe({vchoicesSub3 <- c(levels(datalist[[data]]()[,input$Tab3_Subset_Variable_3]), "None Selected")
   #  updateSelectInput(session, "Tab3_Subset_Variable_Levels_3", choices = vchoicesSub3, selected = "None Selected")})
##
        Clinical_Sub1 <- metaReactive2({
          #  validate(need(!is.null(input$Tab3_Subset_Variable_Levels_1) & !is.null(input$Tab3_Subset_Variable_Levels_2) & !is.null(input$Tab3_Subset_Variable_Levels_3), "Please make sure there are no NULL selected variable levels"))
            if(isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
              #  validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({..(datalist[[data]]()) })
            } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
              #  validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1))})
            } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
               # validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2)) })
            } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
              #  validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3)) })
            } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
               # validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2))})
            } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
               # validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))})
            } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
                #validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))})
            } else {
              #  validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
              # validate(need("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) &  "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3), "Please make sure levels are selected correctly i.e. no None Selected."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))}) } })

   #    Clinical_Sub_1  <- metaReactive2({
   #        validate(need(!is.null(input$Tab3_Subset_Variable_Levels_1) & !is.null(input$Tab3_Subset_Variable_Levels_2) & !is.null(input$Tab3_Subset_Variable_Levels_3), "Please make sure there are no NULL selected variable levels"))
   #        if(isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({..(Formatted_Data()) })
   #        } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1))})
   #        } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2)) })
   #        } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3)) })
   #        } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2))})
   #        } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))})
   #        } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))})
   #        } else {
   #            validate(need(!is.null(input$cBioData_rows_selected) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own clinical patient and/or sample file."))
   #            validate(need("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) &  "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3), "Please make sure levels are selected correctly i.e. no None Selected."))
   #            metaExpr({filter(..(Formatted_Data()), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2), ..(Formatted_Data())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))}) } })

        Clinical_Sub <- metaReactive2({metaExpr({
            ..(Clinical_Sub1()) %>% mutate_at(..(input$Tab3_Variables_to_Factor), list(as.character)) %>%  mutate_at(..(input$Tab3_Variables_to_Factor), list(as.factor))
        }) })


        output$TableRecode1 = metaRender(renderDataTable, {
            datatable(..(Clinical_Sub()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "350px"))})

        output$TableLevels = metaRender(renderPrint, {sapply(..(Clinical_Sub())[,c(..(input$Tab3_Subset_Variable_1), ..(input$Tab3_Subset_Variable_2), ..(input$Tab3_Subset_Variable_3))], levels)})

    })
}
