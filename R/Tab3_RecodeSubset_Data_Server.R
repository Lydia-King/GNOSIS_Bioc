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
        output$DataTypes = metaRender(renderPrint, {eval(parse(text=expression_text))})
        output$DataLevels = metaRender(renderPrint, {sapply(..(datalist[[data]]()), levels)})
    })
}

# 2) Subset Based on 3 categorical variables and 2 numerical values?
# Categorical
# Subset data based on variable factor levels

Tab3_Subset_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({vchoicesSub <- names(datalist[[data]]())
        updateSelectInput(session, "Tab3_Subset_Variable_1", choices = vchoicesSub, selected = vchoicesSub[1])
        updateSelectInput(session, "Tab3_Subset_Variable_2", choices = vchoicesSub, selected = vchoicesSub[2])
        updateSelectInput(session, "Tab3_Subset_Variable_3", choices = vchoicesSub, selected = vchoicesSub[3])})

        observe({vchoicesSub1 <- c(levels(datalist[[data]]()[,input$Tab3_Subset_Variable_1]), "None Selected")
        updateSelectInput(session, "Tab3_Subset_Variable_Levels_1", choices = vchoicesSub1, selected = "None Selected")})

        observe({vchoicesSub2 <- c(levels(datalist[[data]]()[,input$Tab3_Subset_Variable_2]), "None Selected")
        updateSelectInput(session, "Tab3_Subset_Variable_Levels_2", choices = vchoicesSub2, selected = "None Selected")})

        observe({vchoicesSub3 <- c(levels(datalist[[data]]()[,input$Tab3_Subset_Variable_3]), "None Selected")
        updateSelectInput(session, "Tab3_Subset_Variable_Levels_3", choices = vchoicesSub3, selected = "None Selected")})

        Clinical_Sub1 <- metaReactive2({
            validate(need(!is.null(input$Tab3_Subset_Variable_Levels_1) & !is.null(input$Tab3_Subset_Variable_Levels_2) & !is.null(input$Tab3_Subset_Variable_Levels_3), "Please make sure there are no NULL selected variable levels"))
            if(isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
                metaExpr({..(datalist[[data]]()) })
            } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1))})
            } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2)) })
            } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3)) })
            } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & input$Tab3_Subset_Variable_Levels_3 == "None Selected")){
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2))})
            } else if (isTRUE("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) & input$Tab3_Subset_Variable_Levels_2 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))})
            } else if (isTRUE(input$Tab3_Subset_Variable_Levels_1 == "None Selected" & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3))){
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))})
            } else {
                validate(need("None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_1) &  "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_2) & "None Selected" %!in% c(input$Tab3_Subset_Variable_Levels_3), "Please make sure levels are selected correctly i.e. no None Selected."))
                metaExpr({filter(..(datalist[[data]]()), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_1)] %in% ..(input$Tab3_Subset_Variable_Levels_1), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_2)] %in% ..(input$Tab3_Subset_Variable_Levels_2), ..(datalist[[data]]())[,..(input$Tab3_Subset_Variable_3)] %in% ..(input$Tab3_Subset_Variable_Levels_3))}) } })


        Clinical_Sub <- metaReactive2({metaExpr({
            ..(Clinical_Sub1()) %>% mutate_at(c(..(input$Tab3_Subset_Variable_1), ..(input$Tab3_Subset_Variable_2), ..(input$Tab3_Subset_Variable_3)), list(as.character)) %>%  mutate_at(c(..(input$Tab3_Subset_Variable_1), ..(input$Tab3_Subset_Variable_2), ..(input$Tab3_Subset_Variable_3)), list(as.factor))
        }) })


        output$TableRecode1 = metaRender(renderDataTable, {
            datatable(..(Clinical_Sub()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "350px"))})

        output$TableLevels = metaRender(renderPrint, {sapply(..(Clinical_Sub())[,c(..(input$Tab3_Subset_Variable_1), ..(input$Tab3_Subset_Variable_2), ..(input$Tab3_Subset_Variable_3))], levels)})

        return(list(subset_data = Clinical_Sub))
    })
}

# 3) Carry out Survival recoding
Tab3_Recode_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {

        observe({vchoicesRecode <- c(names(datalist[[data]]()), "None Selected")
        updateSelectInput(session, "Tab3_Select_OS_Column", choices = vchoicesRecode, selected = "None Selected")
        updateSelectInput(session, "Tab3_Select_DSS_Column", choices = vchoicesRecode, selected = "None Selected")})

        dataClinicalSurv <- metaReactive2({ if(input$Tab3_Recode_Survival_Yes_or_No == "Yes" & is.null(datalist[[data]]())){
            return(NULL)
        } else if(input$Tab3_Select_OS_Column == "None Selected" & input$Tab3_Select_DSS_Column == "None Selected"){
            metaExpr({..(datalist[[data]]())})
        } else if(input$Tab3_Recode_Survival_Yes_or_No == "Yes" & !is.null(datalist[[data]]()) & input$Tab3_Select_OS_Column != "None Selected" & input$Tab3_Select_DSS_Column == "None Selected"){
            metaExpr({..(datalist[[data]]()) %>% mutate(OS = ifelse(..(datalist[[data]]())[,..(input$Tab3_Select_OS_Column)] == ..(input$Tab3_OS_Event), 1, 0))})
        } else if(input$Tab3_Recode_Survival_Yes_or_No == "Yes" & !is.null(datalist[[data]]()) & input$Tab3_Select_OS_Column == "None Selected" & input$Tab3_Select_DSS_Column != "None Selected"){
            metaExpr({ ..(datalist[[data]]()) %>% mutate(DSS = ifelse(..(datalist[[data]]())[,..(input$Tab3_Select_DSS_Column)] == ..(input$Tab3_DSS_Event), 1, 0)) })
        } else if(input$Tab3_Recode_Survival_Yes_or_No == "Yes" & !is.null(datalist[[data]]()) & input$Tab3_Select_OS_Column != "None Selected" & input$Tab3_Select_DSS_Column != "None Selected"){
            metaExpr({..(datalist[[data]]()) %>%  mutate(OS = ifelse(..(datalist[[data]]())[,..(input$Tab3_Select_OS_Column)] == ..(input$Tab3_OS_Event), 1, 0), DSS = ifelse(..(datalist[[data]]())[,..(input$Tab3_Select_DSS_Column)] == ..(input$Tab3_DSS_Event), 1, 0)) })
        } else if(input$Tab3_Recode_Survival_Yes_or_No == "No" & !is.null(datalist[[data]]())){
            metaExpr({..(datalist[[data]]())})
        } else {
            return(NULL) } })

        TableRecode_Pre =  metaReactive2({
            if(input$Tab3_Recode_Survival_Yes_or_No == "No" | (input$Tab3_Select_OS_Column == "None Selected" & input$Tab3_Select_DSS_Column == "None Selected")){
                metaExpr({ ..(dataClinicalSurv())})}
            else if(input$Tab3_Recode_Survival_Yes_or_No != "No" & (input$Tab3_Select_OS_Column != "None Selected" & input$Tab3_Select_DSS_Column == "None Selected")){
                metaExpr({..(dataClinicalSurv())[,c(..(input$Tab3_Select_OS_Column), "OS")]} )}
            else if(input$Tab3_Recode_Survival_Yes_or_No != "No" & (input$Tab3_Select_OS_Column == "None Selected" & input$Tab3_Select_DSS_Column != "None Selected")){
                metaExpr({ ..(dataClinicalSurv())[,c(..(input$Tab3_Select_DSS_Column), "DSS")]})}
            else if(input$Tab3_Recode_Survival_Yes_or_No != "No" & (input$Tab3_Select_OS_Column != "None Selected" & input$Tab3_Select_DSS_Column != "None Selected")){
                metaExpr({..(dataClinicalSurv())[,c(..(input$Tab3_Select_OS_Column), ..(input$Tab3_Select_DSS_Column), "OS","DSS")]}) }})

        output$TableRecode = metaRender(renderDataTable, {
            datatable(..(TableRecode_Pre()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "350px"))})

        return(list(recode_data = dataClinicalSurv))
    })
}


# Just concerning CNA and Clin datasets

Tab3_CNACalc_Server <-  function(id, datalist, datalist_Recode, data, rowselect) {
    moduleServer(id, function(input, output, session) {

        CNA_Clin <- metaReactive2({
                     validate(need("PATIENT_ID" %in% colnames(datalist[["CNA_Val"]]()) | input$Tab3_CNA_of_Interest != "None", "Please choose to calculate CNA Scores, select specific genes to analyse or make sure CNA file has PATIENT_ID column."))
                     if(input$Tab3_CNA_of_Interest == "Single Gene"){
                         gene_list <- unlist(str_split(c(input$Tab3_Select_Genes), pattern = ", "))
                         if(sum(gene_list %in% datalist[["CNA_Val"]]()[,c("Hugo_Symbol")]) != length(gene_list)){
                             return(NULL)
                         } else {
                             metaExpr({gene_list <- unlist(str_split(c(..(input$Tab3_Select_Genes)), pattern = ", "))
                             Gene <- ..(datalist[["CNA_Val"]]()) %>% filter(Hugo_Symbol %in% gene_list)
                             CNA_Status <- as.data.frame(t(Gene[,..(input$Tab3_CNA_Start_Column):ncol(Gene)]))
                             names(CNA_Status) <- gene_list
                             CNA_Status$PATIENT_ID <- rownames(CNA_Status)
                             rownames(CNA_Status) = NULL
                             CNA_Status <- CNA_Status %>% select(PATIENT_ID, all_of(gene_list)) %>% mutate_if(is.numeric, list(as.factor))
                             # Merge with Clinical
                             if(!is.null(datalist[["patient_data_test"]]())){
                             CNA_Status <- merge(..(datalist_Recode[[data]]()), CNA_Status, by.x = input$Tab3_Merge_Column, by.y = "PATIENT_ID")
                             } else {CNA_Status <- CNA_Status}

                             CNA_Status }) }
                     } else {
                         validate(need(is.numeric(datalist[["CNA_Val"]]()[,input$Tab3_CNA_Start_Column]), "Please make sure CNA start column is numeric."))
                         metaExpr({ PATIENT_ID <- colnames(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))])
                         Scores <- as.data.frame(PATIENT_ID)
                         Scores$CNA_Score <- colSums(abs(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))]), na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))
                         Scores <- na.omit(Scores) # remove NAs (Could have All and CCA)

                         # CNA Score Amp and Del for Each Patient CCA (Apply)
                         Scores$Amp_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x > 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
                         Scores$Del_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x < 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
                         Scores$Del_Score <- abs(Scores$Del_Score)

                         if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){
                             Scores$Score_Quartile <- split_quantile(x=Scores$CNA_Score, type = ..(input$Tab3_Number_of_Segments)) # Segment Scores
                         }

                         CNA_Metrics <- Scores
                         CNA_Metrics_All <- merge(..(datalist_Recode[["recode_data"]]()), CNA_Metrics, by.x = input$Tab3_Merge_Column, by.y = "PATIENT_ID") # Make sure both files have PATIENT_ID

                         if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){
                             CNA_Metrics_All$Subset_Score_Quartile <- split_quantile(x=CNA_Metrics_All$CNA_Score, type = ..(input$Tab3_Number_of_Segments)) # Segment Scores
                         }

                         CNA_Metrics_All
                         })

                }
        })
                    #     validate(need("PATIENT_ID" %in% colnames(CNA_Validated()) | input$Tab3_CNA_of_Interest != "None", "Please choose to calculate CNA Scores, select specific genes to analyse or make sure CNA file has PATIENT_ID column."))
             #       if(input$Tab3_CNA_of_Interest == "None"){
           ##             metaExpr({CNA_Status <- ..(datalist[["CNA_Val"]]()) # Make sure both files have PATIENT_ID
          #              return(CNA_Status) })
          #          }
             #   }
       #     }
                 #   } else if(input$Tab3_CNA_of_Interest == "Single Gene"){
                 #       gene_list <- unlist(str_split(c(input$Tab3_Select_Genes), pattern = ", "))
                 #       if(sum(gene_list %in% datalist[["CNA_Val"]]()[,c("Hugo_Symbol")]) != length(gene_list)){
                 #           return(NULL)
                 #       } else {
                 #           metaExpr({gene_list <- unlist(str_split(c(..(input$Tab3_Select_Genes)), pattern = ", "))
                 #           Gene <- ..(datalist[["CNA_Val"]]()) %>% filter(Hugo_Symbol %in% gene_list)
                 #           CNA_Status <- as.data.frame(t(Gene[,..(input$Tab3_CNA_Start_Column):ncol(Gene)]))
                 #           names(CNA_Status) <- gene_list
                 #           CNA_Status$PATIENT_ID <- rownames(CNA_Status)
                 #           rownames(CNA_Status) = NULL
                 #           CNA_Status <- CNA_Status %>% select(PATIENT_ID, all_of(gene_list)) %>% mutate_if(is.numeric, list(as.factor))
                 #           CNA_Status}) }
                 #   } else {
                 #       validate(need(is.numeric(datalist[["CNA_Val"]]()[,input$Tab3_CNA_Start_Column]), "Please make sure CNA start column is numeric."))
                 #       metaExpr({ PATIENT_ID <- colnames(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))])
                 #       Scores <- as.data.frame(PATIENT_ID)
                 #       Scores$CNA_Score <- colSums(abs(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))]), na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))
                 #       Scores <- na.omit(Scores) # remove NAs (Could have All and CCA)
#
                 #       # CNA Score Amp and Del for Each Patient CCA (Apply)
                 #       Scores$Amp_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x > 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
                 #       Scores$Del_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x < 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
                 #       Scores$Del_Score <- abs(Scores$Del_Score)
#
                 #       if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){ Scores$Score_Quartile <- split_quantile(x=Scores$CNA_Score, type = ..(input$Tab3_Number_of_Segments))
                 #       CNA_Metrics <- Scores %>% select(c("PATIENT_ID", "CNA_Score", "Amp_Score", "Del_Score", "Score_Quartile"))} # Segment Scores
                 #       else{CNA_Metrics <- Scores %>% select(c("PATIENT_ID", "CNA_Score", "Amp_Score", "Del_Score"))}
                 #       CNA_Metrics
                 #       })
#
   #              #   }
   #     })
        #  else {
        #     validate(need("PATIENT_ID" %in% colnames(datalist[["CNA_Val"]]()) | input$Tab3_CNA_of_Interest != "None", "Please choose to calculate CNA Scores, select specific genes to analyse or make sure CNA file has PATIENT_ID column."))
        #     if(input$Tab3_CNA_of_Interest == "Single Gene"){
        #         gene_list <- unlist(str_split(c(input$Tab3_Select_Genes), pattern = ", "))
        #         if(sum(gene_list %in% datalist[["CNA_Val"]]()[,c("Hugo_Symbol")]) != length(gene_list)){
        #             return(NULL)
        #         } else {
        #             metaExpr({gene_list <- unlist(str_split(c(..(input$Tab3_Select_Genes)), pattern = ", "))
        #             Gene <- ..(datalist[["CNA_Val"]]()) %>% filter(Hugo_Symbol %in% gene_list)
        #             CNA_Status <- as.data.frame(t(Gene[,..(input$Tab3_CNA_Start_Column):ncol(Gene)]))
        #             names(CNA_Status) <- gene_list
        #             CNA_Status$PATIENT_ID <- rownames(CNA_Status)
        #             rownames(CNA_Status) = NULL
        #             CNA_Status <- CNA_Status %>% select(PATIENT_ID, all_of(gene_list)) %>% mutate_if(is.numeric, list(as.factor))
        #             # Merge with Clinical
        #             CNA_Status <- merge(..(datalist_Recode[["recode_data"]]()), CNA_Status, by.x = "PATIENT_ID", by.y = "PATIENT_ID") # Make sure both files have PATIENT_ID
        #             CNA_Status }) }
        #     } else {
        #         validate(need(is.numeric(datalist[["CNA_Val"]]()[,input$Tab3_CNA_Start_Column]), "Please make sure CNA start column is numeric."))
        #         metaExpr({ PATIENT_ID <- colnames(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))])
        #         Scores <- as.data.frame(PATIENT_ID)
        #         Scores$CNA_Score <- colSums(abs(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))]), na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))
        #         Scores <- na.omit(Scores) # remove NAs (Could have All and CCA)

        #         # CNA Score Amp and Del for Each Patient CCA (Apply)
        #         Scores$Amp_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x > 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
        #         Scores$Del_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x < 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
        #         Scores$Del_Score <- abs(Scores$Del_Score)

        #         if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){
        #             Scores$Score_Quartile <- split_quantile(x=Scores$CNA_Score, type = ..(input$Tab3_Number_of_Segments)) # Segment Scores
        #         }

        #         CNA_Metrics <- Scores
        #         CNA_Metrics_All <- merge(..(datalist_Recode[["recode_data"]]()), CNA_Metrics, by.x = "PATIENT_ID", by.y = "PATIENT_ID") # Make sure both files have PATIENT_ID

        #         if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){
        #             CNA_Metrics_All$Subset_Score_Quartile <- split_quantile(x=CNA_Metrics_All$CNA_Score, type = ..(input$Tab3_Number_of_Segments)) # Segment Scores
        #         }

        #         CNA_Metrics_All
        #         })
        #     }
        # }
    #}#lse {
    # validate(need("PATIENT_ID" %in% colnames(datalist[["CNA_Val"]]()) | input$Tab3_CNA_of_Interest != "None", "Please choose to calculate CNA Scores, select specific genes to analyse or make sure CNA file has PATIENT_ID column."))
    # if(input$Tab3_CNA_of_Interest == "Single Gene"){
    #     gene_list <- unlist(str_split(c(input$Tab3_Select_Genes), pattern = ", "))
    #     if(sum(gene_list %in% datalist[["CNA_Val"]]()[,c("Hugo_Symbol")]) != length(gene_list)){
    #         return(NULL)
    #     } else {
    #         metaExpr({gene_list <- unlist(str_split(c(..(input$Tab3_Select_Genes)), pattern = ", "))
    #         Gene <- ..(datalist[["CNA_Val"]]()) %>% filter(Hugo_Symbol %in% gene_list)
    #         CNA_Status <- as.data.frame(t(Gene[,..(input$Tab3_CNA_Start_Column):ncol(Gene)]))
    #         names(CNA_Status) <- gene_list
    #         CNA_Status$PATIENT_ID <- rownames(CNA_Status)
    #         rownames(CNA_Status) = NULL
    #         CNA_Status <- CNA_Status %>% select(PATIENT_ID, all_of(gene_list)) %>% mutate_if(is.numeric, list(as.factor))
    #         # Merge with Clinical
    #         CNA_Status <- merge(..(datalist_Recode[["recode_data"]]()), CNA_Status, by.x = input$Tab3_Merge_Column, by.y = "PATIENT_ID") # Make sure both files have PATIENT_ID
    #         CNA_Status }) }
    # } else {
    #     validate(need(is.numeric(datalist[["CNA_Val"]]()[,input$Tab3_CNA_Start_Column]), "Please make sure CNA start column is numeric."))
    #     metaExpr({ PATIENT_ID <- colnames(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))])
    #     Scores <- as.data.frame(PATIENT_ID)
    #     Scores$CNA_Score <- colSums(abs(..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))]), na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))
    #     Scores <- na.omit(Scores) # remove NAs (Could have All and CCA)

    #     # CNA Score Amp and Del for Each Patient CCA (Apply)
    #     Scores$Amp_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x > 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
    #     Scores$Del_Score <- na.omit(apply(X = ..(datalist[["CNA_Val"]]())[,..(input$Tab3_CNA_Start_Column):ncol(..(datalist[["CNA_Val"]]()))], MARGIN = 2, function(x) sum(x[x < 0], na.rm = ..(input$Tab3_CNA_Remove_NAs_Yes_or_No))))
    #     Scores$Del_Score <- abs(Scores$Del_Score)

    #     if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){
    #         Scores$Score_Quartile <- split_quantile(x=Scores$CNA_Score, type = ..(input$Tab3_Number_of_Segments)) # Segment Scores
    #     }

    #     CNA_Metrics <- Scores
    #     CNA_Metrics_All <- merge(..(datalist_Recode[["recode_data"]]()), CNA_Metrics, by.x = input$Tab3_Merge_Column, by.y = "PATIENT_ID") # Make sure both files have PATIENT_ID

    #     if(..(input$Tab3_Segment_CNA_Yes_or_No) == "TRUE"){
    #         CNA_Metrics_All$Subset_Score_Quartile <- split_quantile(x=CNA_Metrics_All$CNA_Score, type = ..(input$Tab3_Number_of_Segments)) # Segment Scores
    #     }

    #     CNA_Metrics_All
    #     })

    # }

    #  })

    output$TableCNACalc = metaRender(renderDataTable, {
        datatable(..(CNA_Clin()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "400px"))})

#   # Dataset after all the manipulations -> use for everything going forward
#   Whole_Data <- metaReactive2({
#       validate(need(!is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File) | !is.null(rowselect()), "Please input clinical file, sample file and/or CNA file OR select cBioPortal dataset"))
#       validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(rowselect()), "Please only select cBioPortal dataset OR upload your own data"))
#       if(!is.null(input$Input_CNA_File) & input$Tab3_CNA_of_Interest != "None"){
#           metaExpr({..(CNA_Clin())})
#       } else if(!is.null(rowselect()) & input$Tab3_CNA_of_Interest == "None"){
#           metaExpr({..(datalist_Recode[["subset_data"]]())})
#       } else if(!is.null(rowselect()) & input$Tab3_CNA_of_Interest != "None"){
#           metaExpr({..(CNA_Clin())})
#       } else {
#           metaExpr({..(datalist_Recode[["subset_data"]]())})
#       }
#   })

#   output$TableData = metaRender(renderDataTable, {
#       datatable(..(Whole_Data()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "400px"))})

#   output$Tab3_Download_File = downloadHandler('Processed_Data.txt', content = function(file) {
#       write.table(Whole_Data(), file, sep=input$Tab3_Download_File_Separator, quote = input$Tab3_Download_File_Quote, row.names = input$Tab3_Download_File_Row_Names)
#   })

    })
}
