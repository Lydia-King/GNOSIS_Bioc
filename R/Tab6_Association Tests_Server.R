## Tab 6 - Association Tests
# Association analysis: Fishers exact test, chi-squared, ANOVA and KW

Tab6_Chi_Square_Server <- function(id, datalist, datalist1, data, rowselect) {
    moduleServer(id, function(input, output, session) {

        # 1) Chi-Squared
        observe({vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
        updateSelectizeInput(session, "Tab6_Select_Categorical_Variable_1", choices = vchoicesASS, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab6_Select_Categorical_Variable_2", choices = vchoicesASS, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab6_Select_Categorical_Variable_3", choices = vchoicesASS, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab6_Select_Continuous_Variable_1", choices = vchoicesASS, selected = "None Selected", server = TRUE)})

        # Create Table
        data_Association2 <- metaReactive2({
            metaExpr({ my_list <- list()
            for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
                my_list[[i]] <- table(..(datalist1[[data]]())[,..(input$Tab6_Select_Categorical_Variable_1)], ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_2)[i]])
            }
            my_list}) })

        # Print Each Chi-Squared Test
        Chi <- metaReactive2({
            validate(need(!is.null(rowselect()) | !is.null(datalist[["patient_manual_data"]]()) | !is.null(datalist[["sample_manual_data"]]()) | !is.null(datalist[["CNA_manual_data"]]()), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
            validate(need(c(is.null(datalist[["patient_manual_data"]]()) & is.null(datalist[["sample_manual_data"]]()) & is.null(datalist[["CNA_manual_data"]]())) | is.null(rowselect()), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
            validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2) | input$Tab6_Select_Categorical_Variable_2 == "None Selected" | input$Tab6_Select_Categorical_Variable_1 == "None Selected", paste("\n \n Please select categorical variables of interest", "\n", "\n", "\n")))
            if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){return(NULL)}
            else{metaExpr({for(i in 1:length(c(..(input$Tab6_Select_Categorical_Variable_2)))){
                cat(noquote(paste("Categorical Variable 1:", ..(input$Tab6_Select_Categorical_Variable_1), "and", "Categorical Variable 2:", ..(input$Tab6_Select_Categorical_Variable_2)[i], "\n")))
                print(chisq.test(..(data_Association2())[[i]]))}})}
        })

        Chi_Code <- metaReactive2({
            validate(need(!is.null(rowselect()) | !is.null(datalist[["patient_manual_data"]]()) | !is.null(datalist[["sample_manual_data"]]()) | !is.null(datalist[["CNA_manual_data"]]()), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
            validate(need(c(is.null(datalist[["patient_manual_data"]]()) & is.null(datalist[["sample_manual_data"]]()) & is.null(datalist[["CNA_manual_data"]]())) | is.null(rowselect()), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
            validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2) | input$Tab6_Select_Categorical_Variable_2 == "None Selected" | input$Tab6_Select_Categorical_Variable_1 == "None Selected", paste("\n \n Please select categorical variables of interest", "\n", "\n", "\n")))
            if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){return(NULL)}
            else{metaExpr({
                Chi_List <- list()
                for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
                    name <- ..(input$Tab6_Select_Categorical_Variable_2)[i]
                    Chi_List[[name]] <- chisq.test(..(data_Association2())[[i]])}
                Chi_List})}
        })

        output$Cat1 <-  metaRender(renderPrint, { ..(Chi()) })

        data_Association1Ad <- metaReactive2({
            validate(need(!is.null(rowselect()) | !is.null(datalist[["patient_manual_data"]]()) | !is.null(datalist[["sample_manual_data"]]()) | !is.null(datalist[["CNA_manual_data"]]()), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
            validate(need(c(is.null(datalist[["patient_manual_data"]]()) & is.null(datalist[["sample_manual_data"]]()) & is.null(datalist[["CNA_manual_data"]]())) | is.null(rowselect()), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
            validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2) | "None Selected" %!in% input$Tab6_Select_Categorical_Variable_2  | "None Selected" %!in% input$Tab6_Select_Categorical_Variable_1, "\n \n Please select categorical variables of interest \n \n \n"))

            if(isTRUE("None Selected" %in% input$Tab6_Select_Categorical_Variable_1 |  "None Selected" %in% input$Tab6_Select_Categorical_Variable_2)){
                data.frame(Variables = character(10),
                           X = character(10),
                           df = character(10),
                           Pval = character(10),
                           Adj_Pval = character(10))}
            else{
                metaExpr({
                    Variables <- Pval <- X <- df <- c()

                    for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
                        Variables <- c(Variables, paste(..(input$Tab6_Select_Categorical_Variable_1), "&", ..(input$Tab6_Select_Categorical_Variable_2)[i], sep=" "))
                        Test <- chisq.test(..(data_Association2())[[i]])
                        X <- c(X, round(as.numeric(Test$statistic), digits = 3))
                        df <- c(df, as.numeric(Test$parameter))
                        Pval <- c(Pval, signif(Test$p.value, digits=3))
                    }
                    Table <- cbind.data.frame(Variables, X, df, Pval)
                    Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n=length(..(input$Tab6_Select_Categorical_Variable_2))), digits=3)
                    Table %>% select(Variables, X, df, Pval, Adj_Pval)})
            }})

        # Data Table with Adjusted P-values
        output$Cat1Ad <- metaRender(renderDataTable, {
            datatable(..(data_Association1Ad()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))})
    })
}

## 2) Fishers Exact Test
## Simulated Fishers Test
#SimF <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2), paste("\n \n Please select categorical variables of interest", "\n", "\n", "\n")))
#    if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){return(NULL)}
#    else{metaExpr({for(i in 1:length(c(..(input$Tab6_Select_Categorical_Variable_2)))){
#        cat(noquote(paste("Categorical Variable 1:", ..(input$Tab6_Select_Categorical_Variable_1), "and", "Categorical Variable 2:", ..(input$Tab6_Select_Categorical_Variable_2)[i], "\n")))
#        print(fisher.test(..(data_Association2())[[i]], simulate.p.value = T))}})}
#})
#
#SimF_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2), paste("\n \n Please select categorical variables of interest", "\n", "\n", "\n")))
#    if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        SimF_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
#            name <- ..(input$Tab6_Select_Categorical_Variable_2)[i]
#            SimF_List[[name]] <- fisher.test(..(data_Association2())[[i]], simulate.p.value = T)}
#        SimF_List})}})
#
#output$Cat2 <-  metaRender(renderPrint, { ..(SimF()) })
#
#data_Association2Ad <-  metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2), paste("\n \n Please select categorical variables of interest", "\n", "\n", "\n")))
#
#    if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){
#        data.frame(Variables = character(10),
#                   Pval = character(10),
#                   Adj_Pval = character(10))}
#    else{
#        metaExpr({
#            Variables <- Pval <- c()
#
#            for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
#                Variables <- c(Variables, paste(..(input$Tab6_Select_Categorical_Variable_1), "&", ..(input$Tab6_Select_Categorical_Variable_2)[i], sep=" "))
#                Test <- fisher.test(..(data_Association2())[[i]], simulate.p.value = T)
#                Pval <- c(Pval, signif(Test$p.value, digits=3))
#            }
#
#            Table <- cbind.data.frame(Variables, Pval)
#            Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n=length(..(input$Tab6_Select_Categorical_Variable_2))), digits=3)
#            Table %>% select(Variables, Pval, Adj_Pval)})}
#})
#
#output$Cat2Ad <- metaRender(renderDataTable, {
#    DT::datatable(..(data_Association2Ad()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))})
#
## Fishers Exact Test
#Fis <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2), " \n \n Please select categorical variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){return(NULL)}
#    else{metaExpr({ for(i in 1:length(c(..(input$Tab6_Select_Categorical_Variable_2)))){
#        tryCatch({
#            cat(noquote(paste("Categorical Variable 1:", ..(input$Tab6_Select_Categorical_Variable_1), "and", "Categorical Variable 2:", ..(input$Tab6_Select_Categorical_Variable_2)[i], "\n")))
#            print(fisher.test(..(data_Association2())[[i]], simulate.p.value = F))
#            # continue if error
#        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}})}
#})
#
#F_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Categorical_Variable_2), paste("\n \n Please select categorical variables of interest", "\n", "\n", "\n")))
#    if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        F_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
#            name <- ..(input$Tab6_Select_Categorical_Variable_2)[i]
#            tryCatch({
#                F_List[[name]] <- fisher.test(..(data_Association2())[[i]], simulate.p.value = F)
#                # continue if error
#            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
#        F_List})}})
#
#output$Cat3 <-  metaRender(renderPrint, { ..(Fis()) })
#
#data_Association3Ad <- metaReactive2({
#    if(isTRUE(input$Tab6_Select_Categorical_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_2 == "None Selected")){
#        data.frame(Variables = character(10),
#                   Pval = character(10),
#                   Adj_Pval = character(10))
#    }
#    else{
#        metaExpr({
#            Variables <- Pval <- c()
#
#            for(i in 1:length(..(input$Tab6_Select_Categorical_Variable_2))){
#                tryCatch({
#                    Test <- fisher.test(..(data_Association2())[[i]])
#                    Pval <- c(Pval, signif(Test$p.value, digits=3))
#                    Variables <- c(Variables, paste(..(input$Tab6_Select_Categorical_Variable_1), "&", ..(input$Tab6_Select_Categorical_Variable_2)[i], sep=" "))
#                    # continue if error
#                }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#            }
#            Table <- cbind.data.frame(Variables, Pval)
#            Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n=length(..(input$Tab6_Select_Categorical_Variable_2))), digits=3)
#            Table %>% select(Variables, Pval, Adj_Pval) })
#    }})
#
#output$Cat3Ad <- metaRender(renderDataTable, {
#    DT::datatable(..(data_Association3Ad()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))})
#
## ANOVA Assumptions
## 1
#Assump1 <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest, \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#        cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n", "\n")))
#        print(leveneTest(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], ..(datalist[["data"]]()), center=mean)) }})}})
#
#Levene_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        Levene_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            Levene_List[[name]] <- leveneTest(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], ..(datalist[["data"]]()), center=mean)
#            Levene_List}})}})
#
#output$ANOVAAss1  <-  metaRender(renderPrint, { ..(Assump1()) })
#
#Assump2 <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest, \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({ for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#        cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n")))
#        print(fligner.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], ..(datalist[["data"]]())))}})} })
#
#Fligner_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        Fligner_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            Fligner_List[[name]] <- fligner.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], ..(datalist[["data"]]()))
#            Fligner_List}})}})
#
#output$ANOVAAss2  <-  metaRender(renderPrint, { ..(Assump2()) })
#
#Assump3 <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{
#        metaExpr({for(i in 1:length(c(..(input$Tab6_Select_Continuous_Variable_1)))){
#            cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n", "\n")))
#            Data1 <- ..(datalist[["data"]]()) %>% group_by(!!as.name(..(input$Tab6_Select_Categorical_Variable_3))) %>% shapiro_test(!!as.name(..(input$Tab6_Select_Continuous_Variable_1)[i]))
#            print(as.data.frame(Data1))
#            print(shapiro.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]])) }})}}) # It tests the null hypothesis that the population variances are equal
#
#Shapiro_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        Shapiro_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            Shapiro_List[[name]] <- shapiro.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]])
#            Shapiro_List}})}})
#
#output$ANOVAAss3 <-  metaRender(renderPrint, { ..(Assump3()) })
#
## ANOVA
#ANOVA_R <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({ for(i in 1:length(c(..(input$Tab6_Select_Continuous_Variable_1)))){
#        res.aov <- aov(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], data = ..(datalist[["data"]]()))
#        cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n", "\n")))
#        print(summary(res.aov))}})}})
#
#ANOVA_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        ANOVA_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            ANOVA_List[[name]] <-  summary(aov(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], data = ..(datalist[["data"]]())))
#            ANOVA_List}})}})
#
#output$ANOVA <-  metaRender(renderPrint, { ..(ANOVA_R()) })
#
#data_ANOVAAd <-metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){
#        data.frame(Variables = character(10),
#                   X = character(10),
#                   df = character(10),
#                   Pval = character(10),
#                   Adj_Pval = character(10))
#    }
#    else{
#        metaExpr({
#            Variables <- Pval <- c()
#
#            for(i in 1:length(c(..(input$Tab6_Select_Continuous_Variable_1)))){
#                Variables <- c(Variables, paste(..(input$Tab6_Select_Categorical_Variable_3), "&", ..(input$Tab6_Select_Continuous_Variable_1)[i], sep=" "))
#                Test <- summary(aov(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], data = ..(datalist[["data"]]())))
#                Pval <- c(Pval, signif(as.numeric(Test[[1]]$`Pr(>F)`[1]), digits=3))
#
#            }
#            Table <- cbind.data.frame(Variables, Pval)
#            Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n=length(..(input$Tab6_Select_Continuous_Variable_1))), digits=3)
#            Table %>% select(Variables, Pval, Adj_Pval)})}
#})
#
#output$ANOVAAd <- metaRender(renderDataTable, {
#    DT::datatable(..(data_ANOVAAd()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))})
#
## Kruskal Wallis
#KW_R <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({ for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#        Kw <- kruskal.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], data = ..(datalist[["data"]]()))
#        cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n")))
#        print(Kw)}})}
#})
#
#KW_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        KW_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            KW_List[[name]] <- kruskal.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], data = ..(datalist[["data"]]()))
#        }
#        KW_List})}})
#
#output$KW <-  metaRender(renderPrint, { ..(KW_R()) })
#
## Adj
#data_KWAdj <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){
#        data.frame(Variables = character(10),
#                   Statistic = character(10),
#                   df = character(10),
#                   Pval = character(10),
#                   Adj_Pval = character(10))
#    } else {
#        metaExpr({
#            Variables <- Statistic <- df <- Pval <- c()
#
#            for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#                Variables <- c(Variables, paste(..(input$Tab6_Select_Categorical_Variable_3), "&", ..(input$Tab6_Select_Continuous_Variable_1)[i], sep=" "))
#                Test <- kruskal.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]] ~ ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], data = ..(datalist[["data"]]()))
#                Statistic <- c(Statistic, signif(Test$statistic[[1]], digits = 3))
#                df <- c(df, Test$parameter[[1]])
#                Pval <- c(Pval, signif(as.numeric(Test$p.value[[1]]), digits=3))
#            }
#
#            Table <- cbind.data.frame(Variables, Statistic, df, Pval)
#            Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n=length(..(input$Tab6_Select_Continuous_Variable_1))), digits=3)
#            Table %>% select(Variables, Statistic, df, Pval, Adj_Pval)
#        })}
#})
#
#output$KWAd <- metaRender(renderDataTable, {
#    DT::datatable(..(data_KWAdj()), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))})
#
#PWC <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    validate(need(input$Tab6_Select_Continuous_Variable_1 != "None Selected" & input$Tab6_Select_Categorical_Variable_3 != "None Selected", "\n \n Please select variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({ for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#        cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n", "\n")))
#        print(pairwise.t.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]], ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], p.adjust.method = "BH")) }})}
#})
#
#WT_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    validate(need(input$Tab6_Select_Continuous_Variable_1 != "None Selected" & input$Tab6_Select_Categorical_Variable_3 != "None Selected", "\n \n Please select variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        PairwiseT_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            PairwiseT_List[[name]] <- pairwise.t.test(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]], ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], p.adjust.method = "BH")
#        }
#        PairwiseT_List})}})
#
#output$PC <-  metaRender(renderPrint, { ..(PWC()) })
#
#PWCDunn <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    validate(need(input$Tab6_Select_Continuous_Variable_1 != "None Selected" & input$Tab6_Select_Categorical_Variable_3 != "None Selected", "\n \n Please select variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({ for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#        cat(noquote(paste("Categorical Variable:", ..(input$Tab6_Select_Categorical_Variable_3), "and", "Continuous Variable:", ..(input$Tab6_Select_Continuous_Variable_1)[i], "\n")))
#        print(DunnTest(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]], ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], method = "BH", out.list = F)) }})}
#})
#
#PWD_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Continuous_Variable_1), "\n \n Please select continuous variables of interest \n \n \n"))
#    validate(need(input$Tab6_Select_Continuous_Variable_1 != "None Selected" & input$Tab6_Select_Categorical_Variable_3 != "None Selected", "\n \n Please select variables of interest \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Continuous_Variable_1 == "None Selected" | input$Tab6_Select_Categorical_Variable_3 == "None Selected")){return(NULL)}
#    else{metaExpr({
#        PairwiseD_List <- list()
#        for(i in 1:length(..(input$Tab6_Select_Continuous_Variable_1))){
#            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
#            PairwiseD_List[[name]] <- DunnTest(..(datalist[["data"]]())[,..(input$Tab6_Select_Continuous_Variable_1)[i]], ..(datalist[["data"]]())[,..(input$Tab6_Select_Categorical_Variable_3)], method = "BH", out.list = F)
#        }
#        PairwiseD_List})}})
#
#output$Dunn <- metaRender(renderPrint, { ..(PWCDunn()) })
#
### Compare Groups
#
#observe({vchoicesCG <- c(names(datalist[["data"]]()), "None Selected")
#updateSelectizeInput(session, "Tab6_Select_Response_Variable", choices = vchoicesCG, selected = "None Selected", server = TRUE)
#updateSelectizeInput(session, "Tab6_Select_Explanatory_Variable", choices = vchoicesCG, selected = "None Selected", server = TRUE)})
#
#Compare_Code <- metaReactive2({
#    validate(need(!is.null(input$cBioData_rows_selected) | !is.null(input$Input_Patient_File) | !is.null(input$Input_Sample_File) | !is.null(input$Input_CNA_File), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
#    validate(need(c(is.null(input$Input_Patient_File) & is.null(input$Input_Sample_File) & is.null(input$Input_CNA_File)) | is.null(input$cBioData_rows_selected), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
#    validate(need(!is.null(input$Tab6_Select_Response_Variable) && !is.null(input$Tab6_Select_Explanatory_Variable) , "\n \n \n Please select response and explanatory variables \n \n \n \n"))
#    validate(need(input$Tab6_Select_Response_Variable != "None Selected" & input$Tab6_Select_Explanatory_Variable != "None Selected", "\n \n \n Please select response and explanatory variables \n \n \n \n"))
#    if(isTRUE(input$Tab6_Select_Response_Variable == "None Selected" | input$Tab6_Select_Explanatory_Variable == "None Selected")){return(NULL)}
#    else{metaExpr({
#        res <- compareGroups(as.formula(paste(..(input$Tab6_Select_Response_Variable), '~', paste(c(..(input$Tab6_Select_Explanatory_Variable)), collapse = "+"))),
#                             data = ..(datalist[["data"]]()), max.ylev = 10, method = NA)
#        res})} })
#
#output$CG <- metaRender(renderPrint, { print(..(Compare_Code())) })
#output$CG_Table <- metaRender(renderPrint, { createTable(..(Compare_Code())) })#
