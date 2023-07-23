## Tab 9 - Recursive partitioning Survival Trees
# Survival Trees - Rpart an Ctree

Tab9_RPST_Rpart_Server <- function(id, datalist, datalist1, data, rowselect) { # inputs
    moduleServer(id, function(input, output, session) {

        # Select Inputs for both Rpart and Ctree
        observe({vchoicesRpart <- c(names(datalist1[[data]]()), "None Selected")
        updateSelectizeInput(session, "Tab9_Rpart_Select_Variables", choices = vchoicesRpart, server = TRUE)
     #   updateSelectizeInput(session, "Tab9_Ctree_Select_Variables", choices = vchoicesRpart, server = TRUE)
        updateSelectizeInput(session, "Tab9_Rpart_Survival_Time", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab9_Rpart_Event_Status", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)})
    #    updateSelectizeInput(session, "Tab9_Ctree_Survival_Time", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)
    #    updateSelectizeInput(session, "Tab9_Ctree_Event_Status", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)})

        ## Rpart
        FormulaRpart <- metaReactive2({metaExpr({noquote(paste(..(input$Tab9_Rpart_Select_Variables), collapse="+")) })})

        output$RpartTreeForm = metaRender(renderPrint, { ..(FormulaRpart()) })

        # Rpart works when there is time/event information and at least 1 other piece of info
        # Create Complete Case Data and Data where every predictor variable has at least 1 observation for patient
        Whole_Data_Rpart <- metaReactive2({
            metaExpr({..(datalist1[[data]]()) %>% filter(eval(parse(text = ..(input$Tab9_Rpart_Survival_Time))) > 0) %>% completeFun(data =., c(..(input$Tab9_Rpart_Survival_Time), ..(input$Tab9_Rpart_Event_Status), ..(input$Tab9_Rpart_Select_Variables))) }) })


        # Fit Rpart Tree (Arguments not yet used in formula include maxcompete, maxsurrogate, usesurrogate, surrogatestyle)
        pfit <- metaReactive2({
            validate(need(!is.null(rowselect()) | !is.null(datalist[["patient_manual_data"]]()) | !is.null(datalist[["sample_manual_data"]]()) | !is.null(datalist[["CNA_manual_data"]]()), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
            validate(need(c(is.null(datalist[["patient_manual_data"]]()) & is.null(datalist[["sample_manual_data"]]()) & is.null(datalist[["CNA_manual_data"]]())) | is.null(rowselect()), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
            validate(need(!is.null(input$Tab9_Rpart_Select_Variables), "Please select variables to include in rpart survival tree"))
            if(input$Tab9_Rpart_Survival_Time == "None Selected" | input$Tab9_Rpart_Event_Status == "None Selected"){return(NULL)}
            else {
                metaExpr({rpart(paste("Surv(", "as.numeric(", ..(input$Tab9_Rpart_Survival_Time), ")", ",",  ..(input$Tab9_Rpart_Event_Status), ") ~ ", ..(FormulaRpart()), sep = ""), data = ..(Whole_Data_Rpart()), method =  "exp", model=TRUE,
                                control = rpart.control(minsplit = ..(input$Tab9_Rpart_Minsplit), minbucket = ..(input$Tab9_Rpart_Minbucket), cp = ..(input$Tab9_Rpart_Cp),
                                                        xval = ..(input$Tab9_Rpart_Xval), maxdepth = ..(input$Tab9_Rpart_Maxdepth)))})}})


        # Plot Rpart Tree - Can also Download it
        RpartTreePlot_Re <- metaReactive2({if(is.null(pfit())){ggplot() + theme_void()}
            else{metaExpr({plot(partykit::as.party(..(pfit()))) })}})

        output$RpartTreePlot <- metaRender(renderPlot, { ..(RpartTreePlot_Re()) })

        # Survival Curves for each Node in Rpart Tree -> Add Node to Dataframe
        data_node_info <- metaReactive2({ metaExpr({..(Whole_Data_Rpart()) %>% mutate(Node_Rpart = as.factor(..(pfit())$where)) %>%
                data.frame(Time = .[[..(input$Tab9_Rpart_Survival_Time)]], Node = .[["Node_Rpart"]], Cen  = .[[..(input$Tab9_Rpart_Event_Status)]]) %>% select(Time, Node, Cen) })})

        Surv_Curve <-  metaReactive2({ metaExpr({survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Node, data = ..(data_node_info())) })})

        PercentSurvPlotRpart <- metaReactive2({
            if(is.null(datalist1[[data]]()) | input$Tab9_Rpart_Survival_Time  == "None Selected" | input$Tab9_Rpart_Event_Status  == "None Selected"){ggplot() + theme_void()}
            else{metaExpr({ggsurvplot(..(Surv_Curve()), censor.shape="", xlab=..(input$Tab9_Surv_Rpart_X_Axis_Title), ylab=..(input$Tab9_Surv_Rpart_Y_Axis_Title), data = ..(data_node_info()), size = 1,
                                      conf.int = ..(input$Tab9_Surv_Rpart_Display_CI), risk.table = ..(input$Tab9_Surv_Rpart_Display_Risk_Table), pval = ..(input$Tab9_Surv_Rpart_Display_Pval),
                                      legend = c(..(input$Tab9_Surv_Rpart_Legend_Position)), legend.labs = rownames(summary(..(Surv_Curve())$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_bw() + theme(plot.title = element_text(size= 18, hjust = 0.5)) + theme(legend.title = element_text(colour="black", size=15, face="bold")), break.time.by =50, risk.table.y.text.col = T, risk.table.y.text = FALSE,
                                      legend.title = ..(input$Tab9_Surv_Rpart_Legend_Title), title = (..(input$Tab9_Surv_Rpart_Plot_Title)), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black")) })}})

        # Plot
        output$Surv_Curve1 <- metaRender(renderPlot, { ..(PercentSurvPlotRpart()) })

        return(list(Rpart_plot = plot(partykit::as.party(pfit())), Rpart_KM_Curves = PercentSurvPlotRpart()))
    })
}

Tab9_RPST_Ctree_Server <- function(id, datalist, datalist1, data, rowselect) { # inputs
    moduleServer(id, function(input, output, session) {

        # Select Inputs for both Rpart and Ctree
        observe({vchoicesRpart <- c(names(datalist1[[data]]()), "None Selected")
 #       updateSelectizeInput(session, "Tab9_Rpart_Select_Variables", choices = vchoicesRpart, server = TRUE)
        updateSelectizeInput(session, "Tab9_Ctree_Select_Variables", choices = vchoicesRpart, server = TRUE)
     #   updateSelectizeInput(session, "Tab9_Rpart_Survival_Time", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)
    #    updateSelectizeInput(session, "Tab9_Rpart_Event_Status", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab9_Ctree_Survival_Time", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)
        updateSelectizeInput(session, "Tab9_Ctree_Event_Status", choices =  vchoicesRpart, selected = "None Selected", server = TRUE)})


        ## Ctree
        FormulaCtree <- metaReactive2({ metaExpr({noquote(paste(..(input$Tab9_Ctree_Select_Variables), collapse="+")) })})

        output$CtreeTreeForm <- metaRender(renderPrint, { ..(FormulaCtree()) })

        Whole_Data_Ctree <- metaReactive2({
            if(input$Tab9_Ctree_Use_Complete_Cases_Only == TRUE){
                metaExpr({ ..(datalist1[[data]]()) %>% completeFun(data = ., c(..(input$Tab9_Ctree_Survival_Time) , ..(input$Tab9_Ctree_Event_Status), ..(input$Tab9_Ctree_Select_Variables))) })
            } else {metaExpr({..(datalist1[[data]]()) %>% completeFun(data = ., c(..(input$Tab9_Ctree_Event_Status)))})}})

        # Fit Ctree
        pfitctree <- metaReactive2({
            validate(need(!is.null(rowselect()) | !is.null(datalist[["patient_manual_data"]]()) | !is.null(datalist[["sample_manual_data"]]()) | !is.null(datalist[["CNA_manual_data"]]()), paste("\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file", "\n", "\n", "\n")))
            validate(need(c(is.null(datalist[["patient_manual_data"]]()) & is.null(datalist[["sample_manual_data"]]()) & is.null(datalist[["CNA_manual_data"]]())) | is.null(rowselect()), paste("\n \n Please only select cBioPortal dataset OR upload your own data", "\n", "\n", "\n")))
            validate(need(!is.null(input$Tab9_Ctree_Select_Variables) & input$Tab9_Ctree_Select_Variables != "None Selected", "Please select variables to include in ctree survival tree"))
            if(isTRUE(input$Tab9_Ctree_Survival_Time == "None Selected" | input$Tab9_Ctree_Event_Status == "None Selected")){return(NULL)}
            else {
                metaExpr({partykit::ctree(as.formula(paste("Surv(", ..(input$Tab9_Ctree_Survival_Time), ",", ..(input$Tab9_Ctree_Event_Status), ") ~ ",  ..(FormulaCtree()), sep = "")), data =  ..(Whole_Data_Ctree()),
                                          control = ctree_control(teststat = ..(input$Tab9_Ctree_Teststat), splitstat = ..(input$Tab9_Ctree_Splitstat),
                                                                  testtype = ..(input$Tab9_Ctree_Testtype), alpha = ..(input$Tab9_Ctree_Alpha),
                                                                  minsplit = ..(input$Tab9_Ctree_Minsplit), minbucket = ..(input$Tab9_Ctree_Minbucket),
                                                                  minprob = ..(input$Tab9_Ctree_Minprob), stump = ..(input$Tab9_Ctree_Stump), maxvar = ..(input$Tab9_Ctree_Maxvar),
                                                                  maxdepth = ..(input$Tab9_Ctree_Maxdepth))) })}})

        # Plot Ctree Tree - Can also Download it
        CtreeTreePlot_Re <- metaReactive2({if(is.null(pfitctree())){ggplot() + theme_void()}
            else{metaExpr({plot(..(pfitctree())) })}})

        output$CTreePlot <- metaRender(renderPlot, { ..(CtreeTreePlot_Re()) })

        # Survival Curves for each Node in Ctree Tree -> Add Node to Dataframe
        data_node_info_ctree <-  metaReactive2({ metaExpr({..(Whole_Data_Ctree()) %>% mutate(Node_Ctree = as.factor(predict(..(pfitctree()), type="node"))) %>%
                data.frame(Time = .[[..(input$Tab9_Ctree_Survival_Time)]], Node = .[["Node_Ctree"]], Cen  = .[[..(input$Tab9_Ctree_Event_Status)]]) %>% select(Time, Node, Cen) }) })

        Surv_Curvectree <- metaReactive2({ metaExpr({survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Node, data = ..(data_node_info_ctree())) })})

        PercentSurvPlotCtree <-  metaReactive2({
            if(isTRUE(is.null(datalist1[[data]]()) | input$Tab9_Ctree_Survival_Time  == "None Selected" | input$Tab9_Ctree_Event_Status  == "None Selected")){ggplot() + theme_void()}
            else{metaExpr({ggsurvplot(..(Surv_Curvectree()), censor.shape="", xlab=..(input$Tab9_Surv_Ctree_X_Axis_Title), ylab=..(input$Tab9_Surv_Ctree_Y_Axis_Title), data = ..(data_node_info_ctree()), size = 1, conf.int = ..(input$Tab9_Surv_Ctree_Display_CI), risk.table = ..(input$Tab9_Surv_Ctree_Display_Risk_Table), pval = ..(input$Tab9_Surv_Ctree_Display_Pval),
                                      legend = c(..(input$Tab9_Surv_Ctree_Legend_Position)), legend.labs = rownames(summary(..(Surv_Curvectree())$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_bw() + theme(plot.title = element_text(size= 18, hjust = 0.5)) + theme(legend.title = element_text(colour="black", size=15, face="bold")), break.time.by =50, risk.table.y.text.col = T, risk.table.y.text = FALSE,
                                      legend.title = ..(input$Tab9_Surv_Ctree_Legend_Title), title = (..(input$Tab9_Surv_Ctree_Plot_Title)), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black")) })}})

        # Plot
        output$Surv_CurveCtree <- metaRender(renderPlot, { ..(PercentSurvPlotCtree()) })

        return(list(Ctree_plot = plot(pfitctree()), Ctree_KM_Curves = PercentSurvPlotCtree()))

    })
}


