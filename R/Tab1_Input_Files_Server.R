Tab1_API_Files_Server <- function(API) {
    moduleServer(API, function(input, output, session) {


        API_data <- metaReactive2({ metaExpr({
            tryCatch({
            cbio <- cBioPortal(hostname = "www.cbioportal.org", protocol = "https", api. = "/api/api-docs")
            as.data.frame(getStudies(cbio))
            },  error = function(e){return(NULL)})
        })
        })

        output$cBioData <- metaRender(renderDataTable, {metaExpr({ datatable(..(API_data()), selection = "single",
                                                                             options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE,
                                                                                            scrollY = "650px")) })})
    })
}


Tab1_Input_Files_Manual_Server <- function(tab1_input_manual, rowselect) {
    moduleServer(tab1_input_manual, function(input, output, session) {

        patient_data <- metaReactive2({req(input$Input_Patient_File)
            metaExpr({read.delim(
                ..(input$Input_Patient_File$datapath),
                header = ..(input$Tab1_Clin_Header_Yes_or_No),
                sep = ..(input$Tab1_Clin_Separator),
                quote = ..(input$Tab1_Clin_Quote),
                na.strings=c(""," ","NA"),
                skip = ..(input$Tab1_Clin_Skip_Lines))})
        })

        sample_data <- metaReactive2({req(input$Input_Sample_File)
            metaExpr({read.delim(
                ..(input$Input_Sample_File$datapath),
                header = ..(input$Tab1_Sample_Header_Yes_or_No),
                sep = ..(input$Tab1_Sample_Separator),
                quote = ..(input$Tab1_Sample_Quote),
                na.strings=c(""," ","NA"),
                skip = ..(input$Tab1_Sample_Skip_Lines))})
        })

        API_data_output <- reactive({
            if(is.null(rowselect())){
                return(NULL)
            } else {
                tryCatch({
                    cbio <- cBioPortal(hostname = "www.cbioportal.org", protocol = "https", api. = "/api/api-docs")
                    API <-  as.data.frame(getStudies(cbio))
                    dataset <- ..(rowselect())
                    samp <- ..(API())[dataset, "studyId"]
                    cBioDataPack(samp, ask = FALSE)
                },  error = function(e){return(NULL)})
            }
        })

        dataClinical <- metaReactive2({
            if(is.null(input$Input_Sample_File) & !is.null(input$Input_Patient_File) & is.null(API_data_output())) {
                validate(need(ncol(patient_data()) > 2, "Inputted file only has one column, please select file delimiters and options carefully."))
                validate(need("PATIENT_ID" %in% colnames(patient_data()), "Inputted file does not have PATIENT_ID information - Are you using the right file?"))
                metaExpr({..(patient_data())} )}
            else if(is.null(input$Input_Patient_File) & !is.null(input$Input_Sample_File) & is.null(API_data_output())){
                validate(need(ncol(sample_data) > 2, "Inputted file only has one column, please select file delimiters and options carefully."))
                validate(need("PATIENT_ID" %in% colnames(sample_data), "Inputted file does not have PATIENT_ID information - Are you using the right file?"))
                metaExpr({..(sample_data)})
            } else if(is.null(input$Input_Sample_File) & is.null(input$Input_Patient_File) & is.null(API_data_output())){
                validate(need(!is.null(API_data_output()) |  !is.null(input$Input_Sample_File) | !is.null(input$Input_Patient_File), "Please select cBioPortal dataset or upload your own data."))
            } else if(!is.null(input$Input_Sample_File) & !is.null(input$Input_Patient_File) & is.null(API_data_output())){
                validate(need(ncol(patient_data) > 2, "Inputted file only has one column, please select file delimiters and options carefully."))
                validate(need("PATIENT_ID" %in% colnames(patient_data), "Inputted file does not have PATIENT_ID information - Are you using the right file?"))
                validate(need(ncol(sample_data) > 2, "Inputted file only has one column, please select file delimiters and options carefully."))
                validate(need("PATIENT_ID" %in% colnames(sample_data), "Inputted file does not have PATIENT_ID information - Are you using the right file?"))
                metaExpr({merge(..(patient_data), ..(sample_data), by.x = "PATIENT_ID", by.y="PATIENT_ID") })
            } else if(is.null(input$Input_Sample_File) & is.null(input$Input_Patient_File) & !is.null(API_data_output())){
                validate(need(!is.null(API_data_output()), "Please select cBioPortal dataset."))
                metaExpr({dat <- as.data.frame(..(data_cBioPortal())@colData)
                rownames(dat) = NULL
                dat})
            } else {
                validate(need(is.null(API_data_output()) |  c(is.null(input$Input_Sample_File) & is.null(input$Input_Patient_File)), "Please only select cBioPortal dataset OR upload your own data."))
            }
        })

        CNA_data <- metaReactive2({req(input$Input_CNA_File)
            metaExpr({read.delim(..(input$Input_CNA_File$datapath), header = ..(input$Tab1_CNA_Header_Yes_or_No), sep = ..(input$Tab1_CNA_Separator), quote = ..(input$Tab1_CNA_Quote), check.names = F, na.strings=c(""," ","NA"), skip=..(input$Tab1_CNA_Skip_Lines))})})

        cna_files <- metaReactive2({ metaExpr({names(..(API_data_output()))[grepl("cna", names(..(API_data_output())))]}) })

        observe({vchoices <- c(cna_files(), "None Selected")
        updateSelectizeInput(session, "Tab2_CNA_Data", choices = vchoices, selected = "None Selected", server = TRUE)})

        CNA_Validated <- metaReactive2({
            if(is.null(input$Input_CNA_File) & is.null(API_data_output())){
                validate(need(!is.null(input$Input_CNA_File) | !is.null(API_data_output()), "Please select cBioPortal dataset or upload your own CNA data."))
            } else if(!is.null(input$Input_CNA_File) & is.null(API_data_output())) {
                validate(need(ncol(CNA_data()) > 1, "Inputted file only has one column, please select file delimiters and options carefully."))
                validate(need("Hugo_Symbol" %in% colnames(CNA_data()), "Inputted file does not have Gene information (Hugo_Symbol) - Are you using the right file?"))
                metaExpr({..(CNA_data())})
            } else if(is.null(input$Input_CNA_File) & !is.null(API_data_output())){
                if(length(cna_files()) == 1){
                    if(cna_files() == "cna_hg19.seg"){
                        validate(need(cna_files() != "cna_hg19.seg", "Warning: CNA file is not compatible, please upload a different CNA file."))
                    } else {
                        validate(need("Hugo_Symbol" %in% colnames(assays(data_cBioPortal())[[cna_files()]]), "Selected file does not have Gene information (Hugo_Symbol) - Are you using the right file?"))
                        metaExpr({ assays(..(data_cBioPortal()))[[..(cna_files())]] })
                    }
                }  else if(length(cna_files()) == 0) {
                    validate(need(length(cna_files()) != 0, "Warning: Selected study has no corresponding CNA data"))
                } else {
                    validate(need(input$Tab2_CNA_Data != "None Selected", "Study has multiple CNA files, please select one using sidebar"))
                    if(input$Tab2_CNA_Data == "cna_hg19.seg"){
                        validate(need(input$Tab2_CNA_Data != "cna_hg19.seg", "Warning: CNA file is not compatible, please upload a different CNA file."))
                    } else {
                        metaExpr({
                            dat <- as.data.frame(assays(..(data_cBioPortal()))[[..(input$Tab2_CNA_Data)]]) %>% mutate(Hugo_Symbol = rownames(.)) %>% select(Hugo_Symbol, everything())
                            rownames(dat) <- NULL
                            validate(need("Hugo_Symbol" %in% colnames(dat), "Selected file does not have Gene information (Hugo_Symbol) - Are you using the right file?"))
                            dat })
                    }

                }
            } else {
                validate(need(is.null(API_data_output()) |  is.null(input$Input_CNA_File), "Please only select cBioPortal dataset OR upload your own data."))
            }
        })

        MAF_data <- metaReactive2({req(input$Input_MAF_File)
            metaExpr({read.delim(..(input$Input_MAF_File$datapath), header = ..(input$Tab1_MAF_Header_Yes_or_No), sep = ..(input$Tab1_MAF_Separator), quote = ..(input$Tab1_MAF_Quote), na.strings=c(""," ","NA"), skip=..(input$Tab1_MAF_Skip_Lines), row.names = NULL)})})

        MAF_Validated <- metaReactive2({
            if(is.null(input$Input_MAF_File) & is.null(API_data_output())){
                validate(need(!is.null(input$Input_MAF_File) | !is.null(API_data_output()), "Please select cBioPortal dataset or upload your own mutation data."))
            } else if(!is.null(input$Input_MAF_File) & is.null(API_data_output())) {
                validate(need(ncol(dataInputMAF()) > 1, "Inputted file only has one column, please select file delimiters and options carefully."))
                validate(need("Hugo_Symbol" %in% colnames(dataInputMAF()), "Inputted file does not have Gene information (Hugo_Symbol) - Are you using the right file?"))
                metaExpr({..(dataInputMAF())})
            } else if(is.null(input$Input_MAF_File) & !is.null(API_data_output())){
                if(length(mutation_files()) == 1){
                    metaExpr({dat <- as.data.frame(..(data_cBioPortal())[[..(mutation_files())]]) %>% mutate(Hugo_Symbol = rownames(..(data_cBioPortal())[[..(mutation_files())]])) %>% dplyr::rename(., Start_Position = start, End_Position = end, Chromosome = seqnames, Tumor_Sample_Barcode = group_name) %>%
                        select(Hugo_Symbol, everything())
                    validate(need("Hugo_Symbol" %in% colnames(dat), "Selected file does not have Gene information (Hugo_Symbol) - Are you using the right file?"))
                    dat })
                }  else if(length(mutation_files()) == 0) {
                    validate(need(length(mutation_files()) != 0, "Warning: Selected study has no corresponding mutation data"))
                } else {
                    validate(need(input$Tab2_MAF_Data != "None Selected", "Study has multiple mutation files, please select one using sidebar"))
                    metaExpr({
                        dat <-as.data.frame(..(data_cBioPortal())[[..(input$Tab2_MAF_Data)]]) %>% mutate(Hugo_Symbol = rownames(..(data_cBioPortal())[[..(input$Tab2_MAF_Data)]])) %>% dplyr::rename(., Start_Position = start, End_Position = end, Chromosome = seqnames, Tumor_Sample_Barcode = group_name) %>%
                            select(Hugo_Symbol, everything())
                        validate(need("Hugo_Symbol" %in% colnames(dat), "Selected file does not have Gene information (Hugo_Symbol) - Are you using the right file?"))
                        dat })
                }
            } else {
                validate(need(is.null(API_data_output()) |  is.null(input$Input_MAF_File), "Please only select cBioPortal dataset OR upload your mutation own data."))
            }
        })

        list(patient_data, sample_data, CNA_data, MAF_data, dataClinical, CNA_Validated, MAF_Validated, API_data_output)
    })
}

Count_Col <- function(dataset) {
    moduleServer(dataset, function(input, output, session) {
        metaExpr({ncol(..(dataset))})
    })
}

Count_Row <- function(dataset) {
    moduleServer(dataset, function(input, output, session) {
        metaExpr({nrow(..(dataset))})
    })
}

Tab1_Input_Files_Preview_Server <- function(id, dataset, length_px, select_dt) {
    moduleServer(id, function(input, output, session) {

        loading_API <- function() {
            req(!is.null(input$cBioData_rows_selected) & is.null(input$Input_Sample_File) & is.null(input$Input_Patient_File))
            try(message(data_cBioPortal(), silent = T))

            message("DONE!")
            Sys.sleep(5)
            message("Displaying Data")
        }


            observe({
                withCallingHandlers({
                    shinyjs::html("text", "")
                    loading_API()
                },
                message = function(m) {
                    print(m$message)
                    if(m$message != "Displaying Data\n"){
                        hide("Preview")
                        show("text")
                        shinyjs::html(id = "text", html = paste(m$message, "<br>", sep = " "), add = TRUE)
                    } else {
                        hide("text")
                        show("Preview")
                    }
                }
                )
            })

        output$Preview <- metaRender(renderDataTable, {metaExpr({ datatable(..(dataset), selection = select_dt, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = length_px)) })})
    })
}

