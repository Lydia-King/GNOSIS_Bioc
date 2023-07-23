## Display dataframes/tables


## Download Plot
Download_Server <- function(id, datalist, plot) {
    moduleServer(id, function(input, output, session) {
        output$Download_PNG <- downloadHandler(filename = function(){paste(plot, ".png", sep="")}, content = function(file){
            png(file, width = input$Plot_Width, height = input$Plot_Height, units="in", res = 1200)
            print(datalist[[plot]]())
            dev.off()})

        output$Download_SVG <- downloadHandler(filename = function(){paste(plot, ".svg", sep="")}, content = function(file){
            svg(file, width = input$Plot_Width, height = input$Plot_Height)
            print(datalist[[plot]]())
            dev.off()})
    })
}
