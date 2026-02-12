
shinyServer(function(input, output, session) {

# ------------------ App server logic (Edit anything below) --------------- #
### For all tags and Server-side selectize
observe_helpers()
optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
updateSelectizeInput(session, "sc1a1inpg1", choices = sort(names(sc1gene)), server = TRUE,
                        selected = sc1def$gene1, options = list(
                                maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "sc1a2inpg2", choices = sort(names(sc1gene)), server = TRUE,
                        selected = sc1def$gene1, options = list(
                                maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "sc1a4inpg1", choices = sort(names(sc1gene)), server = TRUE,
                        selected = sc1def$gene1, options = list(
                                maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "sc1a4inpg2", choices = sort(names(sc1gene)), server = TRUE,
                        selected = sc1def$gene2, options = list(
                                maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "sc1b1inpg1", choices = sort(names(sc1gene)), server = TRUE,
                        selected = sc1def$gene1, options = list(
                                maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "sc1b1inpg2", choices = sort(names(sc1gene)), server = TRUE,
                        selected = sc1def$gene2, options = list(
                                maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "sc1c1inpg2", server = TRUE,
                        choices = c(sc1conf[is.na(fID)]$UI,sort(names(sc1gene))),
                        selected = sc1conf[is.na(fID)]$UI[1], options = list(
                                maxOptions = length(sc1conf[is.na(fID)]$UI) + 3,
                                create = TRUE, persist = TRUE, render = I(optCrt)))
### Plots for tab a0 ##########################################################
output$sc1a0sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a0sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a0sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a0sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a0sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a0sub2_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a0sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a0sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a0sub3_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
observe({ switch(input$sc1a0asp,
                        "Fixed" = shinyjs::toggle(id = "sc1a0tog5_open",anim = TRUE, condition = TRUE),
                        shinyjs::toggle(id = "sc1a0tog5_open",anim = TRUE, condition = FALSE))
})
observe({ switch(input$sc1a0lab1,
                        "No labels" = shinyjs::toggle(id = "sc1a0tog6_open",anim = TRUE, condition = FALSE),
                        shinyjs::toggle(id = "sc1a0tog6_open",anim = TRUE, condition = TRUE))
})

observe({
        maxoverlaps <- length(strsplit(sc1conf[UI == input$sc1a0inp1]$fCL,"\\|")[[1]])
        updateSliderInput(session, "sc1a0overlaps", max = maxoverlaps)
})


shinyjs::onclick("sc1a0tog7",
                        shinyjs::toggleElement(id = "sc1a0tog7_open", time = 0,anim = TRUE))
sc1a0col1.choices = reactive({
        grp = as.character(sc1conf[UI == input$sc1a0inp1]$grp)
        color.category = switch(grp,"TRUE"="qual","FALSE" = c("seq","div"))
        c("default",rownames(pal.info)[pal.info$category %in% color.category])
})
observe({updateSelectInput(session, "sc1a0col1",choices = sc1a0col1.choices(),selected = "default")})
observe({
        xlim = range(sc1meta[,sc1conf[UI == input$sc1a0drX]$ID,with = FALSE])
        ylim = range(sc1meta[,sc1conf[UI == input$sc1a0drY]$ID,with = FALSE])
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "sc1a0xlim", value = xlim,
                                min = floor(xlim[1]), max = ceiling(xlim[2]))
        updateSliderInput(session, "sc1a0ylim", value = xlim,
                                min = floor(ylim[1]), max = ceiling(ylim[2]))
})
sc1a0oup1 <- reactive({
        scDRcell(sc1conf, sc1meta, input$sc1a0drX, input$sc1a0drY, input$sc1a0inp1, input$sc1a0inpsplt,
                        input$sc1a0sub1_1, input$sc1a0sub1_2, input$sc1a0sub2_1, input$sc1a0sub2_2,input$sc1a0sub3_1, input$sc1a0sub3_2,
                        input$sc1a0siz, input$sc1a0col1, input$sc1a0colinv,input$sc1a0ord1,
                        input$sc1a0fsz, input$sc1a0asp, input$sc1a0txt, input$sc1a0title, input$sc1a0lab1,input$sc1a0leg,input$sc1a0legpos,
                        input$sc1a0arrange, input$sc1a0xlim, input$sc1a0ylim, input$sc1a0mintxt, input$sc1a0alpha,
                        input$sc1a0bg, inpmaxoverlaps = input$sc1a0overlaps)
})
output$sc1a0oup1 <- renderPlot({sc1a0oup1()})
output$sc1a0oup1.ui <- renderUI({
        plotOutput("sc1a0oup1", height = pList[input$sc1a0psz])
})
output$sc1a0oup1.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a0drX,"_",input$sc1a0drY,"_",input$sc1a0inp1,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a0oup1.h, width = input$sc1a0oup1.w,
                dpi = 600,plot = sc1a0oup1() )
        })
output$sc1a0oup1.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a0drX,"_",input$sc1a0drY,"_",input$sc1a0inp1,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a0oup1.h, width = input$sc1a0oup1.w,
                dpi = 600,plot = sc1a0oup1() )
        })
output$meta_data.rds <- downloadHandler(
        filename = function() { "meta_data.rds" },
        content = function(file) { file.copy("sc1meta.rds", file)
        })
output$csr_gexpr.h5ad <- downloadHandler(
        filename = function() { "csr_gexpr.h5ad" },
        content = function(file) { file.copy("sc1csr_gexpr.h5ad", file)
        })
sc1a0ggData <- reactive({
        scDRcellnum(sc1conf, sc1meta, input$sc1a0inp1, input$sc1a0inpsplt,
                        input$sc1a0sub1_1, input$sc1a0sub1_2, input$sc1a0sub2_1, input$sc1a0sub2_2,
                        input$sc1a0sub3_1, input$sc1a0sub3_2)
})
output$sc1a0.dt <- renderDataTable({
        datatable(sc1a0ggData(), rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        buttons = c("copy")))
})
output$sc1a0oup.csv <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a0drX,"_",input$sc1a0drY,"_",input$sc1a0inp1,".csv") },
        content = function(file) { write.csv(x = sc1a0ggData(), file)
        })
output$sc1a0oup.xlsx <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a0drX,"_",input$sc1a0drY,"_",input$sc1a0inp1,".xlsx") },
        content = function(file) { write.xlsx(x = sc1a0ggData(), file,
                                                colNames = TRUE, borders = "surrounding")
        })

### Plots for tab a1 ##########################################################
output$sc1a1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a1sub2_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a1sub3_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a1bg.ui <- renderUI({
        bg = switch(input$sc1a1col1,
                        "default" = "lightgrey",
                        if(!input$sc1a1colinv) {
                                color_generator(input$sc1a1col1)[1]
                        } else rev(color_generator(input$sc1a1col1))[1])
        selectInput("sc1a1bg", "Select a background color",
                        choices= unique(c(bg,"snow3","lightgrey","lightgrey","grey"),selected=bg))
})
observe({ switch(input$sc1a1asp1,
                        "Fixed" = shinyjs::toggle(id = "sc1a1tog6_open",anim = TRUE, condition = TRUE),
                        shinyjs::toggle(id = "sc1a1tog6_open",anim = TRUE, condition = FALSE))
})
sc1a1xlim <- reactive({range(sc1meta[,sc1conf[UI == input$sc1a0drX]$ID,with = FALSE])})
sc1a1ylim <- reactive({range(sc1meta[,sc1conf[UI == input$sc1a0drY]$ID,with = FALSE])})

observe({
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "sc1a1xlim", value = xlim,
                                min = floor(sc1a1xlim()[1]), max = ceiling(sc1a1xlim()[2]))
        updateSliderInput(session, "sc1a1ylim", value = xlim,
                                min = floor(sc1a1ylim()[1]), max = ceiling(sc1a1ylim()[2]))
})

sc1a1Data <- reactive({
        scDRgeneData(sc1conf, sc1meta, sc1max, input$sc1a1drX, input$sc1a1drY, input$sc1a1inpg1,input$sc1a1inpsplt,
                        input$sc1a1grp,input$sc1a1sub1_1, input$sc1a1sub1_2, input$sc1a1sub2_1, input$sc1a1sub2_2, input$sc1a1sub3_1, input$sc1a1sub3_2,
                        "sc1gexpr.h5", sc1gene)
})
sc1a1oup1 <- reactive({
        scDRgene(sc1a1Data(), sc1conf, sc1max, input$sc1a1drX, input$sc1a1drY, input$sc1a1inpg1,input$sc1a1inpsplt,
                        input$sc1a1siz, input$sc1a1max,input$sc1a1col1, input$sc1a1colinv, input$sc1a1bg,input$sc1a1ord1,
                        input$sc1a1fsz, input$sc1a1asp1, input$sc1a1txt,input$sc1a1title,
                        input$sc1a1leg, input$sc1a1legpos,input$sc1a1arrange,
                        inpxlim = input$sc1a1xlim, inpylim = input$sc1a1ylim,inpmintxt = input$sc1a1mintxt)
})
output$sc1a1oup1 <- renderPlot({sc1a1oup1()})
output$sc1a1oup1.ui <- renderUI({
        plotOutput("sc1a1oup1", height = pList[input$sc1a1psz], brush = "plot_brush")
})
output$sc1a1_all.dt <- renderDataTable({
        datatable(scDRexNum(sc1a1Data(),input$sc1a1inpg1),
                        rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        buttons = c("copy", "print"))) %>%
                formatRound(columns = c("percent"), digits = 2)
})
output$sc1a1_selec.dt <- renderDataTable({
        sub_ggData = tableBrush(sc1a1Data(), plot_brush = input$plot_brush)
        datatable(scDRexNum(sub_ggData,input$sc1a1inpg1),
                        rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        buttons = c("copy", "print"))) %>%
                formatRound(columns = c("percent"), digits = 2)
})
sc1a1oup2 <- reactive({
        sub_ggData = tableBrush(sc1a1Data(), plot_brush = input$plot_brush)
        DensityPlot(sub_ggData = switch (as.character(nrow(sub_ggData) >0),
                                                "TRUE" = sub_ggData,
                                                "FALSE" = sc1a1Data()),
                        sc1conf,input$sc1a1inpg1,input$sc1a1grp,
                        input$sc1a1siz, input$sc1a1col2, input$sc1a1fsz, input$sc1a1asp2,input$sc1a1ord2,
                        TRUE, input$sc1a1dtype)
})

output$sc1a1oup2 <- renderPlot({sc1a1oup2()})
output$sc1a1oup2.ui <- renderUI({
        plotOutput("sc1a1oup2", height = pList[input$sc1a1psz])
})
output$sc1a1oup1.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                        input$sc1a1inpg1,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
                dpi = 600,plot = sc1a1oup1())
        })
output$sc1a1oup1.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                        input$sc1a1inpg1,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
                dpi = 600,plot = sc1a1oup1())
        })

### Plots for tab a2 ##########################################################
output$sc1a2sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a2sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a2sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a2sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a2sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a2sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a2sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a2sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a2sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1a2col1.choices = reactive({
        grp = as.character(sc1conf[UI == input$sc1a2inp1]$grp)
        color.category = switch(grp,"TRUE"="qual","FALSE" = c("seq","div"))
        c("default",rownames(pal.info)[pal.info$category %in% color.category])
})
observe({updateSelectInput(session, "sc1a2col1",choices = sc1a2col1.choices(),selected = "default")})
sc1a2oup1 <- reactive({
        scDRcell(sc1conf, sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp1,"no split",
                        input$sc1a2sub1_1, input$sc1a2sub1_2,input$sc1a2sub2_1, input$sc1a2sub2_2,input$sc1a2sub3_1, input$sc1a2sub3_2,
                        input$sc1a2siz, input$sc1a2col1, input$sc1a2colinv1,input$sc1a2ord1,
                        input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, inptitle = FALSE,
                        input$sc1a2lab1,input$sc1a2leg1,input$sc1a2legpos1)
})
output$sc1a2oup1 <- renderPlot({sc1a2oup1()})
output$sc1a2oup1.ui <- renderUI({
        plotOutput("sc1a2oup1", height = pList[input$sc1a2psz])
})
output$sc1a2oup1.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                        input$sc1a2inp1,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a2oup1.h, width = input$sc1a2oup1.w,
                dpi = 600,plot = sc1a2oup1() )
        })
output$sc1a2oup1.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                        input$sc1a2inp1,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a2oup1.h, width = input$sc1a2oup1.w,
                dpi = 600,plot = sc1a2oup1() )
        })
output$sc1a2.dt <- renderDataTable({
        ggData = scDRcellnum(sc1conf, sc1meta, input$sc1a2inp1, "no split",
                                input$sc1a2sub1_1, input$sc1a2sub1_2, input$sc1a2sub2_1, input$sc1a2sub2_2,
                                input$sc1a2sub3_1, input$sc1a2sub3_2)
        datatable(ggData, rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
})
sc1a2Data <- reactive({
        scDRgeneData(sc1conf, sc1meta,sc1max, input$sc1a2drX, input$sc1a2drY, input$sc1a2inpg2,"no split",input$sc1a1grp,
                        input$sc1a2sub1_1, input$sc1a2sub1_2,input$sc1a2sub2_1, input$sc1a2sub2_2,input$sc1a2sub3_1, input$sc1a2sub3_2,
                        "sc1gexpr.h5", sc1gene)
})
sc1a2oup2 <- reactive({
        scDRgene(sc1a2Data(), sc1conf, sc1max, input$sc1a2drX, input$sc1a2drY, input$sc1a2inpg2,"no split",
                        input$sc1a2siz, input$sc1a2max,input$sc1a2col2,input$sc1a2colinv2, "lightgrey", input$sc1a2ord2,
                        input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt,inptitle = FALSE,input$sc1a2leg2, input$sc1a2legpos2)
})
output$sc1a2oup2 <- renderPlot({sc1a2oup2()})
output$sc1a2oup2.ui <- renderUI({
        plotOutput("sc1a2oup2", height = pList[input$sc1a2psz])
})
output$sc1a2oup2.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                        input$sc1a2inpg2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a2oup2.h, width = input$sc1a2oup2.w,
                dpi = 600,plot = sc1a2oup2())
        })
output$sc1a2oup2.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                        input$sc1a2inpg2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a2oup2.h, width = input$sc1a2oup2.w,
                dpi = 600,plot = sc1a2oup2() )
        })
output$sc1a1oup2.png <- downloadHandler(
        filename = function() { paste0("sc1_density_",input$sc1a1inpg1,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
                dpi = 600,plot = sc1a1oup2())
        })
output$sc1a1oup2.jpeg <- downloadHandler(
        filename = function() { paste0("sc1_density_",input$sc1a1inpg1,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
                dpi = 600,plot = sc1a1oup2())
        })


### Plots for tab a3 ##########################################################
output$sc1a3sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a3sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a3sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a3sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a3sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a3sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a3sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a3sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a3sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1a3col1.choices = reactive({
        grp = as.character(sc1conf[UI == input$sc1a3inp1]$grp)
        color.category = switch(grp,"TRUE"="qual","FALSE" = c("seq","div"))
        c("default",rownames(pal.info)[pal.info$category %in% color.category])
})
observe({updateSelectInput(session, "sc1a3col1",choices = sc1a3col1.choices(),selected = "default")})
sc1a3oup1 <- reactive({
        scDRcell(sc1conf, sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp1,"no split",
                        input$sc1a3sub1_1, input$sc1a3sub1_2,input$sc1a3sub2_1, input$sc1a3sub2_2,input$sc1a3sub3_1, input$sc1a3sub3_2,
                        input$sc1a3siz, input$sc1a3col1, input$sc1a3colinv1, input$sc1a3ord1,
                        input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt, inptitle = FALSE, input$sc1a3lab1)
})

output$sc1a3oup1 <- renderPlot({sc1a3oup1()})
output$sc1a3oup1.ui <- renderUI({
        plotOutput("sc1a3oup1", height = pList[input$sc1a3psz])
})
output$sc1a3oup1.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                        input$sc1a3inp1,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a3oup1.h, width = input$sc1a3oup1.w,
                dpi = 600,plot = sc1a3oup1())
        })
output$sc1a3oup1.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                        input$sc1a3inp1,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a3oup1.h, width = input$sc1a3oup1.w,
                dpi = 600,plot = sc1a3oup1())
        })
sc1a3col2.choices = reactive({
        grp = as.character(sc1conf[UI == input$sc1a3inp2]$grp)
        color.category = switch(grp,"TRUE"="qual","FALSE" = c("seq","div"))
        c("default",rownames(pal.info)[pal.info$category %in% color.category])
})
observe({updateSelectInput(session, "sc1a3col2",choices = sc1a3col2.choices(),selected = "default")})
sc1a3oup2 <- reactive({
        scDRcell(sc1conf, sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp2,"no split",
                        input$sc1a3sub1_1, input$sc1a3sub1_2,input$sc1a3sub2_1, input$sc1a3sub2_2,input$sc1a3sub2_1, input$sc1a3sub2_2,
                        input$sc1a3siz, input$sc1a3col2, input$sc1a3colinv2, input$sc1a3ord2,
                        input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt, inptitle = FALSE, input$sc1a3lab2)
})
output$sc1a3oup2 <- renderPlot({sc1a3oup2()})
output$sc1a3oup2.ui <- renderUI({
        plotOutput("sc1a3oup2", height = pList[input$sc1a3psz])
})
output$sc1a3oup2.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                        input$sc1a3inp2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a3oup2.h, width = input$sc1a3oup2.w,
                dpi = 600,plot = sc1a3oup2())
        })
output$sc1a3oup2.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                        input$sc1a3inp2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a3oup2.h, width = input$sc1a3oup2.w,
                dpi = 600,plot = sc1a3oup2())
        })


### Plots for tab a4 ##########################################################
output$sc1a4sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a4sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a4sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a4sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a4sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a4sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1a4sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1a4sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1a4sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1a4Data1 <- reactive({
        scDRgeneData(sc1conf, sc1meta,sc1max, input$sc1a4drX, input$sc1a4drY, input$sc1a4inpg1,"no split",sc1def$grp2,
                        input$sc1a4sub1_1, input$sc1a4sub1_2, input$sc1a4sub2_1, input$sc1a4sub2_2,input$sc1a4sub3_1, input$sc1a4sub3_2,
                        "sc1gexpr.h5", sc1gene)
})
sc1a4oup1 <- reactive({
        scDRgene(sc1a4Data1(), sc1conf, sc1max, input$sc1a4drX, input$sc1a4drY, input$sc1a4inpg1,"no split",
                        input$sc1a4siz, input$sc1a4max,input$sc1a4col1,input$sc1a4colinv1,"lightgrey", input$sc1a4ord1,
                        input$sc1a4fsz, input$sc1a4asp, input$sc1a4txt,inptitle = FALSE,input$sc1a4leg, input$sc1a4legpos)
})

output$sc1a4oup1 <- renderPlot({sc1a4oup1()})
output$sc1a4oup1.ui <- renderUI({
        plotOutput("sc1a4oup1", height = pList[input$sc1a4psz])
})
output$sc1a4oup1.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a4drX,"_",input$sc1a4drY,"_",
                                        input$sc1a4inpg1,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a4oup1.h, width = input$sc1a4oup1.w,
                dpi = 600,plot = sc1a4oup1())
        })
output$sc1a4oup1.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a4drX,"_",input$sc1a4drY,"_",
                                        input$sc1a4inpg1,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a4oup1.h, width = input$sc1a4oup1.w,
                dpi = 600,plot = sc1a4oup1())
        })
sc1a4Data2 <- reactive({
        scDRgeneData(sc1conf, sc1meta,sc1max, input$sc1a4drX, input$sc1a4drY, input$sc1a4inpg2,"no split",sc1def$grp2,
                        input$sc1a4sub1_1, input$sc1a4sub1_2, input$sc1a4sub2_1, input$sc1a4sub2_2,input$sc1a4sub3_1, input$sc1a4sub3_2,
                        "sc1gexpr.h5", sc1gene)
})
sc1a4oup2 <- reactive({
        scDRgene(sc1a4Data2(), sc1conf, sc1max, input$sc1a4drX, input$sc1a4drY, input$sc1a4inpg2,"no split",
                        input$sc1a4siz, input$sc1a4max,input$sc1a4col2,input$sc1a4colinv2,"lightgrey", input$sc1a4ord2,
                        input$sc1a4fsz, input$sc1a4asp, input$sc1a4txt,inptitle = FALSE,input$sc1a4leg, input$sc1a4legpos)
})
output$sc1a4oup2 <- renderPlot({sc1a4oup2()})
output$sc1a4oup2.ui <- renderUI({
        plotOutput("sc1a4oup2", height = pList[input$sc1a4psz])
})
output$sc1a4oup2.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a4drX,"_",input$sc1a4drY,"_",
                                        input$sc1a4inpg2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1a4oup2.h, width = input$sc1a4oup2.w,
                dpi = 600,plot = sc1a4oup2())
        })
output$sc1a4oup2.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1a4drX,"_",input$sc1a4drY,"_",
                                        input$sc1a4inpg2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1a4oup2.h, width = input$sc1a4oup2.w,
                dpi = 600,plot = sc1a4oup2())
        })


### Plots for tab b1 ##########################################################
output$sc1b1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1b1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1b1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1b1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1b1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1b1sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1b1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1b1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1b1sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
observe({ switch(input$sc1b1asp,
                        "Fixed" = shinyjs::toggle(id = "sc1b1tog8_open",anim = TRUE, condition = TRUE),
                        shinyjs::toggle(id = "sc1b1tog8_open",anim = TRUE, condition = FALSE))
})
ggData <- reactive({
        scDRcoex(sc1conf, sc1meta, input$sc1b1drX, input$sc1b1drY,
                        input$sc1b1inpg1, input$sc1b1inpg2,input$sc1b1grp,
                        input$sc1b1sub1_1, input$sc1b1sub1_2, input$sc1b1sub2_1, input$sc1b1sub2_2,input$sc1b1sub3_1, input$sc1b1sub3_2,
                        "sc1gexpr.h5", sc1gene)
})
# main co-expression DR plot
sc1b1oup1 <- reactive({switch (input$sc1b1mulcol,
                                "Bi-Colors" = scDRcoexPlot,
                                "Tri-Colors" = scDRcoexPlot3)(ggData(),input$sc1b1drX, input$sc1b1drY,
                                                                input$sc1b1inpg1, input$sc1b1inpg2,
                                                                input$sc1b1siz, input$sc1b1col1, input$sc1b1ord1,
                                                                input$sc1b1fsz, input$sc1b1asp, inpxlim = input$sc1b1xlim, inpylim = input$sc1b1ylim,
                                                                input$sc1b1txt, input$sc1b1title, input$plot_brush)
})
output$sc1b1oup1 <- renderPlot({sc1b1oup1()})
output$sc1b1oup1.ui <- renderUI({
        plotOutput("sc1b1oup1", height = pList2[input$sc1b1psz],
                        brush = "plot_brush")
})
output$sc1b1oup1.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1b1drX,"_",input$sc1b1drY,"_",
                                        input$sc1b1inpg1,"_",input$sc1b1inpg2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1b1oup1.h, width = input$sc1b1oup1.w,
                dpi = 600,plot = sc1b1oup1())
        })
output$sc1b1oup1.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1b1drX,"_",input$sc1b1drY,"_",
                                        input$sc1b1inpg1,"_",input$sc1b1inpg2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1b1oup1.h, width = input$sc1b1oup1.w,
                dpi = 600,plot = sc1b1oup1())
        })
# legend for co-expression DR plot
output$sc1b1oup2 <- renderPlot({
        switch (input$sc1b1mulcol,
                "Bi-Colors" = scDRcoexLeg,
                "Tri-Colors" = scDRcoexLeg3)(input$sc1b1inpg1, input$sc1b1inpg2, input$sc1b1col1, input$sc1b1fsz)
})
output$sc1b1oup2.ui <- renderUI({
        plotOutput("sc1b1oup2", height = "300px")
})
output$sc1b1oup2.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1b1drX,"_",input$sc1b1drY,"_",
                                        input$sc1b1inpg1,"_",input$sc1b1inpg2,"_leg.png") },
        content = function(file) { ggsave(
                file, device = "png", height = 3, width = 4, dpi = 600,
                plot = switch (input$sc1b1mulcol,
                                "Bi-Colors" = scDRcoexLeg,
                                "Tri-Colors" = scDRcoexLeg3)(input$sc1b1inpg1, input$sc1b1inpg2, input$sc1b1col1, input$sc1b1fsz) )
        })
output$sc1b1oup2.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1b1drX,"_",input$sc1b1drY,"_",
                                        input$sc1b1inpg1,"_",input$sc1b1inpg2,"_leg.jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = 3, width = 4,dpi = 600,
                plot = switch (input$sc1b1mulcol,
                                "Bi-Colors" = scDRcoexLeg,
                                "Tri-Colors" = scDRcoexLeg3)(input$sc1b1inpg1, input$sc1b1inpg2, input$sc1b1col1, input$sc1b1fsz) )
        })
# scatter plot for co-expression
sc1b1oup3 <- reactive({
        sub_ggData = tableBrush(ggData(), plot_brush = input$plot_brush)
        scDRcoexScatterPlot(sub_ggData = switch (as.character(nrow(sub_ggData) >0),
                                                        "TRUE" = sub_ggData,
                                                        "FALSE" = ggData()),
                                sc1conf,input$sc1b1inpg1, input$sc1b1inpg2,input$sc1b1grp,
                                input$sc1b1siz, input$sc1b1col1, inpcol2=input$sc1b1col2,
                                input$sc1b1fsz, "Fixed", TRUE,input$sc1b1leg,input$sc1b1ord2, input$sc1b1dtype)
})
output$sc1b1oup3 <- renderPlot({sc1b1oup3()})
output$sc1b1oup3.ui <- renderUI({
        plotOutput("sc1b1oup3", height = pList2[input$sc1b1psz])
})
output$sc1b1oup3.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1b1inpg1,"_",input$sc1b1inpg2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1b1oup3.h, width = input$sc1b1oup3.w,
                dpi = 600,plot = sc1b1oup3())
        })
output$sc1b1oup3.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1b1inpg1,"_",input$sc1b1inpg2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1b1oup3.h, width = input$sc1b1oup3.w,
                dpi = 600,plot = sc1b1oup3())
        })
output$sc1b1_all.dt <- renderDataTable({
        datatable(scDRcoexNum(ggData(),input$sc1b1inpg1,input$sc1b1inpg2),
                        rownames = FALSE, extensions = "Buttons",
                        options = list(searching =FALSE,
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        info = FALSE, paging = FALSE)) %>%
                formatRound(columns = c("percent"), digits = 2)
})
output$sc1b1_all_cor.dt <- renderDataTable({
        datatable(base::t(data.frame(scDRcoexCor(ggData(),input$sc1b1inpg1,input$sc1b1inpg2))),
                        container = sketch, rownames = FALSE, extensions = "Buttons",
                        options = list(searching =FALSE,
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        info = FALSE, paging = FALSE))
})

output$sc1b1_selec.dt <- renderDataTable({
        sub_ggData = tableBrush(ggData(), plot_brush = input$plot_brush)
        datatable(scDRcoexNum(sub_ggData,input$sc1b1inpg1,input$sc1b1inpg2),
                        rownames = FALSE, extensions = "Buttons",
                        options = list(searching =FALSE,
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        info = FALSE, paging = FALSE)) %>%
                formatRound(columns = c("percent"), digits = 2)
})
output$sc1b1_selec_cor.dt <- renderDataTable({
        sub_ggData = tableBrush(ggData(), plot_brush = input$plot_brush)
        datatable(base::t(data.frame(scDRcoexCor(sub_ggData,input$sc1b1inpg1,input$sc1b1inpg2))),
                        container = sketch, rownames = FALSE, extensions = "Buttons",
                        options = list(searching =FALSE,
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        info = FALSE, paging = FALSE))
})

### Plots for tab c1 ##########################################################
output$sc1c1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1c1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1c1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1c1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c1sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1c1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1c1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c1sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1c1Data <- reactive({
        scVioBoxData(sc1conf, sc1meta, input$sc1c1inp1, input$sc1c1inpg2,input$sc1c1inp3,
                        input$sc1c1sub1_1, input$sc1c1sub1_2, input$sc1c1sub2_1, input$sc1c1sub2_2,
                        input$sc1c1sub3_1, input$sc1c1sub3_2,input$sc1c1postive,
                        "sc1gexpr.h5", sc1gene)
})
sc1c1oupSig <- reactive({
        if(input$sc1c1sig){
                scVioBoxSig(sc1c1Data(),input$sc1c1inp3,input$sc1c1pvalmethod,
                                input$sc1c1plab,input$sc1c1pcut)
        } else NULL
})

sc1c1oup <- reactive({
        scVioBox(sc1c1Data(), sc1conf, input$sc1c1inp1, input$sc1c1inpg2,input$sc1c1inp3,
         input$sc1c1typ,input$sc1c1sig,
         input$sc1c1cols, input$sc1c1col,input$sc1c1pts,
         input$sc1c1err,input$sc1c1siz, input$sc1c1alpha,input$sc1c1fsz,
         input$sc1c1scale,input$sc1c1width,
         stat.test = sc1c1oupSig(),input$sc1c1ylim,input$sc1c1plab,input$sc1c1pvalpos,
         input$sc1c1frt, input$sc1c1leg, input$sc1c1legpos)
})
output$sc1c1oup <- renderPlot({sc1c1oup()})
output$sc1c1oup.ui <- renderUI({
        plotOutput("sc1c1oup", height = pList2[input$sc1c1psz])
})
output$sc1c1oup.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1c1typ,"_",input$sc1c1inp1,"_",
                                        input$sc1c1inpg2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1c1oup.h, width = input$sc1c1oup.w,
                dpi = 600,plot = sc1c1oup())
        })
output$sc1c1oup.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1c1typ,"_",input$sc1c1inp1,"_",
                                        input$sc1c1inpg2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1c1oup.h, width = input$sc1c1oup.w,
                dpi = 600,plot = sc1c1oup())
        })
output$sc1c1out1.dt <- renderDataTable({
        ggData_num = scVioBoxDataNum(sc1c1Data(),input$sc1c1inp3)
        datatable(ggData_num, rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        buttons = c("copy", "csv", "excel"))) %>%
                formatRound(c(ncol(ggData_num)-1,ncol(ggData_num)), 4) %>%
                formatStyle(columns = c(1:ncol(ggData_num)), 'text-align' = 'center')
})
output$sc1c1out2.dt <- renderDataTable({
        stat.test = sc1c1oupSig()
        datatable(stat.test[,-1], rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        buttons = c("copy", "csv", "excel"))) %>%
                formatStyle(columns= c(2:ncol(stat.test)), 'text-align' = 'center')
})

### Plots for tab c2 ##########################################################
observe(updateSelectizeInput(session, "sc1c2order", choices = sort(unique(sc1meta[[input$sc1c2inp1]])), server = TRUE,
                                options = list(create = TRUE, persist = TRUE)))
output$sc1c2sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1c2sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c2sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1c2sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1c2sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c2sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1c2sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1c2sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c2sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1c2Data <- reactive({
        scPropData(sc1conf, sc1meta, input$sc1c2inp1,input$sc1c2inp2, input$sc1c2norm, input$sc1c2order,
                        input$sc1c2sub1_1, input$sc1c2sub1_2, input$sc1c2sub2_1, input$sc1c2sub2_2,
                        input$sc1c2sub3_1, input$sc1c2sub3_2)
})
sc1c2oup <- reactive({
        scProp(sc1c2Data(),sc1conf, input$sc1c2inp1, input$sc1c2inp2,input$sc1c2norm,
                input$sc1c2order,input$sc1c2typ, input$sc1c2split,
                input$sc1c2sig,input$sc1c2addline,input$sc1c2cols,input$sc1c2yrange,
                input$sc1c2flpxy,input$sc1c2flpx,
                input$sc1c2pts,input$sc1c2siz,input$sc1c2filt,
                input$sc1c2fsz,input$sc1c2width,input$sc1c2lsz,input$sc1c2lab,input$sc1c2err,
                input$sc1c2plab,input$sc1c2pcut,input$sc1c2pvalpos,
                input$sc1c2frt, input$sc1c2leg, input$sc1c2legpos)
})
output$sc1c2oup <- renderPlot({sc1c2oup()}) #renderPlotly
output$sc1c2oup.ui <- renderUI({
        plotOutput("sc1c2oup", height = pList2[input$sc1c2psz]) #plotlyOutput
})
output$sc1c2.dt <- renderDataTable({
        datatable(sc1c2Data(), rownames = FALSE, extensions = "Buttons",
                        options = list(pageLength = -1, dom = "tB",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        buttons = c("copy", "csv", "excel")))
})
output$sc1c2oup.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1c2typ,"_",input$sc1c2inp1,"_",
                                        input$sc1c2inp2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1c2oup.h, width = input$sc1c2oup.w,
                dpi = 600,plot = sc1c2oup())
        })
output$sc1c2oup.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1c2typ,"_",input$sc1c2inp1,"_",
                                        input$sc1c2inp2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1c2oup.h, width = input$sc1c2oup.w,
                dpi = 600,plot = sc1c2oup())
        })


### Plots for tab d1 ##########################################################
output$sc1d1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1d1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1d1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1d1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1d1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1d1sub2_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1d1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1d1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1d1sub3_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
shinyjs::onclick("sc1d1tog6",
                        shinyjs::toggle(id = "sc1d1tog6_open", anim = TRUE))
output$sc1d1oupTxt <- renderUI({
        geneList = scGeneList(input$sc1d1inp, sc1gene)
        if(nrow(geneList) > 300){
                HTML("More than 300 input genes! Please reduce the gene list!")
        } else {
                oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted")
                if(nrow(geneList[present == FALSE]) > 0){
                        oup = paste0(oup, "<br/>",
                                        nrow(geneList[present == FALSE]), " genes not found (",
                                        paste0(geneList[present == FALSE]$gene, collapse = ", "), ")")
                }
                HTML(oup)
        }
})
observe(updateSelectizeInput(session, "sc1d1inp2", choices = sort(unique(sc1meta[[input$sc1d1grp]])), server = TRUE,
                                options = list(create = TRUE, persist = TRUE)))
sc1d1Data <- reactive({
        scBubbHeatData(sc1conf, sc1meta, input$sc1d1inp, input$sc1d1inp2, input$sc1d1grp, input$sc1d1plt,
                        input$sc1d1sub1_1, input$sc1d1sub1_2, input$sc1d1sub2_1, input$sc1d1sub2_2,
                        input$sc1d1sub3_1, input$sc1d1sub3_2,
                        "sc1gexpr.h5", sc1gene,input$sc1d1zscore,input$sc1d1row, input$sc1d1col)
})
sc1d1oup <- eventReactive(input$sc1d1update,{
        scBubbHeat(sc1d1Data(),input$sc1d1inp, input$sc1d1inp2, sc1gene, input$sc1d1zscore,input$sc1d1row,
                        input$sc1d1col, input$sc1d1plt,
                        input$sc1d1cols,input$sc1d1colinv, input$sc1d1fsz,input$sc1d1frt, input$sc1d1siz, input$sc1d1asp,
                        input$sc1d1leg, input$sc1d1legpos, input$sc1d1xlim, input$sc1d1ylim, input$sc1d1flpxy)
})
output$sc1d1oup <- renderPlot({sc1d1oup()})
output$sc1d1oup.ui <- renderUI({
        plotOutput("sc1d1oup", height = pList[input$sc1d1psz])
})
output$sc1d1oup.png <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",input$sc1d1plt,"_",input$sc1d1grp,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1d1oup.h, width = input$sc1d1oup.w,
                dpi = 600,plot = sc1d1oup())
        })
output$sc1d1oup.jpeg <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",input$sc1d1plt,"_",input$sc1d1grp,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1d1oup.h, width = input$sc1d1oup.w,
                dpi = 600,plot = sc1d1oup())
        })
output$sc1d1oup1.xlsx <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",input$sc1d1plt,"_",input$sc1d1grp,".xlsx") },

        content = function(file) {
                ggData = sc1d1Data()
                ggMat = dcast.data.table(ggData, geneName~grpBy, value.var = "val") %>% as.data.frame() %>%
                        tibble::column_to_rownames("geneName") %>%
                        .[rev(levels(ggData$geneName)),levels(ggData$grpBy)]
                ggMatOut = list("expression" = ggMat)
                if(input$sc1d1plt == "Bubbleplot") {
                        ggMatOut[["proportion"]] = dcast.data.table(ggData, geneName~grpBy, value.var = "prop") %>% as.data.frame() %>%
                                tibble::column_to_rownames("geneName") %>%
                                .[rev(levels(ggData$geneName)),levels(ggData$grpBy)]
                }
                write.xlsx(x = ggMatOut, file, colNames = TRUE, rowNames = TRUE,borders = "surrounding")
        })


### Plots for tab e1 ##########################################################
output$sc1e1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1e1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1e1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1e1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1e1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1e1sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1e1oup <- reactive({
        scCurvs(sc1conf, sc1meta, input$sc1e1inp1, input$sc1e1inp2,
                input$sc1e1sub1_1, input$sc1e1sub1_2, input$sc1e1sub2_1, input$sc1e1sub2_2,
                "sc1gexpr.h5", sc1gene, input$sc1e1log, input$sc1e1norm,input$sc1e1y_0,
                input$sc1e1pts, input$sc1e1cols, input$sc1e1lvls,
                input$sc1e1fsz,input$sc1e1lsz, input$sc1e1frt, input$sc1e1leg)
})
output$sc1e1oup <- renderPlot({sc1e1oup()})
output$sc1e1oup.ui <- renderUI({
        plotOutput("sc1e1oup", height = pList[input$sc1e1psz])
})
output$sc1e1oup.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1e1typ,"_",input$sc1e1inp1,"_",
                                        input$sc1e1inp2,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1e1oup.h, width = input$sc1e1oup.w,
                dpi = 600,plot =    sc1e1oup())
        })

output$sc1e1oup.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1e1typ,"_",input$sc1e1inp1,"_",
                                        input$sc1e1inp2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1e1oup.h, width = input$sc1e1oup.w,
                dpi = 600,plot =    sc1e1oup())
        })
### Plots for tab f1 ##########################################################
output$sc1f1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1f1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1f1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1f1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1f1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1f1sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1f1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1f1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1f1sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1f1grp.choices = reactive({strsplit(sc1conf[UI == input$sc1f1grp]$fID, "\\|")[[1]]})

output$sc1f1ident1.ui <- renderUI({
        checkboxGroupInput("sc1f1ident1", "Select treated group", inline = TRUE,
                                choices = sc1f1grp.choices(), selected = NULL)
})
output$sc1f1ident2.ui <- renderUI({
        checkboxGroupInput("sc1f1ident2", "Select reference group", inline = TRUE,
                                choices = sc1f1grp.choices(), selected = NULL)
})
sc1f1adata_uns <- eventReactive(input$sc1f1update,{
        NeedReRun <- !(.checkIfIdentical(inpsub1_2 = input$sc1f1sub1_2,
                                                inpsub2_2 = input$sc1f1sub2_2, inpsub3_2 = input$sc1f1sub3_2,
                                                inpident_1 = input$sc1f1ident1, inpident_2 = input$sc1f1ident2,
                                                inpDEmethod = input$sc1f1DEmethod,
                                                file.name = "tempData/rank_genes_groups_1.csv"))
        if(NeedReRun){
                print("NeedReRun!")
                scFindMarkers("sc1csr_gexpr.h5ad",
                                input$sc1f1sub1_1, input$sc1f1sub1_2, input$sc1f1sub2_1, input$sc1f1sub2_2,
                                input$sc1f1sub3_1, input$sc1f1sub3_2,
                                input$sc1f1grp, input$sc1f1ident1, input$sc1f1ident2,min_cells = input$sc1f1cutmincell, inpDEmethod = input$sc1f1DEmethod)
        } else {
                print("Don't NeedReRun!")
                sc <- reticulate::import("scanpy")
                sc$read_h5ad(file.path("tempData","rank_genes_groups_1.h5ad"))
        }
})
sc1f1oupData <- reactive({
        loadDEGs(sc1f1adata_uns(),input$sc1f1grp, input$sc1f1ident1, input$sc1f1ident2,
                        input$sc1f1cutp, input$sc1f1cutpval,
                        input$sc1f1cutfc, input$sc1f1cutpct,
                        inprmgene =input$sc1f1rmgene)
})
sc1f1oup1 <- reactive({
                VolcanoPlots(sc1f1oupData(), input$sc1f1inp, sc1gene,
                                input$sc1f1sub1_2,
                                input$sc1f1sub2_2,
                                input$sc1f1sub3_2,
                                input$sc1f1cutp,
                                input$sc1f1cutpval,
                                input$sc1f1cutfc,
                                input$sc1f1cutpct,
                                input$sc1f1top,
                                input$sc1f1sort,
                                input$sc1f1cols,
                                input$sc1f1colinv,
                                input$sc1f1alpha, input$sc1f1psiz, input$sc1f1fsz,
                                input$sc1f1asp, input$sc1f1lab1,input$sc1f1lab2,
                                input$sc1f1leg, input$sc1f1legpos)
})
output$sc1f1oup1 <- renderPlot({sc1f1oup1()})
output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
})
output$sc1f1oup1.ui <- renderUI({
        withSpinner(plotOutput("sc1f1oup1", height = pList[input$sc1f1psz]))
})
de_info <- eventReactive(input$sc1f1update,{
        read.delim("tempData/rank_genes_groups_1.csv", header = FALSE) %>% .$V1
})
output$sc1f1oup1.png <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",de_info(),".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1f1oup1.h, width = input$sc1f1oup1.w,
                dpi = 600,plot = sc1f1oup1())
        })
output$sc1f1oup1.jpeg <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",de_info(),".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1f1oup1.h, width = input$sc1f1oup1.w,
                dpi = 600,plot = sc1f1oup1())
        })
output$sc1f1.dt <- renderDataTable(expr = datatable(sc1f1oupData(), rownames = FALSE,
                                                        options = list(#pageLength = -1, dom = "tB",
                                                                columnDefs = list(list(className = 'dt-center',
                                                                                        targets = "_all")))) %>%
                                                formatRound(c(2:6), 3) %>%
                                                formatSignif(columns = c('p_val','p_val_adj'), digits = 3)
)
output$sc1f1oup1.csv <- downloadHandler(
        filename = function() { paste0( Sys.Date(),"-",de_info(),".csv") },
        content = function(file) { write.csv(x = sc1f1oupData(), file)
        })
output$sc1f1oup1.xlsx <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",de_info(),".xlsx") },
        content = function(file) { write.xlsx(x = sc1f1oupData(), file,
                                                colNames = TRUE, borders = "surrounding")
        })

### Plots for tab f2 ##########################################################
output$sc1f2sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1f2sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1f2sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1f2sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1f2sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1f2sub2_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1f2sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1f2sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1f2sub3_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
observe({ switch(input$sc1f2mode,
                        "FindMarkers" = shinyjs::toggle(id = "sc1f2tog8_open",anim = TRUE, condition = TRUE),
                        shinyjs::toggle(id = "sc1f2tog8_open",anim = TRUE, condition = FALSE))
})

output$sc1f2ident2.ui <- renderUI({
        sc1f2grp.choices = reactive({strsplit(sc1conf[UI == input$sc1f2grp]$fID, "\\|")[[1]]})
        checkboxGroupInput("sc1f2ident2", "Select reference group", inline = TRUE,
                                choices = sc1f2grp.choices(), selected = "rest")
})
observe(updateSelectizeInput(session, "sc1f2inpgrp", choices = sort(unique(sc1meta[[input$sc1f2grp]])), server = TRUE,
                                options = list(create = TRUE, persist = TRUE)))

sc1f2adata <- eventReactive(input$sc1f2update,{
        NeedReRun <- !(.checkIfIdentical(inpident_2 = switch(input$sc1f2mode,
                                                                "FindAllMarkers" = 'rest',
                                                                "FindMarkers" = input$sc1f2ident2),
                                                inpsub1_2 = input$sc1f2sub1_2,
                                                inpsub2_2 = input$sc1f2sub2_2,
                                                inpsub3_2 = input$sc1f2sub3_2,
                                                inpGrp = input$sc1f2grp,
                                                inpDEmethod = input$sc1f2DEmethod,
                                                file.name = "tempData/rank_genes_groups.csv")) &
                        input$sc1f2plt != "Correlation Matrix"
        if(NeedReRun){
                print("NeedReRun!")
                scFindAllMarkers("sc1csr_gexpr.h5ad",
                                        input$sc1f2sub1_1, input$sc1f2sub1_2, input$sc1f2sub2_1, input$sc1f2sub2_2,
                                        input$sc1f2sub3_1, input$sc1f2sub3_2,input$sc1f2grp,
                                        inpident_2 = switch(input$sc1f2mode,
                                                        "FindAllMarkers" = 'rest',
                                                        "FindMarkers" = input$sc1f2ident2),
                                        inpDEmethod = input$sc1f2DEmethod,returnFormat = "full")
        } else {
                print("Don't NeedReRun!")
                LoadAnndata("rank_genes_groups.h5ad",
                                input$sc1f2sub1_1, input$sc1f2sub1_2, input$sc1f2sub2_1, input$sc1f2sub2_2,
                                input$sc1f2sub3_1, input$sc1f2sub3_2,input$sc1f2grp, input$sc1f2inpgrp)
        }
})
        DEGs_info <- eventReactive(input$sc1f2update,{
        read.delim("tempData/rank_genes_groups.csv", header = FALSE) %>% .$V1
})
output$sc1f2oup.ui <- renderImage({
        saveRankGenesGroupsFigure(sc1f2adata(),input$sc1f2plt,input$sc1f2grp, input$sc1f2inpgrp,
                                        input$sc1f2valToPlot, input$sc1f2cutfc, input$sc1f2cutpct,
                                        input$sc1f2zscore,
                                        input$sc1f2top,input$sc1f2cols, input$sc1f2colinv,
                                        input$sc1f2vrange, input$sc1f2dendrogram,
                                        input$sc1f2fsz,input$sc1f2flpxy, input$sc1f2frt)
        plotType = switch(input$sc1f2plt,
                                "Rank_genes" =paste0("rank_genes_groups_",input$sc1f2grp),
                                "Dotplot" = "dotplot_",
                                "MatrixPlot" = "matrixplot_",
                                "Stacked Violin" = "stacked_violin_",
                                "Heatmap" = "heatmap",
                                "TracksPlot" = "tracksplot",
                                "Correlation Matrix" = "correlation_matrix")
        height_factor = switch(input$sc1f2plt,
                                "Rank_genes" = 1,
                                "Dotplot" = 0.6,
                                "MatrixPlot" = 0.6,
                                "Stacked Violin" = 0.6,
                                "Heatmap" = 1,
                                "TracksPlot" = 0.6,
                                "Correlation Matrix" = 0.6) *
                as.integer(sub("px","",pList[input$sc1f2psz]))
        width_factor = switch(input$sc1f2plt,
                                "Rank_genes" = 1,
                                "Dotplot" = 1.6,
                                "MatrixPlot" = 1.6,
                                "Stacked Violin" = 1.6,
                                "Heatmap" = 1.6,
                                "TracksPlot" = 1.6,
                                "Correlation Matrix" = 1)*
                as.integer(sub("px","",pList[input$sc1f2psz]))
        list(src = paste0("figures/",plotType,".png"),
                contentType = 'image/png',
                width  = session$clientData$output_image1_width,
                height = switch(as.character(input$sc1f2flpxy),
                                "FALSE" = height_factor,
                                "TRUE" = width_factor),
                alt = "Figure loading fails")
}, deleteFile = FALSE)

output$sc1f2oup.png <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",input$sc1f2plt,"_",DEGs_info(),".png") },
        content = function(file) {plotType = switch(input$sc1f2plt,
                                                        "Rank_genes" =paste0("rank_genes_groups_",input$sc1f2grp),
                                                        "Dotplot" = "dotplot_",
                                                        "MatrixPlot" = "matrixplot_",
                                                        "Stacked Violin" = "stacked_violin_",
                                                        "Heatmap" = "heatmap",
                                                        "TracksPlot" = "tracksplot",
                                                        "Correlation Matrix" = "correlation_matrix")
        file.copy(paste0("figures/",plotType,".png"), file)
        })
sc1f2DEGs <- reactive({
        return(loadAllDEGs(sc1f2adata(),inpDEmode = input$sc1f2mode,input$sc1f2grp, input$sc1f2cutp, input$sc1f2cutpval,
                                input$sc1f2cutfc, input$sc1f2cutpct,inpident_2 =  input$sc1f2ident2,
                                inprmgene =input$sc1f2rmgene))
})
output$sc1f2.dt <- renderDataTable(expr = datatable(sc1f2DEGs(), rownames = FALSE,
                                                        options = list(#pageLength = -1, dom = "tB",
                                                                columnDefs = list(list(className = 'dt-center',
                                                                                        targets = "_all")))) %>%
                                                formatRound(c(3:ifelse(input$sc1f2mode == 'FindAllMarkers',8,7)), 3) %>%
                                                formatSignif(columns = c('p_val','p_val_adj'), digits = 3)
)
output$sc1f2oup1.csv <- downloadHandler(
        filename = function() { paste0( Sys.Date(),"-",DEGs_info(),".csv") },
        content = function(file) { write.csv(x = sc1f2DEGs(), file)
        })
output$sc1f2oup1.xlsx <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",DEGs_info(),".xlsx") },
        content = function(file) { write.xlsx(x = sc1f2DEGs(), file,
                                                colNames = TRUE, borders = "surrounding")
        })
### Plots for tab g1 ##########################################################
output$sc1g1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1g1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1g1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1g1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1g1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1g1sub2_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1g1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1g1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1g1sub3_2", "Further select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
observe({ switch(input$sc1g1mode,
                        "FindMarkers" = shinyjs::toggle(id = "sc1g1tog8_open",anim = TRUE, condition = TRUE),
                        shinyjs::toggle(id = "sc1g1tog8_open",anim = TRUE, condition = FALSE))
})


output$sc1g1ident2.ui <- renderUI({
        checkboxGroupInput("sc1g1ident2", "Select reference group", inline = TRUE,
                                choices = sc1g1grp.choices(), selected = "rest")
})

output$sc1g1ident2.ui <- renderUI({
        sc1g1grp.choices = reactive({strsplit(sc1conf[UI == input$sc1g1grp]$fID, "\\|")[[1]]})
        checkboxGroupInput("sc1g1ident2", "Select reference group", inline = TRUE,
                                choices = sc1g1grp.choices(), selected = "rest")
})

observe(updateSelectizeInput(session, "sc1g1inpgrp", choices = sort(unique(sc1meta[[input$sc1g1grp]])), server = TRUE,
                                options = list(create = TRUE, persist = TRUE)))
output$sc1g1orderRow.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1g1grp]$fID, "\\|")[[1]]
        selectInput("sc1g1orderRow", "Order rows by:",
                        choices= c("None",sub),selected="None")
})
sc1g1adata <- eventReactive(input$sc1g1update,{
        NeedReRunDE <- !(.checkIfIdentical(inpident_2 = switch(input$sc1g1mode,
                                                                "FindAllMarkers" = 'rest',
                                                                "FindMarkers" = input$sc1g1ident2),
                                                inpsub1_2 = input$sc1g1sub1_2,
                                                inpsub2_2 = input$sc1g1sub2_2,
                                                inpsub3_2 = input$sc1g1sub3_2,
                                                inpGrp = input$sc1g1grp,
                                                inpDEmethod = input$sc1g1DEmethod,
                                                file.name = "tempData/rank_genes_groups.csv"))
        if(NeedReRunDE){
                print("Need Re Run DE!")
                scFindAllMarkers("sc1csr_gexpr.h5ad",
                                        input$sc1g1sub1_1, input$sc1g1sub1_2, input$sc1g1sub2_1, input$sc1g1sub2_2,
                                        input$sc1g1sub3_1, input$sc1g1sub3_2,input$sc1g1grp,
                                        inpident_2 = switch(input$sc1g1mode,
                                                        "FindAllMarkers" = 'rest',
                                                        "FindMarkers" = input$sc1g1ident2),
                                        inpDEmethod = input$sc1g1DEmethod,
                                        return = "uns")
        } else {
                print("Don't Need Re Run DE!")
                sc <- reticulate::import("scanpy")
                sc$read_h5ad("tempData/rank_genes_groups.h5ad")
        }
})

sc1g1DEGs <- reactive({
        print("loadAllDEGs!")
        return(loadAllDEGs(sc1g1adata(),inpDEmode = input$sc1g1mode,input$sc1g1grp,
                                input$sc1g1cutp, input$sc1g1cutpval,
                                input$sc1g1cutfc, input$sc1g1cutpct,
                                inpident_2 = input$sc1g1ident2,
                                inprmgene =input$sc1g1rmgene))
})

sc1g1inpgs <- reactive({
        if(!is.null(input$sc1g1inpgsfile)){
                ex <- gsub(".*\\.","",input$sc1g1inpgsfile$datapath)
                switch (ex,
                        "xlsx" = read_excel(path = input$sc1g1inpgsfile$datapath),
                        "xls" = read_excel(path = input$sc1g1inpgsfile$datapath),
                        "txt" = fread(file = input$sc1g1inpgsfile$datapath,header = TRUE),
                        "csv" = fread(file = input$sc1g1inpgsfile$datapath,header = TRUE),
                        NULL
                )
        }
})
output$sc1g1inpgs.dt <- renderDataTable(expr = if(input$sc1g1inpgs.dt.disp == "head") {
        head(sc1g1inpgs())
} else {sc1g1inpgs()}, rownames = FALSE,
options = list(#pageLength = -1, dom = "tB",
        columnDefs = list(list(className = 'dt-center',
                                targets = "_all")))
)

sc1g1GSEA <- eventReactive(input$sc1g1update,{
        NeedReRunGSEA <- !(.checkIfIdentical(inpGrp = input$sc1g1grp, databases = input$sc1g1dbs,
                                                inpdatapath = input$sc1g1inpgsfile$datapath,
                                                inpsub1_2 = input$sc1g1sub1_2, inpsub2_2 = input$sc1g1sub2_2, inpsub3_2 = input$sc1g1sub3_2,
                                                file.name = "tempData/fgsea_Res.csv"))
        if(NeedReRunGSEA){
                print("Need Re Run GSEA!")
                databases <- read.delim("tempData/Enrichr_databases.txt", header = FALSE) %>% .$V1
                if(!all(input$sc1g1dbs %in% databases)){
                        message("Downloading databases!")
                        pathways_list_saved <- readRDS("tempData/Enrichr.rds")
                        missing_database <- input$sc1g1dbs[!input$sc1g1dbs %in% databases]
                        pathways_list <- lapply(missing_database, function(db){
                        db_name = paste0("https://maayanlab.cloud/Enrichr/geneSetLibrary?mode=text&libraryName=",db)
                        gmtPathways(db_name)
                        })
                        names(pathways_list) <- missing_database
                        pathways_list <- c(pathways_list_saved, pathways_list)
                        save_pathway_data(pathways_list)
                }
                fgseaRes(sc1g1DEGs(),order.by = "log2FC_log10p",
                                databases = input$sc1g1dbs,inpdatabases = sc1g1inpgs(),inpdatapath = input$sc1g1inpgsfile$datapath,
                                inpGrp = input$sc1g1grp,inpsub1_2 = input$sc1g1sub1_2, inpsub2_2 = input$sc1g1sub2_2,
                                inpsub3_2 = input$sc1g1sub3_2)

        } else {
                print("Don't Need Re Run GSEA!")
                readRDS("tempData/fgsea_Res.rds")
        }
})

sc1g1fgsea_res <- reactive({
        if(input$sc1g1plt == "Dotplot"){
                fgseaDendrogram(sc1g1GSEA(),inpgscutpadj = input$sc1g1gscutpadj,inpgscutpval = input$sc1g1gscutpval,
                                Rowv = input$sc1g1rowv,Colv = input$sc1g1colv,order_row = input$sc1g1orderRow,inpX = input$sc1g1inpgrp)
        } else if(input$sc1g1plt == "BarPlot"){
                fgseaSort(sc1g1GSEA(),inpgscutpadj = input$sc1g1gscutpadj,inpgscutpval = input$sc1g1gscutpval,
                                order_row = input$sc1g1orderRow,inpX = input$sc1g1inpgrp)
        }

})
sc1g1oup1 <- reactive({
        if(input$sc1g1plt == "Dotplot"){
                FgseaDotPlot(sc1g1fgsea_res(), inpvalToPlot = paste0(" -log10(",input$sc1g1valToPlot,")"),
                                scale.by =c('size','radius')[1],
                                fill = "NES",
                                cols = if(input$sc1g1colinv) {
                                        rev(color_generator(input$sc1g1cols,n = 50))
                                } else color_generator(input$sc1g1cols,n = 50),
                                inpfullrange = TRUE,
                                inpcircle = input$sc1g1circle,
                                inppsz = input$sc1g1psiz,
                                inpfsz = input$sc1g1fsz,
                                inpflpxy = input$sc1g1flpxy,
                                verbose=T,inpfrt=input$sc1g1frt)
        } else if(input$sc1g1plt == "BarPlot"){
                FgseaBarplot(sc1g1fgsea_res(),
                                fill = "NES",
                                inpcols = if(input$sc1g1colinv) {
                                        color_generator(input$sc1g1cols,n = 2)
                                } else rev(color_generator(input$sc1g1cols,n = 2)),
                                inpfsz = input$sc1g1fsz,
                                inpflpxy = !(input$sc1g1flpxy), inpfrt = input$sc1g1frt,
                                verbose=T)
        }
})
output$sc1g1oup1 <- renderPlot({sc1g1oup1()})
output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
})
output$sc1g1oup1.ui <- renderUI({
        withSpinner(plotOutput("sc1g1oup1", height = pList[input$sc1g1psz]))
})
fgsea_info <- eventReactive(input$sc1g1update,{
        read.delim("tempData/fgsea_Res.csv", header = FALSE) %>% .$V1
})
output$sc1g1oup1.png <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",fgsea_info(),".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1g1oup1.h, width = input$sc1g1oup1.w,
                dpi = 600,plot = sc1g1oup1())
        })
output$sc1g1oup1.jpeg <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",fgsea_info(),".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1g1oup1.h, width = input$sc1g1oup1.w,
                dpi = 600,plot = sc1g1oup1())
        })
output$sc1g1.dt <- renderDataTable(expr = datatable(sc1g1fgsea_res(), rownames = FALSE#,
                                                        #options = list(#pageLength = -1, dom = "tB",
                                                        #    columnDefs = list(list(className = 'dt-left',
                                                        #                           targets = "_all")))
) %>%
        formatRound(c(2:6,9:10),digits= 3) %>%
        formatSignif(columns = c('pval','padj'), digits = 3)
)

output$sc1g1oup1.csv <- downloadHandler(
        filename = function() { paste0( Sys.Date(),"-",fgsea_info(),".csv") },
        content = function(file) { write.csv(x = sc1g1fgsea_res(), file)
        })
output$sc1g1oup1.xlsx <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",fgsea_info(),".xlsx") },
        content = function(file) { write.xlsx(x = sc1g1fgsea_res(), file,
                                                colNames = TRUE, borders = "surrounding")
        })
### Plots for tab n1 ##########################################################
output$sc1n1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1n1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1n1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1n1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1n1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1n1sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1n1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1n1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1n1sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
# Vol canoplot correlation ---------------------

sc1n1oupData1 <- eventReactive(input$sc1n1update1,{
        NeedReRunCor <- !(.checkIfIdentical(inpsub1_2 = input$sc1n1sub1_2, inpsub2_2 = input$sc1n1sub2_2, inpsub3_2 = input$sc1n1sub3_2,
                                                min_expr = input$sc1n1minexpr,
                                                file.name = "tempData/cor_info.csv"))
        if(NeedReRunCor){
                showNotification("Need Re Run Correlation! Once complete, click Load correlation data", duration = 15,type = "message")
                if (file.exists("tempData/scFindCor.h5")) {
                        #Delete file if it exists
                        file.remove("tempData/scFindCor.h5")
                }
                scFindCor(sc1conf, sc1meta,
                                input$sc1n1sub1_1, input$sc1n1sub1_2,
                                input$sc1n1sub2_1, input$sc1n1sub2_2,
                                input$sc1n1sub3_1, input$sc1n1sub3_2,
                                min_expr = input$sc1n1minexpr)
        } else showNotification("Don't Need Re Run Correlation, loading data", duration = 5,type = "message")
        tryCatch({ loadCorDataGene("tempData/scFindCor.h5",gene = input$sc1n1inp1)
        },error=function(cond) {
                showNotification("loading Correlation Data fail!", duration = 10,type = "error")
                showNotification(cond)
        },
        warning=function(cond) {
                showNotification(cond)
        }
        )
})
cor_info <- eventReactive({input$sc1n1update1 | input$sc1n1update2},{
        read.delim("tempData/cor_info.csv", header = FALSE) %>% .$V1
})
# update feature  ---------------
sc1n1inp.choices <- eventReactive({input$sc1n1update1 | input$sc1n1update2},{
        NeedReRunCor <- !(.checkIfIdentical(inpsub1_2 = input$sc1n1sub1_2, inpsub2_2 = input$sc1n1sub2_2, inpsub3_2 = input$sc1n1sub3_2,
                                                min_expr = input$sc1n1minexpr,
                                                file.name = "tempData/cor_info.csv"))

        if(!NeedReRunCor & file.exists("tempData/CorFeatures.rds")) {
                tryCatch({ readRDS("tempData/CorFeatures.rds") %>% .[["features"]] %>% sort
                },error=function(cond) {
                        showNotification("loading Correlation Data fail!", duration = 10,type = "error")
                        showNotification(cond)
                },
                warning=function(cond) {
                        showNotification(cond)
                }
                )
        } else sc1def$gene2
})
observe(updateSelectizeInput(session, "sc1n1inp1", choices = sc1n1inp.choices(), server = TRUE,selected = sc1n1inp.choices()[1],
                                options = list(maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt))))

observe(updateSelectizeInput(session, "sc1n1inp2", choices = sc1n1inp.choices(), server = TRUE,
                                options = list(maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt))))

sc1n1oup1 <- eventReactive({input$sc1n1update1 | input$sc1n1update2 | input$sc1n1update3},{
        sc1n1oupDataList1 = sc1n1oupData1()
        if(sc1n1oupDataList1[["gene"]] != input$sc1n1inp1) {
                sc1n1oupDataList1 = loadCorDataGene("tempData/scFindCor.h5",
                                                        gene = input$sc1n1inp1)
        }

        CorPlots(ggData = sc1n1oupDataList1[["data"]],
                        input$sc1n1inp1,
                        strSplitClean(input$sc1n1inp),
                        sc1n1inp.choices(),
                        cor_info(),
                        input$sc1n1cutp1,
                        input$sc1n1cutpval1,
                        input$sc1n1cutfc1,
                        input$sc1n1top1,
                        input$sc1n1lab1,
                        input$sc1n1lab2,
                        input$sc1n1alpha1,
                        input$sc1n1siz1, input$sc1n1fsz1,
                        input$sc1n1asp1)
})
output$sc1n1oup1 <- renderPlot({sc1n1oup1()})
output$sc1n1oup1.ui <- renderUI({
        withSpinner(plotOutput("sc1n1oup1", height = pList[input$sc1n1psz1]))
})

output$sc1n1oup1.png <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",cor_info(),".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1n1oup1.h, width = input$sc1n1oup1.w,
                dpi = 600,plot = sc1n1oup1())
        })
output$sc1n1oup1.jpeg <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",cor_info(),".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1n1oup1.h, width = input$sc1n1oup1.w,
                dpi = 600,plot = sc1n1oup1())
        })
# network ---------------------
sc1n1oupData2 <- eventReactive(input$sc1n1update1,{
        NeedReRun <- !(.checkIfIdentical(input$sc1n1sub1_2, input$sc1n1sub2_2, input$sc1n1sub3_2,
                                                min_expr = input$sc1n1minexpr,
                                                file.name = "tempData/cor_info.csv"))
        if(NeedReRun){
                showNotification("Need Re Run Correlation! Once complete, click Load correlation data", duration = 15,type = "message")
                if (file.exists("tempData/scFindCor.h5")) {
                        #Delete file if it exists
                        file.remove("tempData/scFindCor.h5")
                }
                scFindCor(sc1conf, sc1meta,
                                input$sc1n1sub1_1, input$sc1n1sub1_2,
                                input$sc1n1sub2_1, input$sc1n1ub2_2,
                                input$sc1n1sub3_1, input$sc1n1ub3_2,
                                min_expr = input$sc1n1minexpr)
        } else showNotification("Don't Need Re Run Correlation, loading data", duration = 5,type = "message")
        tryCatch({loadCorPvalGenes("tempData/scFindCor.h5",
                                        "tempData/CorFeatures.rds",
                                        gene = strSplitClean(input$sc1n1inp2),
                                        topN = input$sc1n1top2,
                                        inpcutp = input$sc1n1cutp2,
                                        corPosNeg = input$sc1n1posneg)},error=function(cond) {
                                                showNotification("loading Correlation Data fail!", duration = 10,type = "error")
                                                showNotification(cond)
                                        },
                        warning=function(cond) {
                                showNotification(cond)
                        }
        )
})

sc1n1oup2 <- eventReactive(input$sc1n1update4,{
        sc1n1oupDataList2 = sc1n1oupData2()
        NeedReRun <- !(.checkIfIdentical(inpsub1_2 = input$sc1n1sub1_2, inpsub2_2 = input$sc1n1sub2_2, inpsub3_2 = input$sc1n1sub3_2,
                                                min_expr = input$sc1n1minexpr,
                                                file.name = "tempData/cor_info.csv"))
        if(!NeedReRun) sc1n1oupDataList2 = sc1n1oupData2()
        if(!identical(sc1n1oupDataList2[["genes"]],strSplitClean(input$sc1n1inp2))) {
                sc1n1oupDataList2 = loadCorPvalGenes("tempData/scFindCor.h5",
                                                        "tempData/CorFeatures.rds",
                                                        gene = strSplitClean(input$sc1n1inp2),
                                                        topN = input$sc1n1top2,
                                                        inpcutp = input$sc1n1cutp2,
                                                        corPosNeg = input$sc1n1posneg)
        }
        CorNetwork(corMat = sc1n1oupDataList2[["cor"]],
                        pvalMat = sc1n1oupDataList2[["pval"]],
                        cor_info = cor_info(),
                        input$sc1n1cutpval2,
                        input$sc1n1cutfc2, input$sc1n1posneg,
                        input$sc1n1col2,input$sc1n1col2inv,
                        input$sc1n1linklen, input$sc1n1linkwid, input$sc1n1fsz2,input$sc1n1name,
                        input$sc1n1leg, input$sc1n1legpos)
})
output$sc1n1oup2.ui <- renderForceNetwork({sc1n1oup2()})

output$sc1n1oup2.png <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",cor_info(),".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1n1oup2.h, width = input$sc1n1oup2.w,
                dpi = 600,plot = sc1n1oup2())
        })
output$sc1n1oup2.network <- downloadHandler(
        filename = function() { paste0(Sys.Date(),"-",cor_info(),"_",input$sc1n1inp2,'.html') },
        content = function(file) {
                saveNetwork(sc1n1oup2(), file = file)
        })
### Plots for tab t1 ##########################################################
output$sc1t1sub1.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1t1sub1_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1t1sub1_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = c("CD4+ T-cells", "CD8+ T-cells"))
})
output$sc1t1sub2.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1t1sub2_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1t1sub2_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
output$sc1t1sub3.ui <- renderUI({
        sub = strsplit(sc1conf[UI == input$sc1t1sub3_1]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1t1sub3_2", "Select which cells to show", inline = TRUE,
                                choices = sub, selected = NULL)
})
sc1t1oup <- reactive({
        scRepertoire(sc1conf, sc1meta, input$sc1t1inp1, input$sc1t1inp2,
                        input$sc1t1sub1_1, input$sc1t1sub1_2, input$sc1t1sub2_1, input$sc1t1sub2_2,
                        input$sc1t1sub3_1, input$sc1t1sub3_2,
                        input$sc1t1typ, input$sc1t1norm,input$sc1t1divindex, input$sc1t1cloneCall,
                        input$sc1t1lvls, input$sc1t1cols, input$sc1t1flp,
                        input$sc1t1pts,input$sc1t1siz,input$sc1t1fsz,input$sc1t1lsz,
                        input$sc1t1frt, input$sc1t1leg, input$sc1t1legpos)
})
output$sc1t1oup <- renderPlot({ sc1t1oup()}) #renderPlotly
output$sc1t1oup.ui <- renderUI({
        plotOutput("sc1t1oup", height = pList2[input$sc1t1psz]) #plotlyOutput
})
output$sc1t1oup.png <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1t1typ,"_",input$sc1t1inp1,"_",
                                        input$sc1t1typ,".png") },
        content = function(file) { ggsave(
                file, device = "png", height = input$sc1t1oup.h, width = input$sc1t1oup.w,
                dpi = 600,plot = sc1t1oup())
        })
output$sc1t1oup.jpeg <- downloadHandler(
        filename = function() { paste0("sc1",input$sc1t1typ,"_",input$sc1t1inp1,"_",
                                        input$sc1t1inp2,".jpeg") },
        content = function(file) { ggsave(
                file, device = "jpeg", height = input$sc1t1oup.h, width = input$sc1t1oup.w,
                dpi = 600,plot = sc1t1oup())
        })
})
