source("util.R")
library(shinyhelper)
library(shinycssloaders)
library(bslib)

### Start server code
shinyUI(fluidPage(
### HTML formatting of error messages
tags$head(tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}"))),
list(tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }"))),
theme=bs_theme(bootswatch = "default"),

### Page title
titlePanel("Human BladderCancer scRNA-seq"),
navbarPage(
NULL,
#theme=bs_theme(bootswatch = "default"),
### Tab.a0: cellInfo on dimRed
tabPanel(
    HTML("CellInfo"),
    h4("Cell information on reduced dimensions"),
    "In this tab, you can visualise cell information on a single or multiple low-dimensional representions.",
    br(),br(),
    fluidRow(
        column(
            5, h4("Dimension Reduction"),
            fluidRow(
                column(
                    12, selectInput("sc1a0drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                                    selected = sc1def$dimred[1]),
                    selectInput("sc1a0drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                            selected = sc1def$dimred[2]))
                )
            ), # End of column (3 space)
        column(
            6, actionButton("sc1a0tog1", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1a0tog1 % 2 == 1",
                selectInput("sc1a0sub1_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp1),
                uiOutput("sc1a0sub1.ui"),
                #---------------------
                actionButton("sc1a0tog2", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1a0tog2 % 2 == 1",
                    selectInput("sc1a0sub2_1", "Cell information to subset:",
                                            choices = sc1conf[grp == TRUE]$UI,
                                            selected = sc1def$grp2),
                    uiOutput("sc1a0sub2.ui"),
                    #---------------------
                    actionButton("sc1a0tog3", "Toggle to further subset cells"),
                    conditionalPanel(
                        condition = "input.sc1a0tog3 % 2 == 1",
                        selectInput("sc1a0sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1a0sub3.ui")
                    )
                )
            )
        ), # End of column (3 space)
        column(
            6, actionButton("sc1a0tog4", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1a0tog4 % 2 == 1",
                fluidRow(
                    column(
                        6, sliderInput("sc1a0siz", "Point size:",
                                                        min = 0, max = 4, value = 1.25, step = 0.25),
                        radioButtons("sc1a0psz", "Plot size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Large", inline = TRUE),
                        radioButtons("sc1a0fsz", "Font size:",
                                                    choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE),
                        radioButtons("sc1a0asp", "Aspect ratio:",
                                                    choices = c("Square", "Fixed", "Free"),
                                                    selected = "Square", inline = TRUE),
                        checkboxInput("sc1a0txt", "Show axis text", value = TRUE),
                        checkboxInput("sc1a0title", "Show axis title", value = TRUE),
                        shinyjs::useShinyjs(),
                        shinyjs::hidden(
                            div(id = "sc1a0tog5_open",
                                sliderInput("sc1a0xlim", "x-axis range:",
                                            min = -20, max = 20, value = c(-20,20), step = 0.25),
                                sliderInput("sc1a0ylim", "y-axis range:",
                                            min = -20, max = 20, value = c(-20,20), step = 0.25),
                                checkboxInput("sc1a0mintxt", "Show minor axis text", value = FALSE)

                            )
                        )

                    ),
                    column(
                        6,
                        sliderInput("sc1a0alpha", "Label transparency:",
                                    min = 0, max = 1, value = 0.9, step = 0.05),
                        selectInput("sc1a0col1","Select a color palette",
                                                choices=rownames(pal.info),
                                                selected="default"),
                        checkboxInput("sc1a0colinv", "Reverse color", value = FALSE),
                        #colourpicker::colourInput(inputId ="sc1a0bg",returnName = T,
                        #                          palette = "limited", allowedCols = colors()[grep('[1-9]', grDevices::colors(), invert = T)],
                        #                          label = "Color for low values", value = "lightgrey"),
                        selectInput("sc1a0ord1", "Plot order:",
                                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                                    selected = "Original"),
                        selectInput("sc1a0lab1", "Labels:",
                                        choices = c("No labels","black text","black labels","color labels"),
                                                    selected = "color labels"),
                        shinyjs::useShinyjs(),
                        shinyjs::hidden(
                                div(id = "sc1a0tog6_open",
                                    numericInput("sc1a0overlaps", "max labels to show",
                                                    min = 5,max = 30, value = 25,step = 1)
                                )
                        ),
                        checkboxInput("sc1a0leg", "Show Legend", value = FALSE),
                        radioButtons("sc1a0legpos", "Legend positions:",
                                        choices = c("top", "right", "bottom"),
                                        selected = "right", inline = TRUE)
                    )
                )
            )
        ) # End of column (6 space)
    ),     # End of fluidRow (4 space)
    fluidRow(
        column(
            12, h4("Cell information"),
            fluidRow(
                column(
                    4, selectInput("sc1a0inp1", "group by:",
                                                    choices = sc1conf$UI,
                                                    selected = sc1def$grp1) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                title = "Cell information to colour cells by",
                                content = c("Select cell information to colour cells,
                                                        equivalent to 'group.by' in Seurat DimPlot"))
                ),
                column(
                    4, selectInput("sc1a0inpsplt", "split by:",
                                                    choices = c("no split",sc1conf$ID[!is.na(sc1conf$fID)]),
                                                    selected = "no split") %>%
                        helper(type = "inline", size = "m", fade = TRUE,icon = "circle-question",
                                        title = "Cell information to split cells by",
                                        content = c("Select cell information to split cells,
                                                                equivalent to split.by in Seurat DimPlot",
                                                                "- Default is no split"))
                ),
                column(
                    4,
                    shinyjs::useShinyjs(),
                    a(id = "sc1a0tog7", "Show/hide split options"),
                    shinyjs::hidden(
                        div(id = "sc1a0tog7_open",
                            radioButtons("sc1a0arrange", NULL,
                                            choices = c("auto","1row","1column"),
                                            selected = "auto", inline = TRUE)
                        )
                    )
                )
            ),
            fluidRow(column(12, withSpinner(uiOutput("sc1a0oup1.ui")))),
            downloadButton("sc1a0oup1.png", "Download png"),
            downloadButton("sc1a0oup1.jpeg", "Download jpeg"),
            downloadButton("meta_data.rds", "Download annotation and UMAP"),
            downloadButton("csr_gexpr.h5ad", "Download normalized gene expression(h5ad)"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a0oup1.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 13, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a0oup1.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 13, step = 0.5)), br(),
            actionButton("sc1a0tog9", "Toggle to show cell numbers"),
            conditionalPanel(
                condition = "input.sc1a0tog9 % 2 == 1",
                h4("Cell numbers"),
                dataTableOutput("sc1a0.dt"),
                downloadButton("sc1a0oup.csv", "Download csv"),
                downloadButton("sc1a0oup.xlsx", "Download xlsx")
            )
        ) # End of column (12 space)
    )        # End of fluidRow (4 space)
),         # End of tab (2 space)
### Tab.a1: GeneExpr on dimRed
tabPanel(
    HTML("GeneExpr"),
    h4("GeneExpr on reduced dimensions"),
    "In this tab, you can visualise Gene expression on a single or multiple low-dimensional representions.",
    br(),br(),
    fluidRow(
        column(
            5, h4("Dimension Reduction"),
            fluidRow(
                column(
                    12, selectInput("sc1a1drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                    selected = sc1def$dimred[1]),
                    selectInput("sc1a1drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                selected = sc1def$dimred[2]))
            )
        ), # End of column (3 space)
        column(
            3, actionButton("sc1a1tog1", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1a1tog1 % 2 == 1",
                selectInput("sc1a1sub1_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp1),
                uiOutput("sc1a1sub1.ui"),
                #---------------------
                actionButton("sc1a1tog2", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1a1tog2 % 2 == 1",
                    selectInput("sc1a1sub2_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1a1sub2.ui"),
                    #---------------------
                    actionButton("sc1a1tog3", "Toggle to further subset cells"),
                    conditionalPanel(
                        condition = "input.sc1a1tog3 % 2 == 1",
                        selectInput("sc1a1sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1a1sub3.ui")
                    )
                )
            )
        ), # End of column (3 space)
        column(
            6, actionButton("sc1a1tog4", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1a1tog4 % 2 == 1",
                fluidRow(
                    column(
                        6, sliderInput("sc1a1siz", "Point size:",
                                        min = 0, max = 4, value = 1.25, step = 0.25),
                        checkboxInput("sc1a1max", "unified scale bar", value = TRUE) %>%
                            helper(type = "inline", size = "m", fade = TRUE,
                                    icon = "circle-question",
                                    title = "Use global maximal scale",
                                    content = c("Select maximal expression level in the entire dataset.
                                                This is useful for cross-group comparison")),
                        radioButtons("sc1a1psz", "Plot size:",
                                        choices = c("Small", "Medium", "Large", "Extra Large"),
                                        selected = "Large", inline = TRUE),
                        radioButtons("sc1a1fsz", "Font size:",
                                        choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
                        radioButtons("sc1a1asp1", "Aspect ratio:",
                                        choices = c("Square", "Fixed"),
                                        selected = "Square", inline = TRUE),
                        checkboxInput("sc1a1txt", "Show axis text", value = TRUE),
                        checkboxInput("sc1a1title", "Show axis title", value = TRUE),
                        shinyjs::useShinyjs(),
                        shinyjs::hidden(
                            div(id = "sc1a1tog6_open",
                                sliderInput("sc1a1xlim", "x-axis range:",
                                            min = -20, max = 20, value = c(-20,20), step = 0.25),
                                sliderInput("sc1a1ylim", "y-axis range:",
                                            min = -20, max = 20, value = c(-20,20), step = 0.25),
                                checkboxInput("sc1a1mintxt", "Show minor axis text", value = FALSE)

                            )
                        )
                    ),
                    column(
                        6,
                        selectInput("sc1a1col1","Select a color palette",
                                    choices=c(rownames(pal.info)[pal.info$category %in% c("seq","div")]),
                                    selected="default"),
                        checkboxInput("sc1a1colinv", "Reverse color", value = FALSE),
                        uiOutput("sc1a1bg.ui"),
                        selectInput("sc1a1ord1", "Plot order:",
                                        choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                        selected = "Max-1st"),
                        checkboxInput("sc1a1leg", "Show Legend", value = TRUE),
                        radioButtons("sc1a1legpos", "Legend positions:",
                                        choices = c("top", "right", "bottom"),
                                        selected = "bottom", inline = TRUE)
                    )
                )
            )
        ) # End of column (6 space)
    ),     # End of fluidRow (4 space)
    fluidRow(
        column(
            12, h4("Gene expression"),
            fluidRow(
                column(
                    4, selectInput("sc1a1inpg1", "Gene name:", choices=NULL) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                title = "Gene expression to colour cells by",
                                content = c("Select gene to colour cells by gene expression",
                                            paste0("- Gene expression are coloured in a ",
                                                    "White-Red colour scheme which can be ",
                                                    "changed in the plot controls")))
                ),
                column(
                    4, selectInput("sc1a1inpsplt", "split by:",
                                    choices = c("no split",sc1conf$ID[!is.na(sc1conf$fID)]),
                                    selected = "no split") %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                title = "Cell information to split cells by",
                                content = c("Select cell information to split cells,
                                                                equivalent to split.by in Seurat DimPlot",
                                            "- Default is no split"))
                ),
                column(
                    4,
                    a(id = "sc1a1tog5", "Show/hide split options"),
                    shinyjs::hidden(
                        div(id = "sc1a1tog5_open",
                            radioButtons("sc1a1arrange", NULL,
                                            choices = c("auto","1row","1column"),
                                            selected = "auto", inline = TRUE)
                        )
                    )
                )
            ),
            fluidRow(column(12, withSpinner(uiOutput("sc1a1oup1.ui")))),
            fluidRow(
                column(
                    4,
                    actionButton("sc1a1tog9", "Toggle to show cell numbers / statistics"),
                    conditionalPanel(
                        condition = "input.sc1a1tog9 % 2 == 1",
                    h4("All cells"),
                    dataTableOutput("sc1a1_all.dt"),
                    br(),
                    h4("Selected cells"),
                    dataTableOutput("sc1a1_selec.dt"),
                    style="border-bottom: 2px solid black"
                    )
                ),    # End of column (1 space)
                column(6,
                        downloadButton("sc1a1oup1.png", "Download png"),
                        downloadButton("sc1a1oup1.jpeg", "Download jpeg"),
                        div(style="display:inline-block",
                            numericInput("sc1a1oup1.h", "height:", width = "70px",
                                        min = 4, max = 20, value = 13, step = 0.5)),
                        div(style="display:inline-block",
                            numericInput("sc1a1oup1.w", "width:", width = "70px",
                                        min = 4, max = 20, value = 13, step = 0.5))
                ) # End of column (1 space)
            ),  # End of fluidRow (3 space)
            fluidRow(column(12,
            actionButton("sc1a1tog5", "Toggle Density plot"),
            conditionalPanel(
                condition = "input.sc1a1tog5 % 2 == 1",
                    selectInput("sc1a1grp", "group by:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                actionButton("sc1a1tog5", "Toggle Density plot graphics control"),
                conditionalPanel(
                    condition = "input.sc1a1tog5 % 2 == 1",
                    fluidRow(
                        column(
                            6,
                            radioButtons("sc1a1asp2", "Aspect ratio:",
                                            choices = c("Square", "Fixed", "Free"),
                                            selected = "Free", inline = TRUE),
                            selectInput("sc1a1col2", "Colour for density plot",
                                        choices=c("default",rownames(pal.info)[pal.info$category %in% "qual"]),
                                        selected="default"),
                            radioButtons("sc1a1ord2", "Group order:",
                                            choices = c("As-it-is", "Reverse"),
                                            selected = "As-it-is", inline = TRUE),
                            radioButtons("sc1a1dtype", "Density plot type:",
                                            choices = c("density","density_ridges"),
                                            selected = "density_ridges", inline = TRUE)
                        ) # End of column (4 space)
                    ) # End of fluidRow (1 space)
                ), # End of conditionalPanel (1 space)
                    withSpinner(uiOutput("sc1a1oup2.ui")),
                    downloadButton("sc1a1oup2.png", "Download png"),
                    downloadButton("sc1a1oup2.jpeg", "Download jpeg"),
                    div(style="display:inline-block",
                        numericInput("sc1a1oup1.h", "height:", width = "70px",
                                    min = 4, max = 20, value = 13, step = 0.5)),
                    div(style="display:inline-block",
                        numericInput("sc1a1oup1.w", "width:", width = "70px",
                                    min = 4, max = 20, value = 13, step = 0.5))
                        ) # End of conditionalPanel
                ) # End of fluidRow (1 space)
            ) # End of conditionalPanel
        )  # End of column
    )# End of fluidRow ( space)
),         # End of tab (2 space)
### Tab.a2: cellInfo vs geneExpr on dimRed
tabPanel(
    HTML("CellInfo vs GeneExpr"),
    h4("Cell information vs gene expression on reduced dimensions"),
    "In this tab, you can visualise both cell information and gene ",
    "expression side-by-side on low-dimensional representions.",
    br(),br(),
    fluidRow(
        column(
            5, h4("Dimension Reduction"),
            fluidRow(
                column(
                    12, selectInput("sc1a2drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                                    selected = sc1def$dimred[1]),
                    selectInput("sc1a2drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                            selected = sc1def$dimred[2]))
            )
        ), # End of column (6 space)
        column(
            3, actionButton("sc1a2tog1", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1a2tog1 % 2 == 1",
                selectInput("sc1a2sub1_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp1),
                uiOutput("sc1a2sub1.ui"),
                actionButton("sc1a2tog2", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1a2tog2 % 2 == 1",
                    selectInput("sc1a2sub2_1", "Cell information to subset:",
                                            choices = sc1conf[grp == TRUE]$UI,
                                            selected = sc1def$grp2),
                    uiOutput("sc1a2sub2.ui"),
                    actionButton("sc1a2tog3", "Toggle to further subset cells"),
                    conditionalPanel(
                        condition = "input.sc1a2tog3 % 2 == 1",
                        selectInput("sc1a2sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1a2sub3.ui")
                    )
                )
            )
        ), # End of column (6 space)
        column(
            6, actionButton("sc1a2tog4", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1a2tog4 % 2 == 1",
                fluidRow(
                    column(
                        6, sliderInput("sc1a2siz", "Point size:",
                                                        min = 0, max = 4, value = 1.25, step = 0.25),
                        radioButtons("sc1a2psz", "Plot size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE),
                        radioButtons("sc1a2fsz", "Font size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE)
                    ),
                    column(
                        6, radioButtons("sc1a2asp", "Aspect ratio:",
                                                        choices = c("Square", "Fixed", "Free"),
                                                        selected = "Square", inline = TRUE),
                        checkboxInput("sc1a2txt", "Show axis text", value = FALSE)
                    )
                )
            )
        )    # End of column (6 space)
    ),     # End of fluidRow (3 space)
    fluidRow(
        column(
            6, style="border-right: 2px solid black", h4("Cell information"),
            fluidRow(
                column(
                    6, selectInput("sc1a2inp1", "Cell information:",
                                                    choices = sc1conf$UI,
                                                    selected = sc1def$meta1) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                    title = "Cell information to colour cells by",
                                    content = c("Select cell information to colour cells",
                                                            "- Categorical covariates have a fixed colour palette",
                                                            paste0("- Continuous covariates are coloured in a ",
                                                                        "Blue-Yellow-Red colour scheme, which can be ",
                                                                        "changed in the plot controls")))
                ),# End of column (6 space)
                column(
                    6, actionButton("sc1a2tog5", "Toggle plot controls"),
                    conditionalPanel(
                        condition = "input.sc1a2tog5 % 2 == 1",
                        selectInput("sc1a2col1", "Select a color palette",
                                    choices=rownames(pal.info),
                                    selected="default"),
                        checkboxInput("sc1a2colinv1", "Reverse color", value = FALSE),
                        selectInput("sc1a2ord1", "Plot order:",
                                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                                    selected = "Original"),
                        selectInput("sc1a2lab1", "Labels:",
                                                    choices = c("No labels","black text","black labels","color text","color labels"),
                                                    selected = "color labels"),
                        checkboxInput("sc1a2leg1", "Show Legend", value = FALSE),
                        radioButtons("sc1a2legpos1", "Legend positions:",
                                        choices = c("top", "right", "bottom"),
                                        selected = "bottom", inline = TRUE)
                    )
                )# End of column (6 space)
            ), # End of fluidRow (2 column)
            fluidRow(column(12, withSpinner(uiOutput("sc1a2oup1.ui")))),
            downloadButton("sc1a2oup1.png", "Download png"),
            downloadButton("sc1a2oup1.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a2oup1.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a2oup1.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)), br(),
            actionButton("sc1a2tog9", "Toggle to show cell numbers / statistics"),
            conditionalPanel(
                condition = "input.sc1a2tog9 % 2 == 1",
                h4("Cell numbers / statistics"),
                radioButtons("sc1a2splt", "Split continuous cell info into:",
                                            choices = c("Quartile", "Decile"),
                                            selected = "Decile", inline = TRUE),
                dataTableOutput("sc1a2.dt")
            )
        ), # End of column (6 space)
        column(
            6, h4("Gene expression"),
            fluidRow(
                column(
                    6, selectInput("sc1a2inpg2", "Gene name:", choices=NULL) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                    title = "Gene expression to colour cells by",
                                    content = c("Select gene to colour cells by gene expression",
                                                            paste0("- Gene expression are coloured in a ",
                                                                        "White-Red colour scheme which can be ",
                                                                        "changed in the plot controls")))
                ),
                column(
                    6, actionButton("sc1a2tog6", "Toggle plot controls"),
                    conditionalPanel(
                        condition = "input.sc1a2tog6 % 2 == 1",
                        selectInput("sc1a2col2", "Select a color spectrum",
                                    choices=c(rownames(pal.info)[pal.info$category %in% c("seq","div")]),
                                    selected= "default"),
                        checkboxInput("sc1a2colinv2", "Reverse color", value = FALSE),
                        checkboxInput("sc1a2max", "unified scale bar", value = TRUE) %>%
                            helper(type = "inline", size = "m", fade = TRUE,
                                    icon = "circle-question",
                                    title = "Use global maximal scale",
                                    content = c("Select maximal expression level in the entire dataset.
                                                This is useful for cross-group comparison")),
                        radioButtons("sc1a2ord2", "Plot order:",
                                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                                    selected = "Max-1st", inline = TRUE),
                        checkboxInput("sc1a2leg2", "Show Legend", value = FALSE),
                        radioButtons("sc1a2legpos2", "Legend positions:",
                                        choices = c("top", "right", "bottom"),
                                        selected = "bottom", inline = TRUE)
                    )
                )
            ) ,
            fluidRow(column(12, withSpinner(uiOutput("sc1a2oup2.ui")))),
            downloadButton("sc1a2oup2.png", "Download png"),
            downloadButton("sc1a2oup2.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a2oup2.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a2oup2.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5))
        )    # End of column (6 space)
    )        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab.a3: cellInfo vs cellInfo on dimRed
tabPanel(
    HTML("CellInfo vs CellInfo"),
    h4("Cell information vs cell information on dimension reduction"),
    "In this tab, you can visualise two cell informations side-by-side",
    "on low-dimensional representions.",
    br(),br(),
    fluidRow(
        column(
            5, h4("Dimension Reduction"),
            fluidRow(
                column(
                    12, selectInput("sc1a3drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                                    selected = sc1def$dimred[1]),
                    selectInput("sc1a3drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                            selected = sc1def$dimred[2]))
            )
        ), # End of column (6 space)
        column(
            3, actionButton("sc1a3tog1", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1a3tog1 % 2 == 1",
                selectInput("sc1a3sub1_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp1),
                uiOutput("sc1a3sub1.ui"),
                #---------------
                actionButton("sc1a3tog2", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1a3tog2 % 2 == 1",
                    selectInput("sc1a3sub2_1", "Cell information to subset:",
                                            choices = sc1conf[grp == TRUE]$UI,
                                            selected = sc1def$grp2),
                    uiOutput("sc1a3sub2.ui"),
                    #---------------
                    actionButton("sc1a3tog3", "Toggle to further subset cells"),
                    conditionalPanel(
                        condition = "input.sc1a3tog3 % 2 == 1",
                        selectInput("sc1a3sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1a3sub3.ui")
                    )
                )
            )
        ), # End of column (6 space)
        column(
            6, actionButton("sc1a3tog4", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1a3tog4 % 2 == 1",
                fluidRow(
                    column(
                        6, sliderInput("sc1a3siz", "Point size:",
                                                        min = 0, max = 4, value = 1.25, step = 0.25),
                        radioButtons("sc1a3psz", "Plot size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE),
                        radioButtons("sc1a3fsz", "Font size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE)
                    ),
                    column(
                        6, radioButtons("sc1a3asp", "Aspect ratio:",
                                                        choices = c("Square", "Fixed", "Free"),
                                                        selected = "Square", inline = TRUE),
                        checkboxInput("sc1a3txt", "Show axis text", value = FALSE)
                    )
                )
            )
        )    # End of column (6 space)
    ),     # End of fluidRow (4 space)
    fluidRow(
        column(
            6, style="border-right: 2px solid black", h4("Cell information 1"),
            fluidRow(
                column(
                    6, selectInput("sc1a3inp1", "Cell information:",
                                                    choices = sc1conf$UI,
                                                    selected = sc1def$meta1) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                    title = "Cell information to colour cells by",
                                    content = c("Select cell information to colour cells",
                                                            "- Categorical covariates have a fixed colour palette",
                                                            paste0("- Continuous covariates are coloured in a ",
                                                                        "Blue-Yellow-Red colour scheme, which can be ",
                                                                        "changed in the plot controls")))
                ),
                column(
                    6, actionButton("sc1a3tog5", "Toggle plot controls"),
                    conditionalPanel(
                        condition = "input.sc1a3tog5 % 2 == 1",
                        selectInput("sc1a3col1", "Select a color palette",
                                    choices=rownames(pal.info),
                                    selected="default"),
                        checkboxInput("sc1a3colinv1", "Reverse color", value = FALSE),
                        selectInput("sc1a3ord1", "Plot order:",
                                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                                    selected = "Original"),
                        selectInput("sc1a3lab1", "Labels:",
                                                    choices = c("No labels", "black text","black labels", "color text", "color labels"),
                                                    selected = "black text")
                    )
                )
            ),
            fluidRow(column(12, withSpinner(uiOutput("sc1a3oup1.ui")))),
            downloadButton("sc1a3oup1.png", "Download png"),
            downloadButton("sc1a3oup1.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a3oup1.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a3oup1.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5))
        ), # End of column (6 space)
        column(
            6, h4("Cell information 2"),
            fluidRow(
                column(
                    6, selectInput("sc1a3inp2", "Cell information:",
                                                    choices = sc1conf$UI,
                                                    selected = sc1def$meta2) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                    title = "Cell information to colour cells by",
                                    content = c("Select cell information to colour cells",
                                                            "- Categorical covariates have a fixed colour palette",
                                                            paste0("- Continuous covariates are coloured in a ",
                                                                        "Blue-Yellow-Red colour scheme, which can be ",
                                                                        "changed in the plot controls")))
                ),
                column(
                    6, actionButton("sc1a3tog6", "Toggle plot controls"),
                    conditionalPanel(
                        condition = "input.sc1a3tog6 % 2 == 1",
                        selectInput("sc1a3col2", "Select a color palette",
                                    choices=rownames(pal.info),
                                    selected="default"),
                        checkboxInput("sc1a3colinv2", "Reverse color", value = FALSE),
                        selectInput("sc1a3ord2", "Plot order:",
                                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                                    selected = "Original"),
                        selectInput("sc1a3lab2", "Labels:",
                                                    choices = c("No labels", "black text","black labels","color text", "color labels"),
                                                    selected = "black text")
                    )
                )
            ),
            fluidRow(column(12, withSpinner(uiOutput("sc1a3oup2.ui")))),
            downloadButton("sc1a3oup2.png", "Download png"),
            downloadButton("sc1a3oup2.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a3oup2.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a3oup2.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5))
        )    # End of column (6 space)
    )        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab.a4: geneExpr vs geneExpr on dimRed
tabPanel(
    HTML("GeneExpr vs GeneExpr"),
    h4("Gene expression vs gene expression on dimension reduction"),
    "In this tab, you can visualise two gene expressions side-by-side ",
    "on low-dimensional representions.",
    br(),br(),
    fluidRow(
        column(
            5, h4("Dimension Reduction"),
            fluidRow(
                column(
                    12, selectInput("sc1a4drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                                    selected = sc1def$dimred[1]),
                    selectInput("sc1a4drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                            selected = sc1def$dimred[2]))
            )
        ), # End of column (6 space)
        column(
            3, actionButton("sc1a4tog1", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1a4tog1 % 2 == 1",
                selectInput("sc1a4sub1_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp1),
                uiOutput("sc1a4sub1.ui"),
                #--------------------
                actionButton("sc1a4tog2", "Toggle to subset cells"),
                conditionalPanel(
                    condition = "input.sc1a4tog2 % 2 == 1",
                    selectInput("sc1a4sub2_1", "Cell information to subset:",
                                            choices = sc1conf[grp == TRUE]$UI,
                                            selected = sc1def$grp2),
                    uiOutput("sc1a4sub2.ui"),
                    #--------------------
                    actionButton("sc1a4tog3", "Toggle to subset cells"),
                    conditionalPanel(
                        condition = "input.sc1a4tog3 % 2 == 1",
                        selectInput("sc1a4sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1a4sub3.ui")
                    )
                )
            )
        ), # End of column (6 space)
        column(
            6, actionButton("sc1a4tog4", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1a4tog4 % 2 == 1",
                fluidRow(
                    column(
                        6, sliderInput("sc1a4siz", "Point size:",
                                                        min = 0, max = 4, value = 1.25, step = 0.25),
                        checkboxInput("sc1a4max", "unified scale bar", value = TRUE) %>%
                            helper(type = "inline", size = "m", fade = TRUE,
                                    icon = "circle-question",
                                    title = "Use global maximal scale",
                                    content = c("Select maximal expression level in the entire dataset.
                                                This is useful for cross-group comparison")),
                        radioButtons("sc1a4psz", "Plot size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE),
                        radioButtons("sc1a4fsz", "Font size:",
                                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                                    selected = "Medium", inline = TRUE)
                    ),
                    column(
                        6, radioButtons("sc1a4asp", "Aspect ratio:",
                                                        choices = c("Square", "Fixed", "Free"),
                                                        selected = "Square", inline = TRUE),
                        checkboxInput("sc1a4txt", "Show axis text", value = FALSE),
                        checkboxInput("sc1a4leg", "Show Legend", value = TRUE),
                        radioButtons("sc1a4legpos", "Legend positions:",
                                        choices = c("top", "right", "bottom"),
                                        selected = "bottom", inline = TRUE)
                    )
                )
            )
        )    # End of column (6 space)
    ),     # End of fluidRow (4 space)
    fluidRow(
        column(
            6, style="border-right: 2px solid black", h4("Gene expression 1"),
            fluidRow(
                column(
                    6, selectInput("sc1a4inpg1", "Gene name:", choices=NULL) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                    title = "Gene expression to colour cells by",
                                    content = c("Select gene to colour cells by gene expression",
                                                            paste0("- Gene expression are coloured in a ",
                                                                        "White-Red colour scheme which can be ",
                                                                        "changed in the plot controls")))
                ),
                column(
                    6, actionButton("sc1a4tog5", "Toggle plot controls"),
                    conditionalPanel(
                        condition = "input.sc1a4tog5 % 2 == 1",
                        selectInput("sc1a4col1", "Select a color spectrum",
                                    choices=c(rownames(pal.info)[pal.info$category %in% c("seq","div")]),
                                    selected = "default"),
                        checkboxInput("sc1a4colinv1", "Reverse color", value = FALSE),
                        radioButtons("sc1a4ord1", "Plot order:",
                                        choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                        selected = "Max-1st", inline = TRUE)
                    )
                )
            ),
            fluidRow(column(12, withSpinner(uiOutput("sc1a4oup1.ui")))),
            downloadButton("sc1a4oup1.png", "Download png"),
            downloadButton("sc1a4oup1.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a4oup1.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a4oup1.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5))
        ), # End of column (6 space))
        column(
            6, h4("Gene expression 2"),
            fluidRow(
                column(
                    6, selectInput("sc1a4inpg2", "Gene name:", choices=NULL) %>%
                        helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                    title = "Gene expression to colour cells by",
                                    content = c("Select gene to colour cells by gene expression",
                                                            paste0("- Gene expression are coloured in a ",
                                                                        "White-Red colour scheme which can be ",
                                                                        "changed in the plot controls")))
                ),
                column(
                    6, actionButton("sc1a4tog6", "Toggle plot controls"),
                    conditionalPanel(
                        condition = "input.sc1a4tog6 % 2 == 1",
                        selectInput("sc1a4col2", "Select a color spectrum",
                                    choices=c(rownames(pal.info)[pal.info$category %in% c("seq","div")]),
                                    selected = "default"),
                        checkboxInput("sc1a4colinv2", "Reverse color", value = FALSE),
                        radioButtons("sc1a4ord2", "Plot order:",
                                        choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                        selected = "Max-1st", inline = TRUE)
                    )
                )
            ),
            fluidRow(column(12, withSpinner(uiOutput("sc1a4oup2.ui")))),
            downloadButton("sc1a4oup2.png", "Download png"),
            downloadButton("sc1a4oup2.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                    numericInput("sc1a4oup2.h", "png / jpeg height:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5)),
            div(style="display:inline-block",
                    numericInput("sc1a4oup2.w", "png / jpeg width:", width = "138px",
                                                min = 4, max = 50, value = 8, step = 0.5))
        )    # End of column (6 space)
    )        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab.b1: Gene coexpression plot
tabPanel(
    HTML("Gene coexpression"),
    h4("Coexpression of two genes on reduced dimensions"),
    "In this tab, you can visualise the coexpression of two genes ",
    "on low-dimensional representions.",
    br(),br(),
    fluidRow(
        column(
            5, h4("Gene Expression on Dimension Reduction"),
            fluidRow(
                column(
                    12, selectInput("sc1b1drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                                    selected = sc1def$dimred[1]),
                    selectInput("sc1b1drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                            selected = sc1def$dimred[2]),
                selectInput("sc1b1inpg1", "Gene 1:", choices=NULL) %>%
                    helper(type = "inline", size = "m", fade = TRUE,
                        icon = "circle-question",
                        title = "Gene expression to colour cells by",
                        content = c("Select gene to colour cells by gene expression",
                                    paste0("- Gene expression are coloured in a ",
                                            "White-Red colour scheme which can be ",
                                            "changed in the plot controls"))),
                selectInput("sc1b1inpg2", "Gene 2:", choices=NULL) %>%
                    helper(type = "inline", size = "m", fade = TRUE,
                        icon = "circle-question",
                        title = "Gene expression to colour cells by",
                        content = c("Select gene to colour cells by gene expression",
                                    paste0("- Gene expression are coloured in a ",
                                            "White-Blue colour scheme which can be ",
                                            "changed in the plot controls")))
                )
            )
        ), # End of column (6 space)
        column(
            3, actionButton("sc1b1tog0", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1b1tog0 % 2 == 1",
                selectInput("sc1b1sub1_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp1),
                uiOutput("sc1b1sub1.ui"),
                #---------------------
                actionButton("sc1b1tog1", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1b1tog1 % 2 == 1",
                    selectInput("sc1b1sub2_1", "Cell information to subset:",
                                            choices = sc1conf[grp == TRUE]$UI,
                                            selected = sc1def$grp2),
                    uiOutput("sc1b1sub2.ui"),
                    #---------------------
                    actionButton("sc1b2tog1", "Toggle to further subset cells"),
                    conditionalPanel(
                        condition = "input.sc1b2tog1 % 2 == 1",
                        selectInput("sc1b1sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1b1sub3.ui")
                    )
                )
            )
        ), # End of column (6 space)
        column(
            6, actionButton("sc1b1tog3", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1b1tog3 % 2 == 1",
                fluidRow(
                    column(
                        6, sliderInput("sc1b1siz", "Point size:",
                                                    min = 0, max = 4, value = 1.5, step = 0.25),
                        checkboxInput("sc1b1max", "unified scale bar", value = TRUE) %>%
                            helper(type = "inline", size = "m", fade = TRUE,
                                icon = "circle-question",
                                title = "Use global maximal scale",
                                content = c("Select maximal expression level in the entire dataset.
                                                This is useful for cross-group comparison")),
                        radioButtons("sc1b1psz", "Plot size:",
                                                choices = c("Small", "Medium", "Large", "Extra Large"),
                                                selected = "Medium", inline = TRUE),
                        radioButtons("sc1b1fsz", "Font size:",
                                                choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                                selected = "Medium", inline = TRUE),
                        radioButtons("sc1b1asp", "Aspect ratio:",
                                    choices = c("Square", "Fixed", "Free"),
                                    selected = "Square", inline = TRUE),
                        checkboxInput("sc1b1txt", "Show axis text", value = TRUE),
                        checkboxInput("sc1b1title", "Show axis title", value = TRUE),
                        shinyjs::useShinyjs(),
                        shinyjs::hidden(
                            div(id = "sc1b1tog8_open",
                                sliderInput("sc1b1xlim", "x-axis range:",
                                            min = -20, max = 20, value = c(-20,20), step = 0.25),
                                sliderInput("sc1b1ylim", "y-axis range:",
                                            min = -20, max = 20, value = c(-20,20), step = 0.25)
                            )
                        )
                    ),
                    column(
                        6,
                        radioButtons("sc1b1mulcol","Colour for dimension reduction plot",
                                    choices = c("Bi-Colors", "Tri-Colors"),
                                    selected = "Tri-Colors"),
                        radioButtons("sc1b1col1", NULL,
                                    choices = c("Red (Gene1); Blue (Gene2); Purple (Both)",
                                                "Green (Gene1); Blue (Gene2); Red (Both)",
                                                "Orange (Gene1); Blue (Gene2); Red (Both)",
                                                "Yellow (Gene1); Blue (Gene2); Red (Both)",
                                                "Purple (Gene1); Green (Gene2); Red (Both)"),
                                    selected = "Orange (Gene1); Blue (Gene2); Red (Both)"),
                        radioButtons("sc1b1bg", "background color:",
                                    choices = c("snow3", "lightgrey", "grey"),
                                    selected = "lightgrey", inline = TRUE),
                        radioButtons("sc1b1ord1", "Plot order:",
                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                    selected = "Max-1st", inline = TRUE)
                    )
                )
            )
        )    # End of column (6 space)
    ),     # End of fluidRow (4 space)
    fluidRow(
        column(
            8,style="border-right: 2px solid black",
            withSpinner(uiOutput("sc1b1oup1.ui")),
            downloadButton("sc1b1oup1.png", "Download png"),
            downloadButton("sc1b1oup1.jpeg", "Download jpeg"),
            div(style="display:inline-block",
                numericInput("sc1b1oup1.h", "height:", width = "70px",
                            min = 4, max = 20, value = 10, step = 0.5)),
            div(style="display:inline-block",
                numericInput("sc1b1oup1.w", "width:", width = "70px",
                            min = 4, max = 20, value = 10, step = 0.5)),
            actionButton("sc1b1tog6", "Toggle Scatter plot"),
            conditionalPanel(
                condition = "input.sc1b1tog6 % 2 == 1",
                selectInput("sc1b1grp", "group by:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                actionButton("sc1b1tog7", "Toggle Scatter plot graphics controls"),
                conditionalPanel(
                    condition = "input.sc1b1tog7 % 2 == 1",
                    fluidRow(
                        column(
                            6,
                            selectInput("sc1b1col2", "Colour for scatter plot",
                                        choices=c("default",rownames(pal.info)[pal.info$category %in% "qual"]),
                                        selected="default"),
                            checkboxInput("sc1b1leg", "Show Legend", value = TRUE),
                            radioButtons("sc1b1ord2", "Group order:",
                                        choices = c("As-it-is", "Reverse"),
                                        selected = "As-it-is", inline = TRUE),
                            radioButtons("sc1b1dtype", "Density plot type:",
                                        choices = c("density","density_ridges"),
                                        selected = "density_ridges", inline = TRUE)
                        ) # End of column (4 space)
                    )
                ), # End of conditionalPanel
                withSpinner(uiOutput("sc1b1oup3.ui")),
                downloadButton("sc1b1oup3.png", "Download png"),
                downloadButton("sc1b1oup3.jpeg", "Download jpeg"),
                div(style="display:inline-block",
                    numericInput("sc1b1oup3.h", "height:", width = "70px",
                                min = 4, max = 20, value = 10, step = 0.5)),
                div(style="display:inline-block",
                    numericInput("sc1b1oup3.w", "width:", width = "70px",
                                min = 4, max = 20, value = 10, step = 0.5))
                ) # End of conditionalPanel
        ), # End of column (9 space)
        column(
            4, uiOutput("sc1b1oup2.ui"),
            downloadButton("sc1b1oup2.png", "Download png"),
            downloadButton("sc1b1oup2.jpeg", "Download jpeg"),
            br(),
            style="border-bottom: 2px solid black",
            actionButton("sc1b1tog4", "All cells numbers / statistics"),
            conditionalPanel(
                condition = "input.sc1b1tog4 % 2 == 1",
                h4("All cells"),
                dataTableOutput("sc1b1_all.dt"),
                dataTableOutput("sc1b1_all_cor.dt")
            ),
            br(),

            style="border-top: 2px solid black",
            actionButton("sc1b1tog5", "Selected cells numbers / statistics"),
            conditionalPanel(
                condition = "input.sc1b1tog5 % 2 == 1",
                h4("Selected cells"),
                dataTableOutput("sc1b1_selec.dt"),
                dataTableOutput("sc1b1_selec_cor.dt")
            ) # End of conditionalPanel
        )    # End of column (3 space)
    )        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab.c1: violinplot / boxplot
tabPanel(
    HTML("Violinplot"),
    h4("Cell information / gene expression violin plot / box plot"),
    "In this tab, you can visualise the gene expression or continuous cell information ",
    "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).",
    br(),br(),
    fluidRow(
        column(
            3, style="border-right: 2px solid black",
            selectInput("sc1c1inp1", "Cell Info (X-axis):",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp1) %>%
                helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Cell Info to group cells by",
                    content = c("Select categorical cell information to group cells by",
                                            "- Single cells are grouped by this categorical covariate",
                                            "- Plotted as the X-axis of the violin plot / box plot")),
        selectInput("sc1c1inpg2", "Cell Info / Gene name (Y-axis):",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1) %>%
                helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Cell Info / Gene to plot",
                    content = c("Select cell info / gene to plot on Y-axis",
                                            "- Can be continuous cell information (e.g. nUMIs / scores)",
                                            "- Can also be gene expression")),
            selectInput("sc1c1inp3", "Cell Info to split:",
                        choices = c("no split",sc1conf[grp == TRUE]$UI),
                        selected = "no split"),
            radioButtons("sc1c1typ", "Plot type:",
                                    choices = c("violin", "split violin","boxplot","barplot"),
                                    selected = "violin", inline = FALSE),
            checkboxInput("sc1c1sig", "Run Wilcoxon test", value = FALSE),
            checkboxInput("sc1c1pts", "Show data points", value = FALSE),
            actionButton("sc1c1tog1", "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.sc1c1tog1 % 2 == 1",
                checkboxInput("sc1c1postive", "remove non-expressing cells", value = FALSE),
                selectInput("sc1c1sub1_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp1),
                uiOutput("sc1c1sub1.ui"),
                #---------------------
                actionButton("sc1c1tog2", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1c1tog2 % 2 == 1",
                    selectInput("sc1c1sub2_1", "Cell information to subset:",
                                            choices = sc1conf[grp == TRUE]$UI,
                                            selected = sc1def$grp2),
                    uiOutput("sc1c1sub2.ui"),
                    #----------------------
                    actionButton("sc1c1tog3", "Toggle to further subset cells"),
                    conditionalPanel(
                        condition = "input.sc1c1tog3 % 2 == 1",
                        selectInput("sc1c1sub3_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp2),
                        uiOutput("sc1c1sub3.ui")
                    )
                )
            ),
            actionButton("sc1c1tog4", "Toggle graphics controls"),
            conditionalPanel(
                condition = "input.sc1c1tog4 % 2 == 1",
                selectInput("sc1c1cols","Select a color palette:",
                            choices=c("default","white",rownames(pal.info)[pal.info$category %in% "qual"]),
                            selected="default"),
                colourpicker::colourInput(inputId ="sc1c1col",returnName = TRUE,
                                        palette = "square", #allowedCols = colors()[grep('[0-9]', grDevices::colors(), invert = T)],
                                        label = "manually pick one color", value = "#FFFFFF",
                                        allowTransparent = TRUE,
                                        closeOnClick = FALSE),
                sliderInput("sc1c1siz", "Data point size",
                            min = 0, max = 8, value = 1, step = 0.25),
                sliderInput("sc1c1alpha", "Data point transparency",
                            min = 0, max = 1, value = 0.8, step = 0.1),
                selectInput("sc1c1scale","Select violin plot scale:",
                            choices=c("area","count","width"),
                            selected="width"),
                sliderInput("sc1c1width", "violin or bar width",
                            min = 0, max = 1.2, value = 0.8, step = 0.1),
                checkboxInput("sc1c1err", "Show error bar", value = TRUE),
                radioButtons("sc1c1psz", "Plot size:",
                                        choices = c("Small", "Medium", "Large","Extra Large"),
                                        selected = "Medium", inline = TRUE),
                radioButtons("sc1c1fsz", "Font size:",
                                        choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
                radioButtons("sc1c1frt", "Rotate x axis label:",
                                        choices = c(0,30,45,90),
                                        selected = 45, inline = TRUE),
                checkboxInput("sc1c1leg", "Show Legend", value = TRUE),
                radioButtons("sc1c1legpos", "Legend positions:",
                            choices = c("top", "right", "bottom"),
                            selected = "bottom", inline = TRUE)
                ),
            br(),
            actionButton("sc1c1tog5", "Toggle p value controls"),
            conditionalPanel(
                condition = "input.sc1c1tog5 % 2 == 1",
                selectInput("sc1c1pvalmethod","Select method to adjust p values",
                            choices= c("fdr","BH", "BY", "holm", "hochberg", "hommel", "bonferroni"),
                            selected="fdr"),
                sliderInput("sc1c1pcut", "log10 p value cut off",
                            min = 0, max = 300, value = 2, step = 1),
                sliderInput("sc1c1pvalpos", "Change p value position",
                            min = -1, max = 3, value = 0, step = 0.01),
                sliderInput("sc1c1ylim", "Change Y upper limit %",
                            min = -1, max = 4, value = 0.2, step = 0.01),
                radioButtons("sc1c1plab", "Show p-value label type:",
                            selected = "p",inline = FALSE,
                            choices = c("p","p.signif","p.adj","p.adj.signif"))

            )
        ), # End of column (6 space)
        column(9, withSpinner(uiOutput("sc1c1oup.ui")),
                    downloadButton("sc1c1oup.png", "Download png"),
                    downloadButton("sc1c1oup.jpeg", "Download jpeg"), br(),
                    div(style="display:inline-block",
                            numericInput("sc1c1oup.h", "png / jpeg height:", width = "138px",
                                                        min = 4, max = 50, value = 10, step = 0.5)),
                    div(style="display:inline-block",
                            numericInput("sc1c1oup.w", "png / jpeg width:", width = "138px",
                                                        min = 4, max = 50, value = 10, step = 0.5)),br(),
            actionButton("sc1c1tog6", "Toggle to data summary"),
            conditionalPanel(
                condition = "input.sc1c1tog6 % 2 == 1",
                h4("data summary"),
                dataTableOutput("sc1c1out1.dt")
            ),
            actionButton("sc1c1tog7", "Toggle to statistics summary"),
            conditionalPanel(
                condition = "input.sc1c1tog7 % 2 == 1",
                h4("statistics summary"),
                dataTableOutput("sc1c1out2.dt")
            )
        )    # End of column (6 space)
    )        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab1.c2: Proportion plot
tabPanel(
HTML("Proportion plot"),
h4("Proportion / cell numbers across different cell information"),
"In this tab, you can visualise the composition of single cells based on one discrete ",
"cell information across another discrete cell information. ",
"Usage examples include the library or cellcycle composition across clusters.",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        selectInput("sc1c2inp1", "Cell Info (X-axis):",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$meta1) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                        title = "Cell information to plot cells by",
                        content = c("Select categorical cell information to plot cells by",
                                                "- Plotted as the X-axis of the proportion plot")),
        selectInput("sc1c2norm", "Cell Number / Proportion (Y-axis):",
                    choices = c("Cell Number", "Cell % by sample","Cell % by X-axis","Cell % by Y-axis"),
                    selected = "Cell Number"),
        selectInput("sc1c2inp2", "Cell Info to stack or split:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp1),
        radioButtons("sc1c2split", NULL, choices =  c("stack","split"),selected = "stack", inline = TRUE),
        radioButtons("sc1c2typ", "Plot type:",
                                    choices = c("barplot","boxplot","piechart","pairedplot","scatterplot"),
                                    selected = "barplot", inline = FALSE),
        radioButtons("sc1c2sig", "Run statistical test", choices =  c("no test","wilcox.test","t.test","chisq"),
                        inline = TRUE) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Select groups",
                    content = c("paired test only avaible in t.test and in wilcox.test.")),
        checkboxInput("sc1c2pts", "Show data points", value = FALSE),
        actionButton("sc1c2tog10", "Specify X-axis order"),
        conditionalPanel(
            condition = "input.sc1c2tog10 % 2 == 1",
            selectizeInput("sc1c2order", choices=NULL,multiple=TRUE,label = NULL)
        ),
        actionButton("sc1c2tog1", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1c2tog1 % 2 == 1",
            selectInput("sc1c2sub1_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp1),
            uiOutput("sc1c2sub1.ui"),
            actionButton("sc1c2tog2", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1c2tog2 % 2 == 1",
                selectInput("sc1c2sub2_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp2),
                uiOutput("sc1c2sub2.ui"),
                actionButton("sc1c2tog3", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1c2tog3 % 2 == 1",
                    selectInput("sc1c2sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1c2sub3.ui")
                )
            )
        ),
        br(),
        actionButton("sc1c2tog4", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1c2tog4 % 2 == 1",
            selectInput("sc1c2cols","Select a color palette:",
                        choices=c("default",rownames(pal.info)[pal.info$category %in% "qual"]),
                        selected="default"),
            sliderInput("sc1c2width", "violin or bar width",
                        min = 0, max = 1.2, value = 0.8, step = 0.1),
            sliderInput("sc1c2siz", "Data point size",
                        min = 0, max = 8, value = 2, step = 0.25),
            sliderInput("sc1c2filt", "Remove low percentage label %:",
                        min = 0, max = 10, value = 1, step = 0.01),
            radioButtons("sc1c2psz", "Plot size:",
                                        choices = c("Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
            checkboxInput("sc1c2addline", "Add line", value = TRUE),
            checkboxInput("sc1c2err", "Show error bar", value = TRUE),
            radioButtons("sc1c2fsz", "Font size:",
                                        choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
            radioButtons("sc1c2lsz", "line size:",
                                        choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
            selectInput("sc1c2lab", "Labels:",
                            choices = c("No %","No labels", "black text","color labels"),
                            selected = "color labels"),
            checkboxInput("sc1c2flpxy", "Flip X/Y", value = FALSE),
            checkboxInput("sc1c2flpx", "Flip X axis order", value = FALSE),
            radioButtons("sc1c2frt", "Rotate x axis label:",
                                        choices = c(0,30,45,90),
                                        selected = 90, inline = TRUE),
            checkboxInput("sc1c2leg", "Show Legend", value = TRUE),
            radioButtons("sc1c2legpos", "Legend positions:",
                            choices = c("top", "right", "bottom"),
                            selected = "right", inline = TRUE)
            ),
        br(),
        actionButton("sc1c2tog5", "Toggle p value controls"),
        conditionalPanel(
            condition = "input.sc1c2tog5 % 2 == 1",
            sliderInput("sc1c2pcut", "log10 p value cut off",
                        min = 0, max = 300, value = 0, step = 1),
            sliderInput("sc1c2yrange", "Change y-axis upper limit",
                        min = -1, max = 2, value = 0, step = 0.01),
            sliderInput("sc1c2pvalpos", "Increase p value position",
                        min = -1, max = 2, value = 0, step = 0.01),
            radioButtons("sc1c2plab", "Show p-value label type:",
                            selected = "p",inline = FALSE,
                            choices = c("p","p.signif","p.adj","p.adj.signif"))

        )

    ), # End of column (6 space)
    column(9, withSpinner(uiOutput("sc1c2oup.ui")),
                    downloadButton("sc1c2oup.png", "Download png"),
                    downloadButton("sc1c2oup.jpeg", "Download jpeg"), br(),
                    div(style="display:inline-block",
                            numericInput("sc1c2oup.h", "png / jpeg height:", width = "138px",
                                                    min = 4, max = 50, value = 10, step = 0.5)),
                    div(style="display:inline-block",
                            numericInput("sc1c2oup.w", "png / jpeg width:", width = "138px",
                                                    min = 4, max = 50, value = 10, step = 0.5)),
                br(),
                actionButton("sc1c2tog6", "Toggle to show cell numbers"),
                conditionalPanel(
                    condition = "input.sc1c2tog6 % 2 == 1",
                    h4("Toggle to show cell number and percentage"),
                    dataTableOutput("sc1c2.dt")
                )

    )    # End of column (6 space)
)        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab d1: Bubbleplot/ Heatmap
tabPanel(
HTML("Bubbleplot / Heatmap"),
h4("Gene expression bubbleplot / heatmap"),
"In this tab, you can visualise the gene expression patterns of ",
"multiple genes grouped by categorical cell information (e.g. library / cluster).", br(),
"The normalised expression are averaged, log-transformed and then plotted.",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        selectInput("sc1d1grp", HTML("Cell information (X-axis)<br />
                                                    Group / colour by:"),
                    choices = sc1conf[grp == TRUE]$UI,
                    selected = sc1conf[grp == TRUE]$UI[1]) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Select groups",
                    content = c("Select categorical cell information to group cells by",
                                "- Single cells are grouped by this categorical covariate",
                                "- Plotted as the X-axis of the bubbleplot / heatmap")),
        selectizeInput("sc1d1inp2", choices=NULL,multiple=TRUE,label = "Specify cluster order (optional)"),
        textAreaInput("sc1d1inp", HTML("List of gene names (Y-axis)<br />
                                        Separated by , or ; or newline"),
                        height = "200px",
                        value = paste0(sc1def$genes, collapse = ", ")) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "List of genes to plot on bubbleplot / heatmap",
                    content = c("Input genes to plot",
                                "- Maximum 300 genes (due to ploting space limitations)",
                                "- Genes should be separated by comma, semicolon or newline")),
        radioButtons("sc1d1plt", "Plot type:",
                        choices = c("Bubbleplot", "Heatmap"),
                        selected = "Bubbleplot", inline = TRUE),
        actionButton(inputId = "sc1d1update", "Generate Plot",icon("sync",verify_fa = FALSE)),
        checkboxInput("sc1d1zscore", "Scale gene expression", value = TRUE),
        checkboxInput("sc1d1row", "Cluster rows (genes)", value = TRUE),
        checkboxInput("sc1d1col", "Cluster columns (groups)", value = FALSE),
        shinyjs::useShinyjs(),
        a(id = "sc1d1tog6", "Dendragram options"),
        shinyjs::hidden(
            div(id = "sc1d1tog6_open",
                sliderInput("sc1d1xlim", "Expand row dendragram:",
                            min = 0, max = 10, value = 2, step = 0.05),
                sliderInput("sc1d1ylim", "Expand column dendragram:",
                            min = 0, max = 10, value = 2, step = 0.05)

            )
        ),
        actionButton("sc1d1tog2", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1d1tog2 % 2 == 1",
            selectInput("sc1d1sub1_1", "Cell information to subset:",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1),
            uiOutput("sc1d1sub1.ui"),
            #---------------------
            actionButton("sc1d1tog3", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1d1tog3 % 2 == 1",
                selectInput("sc1d1sub2_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                uiOutput("sc1d1sub2.ui"),
                #----------------------
                actionButton("sc1d1tog4", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1d1tog4 % 2 == 1",
                    selectInput("sc1d1sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1d1sub3.ui")
                )
            )
        ),
        br(),
        actionButton("sc1d1tog5", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1d1tog5 % 2 == 1",
            selectInput("sc1d1cols", "Select a color specturm:",
                        choices=c(rownames(pal.info)[pal.info$category %in% "div"]),
                        selected="Blue-White-Red"),
            checkboxInput("sc1d1colinv", "Reverse color", value = FALSE),
            sliderInput("sc1d1siz", "Point size:",
                        min = 0, max = 32, value = 8, step = 1),
            radioButtons("sc1d1psz", "Plot size:",
                            choices = c("Small", "Medium", "Large", "Extra Large"),
                            selected = "Medium", inline = TRUE),
            radioButtons("sc1d1fsz", "Font size:",
                            choices = c("Extra Small","Small", "Medium", "Large","Extra Large"),
                            selected = "Small", inline = TRUE),
            radioButtons("sc1d1frt", "Rotate x axis label:",
                            choices = c(0,30,45,90),
                            selected = 45, inline = TRUE),
            radioButtons("sc1d1asp", "Aspect ratio:",
                            choices = c("Square", "Free"),
                            selected = "Free", inline = TRUE),
            checkboxInput("sc1d1flpxy", "Flip x and y axis", value = FALSE),
            checkboxInput("sc1d1leg", "Show Legend", value = TRUE),
            radioButtons("sc1d1legpos", "Legend positions:",
                            choices = c("top", "right", "bottom"),
                            selected = "bottom", inline = TRUE)
        )

    ),
    column(9, h4(htmlOutput("sc1d1oupTxt")),
            downloadButton("sc1d1oup.png", "Download png"),
            downloadButton("sc1d1oup.jpeg", "Download jpeg"),
            downloadButton("sc1d1oup1.xlsx", "Download xlsx"),
            br(),
            div(style="display:inline-block",
                numericInput("sc1d1oup.h", "png / jpeg height:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)),
            div(style="display:inline-block",
                numericInput("sc1d1oup.w", "png / jpeg width:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)
            ),
            withSpinner(uiOutput("sc1d1oup.ui"))
    )    # End of column (9 space)
)        # End of fluidRow (4 space)
),            # End of tab (2 space)

### Tab1.e1: multiple gene expression trajectory
tabPanel(
HTML("Trajectory"),
h4("Gene expression trajectory"),
"In this tab, you can visualise the multiple genes expression",
"(e.g. multiple genes expression across groups of cells (e.g. libary / clusters).",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        textAreaInput("sc1e1inp1", HTML("List of gene names <br/>
                                        Separated by , or ; or newline"),
                                    height = "200px",
                                    value = paste0(sc1def$genes, collapse = ", ")) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                        title = "List of genes to plot on trajectory plot",
                        content = c("Input genes to plot",
                                                "- Maximum 50 genes (due to ploting space limitations)",
                                                "- Genes should be separated by comma, semicolon or newline")),
        selectInput("sc1e1inp2", "Cell information (X-axis):",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                        title = "Cell information to group cells by",
                        content = c("Select categorical cell information to group cells by",
                                                "- Single cells are grouped by this categorical covariate",
                                                "- Plotted as the X-axis of the violin plot / box plot")),
        actionButton("sc1e1tog1", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1e1tog1 % 2 == 1",
            selectInput("sc1e1sub1_1", "Cell information to subset:",
                                    choices = sc1conf[grp == TRUE]$UI,
                                    selected = sc1def$grp1),
            uiOutput("sc1e1sub1.ui"),
            actionButton("sc1e1tog2", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1e1tog2 % 2 == 1",
                selectInput("sc1e1sub2_1", "Cell information to subset:",
                                        choices = sc1conf[grp == TRUE]$UI,
                                        selected = sc1def$grp2),
                uiOutput("sc1e1sub2.ui")
            )
        ),
        br(),
        actionButton("sc1e1tog", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1e1tog % 2 == 1",
            selectInput("sc1e1cols","Select a color palette:",
                        choices=c("default",rownames(pal.info)[pal.info$category %in% "qual"]),
                        selected="default"),
            checkboxInput("sc1e1norm", "Y-axis shows %", value = FALSE),
            checkboxInput("sc1e1log", "Log transform", value = TRUE),
            checkboxInput("sc1e1y_0", "Y axis starts from zero", value = TRUE),
            radioButtons("sc1e1psz", "Plot size:",
                                        choices = c("Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
            checkboxInput("sc1e1pts", "Show data points", value = TRUE),
            sliderInput("sc1e1lvls", "conf.int.level:",
                                    min = 0, max = 1, value = 0.3, step = 0.05),
            radioButtons("sc1e1fsz", "Font size:",
                                        choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
            radioButtons("sc1e1lsz", "line size:",
                                        choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                        selected = "Medium", inline = TRUE),
            radioButtons("sc1e1frt", "Rotate x axis label:",
                                        choices = c(0,30,45,90),
                                        selected = 0, inline = TRUE),
            checkboxInput("sc1e1leg", "Show Legend", value = TRUE))
    ), # End of column (6 space)
    column(9, withSpinner(uiOutput("sc1e1oup.ui")),
                    downloadButton("sc1e1oup.png", "Download png"),
                    downloadButton("sc1e1oup.jpeg", "Download jpeg"), br(),
                    div(style="display:inline-block",
                            numericInput("sc1e1oup.h", "png / jpeg height:", width = "138px",
                                                    min = 4, max = 50, value = 8, step = 0.5)),
                    div(style="display:inline-block",
                            numericInput("sc1e1oup.w", "png / jpeg width:", width = "138px",
                                                    min = 4, max = 50, value = 10, step = 0.5))
    )    # End of column (6 space)
)        # End of fluidRow (4 space)
),         # End of tab (2 space)

### Tab.f1: Differential analysis 1
tabPanel(
HTML("Differential Analysis 1"),
h4("Run pairwise differential expression analysis"),
"In this tab, you can identify all marker genes from two groups by pairwise differential analysis, then visualize them in a volcano plot",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        selectInput("sc1f1grp", h4("Select groups"),
                    choices = sc1conf[grp == TRUE]$UI,
                    selected = sc1def$grp1),
        uiOutput("sc1f1ident1.ui") %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Select test group",
                    content = c("Select treated group, equivalent to 'ident.1' in Seurat::FindMarkers, 'groups' in scanpy.tl.rank_genes_groups.")),
        uiOutput("sc1f1ident2.ui") %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Select control group",
                    content = c("Select control group, equivalent to 'ident.2' in Seura::FindMarkers, 'reference' in scanpy.tl.rank_genes_groups.
                                        If NULL, use all other cells for comparison")),
        actionButton(inputId = "sc1f1update", "Run differential analysis",icon("sync",verify_fa = FALSE)),
        actionButton("sc1f1tog1", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1f1tog1 % 2 == 1",
            selectInput("sc1f1sub1_1", "Cell information to subset:",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1),
            uiOutput("sc1f1sub1.ui"),
            #-----------------------
            actionButton("sc1f1tog2", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1f1tog2 % 2 == 1",
                selectInput("sc1f1sub2_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                uiOutput("sc1f1sub2.ui"),
                #-----------------------
                actionButton("sc1f1tog3", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1f1tog3 % 2 == 1",
                    selectInput("sc1f1sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1f1sub3.ui")
                )
            )
        ),
        actionButton("sc1f1tog4", "Toggle DE setting"),
        conditionalPanel(
            condition = "input.sc1f1tog4 % 2 == 1",
            selectInput("sc1f1DEmethod", "Select a DE method",
                        choices=c("wilcoxon","logreg",  "t-test_overestim_var","t-test"),
                        selected= "wilcoxon"),
            numericInput("sc1f1top", "Select top N DEGs:", min = 0, max = 20, value = 10, step = 0.5),
            radioButtons("sc1f1cutp", "y-axis by:",
                            choices = c("p_val_adj","p_val"),
                            selected = "p_val_adj", inline = TRUE),
            numericInput("sc1f1cutpval", NULL, min = 0, max = 1, value = 0.05),
            numericInput("sc1f1cutfc", "keep genes with | log2FC | >", value = "0.25"),
            numericInput("sc1f1cutpct", "keep genes with of cells expressing % >", min = 0,max = 100, value = 10),
            sliderInput("sc1f1cutmincell", "keep genes at minimum number of cells:",min = 0, max = 100, value = 50, step = 10),
            radioButtons("sc1f1sort", "Order DEGs by:",
                            choices = c("p_val_adj","FDR","p_val", "avg_log2FC"),
                            selected = "p_val_adj", inline = TRUE),
            checkboxGroupInput("sc1f1rmgene", "Remove unwanted genes:", inline = TRUE,
                                choices = c("Mitochondrial (MT) genes",
                                            "Ribosomal protein large subunit (RPL)",
                                            "Ribosomal protein small subunit (RPS)"),
                                selected = c("Mitochondrial (MT) genes",
                                            "Ribosomal protein large subunit (RPL)",
                                            "Ribosomal protein small subunit (RPS)")
                               )
        ),
        actionButton("sc1f1tog5", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1f1tog5 % 2 == 1",
            selectInput("sc1f1cols", "Select a color spectrum",
                        choices=c(rownames(pal.info)[pal.info$category %in% c("div")]),
                        selected= "Blue-White-Red"),
            checkboxInput("sc1f1colinv", "Reverse color", value = FALSE),
            selectInput("sc1f1lab1", "Labels for top DE genes:",
                            choices = c("No labels", "black text","black labels","color text","color labels"),
                            selected = "color text"),
            selectInput("sc1f1lab2", "Labels for manually input genes:",
                            choices = c("No labels", "red text","red labels"),
                            selected = "red text"),
            sliderInput("sc1f1psiz", "Dot size:",
                        min = 0, max = 8, value = 3, step = 0.5),
            sliderInput("sc1f1alpha", "Dot transparency:",
                        min = 0, max = 1, value = 0.9, step = 0.05),
            radioButtons("sc1f1psz", "Plot size:",
                            choices = c("Small", "Medium", "Large", "Extra Large"),
                            selected = "Large", inline = TRUE),
            radioButtons("sc1f1fsz", "Font size:",
                            choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                            selected = "Extra Large", inline = TRUE),
            radioButtons("sc1f1asp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("sc1f1leg", "Show Legend", value = FALSE),
            radioButtons("sc1f1legpos", "Legend positions:",
                            choices = c("top", "right", "bottom"),
                            selected = "bottom", inline = TRUE)
        ),
        actionButton("sc1f1tog6", "Toggle to add genes manually"),
        conditionalPanel(
            condition = "input.sc1f1tog6 % 2 == 1",
            textAreaInput("sc1f1inp", HTML("List of gene names (Y-axis)<br />
                                                Separated by , or ; or newline"),
                            height = "50px",
                            value = paste0("", collapse = ", "))
        )
    ),# End of column (3 space)
    column(9,
            tags$head(
                HTML(
                    "
                        <script>
                        var socket_timeout_interval
                        var n = 0
                        $(document).on('shiny:connected', function(event) {
                        socket_timeout_interval = setInterval(function(){
                        Shiny.onInputChange('count', n++)
                        }, 15000)
                        });
                        $(document).on('shiny:disconnected', function(event) {
                        clearInterval(socket_timeout_interval)
                        });
                        </script>
                    "
                )
            ),
            withSpinner(uiOutput("sc1f1oup1.ui")),
            downloadButton("sc1f1oup1.png", "Download png"),
            downloadButton("sc1f1oup1.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                numericInput("sc1f1oup1.h", "png / jpeg height:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)),
            div(style="display:inline-block",
                numericInput("sc1f1oup1.w", "png / jpeg width:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)), br(),
            actionButton("sc1f1tog9", "Toggle to show DE results"),
            conditionalPanel(
                condition = "input.sc1f1tog9 % 2 == 1",
                h4("Toggle to show differential analysis results"),
                withSpinner(dataTableOutput("sc1f1.dt"))
            ),
            downloadButton("sc1f1oup1.csv", "Download csv"),
            downloadButton("sc1f1oup1.xlsx", "Download xlsx")
    )    # End of column (9 space)
)        # End of fluidRow (4 space)
),            # End of tab (2 space)

### Tab.f2: Differential Analysis 2
tabPanel(
HTML("Differential Analysis 2"),
h4("Run differential expression analysis in multiple groups"),
"In this tab, you can identify top N marker genes from multiple groups by differential analysis, then visualize them in dotplot or heatmap",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        selectInput("sc1f2grp", h4("Select groups"),
                    choices = sc1conf[grp == TRUE]$UI,
                    selected = sc1def$grp1) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Cell information to group cells by",
                    content = c("Select categorical cell information to group cells by",
                                "- Single cells are grouped by this categorical covariate",
                                "- Plotted as the X-axis of the bubbleplot / heatmap")),
        radioButtons("sc1f2mode", NULL,
                        choices = c("FindAllMarkers","FindMarkers"),
                        selected = "FindAllMarkers"),
        shinyjs::useShinyjs(),
        shinyjs::hidden(
                div(id = "sc1f2tog8_open",
                    uiOutput("sc1f2ident2.ui") %>%
                            helper(type = "inline", size = "m", fade = TRUE,
                                    icon = "circle-question",
                                    title = "Select control group",
                                    content = c("Select control group.
                                    Equivalent to 'ident.2' in Seura::FindMarkers,
                                    'reference' in scanpy.tl.rank_genes_groups."))

                )
        ),
        selectizeInput("sc1f2inpgrp", choices=NULL,multiple=TRUE,label = "Specify cluster order (optional)"),
        selectizeInput("sc1f2plt", "Plot type:",
                        choices = c("Rank_genes","Dotplot","MatrixPlot","Stacked Violin", "Heatmap","TracksPlot","Correlation Matrix"),
                        selected = "Dotplot", multiple=FALSE),
        actionButton(inputId = "sc1f2update", "Run differential analysis",icon("sync",verify_fa = FALSE)),
        checkboxInput("sc1f2zscore", "Scale gene expression", value = TRUE),
        checkboxInput("sc1f2dendrogram", "Cluster columns (groups)", value = TRUE),
        #checkboxInput("sc1f2col", "Cluster genes", value = FALSE),
        actionButton("sc1f2tog2", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1f2tog2 % 2 == 1",
            selectInput("sc1f2sub1_1", "Cell information to subset:",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1),
            uiOutput("sc1f2sub1.ui"),
            #---------------------
            actionButton("sc1f2tog3", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1f2tog3 % 2 == 1",
                selectInput("sc1f2sub2_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                uiOutput("sc1f2sub2.ui"),
                #----------------------
                actionButton("sc1f2tog4", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1f2tog4 % 2 == 1",
                    selectInput("sc1f2sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1f2sub3.ui")
                )
            )
        ),
        actionButton("sc1f2tog5", "Toggle DE setting"),
        conditionalPanel(
            condition = "input.sc1f2tog5 % 2 == 1",
            numericInput("sc1f2top", "Select top N DEGs:",
                            min = -40,max = 40, value = 4,step = 0.5),
            selectInput("sc1f2DEmethod", "Select a DE method",
                        choices=c("wilcoxon","logreg",  "t-test_overestim_var","t-test"),
                        selected= "wilcoxon"),
            selectizeInput("sc1f2valToPlot",label= "Values to Plot",
                            choices=c("mean expression","scores", "logfoldchanges", "pvals", "pvals_adj", "log10_pvals", "log10_pvals_adj"),
                            selected = "mean expression",
                            multiple=FALSE),
            checkboxGroupInput("sc1f2rmgene", "Remove unwanted genes:", inline = TRUE,
                                choices = c("Mitochondrial (MT) genes",
                                            "Ribosomal protein large subunit (RPL)",
                                            "Ribosomal protein small subunit (RPS)"),
                                selected = NULL),
            radioButtons("sc1f2cutp", "filter genes <",
                            choices = c("p_val_adj","p_val"),
                            selected = "p_val_adj", inline = TRUE),
            numericInput("sc1f2cutpval", NULL, min = 0,max = 1, value = 0.05),
            numericInput("sc1f2cutfc", "filter genes with abs(log2FC) >=", min = -10,max = 10, value = 0.25,step= 0.5),
            numericInput("sc1f2cutpct", "filter genes with ptc % >", min = 0,max = 100, value = 10)
        ),
        actionButton("sc1f2tog6", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1f2tog6 % 2 == 1",
            selectInput("sc1f2cols", "Select a color specturm:",
                        choices=matplotlib_colors,
                        selected="viridis"),
            checkboxInput("sc1f2colinv", "Reverse color", value = FALSE),
            sliderInput("sc1f2vrange", "color scale limit:",
                        min = -3, max = 3, value = c(-3,3), step = 0.5),
            radioButtons("sc1f2psz", "Plot size:",
                            choices = c("Small", "Medium", "Large", "Extra Large"),
                            selected = "Medium", inline = TRUE),
            radioButtons("sc1f2fsz", "Font size:",
                            choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                            selected = "Small", inline = TRUE),
            checkboxInput("sc1f2flpxy", "Flip x and y axis", value = FALSE),
            radioButtons("sc1f2frt", "Rotate x group label:",
                            choices = c(0,30,45,90),
                            selected = 90, inline = TRUE)
        )

    ),
    column(9,
            downloadButton("sc1f2oup.png", "Download png"),
            actionButton("sc1f2tog7", "Toggle to show DE results"),
            conditionalPanel(
                condition = "input.sc1f2tog7 % 2 == 1",
                h4("Toggle to show differential analysis results"),
                withSpinner(dataTableOutput("sc1f2.dt"))
            ),
            downloadButton("sc1f2oup1.csv", "Download csv"),
            downloadButton("sc1f2oup1.xlsx", "Download xlsx"), br(),
            withSpinner(imageOutput("sc1f2oup.ui"))
    )# End of column (9 space)

)        # End of fluidRow (4 space)
),            # End of tab (2 space)

### Tab.g1: Gene Set Enrichment Analysis
tabPanel(
HTML("Gene Set Enrichment Analysis"),
h4("Run Gene Set Enrichment Analysis in multiple groups"),
"In this tab, you can identify top N enrichmented gene sets from multiple groups by Enrichr.
To save time, system will NOT re-run the differential analysis, if you keep the same setting as Differential Analysis 2",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        selectInput("sc1g1grp", h4("Select groups"),
                    choices = sc1conf[grp == TRUE]$UI,
                    selected = sc1def$grp1) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Cell information to group cells by",
                    content = c("Select categorical cell information to group cells by",
                                "- Single cells are grouped by this categorical covariate",
                                "- Plotted as the X-axis of the bubbleplot / heatmap")),
        radioButtons("sc1g1mode", NULL,
                        choices = c("FindAllMarkers","FindMarkers"),
                        selected = "FindAllMarkers"),
        shinyjs::useShinyjs(),
        shinyjs::hidden(
                div(id = "sc1g1tog8_open",
                    uiOutput("sc1g1ident2.ui") %>%
                            helper(type = "inline", size = "m", fade = TRUE,
                                    icon = "circle-question",
                                    title = "Select control group",
                                    content = c("Select control group.
                                    Equivalent to 'ident.2' in Seura::FindMarkers,
                                    'reference' in scanpy.tl.rank_genes_groups."))

                )
        ),
        selectizeInput("sc1g1inpgrp", choices=NULL,multiple=TRUE,label = "Specify group order (Dotplot); Choose one group (Barplot)"),
        selectizeInput("sc1g1dbs", "Databases:",
                        choices = libraryName,
                        selected = "MSigDB_Hallmark_2020", multiple=TRUE),
        # Input: Select a file ----
        fileInput("sc1g1inpgsfile", "Upload curated Databases",
                    multiple = TRUE,
                    accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
        selectizeInput("sc1g1plt", "Plot type:",
                        choices = c("Dotplot","BarPlot"),
                        selected = "Dotplot", multiple=FALSE),
        actionButton(inputId = "sc1g1update", "Run Gene Set Enrichment Analysis",icon("sync",verify_fa = FALSE)),
        #checkboxInput("sc1g1zscore", "Scale gene expression", value = TRUE),
        checkboxInput("sc1g1colv", "Cluster columns (groups)", value = FALSE),
        checkboxInput("sc1g1rowv", "Cluster rows (pathways)", value = FALSE),
        uiOutput("sc1g1orderRow.ui"),
        actionButton("sc1g1tog2", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1g1tog2 % 2 == 1",
            selectInput("sc1g1sub1_1", "Cell information to subset:",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1),
            uiOutput("sc1g1sub1.ui"),
            #---------------------
            actionButton("sc1g1tog3", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1g1tog3 % 2 == 1",
                selectInput("sc1g1sub2_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                uiOutput("sc1g1sub2.ui"),
                #----------------------
                actionButton("sc1g1tog4", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1g1tog4 % 2 == 1",
                    selectInput("sc1g1sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1g1sub3.ui")
                )
            )
        ),
        actionButton("sc1g1tog5", "Toggle DE setting"),
        conditionalPanel(
            condition = "input.sc1g1tog5 % 2 == 1",
            selectInput("sc1g1DEmethod", "Select a DE method",
                        choices=c("wilcoxon","logreg",  "t-test_overestim_var","t-test"),
                        selected= "wilcoxon"),
            selectizeInput("sc1g1valToPlot",label= "order genes by",
                            choices=c("pval","padj"),
                            selected = "padj",
                            multiple=FALSE),
            radioButtons("sc1g1cutp", "filter genes <:",
                            choices = c("p_val_adj","p_val"),
                            selected = "p_val", inline = TRUE),
            numericInput("sc1g1cutpval", NULL, min = 0,max = 1, value = 1),
            numericInput("sc1g1cutfc", "filter genes with log2FC >=", min = -10,max = 10, value = 0.05),
            numericInput("sc1g1cutpct", "filter genes with ptc % >", min = 0,max = 100, value = 10),
            numericInput("sc1g1gscutpadj", "filter gene sets with p_val_adj <", min = 0,max = 1, value = 0.25),
            numericInput("sc1g1gscutpval", "filter gene sets with p_val <", min = 0,max = 1, value = 0.05)

        ),
        actionButton("sc1g1tog6", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1g1tog6 % 2 == 1",
            selectInput("sc1g1cols", "Select a color specturm:",
                        choices=c(rownames(pal.info)[pal.info$category %in% "div"]),
                        selected="Blue-White-Red"),
            checkboxInput("sc1g1colinv", "Reverse color", value = FALSE),
            #checkboxInput("sc1g1fullrange", "full range color", value = FALSE),
            radioButtons("sc1g1valToPlot", "dot size by:",
                            choices = c("pval", "padj"),
                            selected = "padj", inline = TRUE),
            checkboxInput("sc1g1circle", "Dot circle", value = TRUE),
            sliderInput("sc1g1psiz", "Dot size:",
                        min = 4, max = 8, value = 3, step = 0.5),
            radioButtons("sc1g1psz", "Plot size:",
                            choices = c("Small", "Medium", "Large", "Extra Large"),
                            selected = "Medium", inline = TRUE),
            radioButtons("sc1g1fsz", "Font size:",
                            choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                            selected = "Small", inline = TRUE),
            checkboxInput("sc1g1flpxy", "Flip x and y axis", value = FALSE),
            radioButtons("sc1g1frt", "Rotate x-axis label:",
                            choices = c(0,30,45,90),
                            selected = 0, inline = TRUE)
        )

    ),
    column(9,
            tags$head(
                HTML(
                    "
                        <script>
                        var socket_timeout_interval
                        var n = 0
                        $(document).on('shiny:connected', function(event) {
                        socket_timeout_interval = setInterval(function(){
                        Shiny.onInputChange('count', n++)
                        }, 15000)
                        });
                        $(document).on('shiny:disconnected', function(event) {
                        clearInterval(socket_timeout_interval)
                        });
                        </script>
                    "
                )
            ),
            actionButton("sc1g1tog8", "Toggle to show uploaded genesets"),
            conditionalPanel(
                condition = "input.sc1g1tog8 % 2 == 1",
                radioButtons("sc1g1inpgs.dt.disp", NULL,
                            choices = c(Head = "head",
                                        All = "all"),inline = TRUE,
                            selected = "head"),
                withSpinner(dataTableOutput("sc1g1inpgs.dt"))
                ),
            withSpinner(uiOutput("sc1g1oup1.ui")),
            downloadButton("sc1g1oup1.png", "Download png"),
            downloadButton("sc1g1oup1.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                numericInput("sc1g1oup1.h", "png / jpeg height:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)),
            div(style="display:inline-block",
                numericInput("sc1g1oup1.w", "png / jpeg width:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)), br(),
            actionButton("sc1g1tog9", "Toggle to show GSEA results"),
            conditionalPanel(
                condition = "input.sc1g1tog9 % 2 == 1",
                h4("Toggle to show GSEA results"),
                withSpinner(dataTableOutput("sc1g1.dt"))
            ),
            downloadButton("sc1g1oup1.csv", "Download csv"),
            downloadButton("sc1g1oup1.xlsx", "Download xlsx")
    )    # End of column (9 space)

)        # End of fluidRow (4 space)
),            # End of tab (2 space)


## Tab1.cor1: Correlation network
tabPanel(
HTML("Correlation Network"),
h4("Run correlation analysis and show results in a volcano plot or network"),
"In this tab, you can visualise the gene-gene Correlation",
"and network relation based correlation.",
br(),br(),
fluidRow(
    column(
        3,style="border-right: 2px solid black",
        h4("Subset groups"),
        actionButton("sc1n1tog1", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1n1tog1 % 2 == 1",
            selectInput("sc1n1sub1_1", "Cell information to subset:",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1),
            uiOutput("sc1n1sub1.ui"),
            #-----------------------
            actionButton("sc1n1tog2", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1n1tog2 % 2 == 1",
                selectInput("sc1n1sub2_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                uiOutput("sc1n1sub2.ui"),
                #-----------------------
                actionButton("sc1n1tog3", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1n1tog3 % 2 == 1",
                    selectInput("sc1n1sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1n1sub3.ui")
                )
            )
        ),
        sliderInput("sc1n1minexpr", "Select genes expressed in at least % of the cells",min = 0, max = 100, value = 10, step = 1),
        actionButton(inputId = "sc1n1update1", "Run correlation",icon("sync",verify_fa = FALSE)),
        actionButton(inputId = "sc1n1update2", "Load correlation data",icon("add",verify_fa = FALSE)),
        h4("Correlation volcano plot"),
        selectInput("sc1n1inp1", "Gene name:", choices=NULL,
                    selected = sc1def$gene2,
                    multiple=FALSE),
        actionButton(inputId = "sc1n1update3", "Generate volcano plot",icon("sync",verify_fa = FALSE)),
        actionButton("sc1n1tog4", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1n1tog4 % 2 == 1",
            fluidRow(
                column(
                    6, sliderInput("sc1n1siz1", "Point size:",
                                    min = 0, max = 8, value = 4, step = 0.25),
                    sliderInput("sc1n1alpha1", "Point transparency:",
                                min = 0, max = 1, value = 0.8, step = 0.05),
                    radioButtons("sc1n1psz1", "Plot size:",
                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                    selected = "Large", inline = TRUE),
                    radioButtons("sc1n1fsz1", "Font size:",
                                    choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                    selected = "Medium", inline = TRUE),
                    radioButtons("sc1n1asp1", "Aspect ratio:",
                                    choices = c("Square", "Fixed", "Free"),
                                    selected = "Square", inline = TRUE)
                    #checkboxInput("sc1n1leg", "Show Legend", value = FALSE),
                    #radioButtons("sc1n1legpos", "Legend positions:",
                    #             choices = c("top", "right", "bottom"),
                    #             selected = "bottom", inline = TRUE),
                    #checkboxGroupInput("sc1n1rmgene", "Remove unwanted genes:", inline = TRUE,
                    #                   choices = c("Mitochondrial (MT) genes",
                    #                               "Ribosomal protein large subunit (RPL)",
                    #                               "Ribosomal protein small subunit (RPS)"),
                    #                   selected = "Mitochondrial (MT) genes")
                ), # end of column 6
                column(
                    6,
                    radioButtons("sc1n1cutp1", "Select p-value or adjusted p-value:",
                                    choices = c("p_val_adj","p_val"),
                                    selected = "p_val_adj", inline = TRUE),
                    textInput("sc1n1cutpval1", "Select p-value cut off:", value = "0.05"),
                    textInput("sc1n1cutfc1", "Select correlation cut off:", value = "0.05"),
                    textInput("sc1n1top1", "Select top N correlated genes:", value = "10"),
                    #selectInput("sc1n1cols", "Select a color spectrum",
                    #            choices=c(rownames(pal.info)[pal.info$category %in% c("div")]),
                    #            selected= "Blue-White-Red"),
                    selectInput("sc1n1lab1", "Labels for top correlated genes:",
                                choices=c("black",cList[["Blue-Yellow-Red"]]),
                                selected= "black"),
                    selectInput("sc1n1lab2", "Labels for manually input genes:",
                                choices=c(cList[["Blue-Yellow-Red"]],"black"),
                                selected = "red")
                ) # end of column 6
            )
        ), # End of column (12 space)
        actionButton("sc1n1tog5", "Toggle to add genes manually"),
        conditionalPanel(
            condition = "input.sc1n1tog5 % 2 == 1",
            textAreaInput("sc1n1inp", HTML("List of gene names (Y-axis)<br />
                                            Separated by , or ; or newline"),
                            height = "50px",
                            value = paste0("", collapse = ", "))
            ), # End of conditionalPanel
        h4("Correlation network"),
        selectInput("sc1n1inp2", "Gene names:", choices=NULL,selected = sc1def$gene2,
                    multiple=TRUE),
        actionButton(inputId = "sc1n1update4", "Generate Network",icon("sync",verify_fa = FALSE)),
        actionButton("sc1n1tog6", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1n1tog6 % 2 == 1",
            fluidRow(
                column(
                    6, sliderInput("sc1n1linklen", "link Distance:",
                                    min = 50, max = 250, value = 120, step = 10),
                    sliderInput("sc1n1linkwid", "link Width:",
                                min = 0, max = 4, value = 2, step = 0.25),
                    radioButtons("sc1n1psz2", "Plot size:",
                                    choices = c("Small", "Medium", "Large", "Extra Large"),
                                    selected = "Large", inline = TRUE),
                    radioButtons("sc1n1fsz2", "Font size:",
                                    choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                                    selected = "Medium", inline = TRUE),
                    checkboxInput("sc1n1name", "Show gene name", value = TRUE),
                    checkboxInput("sc1n1leg", "Show Legend", value = FALSE)
                    #radioButtons("sc1n1legpos", "Legend positions:",
                    #             choices = c("top", "right", "bottom"),
                    #             selected = "bottom", inline = TRUE),
                    #checkboxGroupInput("sc1n1rmgene", "Remove unwanted genes:", inline = TRUE,
                    #                   choices = c("Mitochondrial (MT) genes",
                    #                               "Ribosomal protein large subunit (RPL)",
                    #                               "Ribosomal protein small subunit (RPS)"),
                    #                   selected = "Mitochondrial (MT) genes")
                ), # end of column 6
                column(
                    6,
                    radioButtons("sc1n1cutp2", "Select p-value or adjusted p-value:",
                                    choices = c("p_val_adj","p_val"),
                                    selected = "p_val_adj", inline = TRUE),
                    textInput("sc1n1cutpval2", "Select p-value cut off:", value = "0.05"),
                    textInput("sc1n1cutfc2", "Select correlation cut off:", value = "0.05"),
                    textInput("sc1n1top2", "Select top N correlated genes:", value = "10"),
                    radioButtons("sc1n1posneg", "Select positive and/or negative correlated genes only:",
                                    choices = c("positive only","negative only","both"),
                                    selected = "positive only", inline = TRUE),
                    selectInput("sc1n1col2","Select a color palette",
                                choices=c("default",rownames(pal.info)[pal.info$category %in% c("div")]),
                                selected="Blue-White-Red"),
                    checkboxInput("sc1n1col2inv", "Reverse color", value = FALSE)

                ) # end of column 6
            )
        ) # End of column (12 space)
    ), # End of column (12 space)
column(9,
        uiOutput("sc1n1oup1.ui"),
        downloadButton("sc1n1oup1.png", "Download png"),
        downloadButton("sc1n1oup1.jpeg", "Download jpeg"), br(),
        div(style="display:inline-block",
            numericInput("sc1n1oup1.h", "png / jpeg height:", width = "138px",
                        min = 4, max = 50, value = 13, step = 0.5)),
        div(style="display:inline-block",
            numericInput("sc1n1oup1.w", "png / jpeg width:", width = "138px",
                        min = 4, max = 50, value = 13, step = 0.5)), br(),


        withSpinner(forceNetworkOutput("sc1n1oup2.ui",height = "500px")),
        downloadButton("sc1n1oup2.network", "Download html")
        #downloadButton("sc1n1oup2.csv", "Download csv"),
        #downloadButton("sc1n1oup2.xlsx", "Download xlsx"), br(),
)         # End of tab (2 space)
)     # End of fluidRow (1 column)
),            # End of tab (2 space)
### Tab1.t1: TCR plot
tabPanel(
HTML("TCR plot"),
h4("T-cell receptor (TCR) and immunoglobulin (Ig) enrichment information"),
"In this tab, users can visualise the single-cell immune receptor profiling",
br(),br(),
fluidRow(
    column(
        3, style="border-right: 2px solid black",
        selectInput("sc1t1inp1", "Cell information to plot (X-axis):",
                    choices = sc1conf[grp == TRUE]$UI,
                    selected = sc1def$grp2),
        selectInput("sc1t1inp2", "Cell information to group:",
                    choices = sc1conf[grp == TRUE]$UI,
                    selected = sc1def$grp1) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                    icon = "circle-question",
                    title = "Cell information to group / colour cells by",
                    content = c("Select categorical cell information to group / colour cells by",
                                "- Proportion / cell numbers are shown in different colours")),
        radioButtons("sc1t1typ", "Plot Type:",
                        choices = c("Unique Barplot","Cumulative clone sizes distribution",
                                    "Proportion Barplot","Paired Diversity","unpaired Diversity",
                                    "Paired scatter Clonotype","unpaired scatter Clonotype",
                                    "Novel & Expand & Contract Barplot_1",
                                    "Novel & Expand & Contract Barplot_2"),
                        selected = c("Unique Barplot"), inline = FALSE),
        checkboxInput("sc1t1norm", "Y-axis shows %", value = FALSE),
        radioButtons("sc1t1cloneCall", "Definition of clonotype:",
                        choices = c("TCR/Ig genes", "CDR3 nucleotide","CDR3 amino acid",
                                    "TCR/Ig + CDR3 nucleotide"),
                        selected = "TCR/Ig + CDR3 nucleotide", inline = TRUE),
        actionButton("sc1t1tog1", "Toggle to subset cells"),
        conditionalPanel(
            condition = "input.sc1t1tog1 % 2 == 1",
            selectInput("sc1t1sub1_1", "Cell information to subset:",
                        choices = sc1conf[grp == TRUE]$UI,
                        selected = sc1def$grp1),
            uiOutput("sc1t1sub1.ui"),
            actionButton("sc1t1tog2", "Toggle to further subset cells"),
            conditionalPanel(
                condition = "input.sc1t1tog2 % 2 == 1",
                selectInput("sc1t1sub2_1", "Cell information to subset:",
                            choices = sc1conf[grp == TRUE]$UI,
                            selected = sc1def$grp2),
                uiOutput("sc1t1sub2.ui"),
                actionButton("sc1t1tog3", "Toggle to further subset cells"),
                conditionalPanel(
                    condition = "input.sc1t1tog3 % 2 == 1",
                    selectInput("sc1t1sub3_1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp2),
                    uiOutput("sc1t1sub3.ui")
                )
            )
        ),
        br(),
        actionButton("sc1t1tog4", "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.sc1t1tog4 % 2 == 1",
            sliderInput("sc1t1lvls", "Significant level for Clone change:",
                        min = 0, max = 0.1, value = 0.03, step = 0.001),
            sliderInput("sc1t1siz", "Data point size",
                        min = 0, max = 8, value = 2, step = 0.25),
            selectInput("sc1t1cols","Select a color palette:",
                        choices=c("default",rownames(pal.info)[pal.info$category %in% "qual"]),
                        selected="default"),
            selectInput("sc1t1divindex", "Diversity index",
                        choices = c("Shannon","Simpson","Inv.Simpson","Chao","ACE"),
                        selected = "Shannon"),
            radioButtons("sc1t1psz", "Plot size:",
                            choices = c("Small", "Medium", "Large"),
                            selected = "Medium", inline = TRUE),
            checkboxInput("sc1t1pts", "Show data points", value = TRUE),
            radioButtons("sc1t1fsz", "Font size:",
                            choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                            selected = "Medium", inline = TRUE),
            radioButtons("sc1t1lsz", "Line size:",
                            choices = c("Extra Small","Small", "Medium", "Large", "Extra Large"),
                            selected = "Medium", inline = TRUE),
            checkboxInput("sc1t1flp", "Flip X/Y", value = FALSE),
            radioButtons("sc1t1frt", "Rotate x axis label:",
                            choices = c(0,30,45,90),
                            selected = 0, inline = TRUE),
            checkboxInput("sc1t1leg", "Show Legend", value = TRUE),
            radioButtons("sc1t1legpos", "Legend positions:",
                            choices = c("top", "right", "bottom"),
                            selected = "bottom", inline = TRUE)
        )

    ), # End of column (6 space)
    column(9, withSpinner(uiOutput("sc1t1oup.ui")),
            downloadButton("sc1t1oup.png", "Download png"),
            downloadButton("sc1t1oup.jpeg", "Download jpeg"), br(),
            div(style="display:inline-block",
                numericInput("sc1t1oup.h", "png / jpeg height:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5)),
            div(style="display:inline-block",
                numericInput("sc1t1oup.w", "png / jpeg width:", width = "138px",
                            min = 4, max = 50, value = 10, step = 0.5))
    )    # End of column (6 space)
)        # End of fluidRow (4 space)
),         # End of tab (2 space)
tabsetPanel(
p(em("This webpage was made using "), a("pyShinyCell",
                                        href = "https://github.com/nyuhuyang/pyShinyCell",target="_blank"))
)
)))



