# ui.R

#library(markdown)


navbarPage(title       = div("airGRteaching",
                             a(href = "https://hydrogr.github.io/airGRteaching/",
                               title = "airGRteaching",
                               target = "_blank", rel = "noopener noreferrer",
                               img(src = "fig/logo_airGRteaching_CMJN_square_0125x0121.png", height = 350 / 9)),
                             a(href = "https://webgr.inrae.fr/en/home/",
                              title = "webgr.inrae.fr",
                              target = "_blank", rel = "noopener noreferrer",
                              img(src = "fig/logo_inrae_hydro_CMJN_square.svg", height = 350 / 9)),
                             a(href = "https://www.inrae.fr/en/",
                               title = "inrae.fr",
                               target = "_blank", rel = "noopener noreferrer",
                               img(src = "fig/logo_inrae_CMJN_square.svg", height = 350 / 9)),
                             style = "position:relative; top:-9px;"),

           windowTitle = "airGRteaching",

           theme       = switch(.GlobalEnv$.ShinyGR.args$theme,
                                Rstudio   = "",
                                Cyborg    = "css/bootstrap.min_Cyborg.css",
                                Cerulean  = "css/bootstrap.min_Cerulean.css",
                                Flatly    = "css/bootstrap.min_Flatly.css",
                                United    = "css/bootstrap.min_United.css",
                                Yeti      = "css/bootstrap.min_Yeti.css",
                                Inrae     = "css/bootstrap.min_Inrae.css",
                                Saclay    = "css/bootstrap.min_Saclay.css"
           ),


           tabPanel(title = "Modelling",
                    icon  = icon("chart-area"),
                    shinyjs::useShinyjs(), # set up shinyjs


                    sidebarLayout(position = "left",


                                  sidebarPanel(width = 3,

                                               h4("Choose a dataset:"),
                                               fluidRow(
                                                 column(width = 12, selectInput("Dataset", label = NULL, choices = .ShinyGR.args$NamesObsBV))
                                               ),

                                               h4("Choose a model:"),
                                               fluidRow(
                                                 column(width = 6, selectInput("HydroModel", label = "Hydrological model",
                                                                               choices = c("GR4J", "GR5J", "GR6J", "GR2M"))),
                                                 column(width = 6, selectInput("SnowModel", label = "Snow model",
                                                                               choices = c("None", "CemaNeige")))
                                               ),


                                               h4("Parameters values:"),
                                               sliderInput("X1", label = "X1  (production store capacity)",
                                                           post = "  [mm]",
                                                           min = 0,
                                                           max = 2500,
                                                           step = 10,
                                                           value = 1250,
                                                           sep = " "),
                                               conditionalPanel(condition = "input.HydroModel == 'GR2M'",
                                                                sliderInput("X2GR2M", label = "X2  (groundwater exchange coeff.)",
                                                                            post = "  [-]",
                                                                            min = +0.05,
                                                                            max = +4,
                                                                            step = 0.05,
                                                                            value = 2,
                                                                            sep = " ")),
                                               conditionalPanel(condition = "input.HydroModel != 'GR2M'",
                                                                sliderInput("X2", label = "X2  (intercatchment exchange coeff.)",
                                                                            post = "  [mm/d]",
                                                                            min = -5,
                                                                            max = +5,
                                                                            step = 0.05,
                                                                            value = 0,
                                                                            sep = " "),
                                                                sliderInput("X3", label = "X3  (routing store capacity)",
                                                                            post = "  [mm]",
                                                                            min = 0,
                                                                            max = 1000,
                                                                            step = 5,
                                                                            value = 500,
                                                                            sep = " "),
                                                                sliderInput("X4",  label = "X4  (unit hydrograph time constant)",
                                                                            post = "  [d]",
                                                                            min = 0.5,
                                                                            max = 10,
                                                                            step = 0.1,
                                                                            value = 5.2,
                                                                            sep = " ")),
                                               conditionalPanel(condition = "input.HydroModel == 'GR5J' || input.HydroModel =='GR6J'",
                                                                sliderInput("X5", label = "X5  (intercatchment exchange threshold)",
                                                                            post = "  [-]",
                                                                            min = -4,
                                                                            max = +4,
                                                                            step = 0.05,
                                                                            value = 0,
                                                                            sep = " ")),
                                               conditionalPanel(condition = "input.HydroModel == 'GR6J'",
                                                                sliderInput("X6", label = "X6  (exponential store depletion coeff.)",
                                                                            post = "  [mm]",
                                                                            min = 0,
                                                                            max = 20,
                                                                            step = 0.5,
                                                                            value = 10,
                                                                            sep = " ")),
                                               conditionalPanel(condition = "input.SnowModel == 'CemaNeige'",
                                                                sliderInput("C1", label = "C1  (weighting coeff. for snow pack thermal state)",
                                                                            post = "  [-]",
                                                                            min = 0,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = 0.5,
                                                                            sep = " "),
                                                                sliderInput("C2", label = "C2 (degree-day melt coefficient)",
                                                                            post = "  [mm/°C/d]",
                                                                            min = 0,
                                                                            max = 10,
                                                                            step = 0.5,
                                                                            value = 5,
                                                                            sep = " ")),

                                               h4("Automatic calibration:"),
                                               fluidRow(
                                                 column(width = 6, selectInput("TypeCrit", label = "Objective function",
                                                                               choices = c("NSE [Q]", "NSE [sqrt(Q)]", "NSE [1/Q]",
                                                                                           "KGE [Q]", "KGE [sqrt(Q)]", "KGE [1/Q]"))),
                                                 column(width = 6, actionButton("CalButton", label = "Run", width = "100%",
                                                                                icon = icon("arrows-rotate"),
                                                                                style = ifelse(.GlobalEnv$.ShinyGR.args$theme != "Cerulean",
                                                                                               "color:#ffffff; background-color:#A4C400; border-color:#A4C400; margin-top:25px; padding:6px;",
                                                                                               "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; margin-top:25px; padding:6px;")))
                                               )
                                  ),


                                  mainPanel(width = 9,

                                            fluidRow(
                                              column(width = 2,
                                                     selectInput("PlotType", label = "Choose a plot:",
                                                                 choices = c("Flow time series", "Model performance", "State variables", "Model diagram"))
                                              ),
                                              column(width = 4, offset = 1,
                                                     sliderInput("Period", label = "Select the time window:",
                                                                 min = as.POSIXct(.ShinyGR.args$SimPer[[1]][1L], tz = "UTC"),
                                                                 max = as.POSIXct(.ShinyGR.args$SimPer[[1]][2L], tz = "UTC"),
                                                                 value = as.POSIXct(.ShinyGR.args$SimPer[[1]], tz = "UTC"),
                                                                 timeFormat = "%F",
                                                                 timezone = "+0000",
                                                                 animate = FALSE)
                                              ),
                                              conditionalPanel(condition = "input.PlotType == 'Model diagram'",
                                                               column(width = 4, offset = 0,
                                                                      conditionalPanel(condition = "input.HydroModel != 'GR2M'",
                                                                                       sliderInput("Event", label = "Select the target date:",
                                                                                                   min = as.POSIXct(.ShinyGR.args$SimPer[[1]][1L], tz = "UTC"),
                                                                                                   max = as.POSIXct(.ShinyGR.args$SimPer[[1]][2L], tz = "UTC"),
                                                                                                   value = as.POSIXct(.ShinyGR.args$SimPer[[1]][1L], tz = "UTC"),
                                                                                                   timeFormat = "%F",
                                                                                                   timezone = "+0000",
                                                                                                   animate = animationOptions(interval = 500),
                                                                                                   step = 3600 * 24)),#step = 3600 * 24
                                                                      conditionalPanel(condition = "input.HydroModel == 'GR2M'",
                                                                                       sliderInput("EventGR2M", label = "Select the target date:",
                                                                                                   min = as.POSIXct(.ShinyGR.args$SimPer[[1]][1L], tz = "UTC"),
                                                                                                   max = as.POSIXct(.ShinyGR.args$SimPer[[1]][2L], tz = "UTC"),
                                                                                                   value = as.POSIXct(.ShinyGR.args$SimPer[[1]][1L], tz = "UTC"),
                                                                                                   timeFormat = "%Y-%m",
                                                                                                   timezone = "+0000",
                                                                                                   animate = animationOptions(interval = 500),
                                                                                                   step = 3600*24*30))
                                                               )

                                              )

                                            ),

                                            fluidRow(conditionalPanel(condition = "input.PlotType == 'Model performance'",
                                                                      column(width = 09,
                                                                             plotOutput("stPlotMP", width = "100%", height = "900px"))),
                                                     conditionalPanel(condition = "input.PlotType == 'Flow time series'",
                                                                      column(width = 09,
                                                                             dygraphs::dygraphOutput("dyPlotTSq", width = "100%", height = "400px"),
                                                                             dygraphs::dygraphOutput("dyPlotTSe", width = "100%", height = "300px"))),
                                                     conditionalPanel(condition = "input.PlotType == 'State variables'",
                                                                      column(width = 09,
                                                                             dygraphs::dygraphOutput("dyPlotSVs", width = "100%", height = "325px"),
                                                                             dygraphs::dygraphOutput("dyPlotSVq", width = "100%", height = "355px"))),
                                                     conditionalPanel(condition = "input.PlotType == 'Model diagram'",
                                                                      column(width = 05,
                                                                             dygraphs::dygraphOutput("dyPlotMDp", width = "100%", height = "190px"),
                                                                             dygraphs::dygraphOutput("dyPlotMDe", width = "100%", height = "215px"),
                                                                             dygraphs::dygraphOutput("dyPlotMDq", width = "100%", height = "235px")),
                                                                      column(width = 04,
                                                                             plotOutput("stPlotMD", width = "100%", height = "665px"))),
                                                     column(width = 03,
                                                            div(tableOutput("Criteria")), style = "font-size:90%",
                                                            #conditionalPanel(condition = "input.PlotType == 'Flow time series' || input.PlotType == 'Model diagram'",
                                                                             radioButtons("ShowOldQsim", label = "Show previous simulation (Qold)",
                                                                                          choices = c("No", "Yes"), inline = TRUE),#),
                                                            downloadButton("DownloadTab" , label = "Download sim. as csv",
                                                                           style = "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; width:170px; height:25px; font-size:95%; padding-top:2px; margin-top:20px;"),
                                                            #conditionalPanel(condition = "input.PlotType == 'Model performance' || input.PlotType == 'Flow time series' || input.PlotType == 'State variables'",
                                                            br(),
                                                            downloadButton("DownloadPlot", label = "Download plot as png",
                                                                           style = "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; width:170px; height:25px; font-size:95%; padding-top:2px; margin-top:10px;")
                                                            #)

                                                     )
                                            )
                                  )
                    )
           ),
           tabPanel(title = "Summary sheet",
                    icon  = icon("table-cells"),
                    sidebarLayout(position = "left",
                                  sidebarPanel(width = 3,
                                               h4("Choose a dataset:"),
                                               fluidRow(column(width = 12, selectInput("DatasetSheet", label = NULL, choices = .ShinyGR.args$NamesObsBV)),
                                                        style = "height:720px;")),
                                  mainPanel(width = 9,
                                            fluidRow(uiOutput("Sheet")))
                    )
           ),
           navbarMenu(title = "Help",
                      icon  = icon("circle-question"),
                      tabPanel(title = "Get started",
                               icon = icon("cog", lib = "glyphicon"),
                               fluidRow(column(width = 12, includeMarkdown("www/tab_fun.md")))
                      ),
                      #! horrible code to avoid fake item when a tabpanel contains an external hyperlink
                      #! (cf.https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/11)
                      tabPanel(title = HTML("GUI help
                                            </a></li><li><a href='https://CRAN.R-project.org/package=airGRteaching'   , target = '_blank', rel = 'noopener noreferrer'><i class='fab fa-r-project'></i>&nbsp;CRAN
                                            </a></li><li><a href='https://hydrogr.github.io/airGRteaching/'           , target = '_blank', rel = 'noopener noreferrer'><i class='fas fa-at'></i>&nbsp;Website
                                            </a></li><li><a href='https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/', target = '_blank', rel = 'noopener noreferrer'><i class='fab fa-gitlab'></i>&nbsp;GitLab"),
                               icon = icon("question"),
                               fluidRow(column(width = 12, a(href = "fig/airGRteaching_GUI_Description.png", target = "_blank", rel = "noopener noreferrer",
                                                    img(src = "fig/airGRteaching_GUI_Description.png", height = "770px",
                                                        alt = "If the image does not appear, click on this link.",
                                                        title = "Click to open in a new window")))),
                      ),
                      tabPanel(title = "About",
                               icon  = icon("bars"),
                               fluidRow(column(width = 6, includeMarkdown("www/tab_about.md")),
                                        column(width = 5, includeMarkdown("www/tab_authors.md"))),
                      )
           )
)
