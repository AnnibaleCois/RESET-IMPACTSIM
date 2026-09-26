################################################################################################################################################################################
# RESET ALCOHOL PROJECT                                                                                                                                                        #
# IMPACT SIMULATOR - UI                                                                                                                                                        # 
# v.1, 2026/09/24                                                                                                                                                              #
# annibale.cois@uct.ac.za                                                                                                                                                      #
################################################################################################################################################################################

ui <- page_sidebar(
  title = div(
    class = "d-flex align-items-center justify-content-between w-100",
          span(toupper("Avoidable burden of disease: scenario simulator"), style = "font-size: 130%;"),
          tags$img(src = "logo_combined.png", height = "40px"),
  
  span(
    style = "float: right;",
    actionButton("show_help",  label = NULL, icon = icon("circle-question"), class = "header-icon-btn"),
    actionButton("show_about", label = NULL, icon = icon("circle-info"), class = "header-icon-btn")
  )
  ),
  theme = theme_vs, 
  sidebar = sidebar(
    width = 480,
    accordion(
      id = "side_acc", multiple = TRUE, open = "scenario",
      accordion_panel(
        "Simulation settings", value = "settings", icon = icon("gear"),
        selectInput(
          inputId = "source",
          label = "Burden data",
          choices = c("NBD 3 - Preliminary" = "nbd", 
                      "GBD 2023" = "gbd"),
          selected = "nbd" 
        ),
        selectInput(
          inputId = "RECALIBRATE",
          label = "Recalibration",
          choices = c("None" = "no",
                      "Proportion of drinkers" = "curr", 
                      "Average daily consumption" = "cons"
                      ),
          selected = "no" 
        ),
        numericInput("R0",  "Recalibration proportion", min = 0.1, max = 1, step = 0.05, value = 0.80)
      ),
      accordion_panel(
        "Scenario", value = "scenario", icon = icon("sliders"),
        helpText(
          strong("curr"),": prevalence of drinkers", 
          br(),
          strong("bing"),": prevalence of binge drinking among drinkers",
          br(),
          strong("cons"),": average daily consumption among dribkers"),
          DTOutput("param_tbl"),
          fluidRow(
            column(6, numericInput("set_all", "Set column to value", 1, min = 0, max = MAXMULT, step = 0.05)),
            column(6, selectInput("set_col", "…for column", c("all", mult_cols)))
          ),
        div(
          actionButton("apply_all", "Apply", class = "btn-secondary btn-sm"),
          actionButton("reset", "Reset to 1", class = "btn-outline-secondary btn-sm"),
          downloadButton("dl_params", "Save params", class = "btn-outline-secondary btn-sm")
        ),
    fileInput("upload", "Load params (CSV)", accept = ".csv")
      )
    ),
    actionButton("run", "Run simulation", class = "btn-primary btn-lg w-100",icon = icon("play"))
  ),
  uiOutput("totals"),
  navset_card_tab(
    nav_panel(
      "Chart",
      icon = icon("chart-column"),
      layout_columns(
        col_widths = c(2,2,2,2,2,2),
        selectInput("variable", "Variable", plot_var, selected = "vbod"),
        selectInput("measure", "Measure", measures),
        selectInput("cat", "Group by", plot_cat, selected = "cause"),
        selectInput("group", "Break down by", plot_group),
        numericInput("topn", "Show top N causes", 20, min = 1, max = 40),
        checkboxInput("fill","100% Columns", FALSE)
      ),
      plotlyOutput("plot", height = "550px"),
      uiOutput("chart_footer"),
      div(class = "tab-footer", footercontent)
    ),
    nav_panel(
      "Table",
      icon = icon("table"),
      downloadButton("dl_res", "Download results"),
      DTOutput("res_tbl"),
      div(class = "tab-footer", footercontent)
    )
  )
)
