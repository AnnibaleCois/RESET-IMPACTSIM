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
  tags$head(tags$style(HTML("
    .shiny-notification .progress-bar {
      background-image: linear-gradient(45deg, rgba(255,255,255,.25) 25%, transparent 25%,
        transparent 50%, rgba(255,255,255,.25) 50%, rgba(255,255,255,.25) 75%, transparent 75%);
      background-size: 1rem 1rem;
      animation: progress-stripes 1s linear infinite;
    }
    @keyframes progress-stripes {
      from { background-position: 1rem 0; }
      to   { background-position: 0 0; }
    }
  "))
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
          "Multiplier for consumption variables within each demographic stratum",
          br(), br(),
          tags$ul(
          tags$li(strong("PREV"),": prevalence of drinkers"), 
          tags$li(strong("HED"),": prevalence of heavy episodic drinking among drinkers"),
          tags$li(strong("LR"),": average daily consumption among low risk drinkers"),
          tags$li(strong("MR"),": average daily consumption among moderate risk drinkers"),
          tags$li(strong("HR"),": average daily consumption among high risk drinkers"),
          tags$li(strong("VR"),": average daily consumption among very high risk drinkers"),
          ),
          "Double-click a cell to edit."),
          DTOutput("param_tbl"),        
          br(),
          fluidRow(
            column(6, numericInput("set_all", "Set column to value", 1, min = 0, max = MAXMULT, step = 0.05)),
            column(6, selectInput("set_col", "…for column", c("all", mult_cols)))
          ),

        div(
          actionButton("apply_all", "Apply", class = "btn-secondary btn-sm"),
          actionButton("reset", "Reset to 1", class = "btn-outline-secondary btn-sm"),
          downloadButton("dl_params", "Save params", class = "btn-outline-secondary btn-sm")
        ),
        br(),
    fileInput("upload", "Load params (CSV)", accept = ".csv")
      )
    ),
    actionButton("run", "Run simulation", class = "btn-primary btn-lg w-100",icon = icon("play"))
  ),
  uiOutput("totals"),
  navset_card_tab(
    nav_panel(
      "Burden - Chart",
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
      "Burden - Table",
      icon = icon("table"),
      downloadButton("dl_res", "Download results"),
      DTOutput("res_tbl"),
      div(class = "tab-footer", footercontent)
    ),
    nav_panel(
      "Assumed exposure", 
      icon = icon("wine-glass"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Differences from observed exposure"),
          uiOutput("cons_summary_cards")#,
          #uiOutput("chart_footer")
        ),
        card(
          card_header("Distribution of alcohol consumption among drinkers"),
          plotlyOutput("dist", height = "550px")
        )
      ),
      div(class = "tab-footer", footercontent)
    )
  )
)
