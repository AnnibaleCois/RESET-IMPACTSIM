################################################################################################################################################################################
# RESET ALCOHOL PROJECT                                                                                                                                                        #
# IMPACT SIMULATOR - SERVER                                                                                                                                                    # 
# v.1, 2026/09/24                                                                                                                                                              #
# annibale.cois@uct.ac.za                                                                                                                                                      #
################################################################################################################################################################################

server <- function(input, output, session) {

  params <- reactiveVal(template)
  
  # SCENARIO TABLE EDITING
 
  output$param_tbl <- renderDT({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Gender"),
          th(rowspan = 2, "Age"),
          th(rowspan = 2, "PREV"),
          th(rowspan = 2, "HED"),
          th(colspan = 4, "Consumption", style = "text-align: center;")
        ),
        tr(
          th("LR"), th("MR"), th("HR"), th("VR")
        )
      )
    ))
    
    datatable(
      params(), rownames = FALSE, selection = "none", container = sketch,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(dom = "t", pageLength = 12, ordering = FALSE)
    )
  })
  proxy <- dataTableProxy("param_tbl")
  
  observeEvent(input$param_tbl_cell_edit, {
    info <- input$param_tbl_cell_edit
    p <- params()
    val <- suppressWarnings(as.numeric(info$value))
    if (is.na(val) || val < 0 || val > MAXMULT) {
      showNotification(paste0("Multipliers must be numbers between 0 and ", MAXMULT), type = "warning")
    } else {
      p[info$row, info$col + 1] <- val    
    }
    params(p)
    replaceData(proxy, p, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$apply_all, {
    if (!isTRUE(is.finite(input$set_all) && input$set_all >= 0 && input$set_all <= 1)) {
      showNotification(paste0("Value must be numbers between 0 and ", MAXMULT), type = "warning")
      return()
    }
    p <- params()
    cols <- if (input$set_col == "all") mult_cols else input$set_col
    p[cols] <- input$set_all
    params(p); replaceData(proxy, p, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$reset, {
    params(template); replaceData(proxy, template, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$upload, {
    up <- tryCatch(read.csv(input$upload$datapath, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    ok <- !is.null(up) && all(c("gender", "agecat", mult_cols) %in% names(up)) &&
      nrow(up) == nrow(template)
    in_range <- ok && all(vapply(up[c(mult_cols)], function(x)
      is.numeric(x) && all(x >= 0 & x <= 1, na.rm = FALSE), logical(1)))
    if (ok && !isTRUE(in_range)) {
      showNotification("All multipliers in the CSV must be numbers between 0 and 1.", type = "error")
    } else if (!ok) {
      showNotification(sprintf("CSV needs columns gender, agecat, %s and %d rows.",
                               paste(c(mult_cols), collapse = ", "), nrow(template)),
                       type = "error")
    } else {
      up <- up[, c("gender", "agecat", mult_cols)]
      params(up); replaceData(proxy, up, resetPaging = FALSE, rownames = FALSE)
    }
  })
  
  output$dl_params <- downloadHandler(
    filename = function() "scenario_params.csv",
    content  = function(file) write.csv(params(), file, row.names = FALSE)
  )
  
  # RUN SIMULATION
  
  run_label <- reactiveVal(NULL)
  res <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    withProgress(message = "Running simulation…", value = 0.5, {
      out <- tryCatch(
        odata(scenario = params(), r0 = input$R0, recalibrate = input$RECALIBRATE, bdm = BDM, bdf = BDF, lb = LB, ub = UB, data1 = DATA1, data2 = DATA2, data3 = DATA3, gl = GL, 
              burden = input$source, rbreaks = RBREAKS),
        error = function(e) {
          showNotification(paste("odata() failed:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      res(out)
    })
    
    if (!is.null(res())) {
      run_label(input$source)
    }
    
  })
  
  # TOTALS 
  
  output$totals <- renderUI({
    d <- res()$outd
    #m <- paste0(input$variable, "_", measures)
    m <- paste0("vbod", "_", measures)
    
    if (is.null(d)) {
      tot <- c("-","-","-","-")
      names(tot) <- m
      layout_columns(
        fill = FALSE,
        !!!lapply(names(tot), function(m) {
          title <- paste0("AVOIDABLE ",strsplit(m, "_")[[1]][2],"S")
          value_box(title = title,
                    value = "-",
                    theme = value_box_theme(bg = valueboxcolors[title])
          )
        })
      )
    } else {
      tot <- colSums(d[, intersect(m, names(d)), drop = FALSE], na.rm = TRUE)
      layout_columns(
        fill = FALSE,
        !!!lapply(names(tot), function(m) {
        title <- paste0("AVOIDABLE ",strsplit(m, "_")[[1]][2],"S")
        value_box(title = title,
                  value = format(round(tot[[m]]), big.mark = "\u2009"),
                  theme = value_box_theme(bg = valueboxcolors[title])
        )
      })
    )
    }
  })

  # CHART 
  
    output$plot <- renderPlotly({
    m <- paste0(input$variable, "_", input$measure)
    d <- res()$outd
    req(d, m %in% names(d))
    dis <- ifelse(input$cat != "none", input$cat, NULL)
    top <- d |>
      group_by(.data[[dis]]) |>
      summarise(tot = sum(.data[[m]], na.rm = TRUE), .groups = "drop") |>
      slice_max(tot, n = input$topn, with_ties = FALSE)

    grp <- if (input$group != "none") input$group else NULL
    plot_d <- d |>
      filter(.data[[dis]] %in% top[[dis]]) |>
      group_by(across(all_of(c(dis, grp)))) |>
      summarise(value = sum(.data[[m]], na.rm = TRUE), .groups = "drop") |>
      mutate(!!dis := factor(.data[[dis]], levels = rev(top[[dis]])))

      if (input$fill) {
        if (!is.null(grp)) {
          p <- ggplot(plot_d, aes(value, .data[[dis]], fill = .data[[grp]])) + 
            geom_col(position = position_fill()) +
            scale_x_continuous(labels = scales::label_comma()) +
            labs(x = input$variable, y = NULL, fill = grp) +
            theme_minimal(base_size = 15) 
        } else {
          p <- ggplot(plot_d, aes(value, .data[[dis]])) + 
            geom_col(position = position_fill()) +
            scale_x_continuous(labels = scales::label_comma()) +
            labs(x = input$variable, y = NULL, fill = grp) +
            theme_minimal(base_size = 15) 
        }
      } else {
        if (!is.null(grp)) {
          p <- ggplot(plot_d, aes(value, .data[[dis]], fill = .data[[grp]])) + 
            geom_col() +
            scale_x_continuous(labels = scales::label_comma()) +
            labs(x = input$variable, y = NULL, fill = grp) +
            theme_minimal(base_size = 15) 
        } else {
          p <- ggplot(plot_d, aes(value, .data[[dis]])) + 
            geom_col(fill = defaultfill ) +
            scale_x_continuous(labels = scales::label_comma()) +
            labs(x = NULL, y = NULL, fill = grp) +
            theme_minimal(base_size = 15) 
        }
      }  
    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(legend = list(orientation = "h", y = -0.15)) |>  
      config(displaylogo = FALSE)                             
    
  })
  
  # Note below the chart  
    output$chart_footer <- renderUI({
      lbl <- run_label()
      txt <- if (is.null(lbl)) "No simulation run yet" else paste("Burden source:", lbl)
      div(class = "tab-footer-left", txt)
    })
    
  # TABLE
    
  output$res_tbl <- renderDT({
    d <- res()$outd
    req(d)
    datatable(d, rownames = FALSE, filter = "top") |>
      formatRound(intersect(measures, names(d)), 0)
  })

  output$dl_res <- downloadHandler(
    filename = function() "avoidable_burden.csv",
    content  = function(file) write.csv(res(), file, row.names = FALSE)
  )
  
  # CONSUMPTION SUMMARY 
  
  risk_category_props <- function(alpha, beta, gender) {
    bounds <- t(RBREAKS[gender][2:5,1])
    diff(c(0, pgamma(bounds, shape = alpha, rate = beta)))
  } # proportion of drinkers in each risk category, from Gamma(alpha, beta)
  
  cons_summary <- reactive({
    d <- res()$shiftexp
    req(d)
    before_mat <- t(mapply(risk_category_props, BASELINE_EXPOSURE$alpha, BASELINE_EXPOSURE$beta, BASELINE_EXPOSURE$gender))
    colnames(before_mat) <- mult_cols[3:6]
    merged <- dplyr::left_join(BASELINE_EXPOSURE, d, by = c("gender", "agecat"))
    after_mat <- t(mapply(risk_category_props, merged$alpha_cf, merged$beta_cf, merged$gender))
    colnames(after_mat) <- mult_cols[3:6]
    list(meta = BASELINE_EXPOSURE[c("gender", "agecat", "pdrink")], before = before_mat, after = after_mat)
  })
  
  output$cons_summary_cards <- renderUI({
    p <- params()
    cols <- c(mult_cols)
    avg_reduction <- sapply(p[cols], function(x) mean(x) - 1)
    reduction <- rep("", length(avg_reduction))
    for (i in c(1:length(avg_reduction))) {
      if(avg_reduction[i]== 0) {
        reduction[i] <- "unchanged"
      } else {
        reduction[i] <- paste0(round(avg_reduction[i]*100,1),"%")
      }
    }
    names(reduction) <- cols
    card_titles <- c(
      curr           = "Prevalence of drinkers",
      bing           = "Proportion of binge drinkers among drinkers",
      Low_risk       = "Average consumption among low risk drinkers",
      Moderate_risk  = "Average consumption among moderate risk drinkers",
      High_risk      = "Average consumption among high risk drinkers",
      Very_high_risk = "Average consumption among very high risk drinkers"
    )
    
    if (!is.null(res())) {
      totred = round((res()$R-1)*100,1)
    if (totred !=0) {
      ltotred <- paste0(totred,"%")
    } else {
      ltotred <- "unchanged"
    }
    } else {
      ltotred <- "unchanged"
    }
    
    tags$div(
    tags$ul(
      lapply(names(avg_reduction), function(cn) {
        tags$li(sprintf("%s: %s", card_titles[[cn]], reduction[[cn]]))
      })
    ),
    br(),
    tags$ul(
      tags$li(sprintf("%s: %s", "Total alcohol consumption", ltotred))
    )
    )
  })
  
  # output$risk_shift_by_group <- renderPlotly({
  #   cs <- cons_summary()
  #   
  #   print(cs$before)
  #   print(cs$after)
  #   
  #   to_long <- function(mat, meta, label) {
  #     as.data.frame(mat) |>
  #       bind_cols(meta) |>
  #       tidyr::pivot_longer(all_of(risk_cols), names_to = "risk_category", values_to = "prop") |>
  #       mutate(period = label)
  #   }
  #   
  #   plot_d <- bind_rows(
  #     to_long(cs$before, cs$meta[c("gender", "agecat")], "Before"),
  #     to_long(cs$after,  cs$meta[c("gender", "agecat")], "After")
  #   ) |>
  #     mutate(period = factor(period, levels = c("Before", "After")),
  #            risk_category = factor(risk_category, levels = risk_cols, labels = c("Low risk","Moderate risk","High risk","Very high risk")),
  #            tooltip = sprintf("%s<br>%s: %s", period, risk_category, scales::percent(prop, accuracy = 0.1)))
  #   p <- ggplot(plot_d, aes(period, prop, fill = risk_category, text = tooltip)) +
  #     geom_col(position = "fill") +
  #     facet_grid(agecat ~ gender) +
  #     scale_y_continuous(labels = scales::percent) +
  #     scale_fill_manual(values = c(
  #       `Low risk`       = "#2E8B57",   # green
  #       `Moderate risk`  = "#F4C430",   # amber
  #       `High risk`      = "#E8732A",   # orange
  #       `Very high risk` = "#C0392B"    # red
  #     )) +
  #     labs(x = NULL, y = "Proportion of drinkers", fill = "Risk category") +
  #     theme_minimal(base_size = 11) +
  #     theme(strip.text.y = element_text(angle = 0), legend.position = "top", legend.direction = "horizontal") +
  #     scale_x_discrete(labels = c("Observed","Simulated"))
  #   
  #   ggplotly(p, height = 900, tooltip = "text") |> config(displaylogo = FALSE)
  # })
  
  
  # ABOUT & HELP
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Help",
      tagList(
        h4("About this app"),
        p("This app simulates the avoidable burden of disease (deaths, YLL, YLD and DALYs)",
          "under hypothetical alcohol-policy scenarios, and lets you explore the results",
          "as a chart and table."),
        
        h4("1. Simulation settings"),
        p("Open the ", strong("Simulation settings"), " panel in the sidebar to set overall",
          "parameters for the run (e.g. year, number of simulations, random seed,",
          "uncertainty interval). These apply to the whole simulation, not to any",
          "single scenario row."),
        
        h4("2. Scenario multipliers"),
        p("The ", strong("Scenario multipliers"), " table has one row per gender and age",
          "group, with three columns:"),
        tags$ul(
          tags$li(strong("curr"), " — multiplier for the prevalence of drinkers"),
          tags$li(strong("bing"), " — multiplier for the prevalence of binge drinking",
                  "among drinkers"),
          tags$li(strong("cons"), " — multiplier for average daily consumption")
        ),
        p("All values must be between 0 and 1. A value of 1 means no change from",
          "baseline; for example, 0.8 means a 20% reduction."),
        p("Double-click a cell to edit it directly, or use ", strong("Set column to value"),
          "to apply the same number to an entire column at once."),
        p(strong("Reset to 1"), " restores all multipliers to baseline (no change).",
          strong("Save params"), " downloads the current table as a CSV, and",
          strong("Load params"), " lets you re-upload a previously saved CSV."),
        
        h4("3. Running the simulation"),
        p("Once the scenario table is set up, click ", strong("Run simulation"), ".",
          "The results (value boxes, chart and table) only update when this button",
          "is clicked — editing the table afterwards will not change the results",
          "until you run it again."),
        
        h4("4. Reading the results"),
        p("The four boxes at the top show the total avoidable ", strong("deaths"), ", ",
          strong("YLL"), " (years of life lost), ", strong("YLD"),
          " (years lived with disability) and ", strong("DALYs"),
          " (disability-adjusted life years) for the scenario, summed across all",
          "diseases. They show a dash until the first simulation has run."),
        p(strong("Chart tab"), ": an interactive bar chart of the top diseases for a",
          "chosen measure (deaths, YLL, YLD or DALY). Use the dropdowns to change the",
          "measure, break the chart down by gender or age group, or adjust how many",
          "diseases are shown. Hover over a bar for its exact value; click a legend",
          "entry to show or hide a group."),
        p(strong("Table tab"), ": the full results as a searchable, sortable table, with",
          "a button to download it as a CSV."),
        
        h4("Tips"),
        tags$ul(
          tags$li("Use ", strong("Set column to value"), " to quickly test an across-the-board",
                  "change, e.g. setting cons to 0.9 for a 10% reduction in consumption",
                  "for every group."),
          tags$li("Save a scenario before making further changes, so you can reload it",
                  "later or share it with others."),
          tags$li("If the results look empty, check that ", strong("Run simulation"),
                  " has been clicked after your most recent edits.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About",
      tagList(
        h4("About"),
        
        p("This app was developed by [Name], [Affiliation],",
          "as part of [Project name / study name]."),
        
        h4("Methodology"),
        p("The underlying simulation model and avoidable burden estimates are based",
          "on [brief description of the model / data sources, e.g. Global Burden of",
          "Disease study, national survey data, etc.]. Full methodological details",
          "are available in [reference / link]."),
        
        h4("Acknowledgements"),
        p("[Names / teams / funders to acknowledge, if any.]"),
        
        h4("Contact"),
        p("For questions, feedback or bug reports, please contact",
          a(href = "mailto:annibale.cois@uct.ac.za", "annibale.cois@uct.ac.za"),
          "or open an issue on the",
          a(href = "https://github.com/AnnibaleCois/RESET-IMPACTSIM", "GitHub repository", target = "_blank"),
          "."),
        
        h4("Citation"),
        p("If you use this app or its results, please cite:"),
        tags$blockquote(
          "[Annibale Cois ([Year]). [App/Project title]. [Version/URL]."
        ),
        
        h4("License"),
        
        p(
          a(href="https://opensource.org/licenses/MIT", img(src="https://img.shields.io/badge/License-MIT-yellow.svg"), alt="License: MIT")
        ),
        p("This app is released under the [license name] license. See the",
          a(href = "https://github.com/AnnibaleCois/RESET-IMPACTSIM/LICENSE", "LICENSE file", target = "_blank"), "for details.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  
}


