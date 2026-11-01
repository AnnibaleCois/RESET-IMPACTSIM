################################################################################################################################################################################
# RESET ALCOHOL PROJECT                                                                                                                                                        #
# IMPACT SIMULATOR - SERVER                                                                                                                                                    # 
# v.1.0, 2026/10/31                                                                                                                                                            #
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
          th(rowspan = 2, "Sex"),
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
      options = list(dom = "t", pageLength = 12, ordering = FALSE),
      callback = JS(sprintf("
        table.on('focusin', 'td input', function() {
          this.type = 'number';
          this.step = '0.05';
          this.min = '0';
          this.max = '%s';
        });
      ", MAXMULT))
    )
  })
  proxy <- dataTableProxy("param_tbl")
  
  observeEvent(input$param_tbl_cell_edit, {
    info <- input$param_tbl_cell_edit
    p <- params()
    val <- suppressWarnings(as.numeric(info$value))
    
    if (is.na(val) || val < 0 || val > MAXMULT) {
      showNotification(
        paste0("Multipliers must be numbers between 0 and ", MAXMULT),
        type = "warning"
      )
    } else {
      p[info$row, info$col + 1] <- round(val, 2)
      params(p)
    }
    
    replaceData(proxy, params(), resetPaging = FALSE, rownames = FALSE)
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
    ok <- !is.null(up) && all(c("sex", "agecat", mult_cols) %in% names(up)) &&
      nrow(up) == nrow(template)
    in_range <- ok && all(vapply(up[c(mult_cols)], function(x)
      is.numeric(x) && all(x >= 0 & x <= 1, na.rm = FALSE), logical(1)))
    if (ok && !isTRUE(in_range)) {
      showNotification("All multipliers in the CSV must be numbers between 0 and 1.", type = "error")
    } else if (!ok) {
      showNotification(sprintf("CSV needs columns sex, agecat, %s and %d rows.",
                               paste(c(mult_cols), collapse = ", "), nrow(template)),
                       type = "error")
    } else {
      up <- up[, c("sex", "agecat", mult_cols)]
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
          showNotification(paste("odata() failed:", conditionMessage(e)), type = "error", duration = NULL)
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
    m <- paste0("vbod", "_", measures)
    m1 <- paste0("abod", "_", measures)
    m2 <- paste0("bod", "_", measures)
    
    if (is.null(d)) {
      tot <- c("-","-","-","-")
      names(tot) <- m
      layout_columns(
        fill = FALSE,
        !!!lapply(names(tot), function(m) {
          title <- paste0("AVERTED ",strsplit(m, "_")[[1]][2],"S")
          value_box(title = title,
                    value = "-",
                    theme = value_box_theme(bg = valueboxcolors[title])
          )
        })
      )
    } else {
      tot <- colSums(d[, intersect(m, names(d)), drop = FALSE], na.rm = TRUE)
      tot1 <- colSums(d[, intersect(m1, names(d)), drop = FALSE], na.rm = TRUE)       
      tot2 <- colSums(d[, intersect(m2, names(d)), drop = FALSE], na.rm = TRUE) 
      tot3 <- round(tot/tot1*100,1)
      names(tot1) <- names(tot)
      names(tot2) <- names(tot)
      names(tot3) <- names(tot)
      layout_columns(
        fill = FALSE,
        !!!lapply(names(tot), function(m) {
        title <- paste0("AVERTED ",strsplit(m, "_")[[1]][2],"S")
        value_box(title = title,
                  value = tagList(
                          span(format(round(tot[[m]]), big.mark = "\u2009")),
                          span(paste0("Attributable burden: ",tot1[[m]]), style = "font-size: 30%; opacity: 0.8; "),
                          span(paste0("Averted burden [%]: ", sprintf("%.1f",round(tot[[m]]/tot1[[m]]*100,1))), style = "font-size: 30%; opacity: 0.8; ")
                  ),
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
  
  # NOTE BELOW THE CHART  
    
    output$chart_footer <- renderUI({
      lbl <- run_label()
      txt <- if (is.null(lbl)) "No simulation run yet" else paste("Burden source:", lbl)
      div(class = "tab-footer-left", txt)
    })
    
  # TABLE
    
  output$res_tbl <- renderDT({
    d <- res()$outd
    req(d)
    d <- d %>% select(-c(bod_DEATH, bod_YLD, bod_YLL, bod_DALY))
    colnames(d) <- c("Sex","Age","Group 1","Group 2", "Cause","Attributable\nDeaths","Attributable\nYLDs","Attributable\nYLLs","Averted\nDeaths","Averted\nYLLs",
                     "Averted\nYLDs","Attributable\nDALYs","Averted\nDALYs")
    d <- d[,c(1:8,12,9:11,13)]
    datatable(d, rownames = FALSE, filter = "top") |>
      formatRound(intersect(measures, names(d)), 0)
  })

  output$dl_res <- downloadHandler(
    filename = function() "avoidable_burden.csv",
    content  = function(file) write.csv(res(), file, row.names = FALSE)
  )
  
  # CONSUMPTION SUMMARY 
  
  risk_category_props <- function(alpha, beta, sex) {
    bounds <- t(RBREAKS[sex][2:5,1])
    diff(c(0, pgamma(bounds, shape = alpha, rate = beta)))
  } # proportion of drinkers in each risk category, from Gamma(alpha, beta)
  
  cons_summary <- reactive({
    d <- res()$shiftexp
    req(d)
    before_mat <- t(mapply(risk_category_props, BASELINE_EXPOSURE$alpha, BASELINE_EXPOSURE$beta, BASELINE_EXPOSURE$sex))
    colnames(before_mat) <- mult_cols[3:6]
    merged <- dplyr::left_join(BASELINE_EXPOSURE, d, by = c("sex", "agecat"))
    after_mat <- t(mapply(risk_category_props, merged$alpha_cf, merged$beta_cf, merged$sex))
    colnames(after_mat) <- mult_cols[3:6]
    list(meta = BASELINE_EXPOSURE[c("sex", "agecat", "pdrink")], before = before_mat, after = after_mat)
  })
  
  output$cons_summary_cards <- renderUI({
    if (!is.null(res())) {
      rdrink <- res()$rdrink
      rbing <- res()$rbing
      rcons <- res()$rcons

      print(c(rdrink,rbing,rcons))
      
      avg_reduction <- c(rdrink,rbing,rcons)
      reduction <- rep("", length(avg_reduction))
      for (i in c(1:length(avg_reduction))) {
        if(abs(avg_reduction[i])< 1e-4) {
          reduction[i] <- "unchanged"
        } else {
          reduction[i] <- sprintf("%+.1f%%", avg_reduction[i]*100)   
        }
      }
      names(reduction) <- c("curr","bing","cons")
      card_titles <- c(
        curr           = "Prevalence of drinkers",
        bing           = "Proportion of binge drinkers among drinkers",
        cons           = "Average consumption among drinkers"
      )
    } else {
      reduction <- rep("-", 3)
      names(reduction) <- c("curr","bing","cons")
      card_titles <- c(
        curr           = "Prevalence of drinkers",
        bing           = "Proportion of binge drinkers among drinkers",
        cons           = "Average consumption among drinkers"
      )
    }
      
    if (!is.null(res())) {
      totredp = round((res()$R-1)*100,1)
      totredl = round((res()$R-1)*TOTAA,0)
    if (totredp !=0) {
      ltotred <- paste0(totredp,"% (",formatC(totredl, format = "f", big.mark = "\u2009", digits = 0)," litres)")
    } else {
      ltotred <- "unchanged"
    }
    } else {
      ltotred <- "-"
    }
    
    
    tags$div(
    tags$ul(
      lapply(names(reduction), function(cn) {
        tags$li(sprintf("%s: %s", card_titles[[cn]], reduction[[cn]]))
      })
    ),
    br(),
    tags$ul(
      tags$li(sprintf("%s: %s", "Total alcohol consumption", ltotred))
    )
    )
  })
  
  # PLOT DISTRIBUTION  
  
  output$hist <- renderPlotly({
    d <- res()$shiftexp[,c("sex","agecat","alpha_cf","beta_cf","pdrink_cf","alpha","beta","pdrink","pop")]
    req(d)
    w <- d$pop*d$pdrink 
    w_cf <- d$pop*d$pdrink_cf
    d$pdrink <- NULL
    d$pdrink_cf <- NULL
    
    binwidth <- 12
    breaks <- seq(0, 156, by = binwidth) 
    lo <- head(breaks, -1)
    hi <- tail(breaks, -1)
    mid <- (lo + hi) / 2
  
    # Proportion of the population falling in each bin
    mix_bin_prob <- function(lo, hi, alpha, beta, w) {
      sapply(seq_along(lo), function(j)
        sum(w * (pgamma(hi[j], shape = alpha, rate = beta) -
                   pgamma(lo[j], shape = alpha, rate = beta))))
    }
  
    p1 <- mix_bin_prob(lo, hi, d$alpha, d$beta, w)
    p2 <- mix_bin_prob(lo, hi, d$alpha_cf, d$beta_cf, w_cf)
    bin_label <- ""
    plot_ly() %>%
      add_bars(x = mid, y = p1, name = "Observed",
               marker = list(color = "rgba(31,119,180,0.75)"), text = bin_label,
               hovertemplate = "Drinks/day: %{x}<br>Number of drinkers: %{y:.0f}<extra>Set 1</extra>") %>%
      add_bars(x = mid, y = p2, name = "Scenario",
               marker = list(color = "rgba(255,127,14,0.75)"), text = bin_label,
               hovertemplate = "Drinks/day: %{x}<br>Number of drinkers: %{y:.0f}<extra>Set 2</extra>") %>%
      layout(barmode = "group",
             bargap = 0.1,
             xaxis = list(title = "Alcohol consumption [drinks/day]", range = c(0, 156), 
                          tickmode = "array", tickvals = mid, ticktext = seq_along(mid)),
             yaxis = list(title = "Number of drinkers"),
             hovermode = "closest")
  })

  # ABOUT & HELP
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Help",
      tagList(
        h4("About this app"),
        p("This app simulates changes in the burden of disease (deaths, YLL, YLD and DALYs)",
          "in response to hypothetical changes in the alcohol exposure of the South African population, and lets you explore the results",
          "as a chart and table."),
        
        h4("1. Simulation settings"),
        p("Open the ", strong("Simulation settings"), " panel in the sidebar to set overall", "parameters for the simulation:"),
        tags$ul(
          tags$li(strong("Burden data"), " — Source of burden data: Third South African National Burden of Disease Study (NBD 3)",
                  " or Global Burden of Disease Study 2023 (GBD 2023)."),
          tags$li(strong("Recalibration"), " — Method for recalibrating the scenario multipliers so that the total alcohol consumption ",
                  "across the whole population matches the value implied by the 'Recalibration proportion' setting: ",
                  "No recalibration (None), Change the proportion of drinkers (Proportion of drinkers), ",
                  "Change the average consumption among drinkers (Average daily conusmption)."),
          tags$li(strong("Recalibration proportion"), " — Total alcohol consumption in the population (as a proportion of the observed consumption).")
        ),
        p("These settings apply to the whole simulation."),
        
        h4("2. Scenario multipliers"),
        p("The ", strong("Scenario multipliers"), " table has one row per sex and age", "group, with six columns:"),
        tags$ul(
          tags$li(strong("PREV"), " — multiplier for the prevalence of drinkers."),
          tags$li(strong("HED"), " — multiplier for the prevalence of heavy episodic drinkers among drinkers."),
          tags$li(strong("LR"), " — multiplier for average daily consumption among low risk individuals."),
          tags$li(strong("MR"), " — multiplier for average daily consumption among moderate risk individuals."),
          tags$li(strong("HR"), " — multiplier for average daily consumption among high risk individuals."),
          tags$li(strong("VR"), " — multiplier for average daily consumption among very high risk individuals.")
        ),
        p("All values must be between 0 and 1, with a value of 1 meaning no change from the observed distribution. A value of 0.8, for example, means a 20% reduction."),
        p("Risk categories are defined based on average daily alcohol consumption according to standard World Health Organization cutoffs: "),
        tags$ul(
          tags$li(strong("Low risk:"), " ≤20 g/day (females) and ≤40 g/day (males)"),
          tags$li(strong("Moderate risk:"), " [20-40) g/day (females) and [40-60) g/day (males)"),
          tags$li(strong("High risk:"), " (40-60] g/day (females) and (60-100] g/day (males)"),
          tags$li(strong("Very high risk:"), " >60 g/day (females) and > 100 g/day (males)")
        ),
        p("Double-click a cell to edit it directly, or use ", strong("Set column to value"),"to apply the same number to an entire column at once.",
        strong("Reset to 1"), " restores all multipliers to baseline (1 = no change).",
        strong("Save scenario"), " downloads the current table as a CSV.",
        strong("Load scenario"), " uploads a previously saved CSV."),
        
        h4("3. Running the simulation"),
        p("Once the scenario table is set up, click ", strong("Run simulation"), ".",
          "The results (value boxes, chart and table) only update when this button",
          "is clicked — editing the table afterwards will not change the results",
          "until you run it again."),
        
        h4("4. Reading the results"),
        p("The four boxes at the top show the total averted ", strong("deaths"), ", ",
          strong("YLL"), " (years of life lost), ", strong("YLD"),
          " (years lived with disability) and ", strong("DALYs"),
          " (disability-adjusted life years) for the scenario, summed across all",
          "diseases. They show a dash until the first simulation has run."),
        p(strong("Chart tab"), ": an interactive bar chart of the top diseases for a",
          "chosen measure (deaths, YLL, YLD or DALY). Use the dropdowns to change the",
          "measure, break the chart down by sex, age group or disease, or adjust how many",
          "diseases are shown. Hover over a bar for its exact value; click a legend",
          "entry to show or hide a group."),
        p(strong("Table tab"), ": the full results as a searchable, sortable table, with"," a button to download it as a CSV."),
        p(strong("Assumed exposure tab"), ": a summary of the differences between the simulated",tags$sup("*")," and the observed exposure."),
        h4("Tips"),
        tags$ul(
          tags$li("Use ", strong("Set column to value"), " to quickly test an across-the-board",
                  "change, e.g. setting PREV to 0.9 for a 10% reduction in consumption",
                  "for every demographic and risk group."),
          tags$li("Save a scenario before making further changes, so you can reload it",
                  "later or share it with others."),
          tags$li("If the results look empty, check that ", strong("Run simulation"),
                  " has been clicked after your most recent edits.")
        ),
        br(),
        
        p(tags$sup("*"),tags$em(" The simulated exposure takes into account the values in the scenario multipliers table to the extent it is possible within epidemiological constraints.",
          "In case of irreconciliable values, the distribution across risk categories might differ from the distribution implied by the table."))
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
        p("This app was developed by Annibale Cois, Nicole Vellios, Magnolia Chikanya and Richard Matzopoulos, School of Public Health, University of Cape Town, as part of the ",
        tags$a(
          "RESET Alcohol Initiative", 
          href = "https://www.vitalstrategies.org/issues/alcohol-policy/reset-alcohol-initiative/", 
          target = "_blank", 
          style = "font-size: 0.85rem; text-decoration: underline;"
        ),"."
        ),
        h4("Methodology"),
        p("The averted burden estimates generated by this App are based on the comparative Risk Assessment (CRA) methodology,",
          "as implemented in the Second South African Comparative Risk Assessment Study (SACRA2)."),
        p("The CRA methodology estimates the contribution of individual risk factors to the burden of disease (quantified in terms of number of deaths,",
          " YLLs, YLDs and DALYs) by comparing the burden due to the observed exposure with an hypothetical counterfactual scenario characterised by a different",
          " exposure distribution."),
        p("The baseline exposure and burden distributions used in the simulation refers to the year 2022."),
        p("Full methodological details are available in", span(" Cois A, Vellios N, Chikanya M, Matzopoulos R. (2026)", style = "font-style: italic;"),
          span(" Lives saved and disease burden avoidable through reduced alcohol consumption in South Africa.", style = "font-style: italic;"),
          span("Cape Town: University of Cape Town.", style = "font-style: italic;")),
        h4("Acknowledgements"),
        p(strong("Vital Strategies Inc."), " which funded the development of the application and the underlying study (subgrant agreement VS00036959, 15/05/2026)"),
        
        h4("Contact"),
        p("For questions, feedback or bug reports, please contact",
          a(href = "mailto:annibale.cois@uct.ac.za", "annibale.cois@uct.ac.za"),
          "or open an issue on the",
          a(href = "https://github.com/AnnibaleCois/RESET-IMPACTSIM", "GitHub repository", target = "_blank"),
          "."),
        
        h4("Citation"),
        p("If you use this app or its results, please cite:"),
        tags$blockquote(
          "Annibale Cois, Magnolia Chikanya (2026). RESET-IMPACTSIM. Version 1.0.0.",br(),
          "URL: ",
          a(href = "https://github.com/AnnibaleCois/RESET-IMPACTSIM", "https://github.com/AnnibaleCois/RESET-IMPACTSIM", target = "_blank")
        ),
        
        h4("License"),
        
        p(
          a(href="https://opensource.org/licenses/MIT", img(src="https://img.shields.io/badge/License-MIT-yellow.svg"), alt="License: MIT")
        ),
        p("This app is released under the MIT license. See the",
          a(href = "https://github.com/AnnibaleCois/RESET-IMPACTSIM/LICENSE", "LICENSE file", target = "_blank"), "for details.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

}


