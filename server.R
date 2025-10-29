library(DT)
library(leaflet)
library(mapview)

cols_list <- c("magnitude", "cdi", "mmi", "sig", "nst",
               "dmin", "gap", "depth", "latitude", "longitude",
               "Year", "Month", "tsunami")

mapviewOptions(legend = FALSE)

server <- function(input, output) {
  
  
  df_f <- reactive({
    df %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  # Yearly
  output$p_yearly <- renderPlot({
    yearly <- df_f() %>%
      group_by(Year) %>%
      summarise(Total = n(),
                Tsunami = sum(tsunami, na.rm = TRUE),
                .groups = "drop")
    
    ggplot(yearly, aes(x = Year)) +
      geom_line(aes(y = Total, color = "Total")) +
      geom_point(aes(y = Total, color = "Total")) +
      geom_line(aes(y = Tsunami, color = "Tsunami")) +
      geom_point(aes(y = Tsunami, color = "Tsunami")) +
      scale_color_manual(values = c("Total" = "steelblue", "Tsunami" = "firebrick")) +
      labs(title = "Earthquakes & Tsunami by Year",
           x = "Year", y = "Events", color = "") +
      theme_classic() + theme(legend.position = "bottom")
  })
  
  # Monthly (1~12 집계)
  output$p_monthly <- renderPlot({
    monthly <- df_f() %>%
      group_by(Month) %>%
      summarise(Total = n(),
                Tsunami = sum(tsunami, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Month = as.integer(Month))
    
    ggplot(monthly, aes(x = Month)) +
      geom_line(aes(y = Total, color = "Total")) +
      geom_point(aes(y = Total, color = "Total")) +
      geom_line(aes(y = Tsunami, color = "Tsunami")) +
      geom_point(aes(y = Tsunami, color = "Tsunami")) +
      scale_x_continuous(breaks = 1:12) +
      scale_color_manual(values = c("Total" = "steelblue", "Tsunami" = "firebrick")) +
      labs(title = "Monthly Occurrences (Aggregated by Month 1–12)",
           x = "Month", y = "Events", color = "") +
      theme_classic() + theme(legend.position = "bottom")
  })
  
  # Monthly Rolling Mean (연/월 시계열 → 롤링 평균)
  output$roll_title <- renderText({
    paste0("Monthly Rolling Mean (window = ", input$roll_w, " months)")
  })
  
  output$p_monthly_roll <- renderPlot({
    # 월별 시계열(날짜축) 만들기
    ts_monthly <- df_f() %>%
      mutate(date = as.Date(sprintf("%d-%02d-01", Year, Month))) %>%
      count(date, name = "Total") %>%
      arrange(date)
    
    validate(
      need(nrow(ts_monthly) > 1, "Not enough data in selected year range.")
    )
    
    # 롤링 평균 계산 (정렬 뒤 누락 월 보간 없이 관측치 기준)
    k <- input$roll_w
    ts_monthly <- ts_monthly %>%
      mutate(
        Total_roll   = if (n() >= k) zoo::rollmean(Total, k, align = "right", fill = NA) else NA_real_
      )
    
    ggplot(ts_monthly, aes(x = date)) +
      geom_line(aes(y = Total), linewidth = 0.7, alpha = 0.7) +
      geom_line(aes(y = Total_roll), linewidth = 1.1) +
      labs(title = NULL, x = "Date", y = "Events",
           caption = "Solid bold line = rolling mean; thin line = raw monthly counts") +
      theme_minimal()
  })
  
  ## tsunami prediction ----
  
  output$tsunami_scatter <- renderPlot({
    ggplot(df, aes_string(x = input$xvar,
                          y = input$yvar,
                          color = "tsunami_factor")) +
      geom_point(size = 3, alpha = 0.7) +
      labs(color = "Tsunami") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$pred_num <- renderText({
    new_event <- data.frame(
      magnitude = input$inp_mag,
      depth     = input$inp_depth,
      sig       = input$inp_sig,
      latitude  = input$inp_lat,
      longitude = input$inp_lon
    )
    
    prob <- predict(tsu_model, newdata = new_event, type = "response")
    paste0("Tsunami's Estimated probability is ",
           round(prob * 100, digits = 1), "%")
  })
  
  output$risk_label <- renderText({
    new_event <- data.frame(
      magnitude = input$inp_mag,
      depth     = input$inp_depth,
      sig       = input$inp_sig,
      latitude  = input$inp_lat,
      longitude = input$inp_lon
    )
    
    prob <- predict(tsu_model, newdata = new_event, type = "response")
    
    if (prob >= 0.5) {
      "It's considered High Risk"
    } else {
      "It's considered Low Risk"
    }
  })
  
  # early earning systems ----
  # 'aes_string' is for string variables
  output$eda_scatter <- renderPlot({
    ggplot(df, aes_string(x=input$xvar, y=input$yvar, color="tsunami_factor")) +
      geom_point(alpha=0.7, size=2.5) +
      labs(color="Tsunami") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  rf_model <- reactiveVal(NULL)
  hold_perf <- reactiveVal(list(cm=NULL, auc=NA))
  
  observeEvent(input$train_btn, {
    dat <- na.omit(df[, c(feature_cols, "tsunami")])
    dat$tsunami <- factor(dat$tsunami, levels=c(0,1), labels=c("No","Yes"))
    
    set.seed(42)
    idx <- sample(1:nrow(dat), size = max(1, floor(0.7*nrow(dat))))
    train <- dat[idx, ]
    test  <- dat[-idx, ]
    mtry_val  <- min(input$mtry, length(feature_cols))
    ntree_val <- input$ntree
    
    rf <- randomForest(
      x = train[, feature_cols, drop=FALSE],
      y = train$tsunami,
      mtry = mtry_val,
      ntree = ntree_val,
      importance = TRUE
    )
    rf_model(rf)
    
    if (nrow(test) > 0) {
      prob_yes <- as.numeric(predict(rf, newdata=test, type="prob")[,"Yes"])
      pred_cls <- ifelse(prob_yes >= 0.5, "Yes", "No")
      
      cm <- table(Actual=test$tsunami, Pred=pred_cls)
      
      roc_resp <- ifelse(test$tsunami=="Yes", 1, 0)
      auc_val <- tryCatch({
        as.numeric(pROC::auc(roc_resp, prob_yes))
      }, error=function(e) NA_real_)
      
      TP <- cm["Yes","Yes"]
      TN <- cm["No","No"]
      FP <- cm["No","Yes"]
      FN <- cm["Yes","No"]
      
      accuracy  <- (TP + TN) / sum(cm)
      precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
      recall    <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
      f1        <- ifelse(!is.na(precision) & !is.na(recall) & (precision+recall)>0,
                          2 * precision * recall / (precision + recall), NA)
      
      hold_perf(list(cm=cm, auc=auc_val,
                     acc=accuracy, prec=precision, rec=recall, f1=f1))
    } else {
      hold_perf(list(cm=NULL, auc=NA, acc=NA, prec=NA, rec=NA, f1=NA))
    }
  }, ignoreInit = TRUE)
  
  output$cm_txt <- renderPrint({
    perf <- hold_perf()
    if (is.null(perf$cm)) "Not enough samples for test." else perf$cm
  })
  
  output$auc_txt <- renderText({
    perf <- hold_perf()
    if (is.na(perf$auc)) "N/A" else paste0(" ", round(perf$auc, 3))
  })
  
  output$metrics_table <- renderTable({
    perf <- hold_perf()
    if (is.null(perf$acc) || all(is.na(c(perf$acc, perf$prec, perf$rec, perf$f1)))) {
      data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1-score"),
        Value = rep("N/A", 4)
      )
    } else {
      data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1-score"),
        Value = round(c(perf$acc, perf$prec, perf$rec, perf$f1), 3)
      )
    }
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  output$varimp_plot <- renderPlot({
    rf <- rf_model()
    validate(need(!is.null(rf), "The model is needed to train first."))
    varImpPlot(rf, main="Random Forest")
  })
  new_event <- eventReactive(input$predict_btn, {
    data.frame(
      magnitude = input$in_mag,
      depth     = input$in_depth,
      sig       = input$in_sig,
      latitude  = input$in_lat,
      longitude = input$in_lon,
      dmin      = input$in_dmin,
      gap       = input$in_gap,
      nst       = input$in_nst,
      mmi       = input$in_mmi,
      cdi       = input$in_cdi
    )
  }, ignoreInit = TRUE)
  
  output$pred_prob <- renderText({
    rf <- rf_model()
    validate(need(!is.null(rf), "Please train the model in the Model tab first."))
    req(new_event())
    prob_yes <- as.numeric(predict(rf, new_event(), type="prob")[,"Yes"])
    paste0("Estimated probability (tsunami = 1): ", round(prob_yes*100, 1), "%")
  })
  
  output$pred_label <- renderText({
    rf <- rf_model()
    validate(need(!is.null(rf), ""))
    req(new_event())
    prob_yes <- as.numeric(predict(rf, new_event(), type="prob")[,"Yes"])
    if (prob_yes >= input$thres) {
      "It is considered High Risk."
    } else {
      "It is considered Low Risk."
    }
  })
  
  # hazard mapping ----
  
  df_filt <- reactive({
    d <- df %>%
      filter(
        magnitude >= input$mag_range[1],
        magnitude <= input$mag_range[2],
        depth     >= input$depth_range[1],
        depth     <= input$depth_range[2]
      )
    if (isTRUE(input$only_tsu)) d <- d %>% filter(tsunami == 1)
    d
  })
  
  df_with_risk <- reactive({
    d <- df_filt()
    if (nrow(d) == 0) return(d)
    
    s_mag <- scale01(d$magnitude)
    s_sig <- scale01(d$sig)
    s_dep <- 1 - scale01(d$depth)   
    
    raw <- input$w_mag * s_mag + input$w_sig * s_sig + input$w_dep * s_dep
    rsk <- scale01(raw)           
    
    d$risk_prob <- rsk
    d$risk_level <- cut(
      d$risk_prob,
      breaks = c(-Inf, input$t_med, input$t_high, Inf),
      labels = c("Low", "Medium", "High"),
      right = FALSE
    )
    d
  })
  
  grid_agg <- reactive({
    d <- df_with_risk()
    if (nrow(d) == 0) return(NULL)
    
    g <- input$grid_deg
    d %>%
      mutate(
        lat_bin = floor(latitude / g) * g,
        lon_bin = floor(longitude / g) * g
      ) %>%
      group_by(lat_bin, lon_bin) %>%
      summarise(
        n = n(),
        mean_prob = mean(risk_prob, na.rm=TRUE),
        tsu_rate = mean(tsunami == 1, na.rm=TRUE),
        .groups = "drop"
      )
  })
  
  pal_prob <- colorNumeric("YlOrRd", domain = c(0,1), na.color = "transparent")
  pal_lvl  <- colorFactor(c("#2ca25f","#feb24c","#de2d26"), levels = c("Low","Medium","High"))
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = mean(df$longitude, na.rm=TRUE), lat = mean(df$latitude, na.rm=TRUE), zoom = 2) %>%
      addLayersControl(
        overlayGroups = c("Events", "Heatmap", "Risk Zones"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    d <- df_with_risk()
    g <- grid_agg()
    if (is.null(d)) return()
    
    m <- leafletProxy("map") %>% clearMarkers() %>% clearHeatmap() %>% clearGroup("Risk Zones")
    
    if (nrow(d) > 0) {
      m <- m %>% addCircleMarkers(
        data = d,
        lng = ~longitude, lat = ~latitude,
        radius = ~pmax(3, 6 * risk_prob), 
        stroke = FALSE, fillOpacity = 0.7,
        color = ~pal_lvl(risk_level),
        popup = ~paste0(
          "<b>Magnitude:</b> ", magnitude,
          "<br><b>Depth (km):</b> ", depth,
          "<br><b>Sig:</b> ", sig,
          "<br><b>Tsunami:</b> ", tsunami,
          "<br><b>Risk Prob:</b> ", sprintf("%.2f", risk_prob),
          "<br><b>Risk Level:</b> ", as.character(risk_level)
        ),
        group = "Events",
        clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
      )
    }
    
    if (nrow(d) > 1) {
      m <- m %>% addHeatmap(
        lng = d$longitude, lat = d$latitude,
        intensity = d$risk_prob,
        blur = 20, max = 1, radius = 15, group = "Heatmap"
      )
    }
    
    if (!is.null(g) && nrow(g) > 0) {
      grid_size <- input$grid_deg
      for (i in seq_len(nrow(g))) {
        lat0 <- g$lat_bin[i]; lon0 <- g$lon_bin[i]
        lat1 <- lat0 + grid_size; lon1 <- lon0 + grid_size
        lab  <- paste0(
          "<b>Grid:</b> [", lat0,", ", lon0, "]",
          "<br><b>Cells:</b> ", g$n[i],
          "<br><b>Mean Risk Prob:</b> ", sprintf("%.2f", g$mean_prob[i]),
          "<br><b>Tsunami Rate:</b> ", sprintf("%.2f", g$tsu_rate[i])
        )
        m <- m %>% addRectangles(
          lng1 = lon0, lat1 = lat0, lng2 = lon1, lat2 = lat1,
          fillColor = pal_prob(g$mean_prob[i]),
          fillOpacity = 0.35, weight = 1, color = "#777",
          popup = lab, group = "Risk Zones"
        )
      }
      
      m <- m %>% addLegend(
        "bottomright", pal = pal_prob, values = c(0,1),
        title = "Mean Risk Prob (grid)", opacity = 0.7, labFormat = labelFormat(transform = function(x) x)
      )
    }
    
    m <- m %>% addLegend(
      "bottomleft", pal = pal_lvl, values = c("Low","Medium","High"),
      title = "Event Risk Level", opacity = 0.9
    )
  })
  
  output$mapplot <- renderLeaflet({
    p2
  })
  
  
  output$eq_table <- renderDT({
    datatable(
      df[, cols_list],
      options = list(pageLength = 30, scrollX = TRUE),
      rownames = TRUE,
      colnames = c("Magnitude", "Community Intensity (CDI)", "Mercalli Intensity (MMI)",
                   "Significance", "Stations (NST)", "Distance Min (°)", "Gap (°)",
                   "Depth (km)", "Latitude", "Longitude", "Year", "Month", "Tsunami")
    )
  })
  
  output$information_tab <- renderTable({
    information_tab
  }, bordered = TRUE, striped = TRUE, hover = TRUE)
  
  
}
