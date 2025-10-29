library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(leaflet.extras)


ui <- fluidPage(
  titlePanel("Global Earthquake-Tsunami Risk Assessment"),
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot", 
                         sidebarLayout(
                           sidebarPanel(
                             sliderInput("year_range", "Year range",
                                         min = min(df$Year, na.rm = TRUE),
                                         max = max(df$Year, na.rm = TRUE),
                                         value = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)),
                                         step = 1, sep = ""),
                             sliderInput("roll_w", "Rolling window (months)",
                                         min = 3, max = 24, value = 12, step = 1)
                           ),
                           
                           mainPanel(
                             h4("Yearly Trend: Total vs Tsunami"),
                             plotOutput("p_yearly", height = 340),
                             tags$hr(),
                             
                             h4("Monthly Trend: Total vs Tsunami"),
                             plotOutput("p_monthly", height = 320),
                             tags$hr(),
                             
                             h4(textOutput("roll_title")),
                             plotOutput("p_monthly_roll", height = 320)
                           )
                         )
                ),
                
                tabPanel("Tsunami Prediction",
                         tabsetPanel(type="tabs",
                                     tabPanel("EDA",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Control bar for Scatter Plot"),
                                                  selectInput(
                                                    "xvar", "X axis",
                                                    choices = c("magnitude","depth","sig","latitude","longitude"),
                                                    selected = "magnitude"
                                                  ),
                                                  selectInput(
                                                    "yvar", "Y axis",
                                                    choices = c("depth","magnitude","sig","latitude","longitude"),
                                                    selected = "depth"
                                                  ),
                                                  tags$hr()
                                                ),
                                                mainPanel(
                                                  plotOutput("tsunami_scatter", height = "750px")
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Prediction",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Input seismic parameters"),
                                                  sliderInput("inp_mag", "Magnitude", min = min(df$magnitude),
                                                              max = max(df$magnitude), value = median(df$magnitude), step = 0.1),
                                                  sliderInput("inp_depth", "Depth (km)", min = min(df$depth),
                                                              max = max(df$depth), value = median(df$depth)),
                                                  sliderInput("inp_sig", "Significance (sig)", min = min(df$sig),
                                                              max = max(df$sig), value = median(df$sig)),
                                                  sliderInput("inp_lat", "Latitude", min = min(df$latitude),
                                                              max = max(df$latitude), value = median(df$latitude)),
                                                  sliderInput("inp_lon", "Longitude", min = min(df$longitude),
                                                              max = max(df$longitude), value = median(df$longitude)),
                                                  helpText("Adjust the sliders to simulate a new earthquake event.")
                                                ),
                                                mainPanel(
                                                  h3("Predicted Tsunami Risk"),
                                                  verbatimTextOutput("pred_num"),
                                                  h5(textOutput("risk_label")),
                                                  tags$hr(),
                                                  p("Model: Logistic regression"),
                                                  p("Predicted probability of tsunami occurrence (tsunami = 1).")
                                                  
                                                )
                                              )
                                     )
                         )),
                
                tabPanel('Early Warning Systems',
                         tabsetPanel(type="tabs",
                                     tabPanel("EDA",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Control bar for Scatter Plot"),
                                                  selectInput("xvar","X axis", choices=feature_cols, selected="magnitude"),
                                                  selectInput("yvar","Y axis", choices=feature_cols, selected="depth")
                                                ),
                                                mainPanel(
                                                  plotOutput("eda_scatter", height="750px")
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Model",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Random Forest Training"),
                                                  sliderInput("ntree","ntree", min=100, max=1000, value=300, step=50),
                                                  sliderInput("mtry","mtry", min=2, max=length(feature_cols), value=4, step=1),
                                                  actionButton("train_btn","Train / Retrain"),
                                                  tags$hr(),
                                                  p("Performance evaluated on a 30% hold-out test set.")
                                                ),
                                                mainPanel(
                                                  h4("Confusion Matrix"),
                                                  verbatimTextOutput("cm_txt"),
                                                  h4("AUC"),
                                                  textOutput("auc_txt"),
                                                  h4("Performance Metrics"),
                                                  tableOutput("metrics_table"), 
                                                  h4("Feature Importance"),
                                                  plotOutput("varimp_plot", height = "350px")
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Real-time Assessment",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Input Seismic Parameters"),
                                                  sliderInput("in_mag","Magnitude", min=min(df$magnitude,na.rm=T),
                                                              max=max(df$magnitude,na.rm=T), value=median(df$magnitude,na.rm=T), step=0.1),
                                                  sliderInput("in_depth","Depth (km)", min=min(df$depth,na.rm=T),
                                                              max=max(df$depth,na.rm=T), value=median(df$depth,na.rm=T)),
                                                  sliderInput("in_sig","Significance (sig)", min=min(df$sig,na.rm=T),
                                                              max=max(df$sig,na.rm=T), value=median(df$sig,na.rm=T)),
                                                  sliderInput("in_lat","Latitude", min=min(df$latitude,na.rm=T),
                                                              max=max(df$latitude,na.rm=T), value=median(df$latitude,na.rm=T)),
                                                  sliderInput("in_lon","Longitude", min=min(df$longitude,na.rm=T),
                                                              max=max(df$longitude,na.rm=T), value=median(df$longitude,na.rm=T)),
                                                  sliderInput("in_dmin","dmin (°)", min=min(df$dmin,na.rm=T),
                                                              max=max(df$dmin,na.rm=T), value=median(df$dmin,na.rm=T), step=0.01),
                                                  sliderInput("in_gap","gap (°)", min=min(df$gap,na.rm=T),
                                                              max=max(df$gap,na.rm=T), value=median(df$gap,na.rm=T)),
                                                  sliderInput("in_nst","nst", min=min(df$nst,na.rm=T),
                                                              max=max(df$nst,na.rm=T), value=median(df$nst,na.rm=T)),
                                                  sliderInput("in_mmi","MMI", min=min(df$mmi,na.rm=T),
                                                              max=max(df$mmi,na.rm=T), value=median(df$mmi,na.rm=T)),
                                                  sliderInput("in_cdi","CDI", min=min(df$cdi,na.rm=T),
                                                              max=max(df$cdi,na.rm=T), value=median(df$cdi,na.rm=T)),
                                                  tags$hr(),
                                                  sliderInput("thres","Alert Threshold (prob)", min=0.1, max=0.9, value=0.5, step=0.05),
                                                  actionButton("predict_btn","Assess Now")
                                                ),
                                                mainPanel(
                                                  h3("Predicted Tsunami Risk"),
                                                  h5(textOutput("pred_prob")),
                                                  p(textOutput("pred_label")),
                                                  tags$hr(),
                                                  p("The model outputs the estimated probability that a tsunami will occur (tsunami = 1)."),
                                                  p("It is considered High Risk if probability ≥ Alert Threshold.")
                                                )
                                              )
                                     )
                         )
                ),
                
                tabPanel('Hazard Mapping',
                         sidebarLayout(
                           sidebarPanel(
                             h4("Filters"),
                             sliderInput("mag_range", "Magnitude", min = floor(min(df$magnitude, na.rm=TRUE)),
                                         max = ceiling(max(df$magnitude, na.rm=TRUE)),
                                         value = c(floor(min(df$magnitude, na.rm=TRUE)), ceiling(max(df$magnitude, na.rm=TRUE))), step = 0.1),
                             sliderInput("depth_range", "Depth (km)", min = floor(min(df$depth, na.rm=TRUE)),
                                         max = ceiling(max(df$depth, na.rm=TRUE)),
                                         value = c(floor(min(df$depth, na.rm=TRUE)), ceiling(max(df$depth, na.rm=TRUE)))),
                             checkboxInput("only_tsu", "Show tsunami=1 only (points layer)", FALSE),
                             
                             tags$hr(),
                             h4("Risk Index (rule-based)"),
                             helpText("Simple rule-based indicator: higher magnitude and significance, and shallower depth indicate higher risk."),
                             sliderInput("w_mag", "Weight: magnitude", min=0, max=2, value=1, step=0.1),
                             sliderInput("w_sig", "Weight: sig", min=0, max=2, value=1, step=0.1),
                             sliderInput("w_dep", "Weight: (1 - depth)", min=0, max=2, value=1, step=0.1),
                             sliderInput("t_high", "High risk ≥", min=0.3, max=0.9, value=0.7, step=0.05),
                             sliderInput("t_med", "Medium risk ≥", min=0.1, max=0.8, value=0.5, step=0.05),
                             
                             tags$hr(),
                             h4("Grid (risk zones)"),
                             helpText("Grid cell size (in degrees) for latitude and longitude."),
                             sliderInput("grid_deg", "Grid size (degrees)", min=0.25, max=5, value=1, step=0.25)
                           ),
                           mainPanel(
                             leafletOutput("map", height = "700px"),
                             tags$br(),
                             p("Layers: Events (clustered), Heatmap, and Risk Zones (grid). Use the left panel to adjust filters for points and heatmaps."),
                             p("Risk Zones are colored based on the average risk probability within each grid cell.")
                             
                           )
                         )
                ),
                
                tabPanel("Map", 
                         fluidRow(
                           column(12, leafletOutput("mapplot", height = "1000px"))
                         )
                         
                ),
                tabPanel("Raw Data",
                         DTOutput("eq_table")
                ),
                tabPanel("About",
                         h2("About This App"),
                         h3("Overview:"),
                         p("The Global Earthquake-Tsunami Risk Assessment Dataset is a comprehensive, machine learning-ready dataset containing seismic characteristics and tsunami potential indicators for 782 significant earthquakes recorded globally from 2001 to 2022. This dataset is specifically designed for tsunami risk prediction, earthquake analysis, and seismic hazard assessment applications."),
                         h5("Data Source: Global Earthquake-Tsunami Risk Assessment Dataset"),
                         p("(https://www.kaggle.com/datasets/ahmeduzaki/global-earthquake-tsunami-risk-assessment-dataset)"),
                         h4("Feature Description"),
                         tableOutput("information_tab")
                )
    )
  )
)
