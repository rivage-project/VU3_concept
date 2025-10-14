ui <- fluidPage(
  tags$head(HTML("<title>Island Vulnerability Index</title>")),
  tags$head(tags$style(HTML(".not_bold label {font-weight:normal;}"))),
  tags$style(HTML("
    thead:first-child > tr:first-child > th {
        border-top: 0;
        font-weight: normal;
    }
")),

  # App title
  titlePanel(title = "RIVAGE Island Vulnerability Index"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      uiOutput("classSelection"),
      uiOutput("archipSelection"),
      uiOutput("islandSelection"),
      HTML('<b>Component weights:</b>'),
      DT::DTOutput("weight_out"),
      HTML("<br>"),
      HTML('<b>Exposure weights:</b>'),
      DT::DTOutput("weight_e"),
      HTML("<br>"),
      HTML('<b>Adaptive Capacity weights:</b>'),
      DT::DTOutput("weight_ac"),
      HTML("<br>"),
      actionButton("run", label = "Display results"),
      HTML("<br>"),
      uiOutput("download_button")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      DT::dataTableOutput("sel_df"),
      br(),
      htmlOutput("sci_name"),
      HTML("<br>"),
      HTML('<b>Exposure</b>'),
      splitLayout(DT::dataTableOutput("radar_data_E_df"),
                  plotOutput("radarplot_E"),
                  cellWidths = c("35%", "65%"),
                  cellArgs = list(style = "vertical-align: top")),
      HTML('<b>Sensitivity</b>'),
      splitLayout(DT::dataTableOutput("radar_data_S_df"),
                  plotOutput("radarplot_S"),
                  cellWidths = c("35%", "65%"),
                  cellArgs = list(style = "vertical-align: top")),
      HTML('<b>Adaptive Capacity</b>'),
      splitLayout(DT::dataTableOutput("radar_data_AC_df"),
                  plotOutput("radarplot_AC"),
                  cellWidths = c("35%", "65%"),
                  cellArgs = list(style = "vertical-align: top")),
      HTML("<br>"),
      plotOutput("graph_all")

    )
    
  )
)
