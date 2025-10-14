library(shiny)
library(magrittr)

# setwd("C:/Users/gabri/Documents/rivage_app")

library(ggplot2)

data_all <- readRDS("30_all_VU_components_45_isl_266_BM_sp.rds")
source("Calculate_components_VU_FUN.R")

E_cols <- 
  c("sed_tot_med", 
    "mean_HM_change", 
    "rdens_osm",
    "nb_alien", 
    "alien_vert_cover")

S_cols <- 
  c("aoh_km2", 
    "nb_diet", 
    "nb_hab", 
    "gen_length_y")

AC_cols <- 
  c('Area', 
    'max_elev', 
    'mean_tri', 
    'PA_prop', 
    'fred',
    'dispersal')

filter_var <-
  function(x, val) {
    if (is.numeric(x)) {
      !is.na(x) & x >= val[1] & x <= val[2]
    } else if (is.factor(x)|is.character(x)) {
      x %in% val
    } else {
      # No control, so don't filter
      TRUE
    }
  }

create_beautiful_radarchart <- function(data, color = "#00AFBB",
                                        vlabels = colnames(data), vlcex = 1.3,
                                        caxislabels = NULL, title = NULL){
  fmsb::radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey",
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title,
    centerzero = TRUE
  )
}

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  weight_out <-
    reactiveValues(data = {
      data.frame(`Exposure` = 1,
                 `Sensitivity` = 1,
                 `Adaptive Capacity` = 1)
    })
  
  weight_e <-
    reactiveValues(data = {
      data.frame(`Climate Change` = 1,
                 `Land Use Change` = 1,
                 `Biological Invasions` = 1)
    })
  
  weight_ac <-
    reactiveValues(data = {
      data.frame(`Abiotic Components` = 1,
                 `Biotic Components` = 1)
    })
  
  #output the datatable based on the dataframe (and make it editable)
  
  output$weight_out <- DT::renderDT({
    
    DT::datatable(weight_out$data,
                  editable = TRUE,
                  options = list(sDom  = 't',
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:2))
                  ),
                  rownames = FALSE)
    
  })
  
  output$weight_e <- DT::renderDT({
    
    DT::datatable(weight_e$data,
                  editable = TRUE,
                  options = list(sDom  = 't',
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:2))
                  ),
                  rownames = FALSE)
    
  })
  
  output$weight_ac <- DT::renderDT({
    
    DT::datatable(weight_ac$data,
                  editable = TRUE,
                  options = list(sDom  = 't',
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:1))
                  ),
                  rownames = FALSE)
    
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  
  observeEvent(input$weight_out_cell_edit, {
    #get values
    info = input$weight_out_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    weight_out$data[i,j+1] <- k
  })
  
  observeEvent(input$weight_e_cell_edit, {
    #get values
    info = input$weight_e_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    weight_e$data[i,j+1] <- k
  })
  
  observeEvent(input$weight_ac_cell_edit, {
    #get values
    info = input$weight_ac_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    weight_ac$data[i,j+1] <- k
  })
  
  # Selection input
  
  class_levs <-
    sort(unique(as.character(data_all$Class)))
  
  archip_levs <-
    sort(unique(as.character(data_all$Archip)))
  
  island_levs <-
    sort(unique(as.character(data_all$Island)))
  
  output$classSelection <- renderUI({
    selectizeInput("Class",
                   "Select class:",
                   choices = class_levs,
                   selected = class_levs,
                   multiple = TRUE)
  })
  
  output$archipSelection <- renderUI({
    selectizeInput("Archip",
                   "Select archipelago:",
                   choices = archip_levs,
                   selected = archip_levs,
                   multiple = TRUE)
  })
  
  # Create table
  
  data_sel <- reactive({
    
    selected_class <-
      filter_var(data_all$Class, input$Class)
    
    selected_archip <-
      filter_var(data_all$Archip, input$Archip)
    
    data_all_sel <-
      data_all[
        selected_class & selected_archip,]
    
    out <-
      vulnerabilityFUN(data_all_sel,
                       ac_abiotic_cols = c('Area', 'max_elev', 'mean_tri', 'PA_prop'),
                       ac_sp_cols = c('dispersal'),
                       ac_isl_cols = c('Area', 'max_elev', 'mean_tri', 'PA_prop', 'fred'),
                       lu_cols = c("mean_HM_change", "rdens_osm"),
                       cc_cols = "sed_tot_med",
                       weight_out = unlist(weight_out$data),
                       weight_e = unlist(weight_e$data),
                       weight_ac = unlist(weight_ac$data))
    
    isl_data <-
      data_all_sel[names(data_all_sel) %in% c("Archip",
                                              "Island",
                                              E_cols,
                                              S_cols,
                                              AC_cols,
                                              "ID")]

    out <- data.frame(out,
                      isl_data[match(out$ID, isl_data$ID),])
    
    out <- out[order(out$VU, decreasing = TRUE),]
    
    out
    
  })
  
  sel_df <- eventReactive(input$run, {
    data_sel()
  })
  
  output$sel_df <-
    DT::renderDataTable({
      
      out_table <- sel_df()
      
      names(out_table)[names(out_table) == "Archip"] <- "Archipelago"
      names(out_table)[names(out_table) == "sci_name"] <- "Species"
      names(out_table)[names(out_table) == "VU"] <- "Vulnerability"
      names(out_table)[names(out_table) == "VU_rank"] <- "Vulnerability Rank"
      names(out_table)[names(out_table) == "E"] <- "Exposure"
      names(out_table)[names(out_table) == "S"] <- "Sensitivity"
      names(out_table)[names(out_table) == "AC"] <- "Adaptive Capacity"
      
      out_table$Species <- paste0("<i>", out_table$Species, "</i>")
      
      DT::datatable(out_table[c(9,10,2:7)], 
                    rownames = FALSE, 
                    selection = "single", 
                    escape = FALSE) %>%
        DT::formatRound(columns = c(4,6,7,8),
                        digits = 3)
    })
  
  # Download data
  
  filename <-
    eventReactive(input$run, {
      function() {
        paste("vu3_", input$Class, "_", input$Archip, "_", input$Island, ".csv", sep = "")
      }
    })
  
  content <-
    eventReactive(input$run, {
      function(file) {
        write.csv(data_sel(), file, row.names = FALSE)
      }
    })
  
  output$downloadData <-
    downloadHandler(filename = filename(),
                    content = content())
  
  download_button <-
    eventReactive(input$run, {
      downloadButton("downloadData", "Download data table")
    })
  
  output$download_button <- renderUI(download_button())
  
  # Clickable table
  
  output$cell_clicked <- renderPrint({
    req(length(input$sel_df_cell_clicked) > 0)
    input$sel_df_cell_clicked$value
  })
  
  # Radar plot
  
  radar_data_E <-  eventReactive(input$sel_df_cell_clicked, {
    
    if(isTruthy(input$sel_df_cell_clicked$value)){
      
      data_all_sel <- sel_df()
      
      var_E_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% E_cols],
                   Value = unlist(data_all_sel[input$sel_df_rows_selected[1],
                                               names(data_all_sel) %in% E_cols]))
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_E_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% E_cols],
                   Value = NA)
      
    }
    
  })
  
  radar_data_E_ecdf <-  eventReactive(input$sel_df_cell_clicked, {
    
    if(isTruthy(input$sel_df_rows_selected[1])){
      
      data_all_sel <- sel_df()
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% E_cols]
      
      data_all_sel <- apply(data_all_sel, 2, function(x) ecdf(x)(x))
      
      out <- data_all_sel[input$sel_df_rows_selected[1],]
      
      out
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_E_df <-
        data_all_sel[1,names(data_all_sel) %in% E_cols]
      
      var_E_df[1,] <- 0
      
      var_E_df
      
    }
    
  })
  
  radar_data_S <-  eventReactive(input$sel_df_cell_clicked, {
    
    if(isTruthy(input$sel_df_cell_clicked$value)){
      
      data_all_sel <- sel_df()
      
      var_S_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% S_cols],
                   Value = unlist(data_all_sel[input$sel_df_rows_selected[1],
                                               names(data_all_sel) %in% S_cols]))
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_S_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% S_cols],
                   Value = NA)
      
    }
    
  })
  
  radar_data_S_ecdf <-  eventReactive(input$sel_df_cell_clicked, {
    
    if(isTruthy(input$sel_df_cell_clicked$value)){
      
      data_all_sel <- sel_df()
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% S_cols]
      
      data_all_sel <- apply(data_all_sel, 2, function(x) ecdf(x)(x))
      
      out <- data_all_sel[input$sel_df_rows_selected[1],]
      
      out
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_S_df <-
        data_all_sel[1,names(data_all_sel) %in% S_cols]
      
      var_S_df[1,] <- 0
      
      var_S_df
      
    }
    
  })
  
  radar_data_AC <-  eventReactive(input$sel_df_cell_clicked, {
    
    if(isTruthy(input$sel_df_cell_clicked$value)){
      
      data_all_sel <- sel_df()
      
      var_AC_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% AC_cols],
                   Value = unlist(data_all_sel[input$sel_df_rows_selected[1],
                                               names(data_all_sel) %in% AC_cols]))
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_AC_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% AC_cols],
                   Value = NA)
      
    }
    
  })
  
  radar_data_AC_ecdf <-  eventReactive(input$sel_df_cell_clicked, {
    
    if(isTruthy(input$sel_df_cell_clicked$value)){
      
      data_all_sel <- sel_df()
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% AC_cols]
      
      data_all_sel <- apply(data_all_sel, 2, function(x) ecdf(x)(x))
      
      out <- data_all_sel[input$sel_df_rows_selected[1],]
      
      out
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_AC_df <-
        data_all_sel[1,names(data_all_sel) %in% AC_cols]
      
      var_AC_df[1,] <- 0
      
      var_AC_df
      
    }
    
  })
  
  output$radar_data_E_df <-
    DT::renderDataTable({
      
      radar_table <- radar_data_E()
      
      radar_table$Variable[radar_table$Variable == "sed_tot_med"] <- "Climate Change (SED)"
      radar_table$Variable[radar_table$Variable == "mean_HM_change"] <- "Human Modification"
      radar_table$Variable[radar_table$Variable == "rdens_osm"] <- "Road Density (km/km2)"
      radar_table$Variable[radar_table$Variable == "nb_alien"] <- "Number of Alien Species"
      radar_table$Variable[radar_table$Variable == "alien_vert_cover"] <- "Coverage of Alien Species"
      
      DT::formatRound(
        DT::datatable(radar_table,
                      options = list(dom = 't'),
                      rownames = FALSE),
        columns = 2,
        digits = 3)
      
      
    })
  
  output$radar_data_S_df <-
    DT::renderDataTable({
      
      radar_table <- radar_data_S()
      
      radar_table$Variable[radar_table$Variable == "aoh_km2"] <- "Area of habitat (km2)"
      radar_table$Variable[radar_table$Variable == "nb_diet"] <- "Dietary Breadth"
      radar_table$Variable[radar_table$Variable == "nb_hab"] <- "Habitat Use Breadth"
      radar_table$Variable[radar_table$Variable == "gen_length_y"] <- "Generation Length (years)"
      
      DT::formatRound(
        DT::datatable(radar_table,
                      options = list(dom = 't'),
                      rownames = FALSE),
        columns = 2,
        digits = 3)
    })
  
  output$radar_data_AC_df <-
    DT::renderDataTable({
      
      radar_table <- radar_data_AC()
      
      radar_table$Variable[radar_table$Variable == "Area"] <- "Island Area (km2)"
      radar_table$Variable[radar_table$Variable == "max_elev"] <- "Maximum Island Elevation (m2)"
      radar_table$Variable[radar_table$Variable == "mean_tri"] <- "Terrain Ruggednness"
      radar_table$Variable[radar_table$Variable == "PA_prop"] <- "Protected Area Coverage"
      radar_table$Variable[radar_table$Variable == "fred"] <- "Functional Redundancy"
      radar_table$Variable[radar_table$Variable == "dispersal"] <- "Dispersal Capacity"
      
      DT::formatRound(
        DT::datatable(radar_table,
                      options = list(dom = 't'),
                      rownames = FALSE),
        columns = 2,
        digits = 3)
    })
  
  output$radarplot_E <- renderPlot({
    
    radar_data_df <- radar_data_E_ecdf()
    
    names(radar_data_df)[names(radar_data_df) == "sed_tot_med"] <- "Climate Change (SED)"
    names(radar_data_df)[names(radar_data_df) == "mean_HM_change"] <- "Human Modification"
    names(radar_data_df)[names(radar_data_df) == "rdens_osm"] <- "Road Density (km/km2)"
    names(radar_data_df)[names(radar_data_df) == "nb_alien"] <- "Number of Alien Species"
    names(radar_data_df)[names(radar_data_df) == "alien_vert_cover"] <- "Coverage of Alien Species"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    # chart_df <-
    #   rbind(Max = 1,
    #         Min = 0,
    #         radar_data_df())
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", length(E_cols)))
    
  }, res = 72)
  
  output$radarplot_S <- renderPlot({
    
    radar_data_df <- radar_data_S_ecdf()
    
    names(radar_data_df)[names(radar_data_df) == "aoh_km2"] <- "Area of habitat (km2)"
    names(radar_data_df)[names(radar_data_df) == "nb_diet"] <- "Dietary Breadth"
    names(radar_data_df)[names(radar_data_df) == "nb_hab"] <- "Habitat Use Breadth"
    names(radar_data_df)[names(radar_data_df) == "gen_length_y"] <- "Generation Length (years)"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    # chart_df <-
    #   rbind(Max = 1,
    #         Min = 0,
    #         radar_data_df())
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", length(S_cols)))
    
  }, res = 72)
  
  output$radarplot_AC <- renderPlot({
    
    radar_data_df <- radar_data_AC_ecdf()
    
    names(radar_data_df)[names(radar_data_df) == "Area"] <- "Island Area (km2)"
    names(radar_data_df)[names(radar_data_df) == "max_elev"] <- "Maximum Island Elevation (m2)"
    names(radar_data_df)[names(radar_data_df) == "mean_tri"] <- "Terrain Ruggednness"
    names(radar_data_df)[names(radar_data_df) == "PA_prop"] <- "Protected Area Coverage"
    names(radar_data_df)[names(radar_data_df) == "fred"] <- "Functional Redundancy"
    names(radar_data_df)[names(radar_data_df) == "dispersal"] <- "Dispersal Capacity"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    # chart_df <-
    #   rbind(Max = 1,
    #         Min = 0,
    #         radar_data_df())
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", length(AC_cols)))
    
  }, res = 72)
  
  # Species name
  
  sci_name <- reactive(
    if(isTruthy(input$run)){
      if(isTruthy(input$sel_df_cell_clicked$value)){
        
        sel_df_name <- sel_df()
        
        paste0("<b>", 
               sel_df_name$sci_name[input$sel_df_rows_selected[1]], 
               " at ",
               sel_df_name$Island[input$sel_df_rows_selected[1]],
               "</b>")
        
      } else {"<b>Click a species name</b>"}
    }
    
  )
  
  output$sci_name <- renderText(sci_name())
  
  output$graph_all <- renderPlot({
    
      VU_sp_isl_info <- sel_df()
      # calculate mean, median and sd value per island
      VU_median_mean <-
        VU_sp_isl_info |>
        dplyr::group_by(Archip, Island) |>
        dplyr::summarise(
          Median = median(VU),
          Mean = mean(VU),
          SD = sd(VU)) |> dplyr::ungroup()
      isl_order <- VU_median_mean$Island[match(sort(VU_median_mean$Mean), VU_median_mean$Mean)]
      VU_median_mean$Island_order <- factor(VU_median_mean$Island, levels = isl_order)
      VU_sp_isl_info$Island_order <- factor(VU_sp_isl_info$Island, levels = isl_order)
      
      ggplot() +
        geom_point(data = VU_sp_isl_info, aes(x = VU, y = Island_order),
                   col = "grey", alpha = .2)+
        geom_point(data = as.data.frame(VU_median_mean), 
                   aes(x=Mean, y=Island, col=Archip), size = 2) +
        geom_errorbar(data = as.data.frame(VU_median_mean), 
                      aes(x=Mean, xmin=Mean-SD, xmax=Mean+SD, y=Island_order, col=Archip)) +
        theme_bw() +
        ggtitle('Vulnerability') +
        xlab('Vulnerability per species per island (Mean +/- SD)') +
        # scale_color_manual("Archipelago", values = archip_col) +
        theme(legend.position = "right") +
        ylab('Island name') +
        labs(color = "Archipelago")
      
    })
  
  
  
}
