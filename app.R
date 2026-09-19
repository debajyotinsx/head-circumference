# Shinylive/Chromium download compatibility (Posit documented workaround).
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  if (grepl("emscripten", R.version$os)) tag$attribs$download <- NULL
  tag
}

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(lubridate)
library(shinyjs) # Required for robust web interactions

# ---------- 1. ROBUST DATA LOADING ----------

# Define column types explicitly to prevent parsing errors
load_who_data <- function(filename) {
  # check for file existence
  if (!file.exists(filename)) {
    stop(paste0("CRITICAL ERROR: '", filename, "' not found. Ensure this file is uploaded with your app deployment."))
  }
  
  df <- read.csv(filename)
  
  # Normalize column names
  colnames(df)[1] <- "age_month"
  
  # Validate structure
  if(!all(c("age_month", "L", "M", "S") %in% colnames(df))) {
    stop(paste("File", filename, "is corrupt or missing required columns (L, M, S)."))
  }
  
  return(df %>% select(age_month, L, M, S))
}

# Load data with Try-Catch for server logs
tryCatch({
  boys_lms <- load_who_data("tab_hcfa_boys_p_0_5.csv")
  girls_lms <- load_who_data("tab_hcfa_girls_p_0_5.csv")
}, error = function(e) {
  stop(paste("App failed to start: ", e$message))
})

# ---------- 2. LMS MATH HELPERS ----------

lms_value <- function(L, M, S, z) {
  ifelse(L == 0, M * exp(S * z), M * (1 + L * S * z)^(1 / L))
}

lms_z <- function(x, L, M, S) {
  ifelse(L == 0, log(x / M) / S, ((x / M)^L - 1) / (L * S))
}

pct_labels <- c("P3","P15","P50","P85","P97")
pct_z <- stats::qnorm(c(.03,.15,.50,.85,.97))
names(pct_z) <- pct_labels

# ---------- 3. UI ----------

ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  
  # CSS for Mobile Responsiveness and Printing
  tags$head(
    tags$style(HTML("
      /* Make plot responsive */
      .shiny-plot-output { max-width: 100%; height: auto; }
      
      /* PRINT STYLES: When user prints, hide sidebars and buttons */
      @media print {
        .col-sm-3, .btn, .dataTables_filter, .dataTables_info, .dataTables_paginate { display: none !important; }
        .col-sm-9 { width: 100% !important; margin: 0; padding: 0; }
        #section-to-print { display: block; }
        body { font-size: 12pt; }
      }
    ")),
    # JavaScript to warn user before refreshing/closing tab (Data Loss Prevention)
    tags$script(HTML("
      window.onbeforeunload = function() {
        return 'Are you sure you want to leave? Your entered data will be lost.';
      };
    "))
  ),
  
  titlePanel("WHO Head Circumference Tracker (0–5 Years)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Patient Details"),
      radioButtons("sex", "Sex", choices = c("Boy" = "M", "Girl" = "F"), inline = TRUE, selected = "M"),
      tags$hr(),
      h4("Add Measurement"),
      radioButtons("age_mode", "Age Mode", choices = c("Manual (Months)" = "manual", "By Dates" = "dates"), inline = TRUE, selected = "manual"),
      
      conditionalPanel(
        condition = "input.age_mode == 'manual'",
        numericInput("age_in", "Age (months)", value = 0, min = 0, max = 60, step = 0.5)
      ),
      conditionalPanel(
        condition = "input.age_mode == 'dates'",
        dateInput("dob_in", "Date of Birth", value = Sys.Date() - 30),
        dateInput("meas_in", "Measurement Date", value = Sys.Date()),
        helpText("Calculates exact age.")
      ),
      
      numericInput("hc_in", "Head Circumference (cm)", value = 35, min = 20, max = 65, step = 0.1),
      
      # Output for error messages
      textOutput("error_msg"),
      tags$style("#error_msg { color: red; font-weight: bold; margin-bottom: 10px; }"),
      
      actionButton("add_row", "Add Data Point", class = "btn-primary"),
      br(), br(),
      
      h4("Controls"),
      sliderInput("range", "Zoom Age Range (Months)", min = 0, max = 60, value = c(0, 60), step = 1),
      checkboxInput("show_labels", "Show Data Labels", value = TRUE),
      tags$hr(),
      fluidRow(
        column(6, actionButton("delete_sel", "Delete Selected", class = "btn-warning btn-sm")),
        column(6, actionButton("clear_all", "Clear All", class = "btn-danger btn-sm"))
      ),
      width = 3
    ),
    
    mainPanel(
      id = "main_content",
      
      # --- Plot Area ---
      plotOutput("plot", height = "600px"),
      
      br(),
      fluidRow(
        column(12, 
               downloadButton("dl_chart", "Download Chart (.png)", class = "btn-info"),
               # Robust Print: Uses window.print() and CSS hiding
               actionButton("print_page", "Print View", icon = icon("print"), class = "btn-default", onclick = "window.print();")
        )
      ),
      
      tags$hr(),
      
      # --- Table Area ---
      h4("Patient Data History"),
      DTOutput("user_table")
    )
  )
)

# ---------- 4. SERVER ----------

server <- function(input, output, session) {
  
  user_df <- reactiveVal(data.frame(age_month = numeric(0), hc_cm = numeric(0)))
  
  lms_tbl <- reactive({ if (input$sex == "M") boys_lms else girls_lms })
  
  # Validation Helper
  validate_inputs <- function() {
    if (input$age_mode == "dates") {
      if (input$meas_in < input$dob_in) return("Error: Measurement date cannot be before Birth date.")
      if (input$meas_in > Sys.Date()) return("Warning: Measurement date is in the future.")
    }
    if (is.na(input$hc_in) || input$hc_in < 30 || input$hc_in > 60) return("Error: Please check Head Circumference value.")
    return(NULL)
  }
  
  lms_dense <- reactive({
    df <- lms_tbl()
    grid_age <- seq(0, 60, by = 0.1)
    L_s <- stats::spline(df$age_month, df$L, xout = grid_age, method = "natural")$y
    M_s <- stats::spline(df$age_month, df$M, xout = grid_age, method = "natural")$y
    S_s <- stats::spline(df$age_month, df$S, xout = grid_age, method = "natural")$y
    tibble(age_month = grid_age, L = L_s, M = M_s, S = S_s)
  })
  
  ref_curves <- reactive({
    lms <- lms_dense()
    bind_rows(lapply(names(pct_z), function(pn) {
      z <- pct_z[[pn]]
      tibble(age_month = lms$age_month, percentile = pn, hc_cm = lms_value(lms$L, lms$M, lms$S, z))
    }))
  })
  
  # Error Message Output
  output$error_msg <- renderText({
    validate_inputs()
  })
  
  observeEvent(input$add_row, {
    # Check validation
    err <- validate_inputs()
    if (!is.null(err) && grepl("Error", err)) return() # Stop if critical error
    
    age <- if (input$age_mode == "manual") {
      as.numeric(input$age_in)
    } else {
      time_length(interval(as_date(input$dob_in), as_date(input$meas_in)), unit = "months")
    }
    
    hc <- as.numeric(input$hc_in)
    
    # Robust Check for Duplicates (Avoid adding exact same point twice)
    current_data <- user_df()
    if (nrow(current_data) > 0) {
      is_duplicate <- any(abs(current_data$age_month - age) < 0.01 & abs(current_data$hc_cm - hc) < 0.01)
      if(is_duplicate) {
        showNotification("Duplicate entry detected.", type = "warning")
        return()
      }
    }
    
    updated_df <- bind_rows(user_df(), data.frame(age_month = round(age, 2), hc_cm = round(hc, 1))) %>%
      arrange(age_month)
    
    user_df(updated_df)
    showNotification("Data point added!", type = "message")
  })
  
  observeEvent(input$delete_sel, {
    sel <- input$user_table_rows_selected
    if (length(sel)) {
      user_df(user_df()[-sel, , drop = FALSE])
    } else {
      showNotification("Select a row in the table to delete.", type = "warning")
    }
  })
  
  observeEvent(input$clear_all, {
    # Added simple confirmation via modal
    showModal(modalDialog(
      title = "Clear All Data",
      "Are you sure you want to delete all entries? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Yes, Clear All", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    user_df(data.frame(age_month = numeric(0), hc_cm = numeric(0)))
    removeModal()
  })
  
  user_with_stats <- reactive({
    pts <- user_df()
    if (nrow(pts) == 0) return(NULL)
    lms_data <- lms_tbl()
    
    # Using approx is safer/faster for single points than spline if points are dense, 
    # but spline is smoother. Adding tryCatch for safety.
    tryCatch({
      L_i <- stats::spline(lms_data$age_month, lms_data$L, xout = pts$age_month)$y
      M_i <- stats::spline(lms_data$age_month, lms_data$M, xout = pts$age_month)$y
      S_i <- stats::spline(lms_data$age_month, lms_data$S, xout = pts$age_month)$y
      z <- lms_z(pts$hc_cm, L_i, M_i, S_i)
      pct <- pnorm(z) * 100
      pts %>% mutate(z_score = z, percentile = pct, label = paste0("P", round(pct, 1)))
    }, error = function(e) return(NULL))
  })
  
  plot_obj <- reactive({
    # Validate range to prevent crash if user types nonsense in slider (rare but possible via API)
    req(input$range)
    
    ref <- ref_curves() %>% 
      filter(age_month >= input$range[1], age_month <= input$range[2])
    
    p <- ggplot() +
      geom_line(data = ref, aes(x = age_month, y = hc_cm, color = percentile), linewidth = 1) +
      scale_color_manual(values = c("P3"="red", "P15"="orange", "P50"="darkgreen", "P85"="orange", "P97"="red"),breaks = pct_labels)  +
      scale_x_continuous(breaks = seq(0, 60, by = 6), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(30, 60, by = 2), expand = c(0, 0)) +
      coord_cartesian(xlim = input$range, ylim = c(30, 58)) +
      labs(title = paste("Head Circumference:", ifelse(input$sex == "M", "Boys", "Girls")), 
           x = "Age (Months)", y = "Head Circumference (cm)") +
      theme_bw(base_size = 14) + # Slightly smaller font for better mobile fit
      theme(legend.position = "bottom")
    
    pts <- user_with_stats()
    if (!is.null(pts)) {
      p <- p + geom_point(data = pts, aes(x = age_month, y = hc_cm), size = 4, fill = "blue", shape = 21, color = "white")
      if (input$show_labels) p <- p + geom_text(data = pts, aes(x = age_month, y = hc_cm, label = label), vjust = -1.2)
    }
    p
  })
  
  output$plot <- renderPlot({
    # Use validate here to give a friendly message if data loading fails or range is invalid
    validate(need(nrow(boys_lms) > 0, "Error loading reference data."))
    plot_obj()
  })
  
  output$dl_chart <- downloadHandler(
    filename = function() { paste0("GrowthChart_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = plot_obj(), device = "png", width = 10, height = 7) }
  )
  
  output$user_table <- renderDT({
    pts <- user_with_stats()
    if (is.null(pts)) {
      df_disp <- data.frame("Age"=numeric(0), "HC"=numeric(0), "Z"=numeric(0), "Pct"=character(0))
    } else {
      df_disp <- pts %>% transmute(
        Age = round(age_month, 2),
        HC = round(hc_cm, 1),
        Z = round(z_score, 2),
        Pct = paste0(round(percentile, 1), "%")
      )
    }
    
    datatable(df_disp, selection = "multiple", rownames = FALSE, extensions = 'Buttons',
              options = list(paging = FALSE, dom = 'Bfrtip',
                             buttons = list('csv', 'excel', 'print')))
  })
}

shinyApp(ui, server)
