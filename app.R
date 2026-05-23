library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(DT)
library(dplyr)
library(plotly)        # Required for: plotlyOutput(), render_funder_bar(), etc.
library(shinyjs)       # Highly recommended for managing UI states and hiding/showing inputs

# ==========================================
# 1. CLOUD DATABASE CONNECTION
# ==========================================
get_db_conn <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = "aws-1-eu-central-1.pooler.supabase.com",
    port     = 5432,
    dbname   = "postgres",
    user     = "postgres.mvxbhugcfpxwmyblkgqc",
    password = Sys.getenv("SUPABASE_PASSWORD")
    )
}

# Ensure baseline database structures exist seamlessly
# Ensure baseline database structures exist seamlessly
conn = get_db_conn()
dbExecute(conn, "CREATE TABLE IF NOT EXISTS funding_sources (id SERIAL PRIMARY KEY, funder_name TEXT, grant_name TEXT, amount REAL, pillar TEXT, country TEXT)")
dbExecute(conn, "CREATE TABLE IF NOT EXISTS allocations (id SERIAL PRIMARY KEY, grant_id INTEGER, pillar TEXT, allocated_amount REAL)")
dbExecute(conn, "CREATE TABLE IF NOT EXISTS activity (id SERIAL PRIMARY KEY, allocation_id TEXT, activity_name TEXT, activity_decript TEXT, activity_type TEXT, pillar TEXT, budget REAL, amount_used REAL, date_recorded TEXT)")
dbDisconnect(conn)

pillars_list <- c("Coordination", "Surveillance", "Case Management", 
                  "Laboratory", "IPC", "CES",
                  "RCCE", "Logistics & Supply Chain", "Research", "Vaccination")

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  if (len > 0) {
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, "_", i), ...))
    }
  }
  inputs
}
# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- dashboardPage(
  header = dashboardHeader(title = "IMST Ebola Response Tracker", titleWidth = 720),
  sidebar = dashboardSidebar(
    width = 320,
    uiOutput("auth_sidebar"),
    uiOutput("sidebar_menu_items")
  ),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
    /* 1. Fix the Header and Sidebar */
    .main-header { position: fixed !important; width: 100%; }
    .main-sidebar { position: fixed !important; height: 100vh !important; }
    
    /* 2. Constrain the content area to scroll independently */
    .content-wrapper { 
      margin-top: 50px !important; 
      height: calc(100vh - 50px) !important; 
      overflow-y: auto !important; 
    }
    
    /* 3. Existing brand colors */
    .skin-blue .main-header .navbar, .skin-blue .main-header .logo { background-color: #006B3F !important; color: #ffffff !important; font-weight: bold; }
    .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #004D2E !important; }
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { border-left-color: #D4AF37 !important; }
    .bg-green-brand { background-color: #006B3F !important; color: white !important; }
    .bg-blue-brand { background-color: #008DC9 !important; color: white !important; }
    .bg-gold-brand { background-color: #D4AF37 !important; color: black !important; }

    /* 4. NEW: Gallery Card Styles */
    .card-pirs { border-left: 10px solid #006838; margin-bottom: 25px; font-size: 0.85rem; background: #fff; padding: 15px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
    .card-label { font-weight: bold; color: #555; text-transform: uppercase; font-size: 0.7rem; display: block; margin-top: 5px; }
    .card-header-custom { display:flex; justify-content:space-between; align-items:center; margin-bottom: 10px; border-bottom: 1px solid #eee; padding-bottom: 5px; }
  "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              h2("Ebola Response Financial Overview"),
              
              # 1. NEW: Narrative section
              wellPanel(
                p("This dashboard provides a comprehensive view of the current funding, allocations, and expenditures ",
                  "for the IMST Ebola response. It tracks progress against targets and illustrates resource flows across activity streams."),
                tags$hr(style = "border-top: 1px solid #ccc;"),
                p(tags$i("Note: Currently, the IMST is developing its continental plan and will publish same soon."))
              ),
              
              # 2. Existing content
              fluidRow(
                valueBoxOutput("total_funding_box", width = 4),
                valueBoxOutput("total_allocated_box", width = 4),
                valueBoxOutput("total_used_box", width = 4)
              ),
              fluidRow(
                box(plotlyOutput("funder_bar"), width = 6),
                box(plotlyOutput("pillar_bar"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("sankey_flow"), width = 12)
              )
      ),
      ## Indicators review page
      tabItem(tabName = "indicators",
              h2("Performance Indicators (PIRS)"),
              p("Review the library of standard indicators for the Ebola response."),
              hr(),
              # Filters
              fluidRow(
                box(title = "Indicator Filters", width = 12, status = "info", solidHeader = TRUE,
                    column(6, selectInput("f_pillar", "Filter by Pillar:", 
                                          choices = c("All", pillars_list))),
                    column(6, selectInput("f_level", "Filter by Results Level:", 
                                          choices = c("All", "Impact", "Outcome", "Output")))
                )
              ),
              uiOutput("indicator_cards_ui")
      ),
      
      # ... (Funding, Allocations, and Activity tabs remain exactly as they were) ...
      tabItem(tabName = "funding",
              fluidRow(
                box(title = "Register New Funding Source", width = 4, status = "primary", solidHeader = TRUE,
                    textInput("funder_name", "Funder Name:"),
                    textInput("grant_name", "Grant Name:"),
                    numericInput("grant_amount", "Total Grant Amount ($):", value = 0, min = 0),
                    selectInput("grant_pillar", "Pillars of Coverage (Select Multiple):", choices = pillars_list, multiple = TRUE),
                    textInput("grant_country", "Country of Coverage:"),
                    actionButton("save_funding", "Save Funding Source", class = "btn-success", icon = icon("plus"))
                ),
                box(title = "Registered Grants", width = 8, status = "primary", solidHeader = TRUE,
                    DTOutput("funding_table")
                )
              )
      ),
      tabItem(tabName = "allocations",
              fluidRow(
                box(title = "Allocate Funding to Pillars", width = 4, status = "warning", solidHeader = TRUE,
                    selectInput("alloc_grant_select", "Select Source Grant:", choices = NULL),
                    selectInput("alloc_pillar", "Target Pillar:", choices = pillars_list),
                    numericInput("alloc_amount", "Allocated Amount ($):", value = 0, min = 0),
                    actionButton("save_allocation", "Save Allocation", class = "btn-warning", icon = icon("share-alt")),
                ),
                box(title = "Pillar Allocations Breakdown", width = 8, status = "warning", solidHeader = TRUE,
                    DTOutput("allocations_table")
                )
              )
      ),
      tabItem(tabName = "activity",
              fluidRow(
                box(title = "Log Activity", width = 4, status = "danger", solidHeader = TRUE,
                    selectInput("act_alloc_select", "Select Active Grant Allocation:", choices = NULL),
                    textInput("act_name", "Activity Name:"),
                    textInput("act_desc", "Description:"),
                    selectInput("act_type", "Activity Type:", choices = c("Meeting", "Document development", "Supply", "Deployment", "Training", "Operational")),
                    numericInput("act_budget", "Activity Budget ($):", value = 0, min = 0),
                    numericInput("act_used", "Amount Used ($):", value = 0, min = 0),
                    dateInput("act_date", "Date of Activity:", value = Sys.Date()),
                    actionButton("save_activity", "Log Activity", class = "btn-danger", icon = icon("shopping-cart"))
                ),
                box(title = "Activity Logs", width = 8, status = "danger", solidHeader = TRUE,
                    DTOutput("activity_table")
                )
              )
      ),
      tabItem(tabName = "reporting",
              fluidRow(
                box(title = "Indicator Reporting", width = 12,
                    # Add the filter here, inside the box
                    selectInput("report_filter", "Filter by Status:",
                                choices = c("All" = "all", 
                                            "Reported" = "reported", 
                                            "Not Reported" = "not_reported"),
                                width = "300px"), # Added width for better look
                    
                    DTOutput("needle_table")
                )
              )
      )
    )
  )
)
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  ##### Source file to be read for visuals
  
  source("viz_logic.R") # Load the external file
  
  output$funder_bar <- render_funder_bar(funding_data())
  output$pillar_bar <- render_pillar_bar(allocations_data())
  output$sankey_flow <- render_sankey(funding_data(), allocations_data(), activity_data())
  
  
  refresh_trigger <- reactiveVal(0)
  editing_id <- reactiveVal(NULL)
  observeEvent(input$tabs, {
    if (input$tabs == "funding") {
      updateTextInput(session, "funder_name", value = "")
      updateTextInput(session, "grant_name", value = "")
      updateNumericInput(session, "grant_amount", value = 0)
      updateSelectInput(session, "grant_pillar", selected = character(0))
      updateTextInput(session, "grant_country", value = "")
    }
  }, ignoreInit = TRUE)
  user_authenticated <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  
  output$auth_sidebar <- renderUI({
    if (!user_authenticated()) {
      div(style = "padding: 15px;",
          passwordInput("login_access", "Enter Access Key:"),
          actionButton("login_btn", "Login", class = "btn-primary btn-block"))
    } else {
      div(style = "padding: 15px; text-align: center; color: #D4AF37;",
          h4(paste("User:", current_user())),
          actionButton("logout_btn", "Logout", class = "btn-danger btn-xs"))
    }
  })
  
  output$sidebar_menu_items <- renderUI({
    menu_list <- list(menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")))
    if (user_authenticated()) {
      menu_list <- append(menu_list, list(
        menuItem("1. Indicators Library", tabName = "indicators", icon = icon("list-ol")),
        menuItem("2. Register Funding", tabName = "funding", icon = icon("hand-holding-usd")),
        menuItem("3. Pillar Allocations", tabName = "allocations", icon = icon("pie-chart")),
        menuItem("4. Activity Tracker", tabName = "activity", icon = icon("file-invoice-dollar")),
        menuItem("5. Indicator Reproting", tabName = "reporting", icon = icon("chart-line"))
      ))
    }
    do.call(sidebarMenu, menu_list)
  })
  
  # --- SERVER LOGIC FOR GALLERY CARDS ---
  output$indicator_cards_ui <- renderUI({
    # 1. Get the data from your database (using your existing reactive)
    data <- pirs_data() 
    
    # 2. Safety check: does data exist?
    if(is.null(data) || nrow(data) == 0) return(h4("No records found in the database."))
    
    # 3. Apply Filters
    if(input$f_pillar != "All") data <- data %>% filter(pillar == input$f_pillar)
    if(input$f_level != "All") data <- data %>% filter(results_level == input$f_level)
    
    if(nrow(data) == 0) return(p("No indicators found for these filters."))
    
    # 4. Generate the cards
    card_list <- lapply(1:nrow(data), function(i) {
      div(class = "card-pirs",
          div(class = "card-header-custom", 
              tags$b(data$id[i]), 
              span(class="badge bg-warning text-dark", data$indicator_tier[i])
          ),
          h5(style="color:#006838; font-weight:bold;", data$indicator[i]),
          layout_column_wrap(width = 1/2,
                             div(span(class="card-label", "Level"), data$results_level[i]),
                             div(span(class="card-label", "Frequency"), data$frequency[i])
          ),
          hr(),
          span(class="card-label", "Definition"), p(data$definition[i])
      )
    })
    
    # 5. Arrange cards in a nice 2-column grid
    split_indices <- split(seq_along(card_list), ceiling(seq_along(card_list) / 2))
    lapply(split_indices, function(indices) fluidRow(column(6, card_list[indices])))
  })
  
  observeEvent(input$login_btn, {
    conn <- get_db_conn()
    user_data <- dbGetQuery(conn, "SELECT name FROM users WHERE access = $1", list(input$login_access))
    dbDisconnect(conn)
    if (nrow(user_data) > 0) {
      user_authenticated(TRUE)
      current_user(user_data$name[1])
      showNotification("Login Successful!", type = "message")
    } else {
      showNotification("Access Denied", type = "error")
    }
  })
  ###########################################################
  ### Indicator reporting 
  # 1. SERVER-SIDE MEMORY (Define once in your server function)
  active_itt_id <- reactiveVal(NULL)
  
  # 2. MODAL BUILDER (Simplified for one button)
  # --- REVISED MODAL BUILDER ---
  show_itt_modal <- function(itt_id) {
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    # ALIASING IS THE KEY: We give them unique names here
    query <- "
    SELECT i.id, i.value as itt_value, i.disaggregation as itt_disagg, i.comments, 
           a.activity_name, p.indicator, p.disaggregation as pirs_instructions
    FROM itt i 
    LEFT JOIN activity a ON i.activity_id = a.id 
    LEFT JOIN pirs p ON i.pirs_id = p.id 
    WHERE i.id = $1"
    
    record <- dbGetQuery(conn, query, list(itt_id))
    
    showModal(modalDialog(
      title = "Edit Indicator",
      size = "l",
      div(style="background:#f4f4f4; padding:10px; margin-bottom:15px; border-left:4px solid #006838;",
          tags$b("Activity: "), record$activity_name, br(),
          tags$b("Indicator: "), record$indicator
      ),
      
      # Use the Aliased names here
      textInput("itt_val", "Value/Progress:", value = record$itt_value),
      
      div(style="margin-bottom: 5px; font-weight: bold; color: #555;", 
          "Instructions: ", span(style="color: #008DC9;", record$pirs_instructions)
      ),
      
      textAreaInput("itt_disag", "Disaggregation Details:", value = record$itt_disagg, 
                    placeholder = record$pirs_instructions),
      
      textAreaInput("itt_comments", "Comments:", value = record$comments),
      
      footer = tagList(modalButton("Close"), actionButton("save_itt_update", "Save Changes", class = "btn-success"))
    ))
  }  
  # 3. TABLE RENDERING (Using one button per row)
  output$needle_table <- renderDT({
    refresh_trigger() 
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    # Ensure we handle NULL if the page is still loading
    current_filter <- if (is.null(input$report_filter)) "all" else input$report_filter
    
    # Determine the SQL filter
    filter_sql <- switch(current_filter,
                         # Reported: Value is not null, not just empty, and not just whitespace
                         "reported"     = "AND (i.value IS NOT NULL AND TRIM(i.value) <> '')",
                         
                         # Not Reported: Everything else (NULL, empty string, or just spaces)
                         "not_reported" = "AND (i.value IS NULL OR TRIM(i.value) = '')",
                         
                         "all"          = ""
    )
    
    # Inject the filter_sql into your query
    query <- sprintf("
    SELECT i.id, a.activity_name, p.indicator, i.value, i.disaggregation, i.comments 
    FROM itt i 
    LEFT JOIN activity a ON i.activity_id = a.id 
    LEFT JOIN pirs p ON i.pirs_id = p.id 
    WHERE a.move_ind::text = 'TRUE' %s", filter_sql)
    
    data <- dbGetQuery(conn, query)
    
    # Keep your existing edit button logic
    data$actions <- paste0(
      '<button class="btn btn-primary btn-sm" onclick="Shiny.setInputValue(\'edit_trigger\', \'', data$id, '\', {priority: \'event\'})">report</button>'
    )
    
    datatable(data, escape = FALSE, options = list(pageLength = 10))
  })  
  # 4. EVENT LISTENER
  observeEvent(input$edit_trigger, {
    active_itt_id(input$edit_trigger) # Store ID in memory
    show_itt_modal(input$edit_trigger)
  })
  
  # 5. SAVE LOGIC
  observeEvent(input$save_itt_update, {
    target_id <- active_itt_id()
    req(target_id)
    
    conn <- get_db_conn()
    # Update based on unique itt ID
    dbExecute(conn, 
              "UPDATE itt SET value=$1, disaggregation=$2, comments=$3 WHERE id=$4",
              list(input$itt_val, input$itt_disag, input$itt_comments, target_id))
    dbDisconnect(conn)
    
    removeModal()
    showNotification("Indicator updated successfully!", type = "message")
    refresh_trigger(refresh_trigger() + 1)
  })
  ################################################################################################
  ###   end of reporting
  observeEvent(input$logout_btn, {
    user_authenticated(FALSE)
    current_user(NULL)
  })
  # --- Resilient Reactive Core Data Pulls ---
  funding_data <- reactive({
    refresh_trigger()
    conn <- get_db_conn()
    df <- dbGetQuery(conn, "SELECT id, funder_name, grant_name, amount, pillar, country FROM funding_sources ORDER BY id DESC")
    dbDisconnect(conn)
    df
  })
  
  allocations_data <- reactive({
    refresh_trigger()
    conn <- get_db_conn()
    df <- tryCatch({
      raw_cols <- dbGetQuery(conn, "SELECT * FROM allocations LIMIT 1")
      amt_col <- if("allocated_amount" %in% names(raw_cols)) "allocated_amount" else "amount"
      
      query <- paste0("SELECT a.id, a.grant_id, f.funder_name, f.grant_name, a.pillar, 
                        a.", amt_col, " as allocated_amount 
                        FROM allocations a 
                        LEFT JOIN funding_sources f ON a.grant_id = f.id 
                        ORDER BY a.id DESC")
      dbGetQuery(conn, query)
    }, error = function(e) { 
      print(paste("Allocations read mapping exception:", e$message))
      return(data.frame()) 
    })
    dbDisconnect(conn)
    df
  })
  
  # --- Activity Data Reactive ---
  activity_data <- reactive({
    refresh_trigger()
    conn <- get_db_conn()
    df <- dbGetQuery(conn, "SELECT * FROM activity ORDER BY id DESC")
    dbDisconnect(conn)
    df
  })
  
  # --- NEW: Indicator Data Reactive ---
  pirs_data <- reactive({
    refresh_trigger() # This ensures it updates if you ever add/edit indicators
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM pirs")
  })
  
  # --- Save Activity Logic ---
  observeEvent(input$save_activity, {
    # 1. Validation: Ensure all fields are filled
    req(input$act_alloc_select, input$act_name, input$act_used)
    
    # 2. Defensive programming: Convert to integer and check validity
    alloc_id <- as.integer(input$act_alloc_select)
    if (is.na(alloc_id)) {
      showNotification("Error: Invalid Allocation ID selected.", type = "error")
      return()
    }
    
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    # 3. Retrieve the pillar name
    a_df <- allocations_data()
    p_name <- a_df$pillar[a_df$id == alloc_id]
    
    # 4. Attempt the insert with a tryCatch to catch the exact error
    tryCatch({
      dbExecute(conn, 
                "INSERT INTO activity (allocation_id, activity_name, activity_decript, activity_type, pillar, budget, amount_used, date_recorded) 
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
                list(alloc_id, input$act_name, input$act_desc, input$act_type, p_name, input$act_budget, input$act_used, as.character(input$act_date))
      )
      
      updateNumericInput(session, "act_used", value = 0)
      updateTextInput(session, "act_name", value = "")
      showNotification("Activity logged successfully!", type = "message")
      refresh_trigger(refresh_trigger() + 1)
      
    }, error = function(e) {
      # This will print the actual error to your R Console instead of crashing
      print(paste("Database Insert Error:", e$message))
      showNotification(paste("Database Error:", e$message), type = "error")
    })
  })
  # Dynamic Dropdown Mapping for Allocations View
  observe({
    df <- funding_data()
    if (nrow(df) > 0) {
      choices <- setNames(df$id, paste(df$funder_name, "-", df$grant_name))
      updateSelectInput(session, "alloc_grant_select", choices = choices)
    } else {
      updateSelectInput(session, "alloc_grant_select", choices = c("No grants registered" = ""))
    }
  })
  
  # Dynamic Dropdown Mapping for Activity Tracker
  observe({
    df <- allocations_data()
    # Ensure df is not empty and has the necessary columns
    if (nrow(df) > 0 && "id" %in% names(df)) {
      # Creating a named vector for the dropdown
      choices <- setNames(df$id, paste0(df$funder_name, " (", df$grant_name, ") - ", df$pillar))
      updateSelectInput(session, "act_alloc_select", choices = choices)
    } else {
      updateSelectInput(session, "act_alloc_select", choices = c("No active pillar allocations" = ""))
    }
  })
  # --- Input Submission Actions ---
  observeEvent(input$save_funding, {
    req(input$funder_name, input$grant_name, input$grant_amount > 0, input$grant_pillar)
    combined_pillars <- paste(input$grant_pillar, collapse = ", ")
    conn <- get_db_conn()
    dbExecute(conn, "INSERT INTO funding_sources (funder_name, grant_name, amount, pillar, country) VALUES ($1, $2, $3, $4, $5)",
              list(input$funder_name, input$grant_name, input$grant_amount, combined_pillars, input$grant_country))
    dbDisconnect(conn)
    updateTextInput(session, "funder_name", value = "")
    updateTextInput(session, "grant_name", value = "")
    updateNumericInput(session, "grant_amount", value = 0)
    updateSelectInput(session, "grant_pillar", selected = character(0))
    showNotification("Funding registered successfully!", type = "message")
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # --- ALLOCATION GUARDRAILS (NEW SUBMISSION) ---
  observeEvent(input$save_allocation, {
    req(input$alloc_grant_select, input$alloc_amount > 0)
    
    selected_grant_id <- as.integer(input$alloc_grant_select)
    f_df <- funding_data()
    grant_total <- as.numeric(f_df$amount[f_df$id == selected_grant_id])
    if(length(grant_total) == 0) grant_total <- 0
    
    a_df <- allocations_data()
    already_allocated <- 0
    if (nrow(a_df) > 0 && "grant_id" %in% names(a_df)) {
      already_allocated <- sum(as.numeric(a_df$allocated_amount[a_df$grant_id == selected_grant_id]), na.rm = TRUE)
    }
    
    remaining_grant_budget <- grant_total - already_allocated
    
    if (input$alloc_amount > remaining_grant_budget) {
      showNotification(
        paste0("Error: Allocation Exceeds Budget! Remaining balance for this grant is $", 
               format(remaining_grant_budget, big.mark = ","), "."),
        type = "error", duration = 10
      )
      return()
    }
    
    conn <- get_db_conn()
    df_raw <- tryCatch({ dbGetQuery(conn, "SELECT * FROM allocations LIMIT 1") }, error = function(e){data.frame()})
    amt_col <- if("allocated_amount" %in% names(df_raw)) "allocated_amount" else "amount"
    
    query <- paste0("INSERT INTO allocations (grant_id, pillar, ", amt_col, ") VALUES ($1, $2, $3)")
    dbExecute(conn, query, list(selected_grant_id, input$alloc_pillar, input$alloc_amount))
    dbDisconnect(conn)
    
    updateNumericInput(session, "alloc_amount", value = 0)
    showNotification("Allocation completed successfully!", type = "message")
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # ==========================================
  # INTERACTIVE MODALS FOR IN-ROW EDITING
  # ==========================================
  # --- UPDATED EDIT TRIGGER FOR FUNDING ---
  observeEvent(input$edit_funding_btn, {
    # 1. Identify the index and pull the data
    index <- as.numeric(strsplit(input$edit_funding_btn, "_")[[1]][3])
    row_data <- funding_data()[index, ]
    
    # 2. Store the ID (Crucial for the update step)
    editing_id(row_data$id)
    
    selected_pillars <- unlist(strsplit(as.character(row_data$pillar), ", "))
    
    # 3. Create the Modal
    showModal(modalDialog(
      title = "Edit Funding Record", size = "m", easyClose = TRUE,
      textInput("edit_f_funder", "Funder Name:", value = row_data$funder_name),
      textInput("edit_f_grant", "Grant Name:", value = row_data$grant_name),
      numericInput("edit_f_amount", "Amount ($):", value = row_data$amount, min = 0),
      selectInput("edit_f_pillar", "Pillars (Select Multiple):", 
                  choices = pillars_list, selected = selected_pillars, multiple = TRUE),
      textInput("edit_f_country", "Country:", value = row_data$country),
      footer = tagList(modalButton("Cancel"), 
                       actionButton("update_funding_db", "Save Changes", class = "btn-success"))
    ))
  })
  
  # --- UPDATED SAVE LOGIC FOR FUNDING ---
  observeEvent(input$update_funding_db, {
    req(editing_id()) # Ensure we know which record we are updating
    
    combined_p <- paste(input$edit_f_pillar, collapse = ", ")
    
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    query <- "UPDATE funding_sources SET funder_name=$1, grant_name=$2, amount=$3, pillar=$4, country=$5 WHERE id=$6"
    
    dbExecute(conn, query, list(
      input$edit_f_funder, 
      input$edit_f_grant, 
      input$edit_f_amount, 
      combined_p, 
      input$edit_f_country, 
      editing_id() # Uses the reactive ID set when "Edit" was clicked
    ))
    
    removeModal()
    showNotification("Funding entry updated successfully!", type = "message")
    refresh_trigger(refresh_trigger() + 1)
  })
  # --- ALLOCATION GUARDRAILS (IN-ROW EDIT / UPDATE) ---
  # --- UPDATED EDIT TRIGGER FOR ALLOCATIONS ---
  observeEvent(input$edit_alloc_btn, {
    # 1. Identify index and pull data
    index <- as.numeric(strsplit(input$edit_alloc_btn, "_")[[1]][3])
    row_data <- allocations_data()[index, ]
    
    # 2. Set the tracking ID
    editing_id(row_data$id)
    
    grants_df <- funding_data()
    grant_choices <- setNames(grants_df$id, paste(grants_df$funder_name, "-", grants_df$grant_name))
    
    # 3. Freshly generate the modal
    showModal(modalDialog(
      title = "Edit Allocation Entry", size = "m", easyClose = TRUE,
      selectInput("edit_a_source", "Source Grant ID:", choices = grant_choices, selected = row_data$grant_id),
      selectInput("edit_a_pillar", "Target Pillar:", choices = pillars_list, selected = row_data$pillar),
      numericInput("edit_a_amount", "Allocated Amount ($):", value = row_data$allocated_amount, min = 0),
      footer = tagList(modalButton("Cancel"), 
                       actionButton("update_alloc_db", "Save Changes", class = "btn-success"))
    ))
  })
  
  # --- UPDATED SAVE LOGIC FOR ALLOCATIONS ---
  observeEvent(input$update_alloc_db, {
    req(editing_id())
    
    # 1. Logic Guardrails
    target_grant_id <- as.integer(input$edit_a_source)
    grants_df <- funding_data()
    grant_max <- as.numeric(grants_df$amount[grants_df$id == target_grant_id])
    if(length(grant_max) == 0) grant_max <- 0
    
    all_allocs <- allocations_data()
    # Check budget excluding the current record being edited
    other_allocs_sum <- sum(as.numeric(all_allocs$allocated_amount[all_allocs$grant_id == target_grant_id & all_allocs$id != editing_id()]), na.rm = TRUE)
    
    if(input$edit_a_amount > (grant_max - other_allocs_sum)) {
      showNotification(paste0("Update Blocked! Maximum available budget left on this grant is $", format(grant_max - other_allocs_sum, big.mark=",")), type="error")
      return()
    }
    
    # 2. Update DB
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    df_raw <- tryCatch({ dbGetQuery(conn, "SELECT * FROM allocations LIMIT 1") }, error = function(e){data.frame()})
    amt_col <- if("allocated_amount" %in% names(df_raw)) "allocated_amount" else "amount"
    
    query <- paste0("UPDATE allocations SET grant_id=$1, pillar=$2, ", amt_col, "=$3 WHERE id=$4")
    dbExecute(conn, query, list(target_grant_id, input$edit_a_pillar, input$edit_a_amount, editing_id()))
    
    removeModal()
    showNotification("Allocation record updated successfully!", type = "message")
    refresh_trigger(refresh_trigger() + 1)
  })
  # --- UPDATED EDIT TRIGGER WITH ALLOCATION SELECTION ---
  observeEvent(input$update_act_db, {
    req(editing_id())
    
    # 1. Capture and validate the inputs
    # Ensure allocation ID is treated as a character/integer as expected by your DB
    new_alloc_id <- as.integer(input$edit_e_alloc) 
    
    conn <- get_db_conn()
    
    # 2. Use tryCatch to stop the crash and print the error to your console instead
    tryCatch({
      query <- "UPDATE activity SET 
                allocation_id=$1, 
                activity_name=$2, 
                activity_decript=$3, 
                activity_type=$4, 
                pillar=$5, 
                budget=$6, 
                amount_used=$7, 
                date_recorded=$8 
                WHERE id=$9"
      
      dbExecute(conn, query, list(
        new_alloc_id,
        input$edit_e_name, 
        input$edit_e_desc, 
        input$edit_e_type, 
        input$edit_e_pillar, 
        input$edit_e_budget, 
        input$edit_e_used, 
        as.character(input$edit_e_date), 
        editing_id()
      ))
      
      dbDisconnect(conn)
      removeModal()
      showNotification("Activity record updated successfully!", type = "message")
      refresh_trigger(refresh_trigger() + 1)
      
    }, error = function(e) {
      dbDisconnect(conn)
      # This will print the specific reason for the crash in your R Console
      print(paste("CRASH ERROR:", e$message))
      showNotification(paste("Update Failed:", e$message), type = "error")
    })
  })
  
  
  # --- STRICT tracker GUARDRAILS (IN-ROW EDIT / UPDATE) ---
  ##kkk
  observeEvent(input$edit_exp_btn, {
    index <- as.numeric(strsplit(input$edit_exp_btn, "_")[[1]][3]) # Change to [4] if [3] causes errors
    row_data <- activity_data()[index, ]
    editing_id(row_data$id)
    
    pirs_list_reactive <- reactive({
      conn <- get_db_conn()
      on.exit(dbDisconnect(conn))
      dbGetQuery(conn, "SELECT id, indicator FROM pirs")
    })
    
    pirs_data <- pirs_list_reactive()
    pirs_choices <- setNames(pirs_data$id, pirs_data$indicator)
    # 1. Prepare allocation choices: Funder - Grant (Pillar)
    alloc_df <- allocations_data()
    # Updated the label to include the Pillar as you requested
    alloc_choices <- setNames(alloc_df$id, paste0(alloc_df$funder_name, " - ", alloc_df$grant_name, " (", alloc_df$pillar, ")"))
    countries=c("Contiental","Central Region", "Western Region", "DRC","Uganda", "Rwanda","Sudan" )
    current_indicator <- row_data$output_indicator
    # Helper to handle potential NULLs in older database records
    val_memo <- if(is.null(row_data$memo_submitted) || is.na(row_data$memo_submitted)) FALSE else as.logical(row_data$memo_submitted)
    val_approved <- if(is.null(row_data$approved) || is.na(row_data$approved)) FALSE else as.logical(row_data$approved)
    val_report <- if(is.null(row_data$report_submitted) || is.na(row_data$report_submitted)) FALSE else as.logical(row_data$report_submitted)
    val_decomit <- if(is.null(row_data$funds_decommistted) || is.na(row_data$funds_decommistted)) FALSE else as.logical(row_data$funds_decommistted)
    val_needle <- isTRUE(as.logical(row_data$move_ind))
    # 2. Show the modal with ALL fields
    showModal(modalDialog(
      title = "Edit Full Activity Record", size = "l", easyClose = TRUE,
      # Now including the allocation box WITH the full label
      
      fluidRow(
        column(6, selectInput("edit_e_alloc", "Select Active Grant Allocation:", 
                              choices = alloc_choices, 
                              selected = row_data$allocation_id)),
        column(6, selectInput("edit_benef", "Select Benefitiary:", 
                              choices = countries, 
                              selected = row_data$benefitiary,
                              multiple = TRUE))
      ),
      
      
      
      
      fluidRow(
        column(6, selectInput("edit_e_pillar", "Pillar:", choices = pillars_list, selected = row_data$pillar)),
        column(6, selectInput("edit_e_support_pillar", "Supporting Pillar:", 
                              choices = pillars_list, 
                              selected = unlist(strsplit(as.character(row_data$support_pillars), ", ")), 
                              multiple = TRUE))
      ),
      
      
      fluidRow(
        column(6, textInput("edit_e_name", "Activity Name:", value = row_data$activity_name)),
        column(6, selectInput("edit_e_type", "Activity Type:", choices = c("Meeting", "Document development", "Supply", "Deployment", "Training", "Operational"), selected = row_data$activity_type))
      ),
      
      textAreaInput("edit_e_desc", "Description:", value = row_data$activity_decript, width = "100%"),
      
      # New line 3
      
      fluidRow(
        column(6, checkboxInput("edit_memo", "Memo Submitted", value = val_memo)),
        column(6, checkboxInput("edit_approval", "Approved", value = val_approved)),
        column(6, selectInput("edit_status", "Current Status",choices = c("Not Started", "On-going", "Completed"), selected = row_data$start_implement))
      ),
      
      
      
      fluidRow(
        column(6, checkboxInput("edit_report", "Report Submitted", value = val_report)),
        column(6, checkboxInput("edit_decomit", "Funds Decommited", value = val_decomit))
      ),
      
      fluidRow(
        column(6, numericInput("edit_e_budget", "Budget ($):", value = row_data$budget, min = 0)),
        column(6, numericInput("edit_e_used", "Amount Used ($):", value = row_data$amount_used, min = 0))
      ),
      fluidRow(
        column(8, 
               selectInput("edit_kpi", "Supporting KPI (Output):", 
                           choices = c("None" = "", pirs_choices), 
                           selected = current_indicator)
        ),
        column(4, 
               # Adding a margin-top helps align the checkbox with the dropdown box
               tags$div(style = "margin-top: 25px;",
                        checkboxInput("edit_needle", "Move the Needle?", value = val_needle)
               )
        )
      ),      
      dateInput("edit_e_date", "Date of Activity:", value = as.Date(row_data$date_recorded)),
      footer = tagList(modalButton("Cancel"), actionButton("update_act_db", "Save Changes", class = "btn-success"))
    ))
    
  })
  # --- PASTE THE NEW SAVE LOGIC HERE ---
  observeEvent(input$update_act_db, {
    req(editing_id())
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    combined_pillars <- paste(input$edit_e_support_pillar, collapse = ", ")
    combined_benefit <- paste(input$edit_benef, collapse = ", ")
    # Added memo_submitted to the query
    query <- "UPDATE activity SET 
              allocation_id=$1, activity_name=$2, activity_decript=$3, 
              activity_type=$4, pillar=$5, budget=$6, amount_used=$7, 
              memo_submitted=$8, approved=$9, start_implement=$10, 
              report_submitted=$11, funds_decommistted=$12, date_recorded=$13, 
              support_pillars=$14, benefitiary=$15, 
              output_indicator=$17, move_ind=$18 
              WHERE id=$16"
    
    
    dbExecute(conn, query, list(
      input$edit_e_alloc,
      input$edit_e_name, 
      input$edit_e_desc, 
      input$edit_e_type, 
      input$edit_e_pillar, 
      input$edit_e_budget, 
      input$edit_e_used, 
      as.logical(input$edit_memo),
      as.logical(input$edit_approval),
      input$edit_status,
      as.logical(input$edit_report),
      as.logical(input$edit_decomit),
      as.character(input$edit_e_date),
      combined_pillars,
      combined_benefit,
      editing_id(),        # $16
      input$edit_kpi,        # $17
      input$edit_needle    # $18
    ))
    # Perform the Upsert on the ITT table using activity_id as the anchor
    query_itt <- "
    INSERT INTO itt (activity_id, pirs_id, moves_needle)
    VALUES ($1, $2, $3)
    ON CONFLICT (activity_id) 
    DO UPDATE SET
      pirs_id = EXCLUDED.pirs_id,
      moves_needle = EXCLUDED.moves_needle;
  "
    
    dbExecute(conn, query_itt, list(
      editing_id(),                   # $1: The Primary Key (activity_id)
      if(input$edit_kpi == "") NULL else input$edit_kpi, # $2
      input$edit_needle               # $3
    ))
    
    removeModal()
    showNotification("Activity record updated!", type = "message")
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # ==========================================
  # DATATABLE RENDERING
  # ==========================================
  output$funding_table <- renderDT({
    df <- funding_data()
    if(is.null(df) || nrow(df) == 0) {
      df_display <- data.frame(Funder=character(), `Grant Name`=character(), `Amount ($)`=numeric(), Pillars=character(), Country=character(), Actions=character(), check.names=FALSE)
    } else {
      actions <- shinyInput(actionButton, nrow(df), 'edit_funding', label = "Edit", class = "btn-primary btn-xs", onclick = 'Shiny.setInputValue(\"edit_funding_btn\", this.id, {priority: \"event\"})')
      df_display <- df %>% mutate(Actions = actions) %>% select(Funder = funder_name, `Grant Name` = grant_name, `Amount ($)` = amount, Pillars = pillar, Country = country, Actions)
    }
    datatable(df_display, escape = FALSE, selection = 'none', options = list(pageLength = 5), rownames = FALSE) %>% 
      formatCurrency(columns = intersect(names(df_display), 'Amount ($)'), currency = "$")
  })
  
  
  
  output$allocations_table <- renderDT({
    df <- allocations_data()
    if(is.null(df) || nrow(df) == 0 || !("allocated_amount" %in% names(df))) {
      df_display <- data.frame(Funder = character(),`Source Grant` = character(), `Pillar` = character(), `Amount ($)` = numeric(), Actions = character(), check.names = FALSE)
    } else {
      actions <- shinyInput(actionButton, nrow(df), 'edit_alloc', label = "Edit", class = "btn-primary btn-xs", onclick = 'Shiny.setInputValue(\"edit_alloc_btn\", this.id, {priority: \"event\"})')
      df_display <- df %>% mutate(Actions = actions) %>% select(Funder = funder_name,`Source Grant` = grant_name, Pillar = pillar, `Amount ($)` = allocated_amount, Actions)
    }
    datatable(df_display, escape = FALSE, selection = 'none', options = list(pageLength = 5), rownames = FALSE) %>% 
      formatCurrency(columns = intersect(names(df_display), 'Amount ($)'), currency = "$")
  })
  
  output$activity_table <- renderDT({
    df <- activity_data()
    if(nrow(df) > 0) {
      # Generate the action buttons
      actions <- shinyInput(actionButton, nrow(df), 'edit_exp', label = "Edit", class = "btn-primary btn-xs", 
                            onclick = 'Shiny.setInputValue(\"edit_exp_btn\", this.id, {priority: \"event\"})')
      
      # Create the display dataframe, explicitly selecting and renaming columns
      df_display <- df %>% 
        mutate(Actions = actions) %>% 
        select(`Activity Name` = activity_name, 
               Description = activity_decript, 
               Type = activity_type, 
               Budget = budget, 
               `Amount Used` = amount_used, 
               Date = date_recorded, 
               Actions)
      
      datatable(df_display, escape = FALSE, options = list(pageLength = 5), rownames = FALSE) %>% 
        formatCurrency(columns = c('Budget', 'Amount Used'), currency = "$")
    } else {
      datatable(data.frame(Message="No activity logs found"), rownames = FALSE)
    }
  })
  # --- KPI Dashboard Summaries ---
  output$total_funding_box <- renderValueBox({
    conn <- get_db_conn(); res <- dbGetQuery(conn, "SELECT SUM(amount) as total FROM funding_sources")$total; dbDisconnect(conn)
    valueBox(value = paste0("$", format(ifelse(is.na(res), 0, res), scientific = FALSE, big.mark = ",")), subtitle = "Total Funding Registered", icon = icon("wallet"), color = "green")
  })
  
  output$total_allocated_box <- renderValueBox({
    df_check <- allocations_data()
    res <- if(nrow(df_check) > 0 && "allocated_amount" %in% names(df_check)) sum(as.numeric(df_check$allocated_amount), na.rm = TRUE) else 0
    box_content <- valueBox(paste0("$", format(res, scientific = FALSE, big.mark = ",")), "Total Funds Allocated", icon = icon("pie-chart"), color = "orange")
    box_content$children[[1]] <- tagAppendAttributes(box_content$children[[1]], class = "bg-blue-brand")
    box_content
  })
  
  output$total_used_box <- renderValueBox({
    df_check <- activity_data()
    res <- if(nrow(df_check) > 0) sum(as.numeric(df_check$amount_used), na.rm = TRUE) else 0
    valueBox(paste0("$", format(res, scientific = FALSE, big.mark = ",")), "Total Funds Used", icon = icon("hourglass-half"), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)