library(shiny)
library(DBI)
library(RSQLite)
library(bslib)
library(DT)

# 1. DATABASE INITIALIZATION
db_name <- "narcc_v9_secure.sqlite"
con <- dbConnect(SQLite(), db_name)

# Added 'narrative' column to the table schema
dbExecute(con, "CREATE TABLE IF NOT EXISTS activities (
    id INTEGER PRIMARY KEY AUTOINCREMENT, 
    code TEXT, 
    name TEXT, 
    focal_email TEXT, 
    is_approved TEXT, 
    is_completed TEXT, 
    is_reported TEXT, 
    is_retired TEXT, 
    is_decommitted TEXT, 
    narrative TEXT, 
    updated TEXT)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS focal_registry (email TEXT PRIMARY KEY, added_date TEXT)")

# 2. UI
ui <- navbarPage(
  title = "NARCC Portal",
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel("Update Status",
           fluidPage(
             column(8, offset = 2,
                    wellPanel(
                      h3("Update Activity Status"),
                      uiOutput("select_act_ui"),
                      hr(),
                      uiOutput("dynamic_update_fields")
                    )
             )
           )
  ),
  
  tabPanel("Admin & Management",
           fluidPage(
             column(8, offset = 2,
                    wellPanel(
                      passwordInput("admin_pin", "Admin PIN"),
                      uiOutput("admin_panel_ui")
                    )
             )
           )
  )
)

# 3. SERVER
server <- function(input, output, session) {
  refresh <- reactiveVal(0)
  
  # --- Admin & Export ---
  output$admin_panel_ui <- renderUI({
    if (is.null(input$admin_pin) || input$admin_pin != "1234") return(helpText("Enter PIN (given)"))
    tagList(
      navset_card_pill(
        nav_panel("Registration",
                  br(),
                  textInput("reg_code", "Code"),
                  textInput("reg_name", "Title"),
                  uiOutput("focal_dropdown_ui"),
                  actionButton("do_reg", "Register Activity", class = "btn-success", width = "100%")
        ),
        nav_panel("Focal Persons",
                  br(),
                  textInput("new_email", "Authorize Email"),
                  actionButton("add_email", "Add to Registry", class = "btn-info"),
                  hr(),
                  DTOutput("email_table")
        ),
        nav_panel("Power BI Push",
                  br(),
                  h4("Manual Data Synchronization"),
                  p("Clicking the button below will generate the 'NARCC_Master_Data.csv' file."),
                  actionButton("push_csv", "Push Data to Power BI (CSV)", class = "btn-danger", icon = icon("share-from-square"), width = "100%")
        )
      )
    )
  })
  
  output$select_act_ui <- renderUI({
    refresh()
    df <- dbGetQuery(con, "SELECT id, code, name FROM activities")
    choices <- setNames(df$id, paste0(df$code, " - ", df$name))
    selectInput("sel_id", "Choose Activity", choices = c(" " = "", choices))
  })
  
  output$dynamic_update_fields <- renderUI({
    req(input$sel_id)
    if(input$sel_id == "") return(NULL)
    
    curr <- dbGetQuery(con, "SELECT * FROM activities WHERE id = ?", list(input$sel_id))
    
    get_sel <- function(val) {
      if (is.null(val) || is.na(val) || val == "" || val == "NA") return(character(0))
      return(val)
    }
    
    tagList(
      fluidRow(
        column(6,
               radioButtons("u_appr", "Approved?", c("No", "Yes"), selected = get_sel(curr$is_approved), inline = TRUE),
               radioButtons("u_comp", "Completed?", c("No", "Yes"), selected = get_sel(curr$is_completed), inline = TRUE),
               radioButtons("u_repo", "Reported?", c("No", "Yes"), selected = get_sel(curr$is_reported), inline = TRUE)
        ),
        column(6,
               radioButtons("u_reti", "Retired?", c("No", "Yes"), selected = get_sel(curr$is_retired), inline = TRUE),
               radioButtons("u_decom", "Decommitted?", c("No", "Yes"), selected = get_sel(curr$is_decommitted), inline = TRUE)
        )
      ),
      hr(),
      # --- NARRATIVE/COMMENT SECTION ---
      textAreaInput("u_note", "Narrative/Comments", value = ifelse(is.na(curr$narrative), "", curr$narrative), rows = 3),
      passwordInput("u_auth_email", "Verify Your Email"),
      actionButton("do_update", "Submit Update", class = "btn-primary", width = "100%")
    )
  })
  
  observeEvent(input$do_update, {
    # 1. VALIDATE EMAIL
    act <- dbGetQuery(con, "SELECT focal_email FROM activities WHERE id = ?", list(input$sel_id))
    if (tolower(trimws(input$u_auth_email)) != tolower(act$focal_email)) {
      return(showNotification("Email Mismatch", type = "error"))
    }
    
    # 2. CONVERT NULLS TO "NA"
    safe_val <- function(x) { if(is.null(x)) return("NA") else return(x) }
    
    # 3. UPDATE DB (Including narrative)
    dbExecute(con, "UPDATE activities SET is_approved=?, is_completed=?, is_reported=?, is_retired=?, is_decommitted=?, narrative=?, updated=? WHERE id=?",
              list(
                safe_val(input$u_appr), 
                safe_val(input$u_comp), 
                safe_val(input$u_repo), 
                safe_val(input$u_reti), 
                safe_val(input$u_decom), 
                input$u_note, 
                as.character(Sys.time()), 
                input$sel_id
              ))
    
    freezeReactiveValue(input, "sel_id")
    updateSelectInput(session, "sel_id", selected = "")
    
    showNotification("Database Updated Successfully")
    refresh(refresh() + 1)
  })
  
  # --- ADMIN ACTIONS ---
  observeEvent(input$do_reg, {
    dbExecute(con, "INSERT INTO activities (code, name, focal_email, is_approved, is_completed, is_reported, is_retired, is_decommitted, narrative, updated) VALUES (?,?,?,?,?,?,?,?,?,?)",
              list(input$reg_code, input$reg_name, tolower(input$reg_focal_email), "NA", "NA", "NA", "NA", "NA", "", as.character(Sys.time())))
    updateTextInput(session, "reg_code", value = "")
    updateTextInput(session, "reg_name", value = "")
    refresh(refresh() + 1)
    showNotification("Registered Successfully")
  })
  
  observeEvent(input$push_csv, {
    d <- dbGetQuery(con, "SELECT * FROM activities")
    write.csv(d, "NARCC_Master_Data.csv", row.names = FALSE)
    showNotification("CSV Refreshed", type = "message")
  })
  
  observeEvent(input$add_email, {
    dbExecute(con, "INSERT OR IGNORE INTO focal_registry VALUES (?,?)", list(tolower(input$new_email), as.character(Sys.Date())))
    updateTextInput(session, "new_email", value = "")
    refresh(refresh() + 1)
  })
  
  output$email_table <- renderDT({
    refresh()
    datatable(dbGetQuery(con, "SELECT * FROM focal_registry"), selection = 'single')
  })
  
  output$focal_dropdown_ui <- renderUI({
    refresh()
    emails <- dbGetQuery(con, "SELECT email FROM focal_registry")$email
    selectInput("reg_focal_email", "Assign Focal Person", choices = emails)
  })
}

onStop(function() { dbDisconnect(con) })
shinyApp(ui, server)