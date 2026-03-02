library(shiny)
library(DBI)
library(RSQLite)
library(bslib)

# 1. Database Connection (v6 to handle new independent columns)
db_name <- "narcc_v6_final.sqlite"
con <- dbConnect(SQLite(), db_name)

# Create tables with independent state columns
dbExecute(con, "CREATE TABLE IF NOT EXISTS activities (
    id INTEGER PRIMARY KEY AUTOINCREMENT, 
    code TEXT, name TEXT, country TEXT, 
    is_approved TEXT, 
    is_completed TEXT, 
    is_reported TEXT, 
    is_retired TEXT, 
    is_decommitted TEXT,
    updated TEXT)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS logs (
    id INTEGER PRIMARY KEY AUTOINCREMENT, 
    act_id INTEGER, note TEXT, date TEXT)")

# 2. User Interface
ui <- navbarPage(
  title = "NARCC Activity Portal",
  theme = bs_theme(bootswatch = "flatly"),
  
  # --- TAB 1: REGISTRATION ---
  tabPanel("Register New Activity",
           fluidPage(
             column(6, offset = 3,
                    wellPanel(
                      h3("Registration Form", style="border-bottom: 2px solid #2c3e50; padding-bottom: 10px;"),
                      passwordInput("admin_pin", "Admin Authorization PIN"),
                      textInput("reg_name", "Activity Title / Name"),
                      textInput("reg_country", "Country of Implementation"),
                      br(),
                      actionButton("do_reg", "Create Activity Record", class = "btn-success", width = "100%")
                    )
             )
           )
  ),
  
  # --- TAB 2: UPDATE STATUS ---
  tabPanel("Update Activity Status",
           fluidPage(
             column(8, offset = 2,
                    wellPanel(
                      h3("Activity Update Checklist", style="border-bottom: 2px solid #2c3e50; padding-bottom: 10px;"),
                      uiOutput("select_act_ui"),
                      hr(),
                      
                      # This panel only appears once an activity is selected
                      conditionalPanel(
                        condition = "input.sel_id != '' && input.sel_id != null",
                        
                        fluidRow(
                          column(6,
                                 radioButtons("u_appr", "1. Has this activity been approved?",
                                              choices = c("No", "Yes"), inline = TRUE),
                                 radioButtons("u_comp", "2. Has the activity been completed?",
                                              choices = c("No", "Yes"), inline = TRUE),
                                 radioButtons("u_repo", "3. Has the report been submitted?",
                                              choices = c("No", "Yes"), inline = TRUE)
                          ),
                          column(6,
                                 radioButtons("u_reti", "4. Has retirement been done?",
                                              choices = c("No", "Yes"), inline = TRUE),
                                 radioButtons("u_decom", "5. Has Finance decommitted the funds?",
                                              choices = c("No", "Yes"), inline = TRUE)
                          )
                        ),
                        
                        hr(),
                        textAreaInput("u_note", "Additional Progress Remarks", rows = 3, 
                                      placeholder = "Enter details regarding any of the above..."),
                        
                        br(),
                        actionButton("do_update", "Submit Official Status Update", 
                                     class = "btn-primary btn-lg", width = "100%")
                      )
                    )
             )
           )
  )
)

# 3. Server Logic
server <- function(input, output, session) {
  refresh <- reactiveVal(0)
  
  # --- Handle Registration ---
  observeEvent(input$do_reg, {
    pin <- Sys.getenv("MY_ADMIN_PIN")
    if (pin == "") pin <- "1234"
    
    if (input$admin_pin != pin) {
      showNotification("Incorrect PIN", type = "error")
      return()
    }
    
    req(input$reg_name, input$reg_country)
    
    tryCatch({
      act_code <- paste0(toupper(substr(input$reg_country, 1, 3)), "-", sample(100:999, 1))
      now <- format(Sys.time(), "%Y-%m-%d %H:%M")
      
      # Insert with default "No" for all 5 states
      dbExecute(con, "INSERT INTO activities (code, name, country, is_approved, is_completed, is_reported, is_retired, is_decommitted, updated) 
                VALUES (?,?,?,?,?,?,?,?,?)",
                params = list(act_code, input$reg_name, input$reg_country, "No", "No", "No", "No", "No", now))
      
      refresh(refresh() + 1)
      showNotification("Activity Registered!", type = "message")
      
      updateTextInput(session, "reg_name", value = "")
      updateTextInput(session, "reg_country", value = "")
      updateTextInput(session, "admin_pin", value = "") 
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error")
    })
  })
  
  # --- Generate Search Dropdown ---
  output$select_act_ui <- renderUI({
    refresh()
    df <- dbGetQuery(con, "SELECT id, name, country FROM activities")
    if(nrow(df) == 0) return(helpText("No activities registered yet."))
    
    choices <- setNames(df$id, paste(df$country, "-", df$name))
    selectizeInput("sel_id", "Search and Select Activity to Update", choices = c("Select..." = "", choices))
  })
  
  # --- Handle Updates ---
  observeEvent(input$do_update, {
    req(input$sel_id)
    now <- format(Sys.time(), "%Y-%m-%d %H:%M")
    
    tryCatch({
      dbExecute(con, "UPDATE activities SET 
                is_approved=?, is_completed=?, is_reported=?, is_retired=?, is_decommitted=?, updated=? 
                WHERE id=?",
                params = list(input$u_appr, input$u_comp, input$u_repo, input$u_reti, input$u_decom, now, input$sel_id))
      
      if(nchar(input$u_note) > 0) {
        dbExecute(con, "INSERT INTO logs (act_id, note, date) VALUES (?,?,?)",
                  params = list(input$sel_id, input$u_note, now))
      }
      
      showNotification("Update Saved Successfully", type = "message")
      
      # Reset form
      updateSelectizeInput(session, "sel_id", selected = "")
      updateTextAreaInput(session, "u_note", value = "")
      
    }, error = function(e) {
      showNotification(paste("Update Failed:", e$message), type = "error")
    })
  })
}

onStop(function() { dbDisconnect(con) })
shinyApp(ui, server)