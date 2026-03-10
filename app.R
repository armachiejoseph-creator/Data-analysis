library(shiny)
library(DBI)
library(RSQLite)
library(bslib)
library(DT)
library(plotly)
library(dplyr)
#dt
# --- 1. STRATEGIC UNIVERSE DATA FUNCTION (UNTOUCHED) ---
generate_complete_system <- function() {
  sun <- data.frame(x=0, y=0, z=0, label="VISION 2027", size=60, color="gold", type="Sun",
                    info="<b>VISION 2027</b><br>A safer, healthier, and prosperous Africa.")
  
  planets <- data.frame(
    id = 1:13,
    label = c("P1: Disease Control", "P2: Surveillance", "P3: Emergency EPR", 
              "P4: NPHI Support", "P5: Lab Systems", "P6: Manufacturing",
              "E1: Digital", "E2: Workforce", "E3: Financing", "E4: Research", 
              "E5: Partnerships", "E6: RCCs", "E7: Governance"),
    r = c(rep(30, 6), rep(18, 7)),
    color = c(rep("#1E90FF", 6), rep("#FF4500", 7)),
    type = c(rep("Priority", 6), rep("Enabler", 7)),
    info = c(
      "Strengthen health systems to prevent and control high-burden diseases.",
      "Strengthen integrated surveillance and early warning systems.",
      "Ensure resilient health systems for emergency preparedness and response.",
      "Empower National Public Health Institutes (NPHIs) to lead core functions.",
      "Expand clinical and public health laboratory systems and networks.",
      "Expand health product innovation and local manufacturing.",
      "Incorporate digital and innovative health approaches.",
      "Attract and grow a best-in-class African public health workforce.",
      "Secure sustainable financial resources and resource allocation.",
      "Strengthen public health research and innovation for decision-making.",
      "Foster respectful partnerships and community engagement.",
      "Enhance Africa CDC's regional and in-country presence.",
      "Strengthen governance, leadership, and accountable processes."
    )
  ) %>% mutate(
    theta = seq(0, 2*pi, length.out=14)[id],
    x = r*cos(theta), y = r*sin(theta), z = runif(13, -5, 5), size = 40
  )
  
  obj_data <- list(
    # P1
    list(l="P1.1: Integrated Strategies", p=1, t=1, d="Addressing RMNCH, NTDs, NCDs, and mental health."), 
    list(l="P1.2: One Health Actions", p=1, t=1, d="Establishing multi-sectoral actions for disease management."), 
    list(l="P1.3: Access to Med-Tech", p=1, t=1, d="Improving access to essential medical technologies."),
    # P2
    list(l="P2.1: Real-time systems", p=2, t=2, d="Establishing real-time disease and event reporting systems."), 
    list(l="P2.2: One Health Surveillance", p=2, t=2, d="Integrating human, animal, and environmental data."), 
    list(l="P2.3: AMR Control", p=2, t=2, d="Implementing strategies to control antimicrobial resistance."), 
    list(l="P2.4: Predictive Analytics", p=2, t=2, d="Utilizing advanced modeling to forecast outbreaks."), 
    list(l="P2.5: Information Systems", p=2, t=2, d="Building digital architecture for secure data exchange."),
    # P3
    list(l="P3.1: Prevention/Prep", p=3, t=3, d="Strengthening prevention and preparedness at all levels."), 
    list(l="P3.2: Response/Recovery", p=3, t=3, d="Building robust response and recovery capabilities."), 
    list(l="P3.3: Cross-border Coord", p=3, t=3, d="Facilitating coordination across geographic boundaries."),
    # P4
    list(l="P4.1: Legal Establishment", p=4, t=4, d="Supporting legal establishment and mandates for NPHIs."), 
    list(l="P4.2: Operational Standards", p=4, t=4, d="Bolstering NPHI functionality to a common standard."), 
    list(l="P4.3: NPHI Collaboration", p=4, t=4, d="Establishing networks for shared learning and resources."), 
    list(l="P4.4: Centers of Excellence", p=4, t=4, d="Supporting regional hubs for research and training."),
    # P5
    list(l="P5.1: Lab Governance", p=5, t=5, d="Strengthening laboratory infrastructure and governance."), 
    list(l="P5.2: Diagnostic Access", p=5, t=5, d="Improving timely disease detection via diagnostics."), 
    list(l="P5.3: Genomic/Molecular Cap", p=5, t=5, d="Enhancing genomics and bioinformatics capacity."), 
    list(l="P5.4: Lab Network Optimization", p=5, t=5, d="Mapping networks for efficient information sharing."),
    # P6
    list(l="P6.1: Local Capacity/Infra", p=6, t=6, d="Scaling infrastructure for local vaccine production."), 
    list(l="P6.2: Market Intelligence", p=6, t=6, d="Providing data-driven insights for market design."), 
    list(l="P6.3: R&D Ecosystem", p=6, t=6, d="Strengthening local R&D and technology transfer."), 
    list(l="P6.4: Investment/Partnership", p=6, t=6, d="Attracting investments for a self-reliant industry."),
    # E1
    list(l="E1.1: Digital Strategies", p=7, t=c(2, 3, 5), d="Implementing tailored digital health strategies."), 
    list(l="E1.2: Data Governance", p=7, t=c(2, 3, 5), d="Developing frameworks for data interoperability."), 
    list(l="E1.3: Health Informatics", p=7, t=c(2, 3, 5), d="Fostering innovations to meet specific challenges."),
    # E2
    list(l="E2.1: Training Programs", p=8, t=c(1, 2, 3, 4), d="Executing epidemiology and leadership training."), 
    list(l="E2.2: Scholarships/Fellowships", p=8, t=c(1, 2, 3, 4), d="Growing a specialized technical workforce."), 
    list(l="E2.3: CHW Institutionalization", p=8, t=c(1, 2, 3, 4), d="Formalizing community health workers."),
    # E3
    list(l="E3.1: Needs Analysis", p=9, t=c(3, 6), d="Conducting financial needs assessments."), 
    list(l="E3.2: Domestic Health Finance", p=9, t=c(3, 6), d="Advocating for domestic budget allocations."), 
    list(l="E3.3: Funding Coordination", p=9, t=c(3, 6), d="Aligning partner resources for sustainability."),
    # E4
    list(l="E4.1: Research Agenda", p=10, t=c(1, 5), d="Setting continental priorities for health research."), 
    list(l="E4.2: Research Ecosystem", p=10, t=c(1, 5), d="Strengthening institutional capacity for trials."), 
    list(l="E4.3: Policy Guidance", p=10, t=c(1, 5), d="Translating research into actionable policy."),
    # E5
    list(l="E5.1: Partner Onboarding", p=11, t=c(4, 6), d="Managing strategic alliances and global peers."), 
    list(l="E5.2: Community Engagement", p=11, t=c(4, 6), d="Active community participation in health planning."),
    # E6
    list(l="E6.1: Regional Priority Alignment", p=12, t=c(2, 3, 4), d="Aligning activities with regional economic needs."), 
    list(l="E6.2: In-country presence", p=12, t=c(2, 3, 4), d="Strengthening the technical footprint in Member States."),
    # E7
    list(l="E7.1: Accountability/Transparency", p=13, t=1:6, d="Streamlining governance for clear accountability."), 
    list(l="E7.2: Digitized Operations", p=13, t=1:6, d="Using digital tools to optimize efficiency."), 
    list(l="E7.3: Africa CDC Workforce", p=13, t=1:6, d="Attracting and retaining high-caliber talent.")
  )
  
  moons <- lapply(obj_data, function(o) {
    p_node <- planets[planets$id == o$p, ]
    m_theta = runif(1, 0, 2*pi)
    data.frame(
      label = o$l, parent_id = o$p, targets = paste(o$t, collapse=","),
      mx = p_node$x + 4*cos(m_theta), my = p_node$y + 4*sin(m_theta), mz = p_node$z + runif(1, -2, 2),
      px = p_node$x, py = p_node$y, pz = p_node$z, size = 15, color = "#ADFF2F",
      info = paste0("<b>Objective:</b> ", o$l, "<br><b>Focus:</b> ", o$d)
    )
  }) %>% bind_rows()
  
  return(list(sun=sun, planets=planets, moons=moons))
}

# --- 2. DATABASE INITIALIZATION ---
db_name <- "narcc_v9_secure.sqlite"
con <- dbConnect(SQLite(), db_name)

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

# --- 3. UI ---
ui <- navbarPage(
  title = "NARCC Portal",
  theme = bs_theme(bootswatch = "flatly"),
  
  # 1st Tab: Activity Tracker
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
  
  # 2nd Tab: Admin Panel
  tabPanel("Admin & Management",
           fluidPage(
             column(8, offset = 2,
                    wellPanel(
                      passwordInput("admin_pin", "Admin PIN"),
                      uiOutput("admin_panel_ui")
                    )
             )
           )
  ),
  
  # 3rd Tab: Strategic Universe (LAST)
  tabPanel("Strategic Universe",
           fluidPage(
             theme = bslib::bs_theme(bootswatch = "darkly"),
             sidebarLayout(
               sidebarPanel(
                 h4("Objective Details"),
                 uiOutput("selection_text"),
                 hr(),
                 helpText("Click on any Moon (green circle) to trigger the path mapping.")
               ),
               mainPanel(plotlyOutput("universe", height = "900px"))
             )
           )
  )
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  refresh <- reactiveVal(0)
  data_master <- generate_complete_system()
  selected_click <- reactiveVal("None")
  
  # --- NARCC TRACKER LOGIC ---
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
    get_sel <- function(val) { if (is.null(val) || is.na(val) || val == "" || val == "NA") return(character(0)) else return(val) }
    
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
      textAreaInput("u_note", "Narrative/Comments", value = ifelse(is.na(curr$narrative), "", curr$narrative), rows = 3),
      passwordInput("u_auth_email", "Verify Your Email"),
      actionButton("do_update", "Submit Update", class = "btn-primary", width = "100%")
    )
  })
  
  observeEvent(input$do_update, {
    act <- dbGetQuery(con, "SELECT focal_email FROM activities WHERE id = ?", list(input$sel_id))
    if (tolower(trimws(input$u_auth_email)) != tolower(act$focal_email)) return(showNotification("Email Mismatch", type = "error"))
    safe_val <- function(x) { if(is.null(x)) return("NA") else return(x) }
    
    dbExecute(con, "UPDATE activities SET is_approved=?, is_completed=?, is_reported=?, is_retired=?, is_decommitted=?, narrative=?, updated=? WHERE id=?",
              list(safe_val(input$u_appr), safe_val(input$u_comp), safe_val(input$u_repo), safe_val(input$u_reti), 
                   safe_val(input$u_decom), input$u_note, as.character(Sys.time()), input$sel_id))
    updateSelectInput(session, "sel_id", selected = "")
    showNotification("Database Updated Successfully")
    refresh(refresh() + 1)
  })
  
  # --- STRATEGIC UNIVERSE LOGIC ---
  observe({
    click_data <- event_data("plotly_click", source = "universe_main")
    if (!is.null(click_data)) selected_click(click_data$customdata)
  })
  
  output$selection_text <- renderUI({
    if (selected_click() == "None") {
      p("No objective selected. Click a moon to begin.")
    } else if (selected_click() %in% data_master$moons$label) {
      m <- data_master$moons[data_master$moons$label == selected_click(), ]
      tagList(h3(m$label), HTML(m$info))
    } else {
      p(paste("Focused on:", selected_click()))
    }
  })
  
  output$universe <- renderPlotly({
    fig <- plot_ly(source = "universe_main") %>%
      add_trace(data=data_master$sun, x=~x, y=~y, z=~z, text=~label, customdata=~label, type='scatter3d', mode='markers+text', hoverinfo = 'text', hovertext=~info, marker=list(size=~size, color=~color)) %>%
      add_trace(data=data_master$planets, x=~x, y=~y, z=~z, text=~label, customdata=~label, type='scatter3d', mode='markers+text', hoverinfo = 'text', hovertext=~info, marker=list(size=~size, color=~color)) %>%
      add_trace(data=data_master$moons, x=~mx, y=~my, z=~mz, customdata=~label, type='scatter3d', mode='markers', hoverinfo = 'text', hovertext=~info, marker=list(size=~size, color=~color))
    
    curr_sel <- selected_click()
    if(curr_sel != "None" && curr_sel %in% data_master$moons$label) {
      m <- data_master$moons[data_master$moons$label == curr_sel, ]
      target_ids <- as.numeric(unlist(strsplit(m$targets, ",")))
      p_info <- data_master$planets[data_master$planets$id == m$parent_id, ]
      fig <- fig %>% add_trace(x=c(m$mx, m$px), y=c(m$my, m$py), z=c(m$mz, m$pz), type='scatter3d', mode='lines', line=list(color='white', width=6), showlegend=F)
      for(tid in target_ids) {
        dest_p <- data_master$planets[data_master$planets$id == tid, ]
        if(p_info$type == "Enabler") {
          fig <- fig %>% add_trace(x=c(m$px, dest_p$x), y=c(m$py, dest_p$y), z=c(m$pz, dest_p$z), type='scatter3d', mode='lines', line=list(color='cyan', width=4, dash='dot'), showlegend=F)
        }
        fig <- fig %>% add_trace(x=c(dest_p$x, 0), y=c(dest_p$y, 0), z=c(dest_p$z, 0), type='scatter3d', mode='lines', line=list(color='gold', width=4), showlegend=F)
      }
    }
    no_axis <- list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE, title = "")
    fig %>% layout(scene = list(xaxis = no_axis, yaxis = no_axis, zaxis = no_axis, bgcolor = "black"), hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12), align = "left"))
  })
  
  # --- ADMIN ACTIONS ---
  output$admin_panel_ui <- renderUI({
    if (is.null(input$admin_pin) || input$admin_pin != "1234") return(helpText("Enter PIN (given)"))
    tagList(
      navset_card_pill(
        nav_panel("Registration", br(), textInput("reg_code", "Code"), textInput("reg_name", "Title"), uiOutput("focal_dropdown_ui"), actionButton("do_reg", "Register Activity", class = "btn-success", width = "100%")),
        nav_panel("Focal Persons", br(), textInput("new_email", "Authorize Email"), actionButton("add_email", "Add to Registry", class = "btn-info"), hr(), DTOutput("email_table")),
        nav_panel("Power BI Push", br(), h4("Manual Data Synchronization"), actionButton("push_csv", "Push Data to Power BI (CSV)", class = "btn-danger", icon = icon("share-from-square"), width = "100%"))
      )
    )
  })
  
  output$focal_dropdown_ui <- renderUI({
    refresh(); emails <- dbGetQuery(con, "SELECT email FROM focal_registry")$email
    selectInput("reg_focal_email", "Assign Focal Person", choices = emails)
  })
  
  observeEvent(input$do_reg, {
    dbExecute(con, "INSERT INTO activities (code, name, focal_email, is_approved, is_completed, is_reported, is_retired, is_decommitted, narrative, updated) VALUES (?,?,?,?,?,?,?,?,?,?)",
              list(input$reg_code, input$reg_name, tolower(input$reg_focal_email), "NA", "NA", "NA", "NA", "NA", "", as.character(Sys.time())))
    updateTextInput(session, "reg_code", value = ""); updateTextInput(session, "reg_name", value = "")
    refresh(refresh() + 1); showNotification("Registered Successfully")
  })
  
  observeEvent(input$push_csv, {
    write.csv(dbGetQuery(con, "SELECT * FROM activities"), "NARCC_Master_Data.csv", row.names = FALSE)
    showNotification("CSV Refreshed", type = "message")
  })
  
  observeEvent(input$add_email, {
    dbExecute(con, "INSERT OR IGNORE INTO focal_registry VALUES (?,?)", list(tolower(input$new_email), as.character(Sys.Date())))
    updateTextInput(session, "new_email", value = ""); refresh(refresh() + 1)
  })
  
  output$email_table <- renderDT({ refresh(); datatable(dbGetQuery(con, "SELECT * FROM focal_registry"), selection = 'single') })
}

onStop(function() { dbDisconnect(con) })
shinyApp(ui, server)