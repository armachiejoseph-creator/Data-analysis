# viz_logic.R

# 1. Funders Bar Chart
render_funder_bar <- function(funding_data) {
  renderPlotly({
    funding_data %>%
      plot_ly(x = ~funder_name, y = ~amount, type = 'bar', marker = list(color = '#006B3F')) %>%
      layout(title = "Total Funding by Funder", xaxis = list(title = "Funder"), yaxis = list(title = "Amount ($)"))
  })
}

# 2. Allocation by Pillar Bar Chart
render_pillar_bar <- function(allocations_data) {
  renderPlotly({
    allocations_data %>%
      group_by(pillar) %>%
      summarise(total_allocated = sum(allocated_amount, na.rm = TRUE)) %>%
      plot_ly(x = ~pillar, y = ~total_allocated, type = 'bar', marker = list(color = '#D4AF37')) %>%
      layout(title = "Total Allocations by Pillar", xaxis = list(title = "Pillar"), yaxis = list(title = "Total Amount ($)"))
  })
}

# 3. Financial Flow Sankey
render_sankey <- function(funding, allocat, activity) {
  renderPlotly({
    # Prepare data layers
    sam_fund <- funding %>% group_by(funder_name) %>% summarise(amount = sum(amount, na.rm = TRUE), .groups = 'drop') %>% mutate(From = "Total Funding") %>% rename(To = funder_name)
    funder_map <- setNames(funding$funder_name, funding$id)
    sam_alloc <- allocat %>% mutate(From = ifelse(is.na(funder_map[as.character(grant_id)]), "Unknown Funder", funder_map[as.character(grant_id)])) %>% group_by(From, pillar) %>% summarise(amount = sum(allocated_amount, na.rm = TRUE), .groups = 'drop') %>% rename(To = pillar)
    sam_acti <- activity %>% group_by(pillar) %>% summarise(amount = sum(budget, na.rm = TRUE), .groups = 'drop') %>% mutate(To = "Total Used") %>% rename(From = pillar)
    
    sankey_links <- bind_rows(sam_fund, sam_alloc, sam_acti)
    nodes <- unique(c(sankey_links$From, sankey_links$To))
    node_map <- setNames(0:(length(nodes) - 1), nodes)
    
    plot_ly(type = "sankey", node = list(label = nodes, pad = 15, thickness = 20),
            link = list(source = node_map[sankey_links$From], target = node_map[sankey_links$To], value = sankey_links$amount))
  })
}