flowchart <- function(desc) {

  nodes <-
    desc %>%
    pivot_longer(-step) %>%
    filter(!is.na(value)) %>%
    unite("node", step, name, remove = FALSE) %>%
    add_rowindex() %>%
    rename(
      id    = .row,
      label = value,
      rank  = step
    ) %>%
    mutate(
      shape     = "rectangle",
      width     = if_else(startsWith(name, "incl"), 1.5, 2.5),
      color     = "Black",
      fillcolor = "White",
      fontcolor = "Black"
    )


  # Define edges for graph --------------------------------------------------

  incl_edges <-
    nodes %>%
    filter(startsWith(name, "incl"))

  excl_edges <-
    nodes %>%
    filter(startsWith(name, "excl"))

  edges <-
    tibble(
      from = incl_edges$id,
      to1 = lead(incl_edges$id),
      to2 = c(excl_edges$id, NA)
    ) %>%
    pivot_longer(-from, values_to = "to") %>%
    select(-name) %>%
    arrange(from, to) %>%
    filter(!is.na(to)) %>%
    {create_edge_df(
      .$from, .$to,
      color    = "black",
      penwidth = 2,
      len      = 1
    )}


  # Make graph --------------------------------------------------------------

  create_graph(
    nodes,
    edges
  ) %>%
  add_global_graph_attrs("layout", "dot", "graph") %>% # Apply cluster ranking
  add_global_graph_attrs("fixedsize", "FALSE", "node") # Nice edge height
}
