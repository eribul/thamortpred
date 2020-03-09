njr_filters <-
  tibble(
    step = 1:5,
    incl_text = c(
      "THA 2008-2015\\l (N = 579,798)\\l",
      "THA due to OA\\l (N = 515,720)\\l",
      "Patients with OA\\l (N = 460,209)\\l",
      "Patients with OA\\l and cemented THA\\l (N = 153,477)\\l",
      "Validation sample\\l (N = 125,428)\\l"
    ),
    excl_next = c(
      "Other diagnoses than OA\\l(N = 64,078)\\l",
      "First THA for bilateral cases\\l(N = 55,511)\\l",
      "Exclusion of (N = 306,732):\\l  - Uncemented (N = )\\l  - Hybrid (N = )\\l  - Reverse hybrid (N = )\\l",
      "Exclusion of (N = 28,049):\\l  - Comorbidity missing (N = 27,181)\\l  - ASA class IV-V (N = 868)\\l",
      NA
    )
  )

flowchart_njr <- flowchart(njr_filters)


combine_graphs(flowchart_shar, flowchart_njr) %>%
  export_graph(
    "graphs/flowchart.png", "png", width = 1024,
    title = "SHAR (model development)                                        NJR (external validation)"
  )
