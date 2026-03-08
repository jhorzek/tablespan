# single row data works

    Code
      tablespan(summarized_table[1, ], formula = as.formula(paste0("1 ~ ", paste0(
        colnames(summarized_table), collapse = " + "))))
    Output
               ┌───────┬───────┬───────┬─────────┬───────┬─────────┬───────┐
               │ cyl   │ vs    │ N     │ mean_hp │ sd_hp │ mean_wt │ sd_wt │
               ├───────┴───────┴───────┴─────────┴───────┴─────────┴───────┤
               │     4       0       1        91              2.14         │
               └───────────────────────────────────────────────────────────┘
      
      Column names: cyl, vs, N, mean_hp, sd_hp, mean_wt, sd_wt

