# template_forest_tte generates correct expressions

    Code
      res
    Output
      $data
      {
          anl <- adtte %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", 
                  "ARM C"))) %>% dplyr::mutate(is_event = CNSR == 0)
          parent <- ANL_ADSL %>% dplyr::filter(ARMCD %in% c("ARM A", 
              "ARM B", "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", 
                  "ARM C")))
      }
      
      $summary
      {
          df <- extract_survival_subgroups(variables = list(tte = "AVAL", 
              is_event = "is_event", arm = "ARMCD", subgroups = c("SEX", 
                  "BMRKR2"), strata = "STRATA2"), control = control_coxph(conf_level = 0.9), 
              data = anl)
      }
      
      $table
      {
          result <- rtables::basic_table() %>% tabulate_survival_subgroups(df, 
              vars = c("n_tot_events", "n_events", "median", "hr", 
              "ci"), time_unit = as.character(anl$AVALU[1]), riskdiff = NULL)
      }
      
      $plot
      $plot[[1]]
      f <- g_forest(tbl = result, col_symbol_size = NULL, font_size = 15, 
          as_list = TRUE)
      
      $plot[[2]]
      table <- f[["table"]] + ggplot2::labs(title = "Forest Plot of Survival Duration for \nStratified by STRATA2", 
          subtitle = NULL)
      
      $plot[[3]]
      plot <- f[["plot"]] + ggplot2::labs(caption = "")
      
      

# template_forest_tte works with risk difference column added

    Code
      res
    Output
      $data
      {
          anl <- adtte %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", 
                  "ARM C"))) %>% dplyr::mutate(is_event = CNSR == 0)
          parent <- ANL_ADSL %>% dplyr::filter(ARMCD %in% c("ARM A", 
              "ARM B", "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", 
                  "ARM C")))
      }
      
      $summary
      {
          df <- extract_survival_subgroups(variables = list(tte = "AVAL", 
              is_event = "is_event", arm = "ARMCD", subgroups = c("SEX", 
                  "BMRKR2"), strata = "STRATA2"), control = control_coxph(conf_level = 0.9), 
              data = anl)
      }
      
      $table
      {
          result <- rtables::basic_table() %>% tabulate_survival_subgroups(df, 
              vars = c("n_tot", "hr", "ci"), time_unit = as.character(anl$AVALU[1]), 
              riskdiff = list(arm_x = NULL, arm_y = NULL, format = "xx.x (xx.x - xx.x)", 
                  col_label = "Prop. Diff", pct = TRUE))
      }
      
      $plot
      $plot[[1]]
      f <- g_forest(tbl = result, col_symbol_size = NULL, font_size = 15, 
          as_list = TRUE)
      
      $plot[[2]]
      table <- f[["table"]] + ggplot2::labs(title = "Forest Plot of Survival Duration for \nStratified by STRATA2", 
          subtitle = NULL)
      
      $plot[[3]]
      plot <- f[["plot"]] + ggplot2::labs(caption = "")
      
      

