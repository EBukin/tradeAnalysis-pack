#' Function for plotting trade balance of one country staking multiple partners at one bar
plot_tb <-
  function(df,
           xVar = "Period",
           yVar = "Value",
           stackVar = "Commodity.Code",
           srackMax = 100,
           xVarName = NA,
           yVarName = NA,
           stackVarName = NA,
           exp = "Export",
           imp = "Import",
           otherCompulsoryVars = c("Reporter.Code", "Trade.Flow"),
           groupVar = "Trade.Flow",
           colourVar = "Trade.Flow",
           horizontalLine = "Trade balance",
           brewScale = TRUE,
           brewScaleType = "seq",
           brewPalName = "Set1",
           revertColours = FALSE) {
    require(plyr)
    require(tidyverse)
    require(dplyr)
    require(tidyr)
    require(RColorBrewer)
    require(scales)
    require(ggplot2)
    
    # xVar <- "Period"
    # xVarName <- NA
    # yVar <- "Value"
    # yVarName <- NA
    # stackVar <- "Partner.Code"
    # stackVarName <- "Partner Code" # NA by default
    # srackMax <- 5
    # if("Reporter" %in% names(df)) {
    #   otherCompulsoryVars <- c("Reporter", "Trade.Flow")
    # } else {
    #   otherCompulsoryVars <- c("Reporter.Code", "Trade.Flow")
    # }
    # 
    # exp <- "Export"
    # imp <- "Import"
    # horizontalLine <- "Trade balance"
    stacVarValeuTradeBal <- '"All"'
    # brewPalName = "Set1"
    # revertColours = FALSE
    
    p_dataName <-
      c(xVar,
        yVar,
        stackVar,
        otherCompulsoryVars,
        groupVar,
        horizontalLine,
        exp,
        imp,
        colourVar) %>% 
      unique()

    # Names for variables in legend
    if (is.na(stackVarName)) {
      stackVarName <- stackVar
    }
    # Names for variables in legend
    if (is.na(xVarName)) {
      xVarName <- xVar
    }
    # Names for variables in legend
    if (is.na(yVarName)) {
      yVarName <- "- Import / + Export (milions USD)"
    }
    
    # Extracting plotting data
    p_data <-
      df %>%
      filter(Trade.Flow.Code %in% c(1, 2)) %>% 
      join_lables() %>%
      select_(.dots = names(.)[names(.) %in% p_dataName]) %>%
      spread(., Trade.Flow, Value, fill = 0) %>%
      select_(.dots = names(.)[names(.) %in% p_dataName]) %>%
      mutate_(.dots = setNames(str_c("-", imp), imp)) %>%
      mutate_(.dots = setNames(str_c(imp, "+", exp), horizontalLine)) %>%
      gather(Trade.Flow, Value, (df %>% select_(.dots = names(.)[names(.) %in% p_dataName]) %>% length(.)+1):length(.))
    
    # Calculating trade balance
    p_data <-
      p_data %>%
      filter(Trade.Flow == horizontalLine) %>%
      group_by_(.dots = names(.)[names(.) %in% c(xVar, otherCompulsoryVars)]) %>%
      summarise_(.dots = setNames(str_c("sum(", yVar, ", na.rm = TRUE)"), yVar)) %>%
      # add_column(setNames(rep(horizontalLine, nrow(.)), stackVar)) %>%
      ungroup() %>%
      bind_rows(p_data %>%
                  filter(Trade.Flow != horizontalLine)) %>%
      # mutate(Commodity.Code = ifelse(is.na(Commodity.Code), stacVarValeuTradeBal, Commodity.Code))
      mutate_(.dots = setNames(
        str_c(
          "ifelse(is.na(",
          stackVar,
          "),",
          stacVarValeuTradeBal ,
          ", ",
          stackVar,
          ")"
        ),
        stackVar
      ))
    
    # Calculatgin how many stacks are present
    nStacks <-
      p_data %>%
      group_by_(.dots = stackVar) %>%
      distinct %>%
      nrow()
    
    # Implement later Rank stacks and group them into other groups
    
    # Ordering the stacking variable propperly. It takes the last time period values
    # as the main on and then if in the past there were any other variables, it also uses those
    # but as the second priority.
    stackingOrder <-
      p_data %>%
      mutate_(.dots = setNames(str_c("abs(", yVar, ")"), yVar)) %>%
      filter(Trade.Flow %in% c(imp, exp)) %>%
      filter_(.dots = str_c(
        "as.numeric(",
        xVar,
        ")==max(as.numeric(",
        xVar,
        "),na.rm = TRUE)"
      )) %>%
      group_by_(.dots = c(xVar, stackVar)) %>%
      filter_(.dots = str_c(yVar, "==max(", yVar, ",na.rm = TRUE) & ", yVar, "!= 0")) %>%
      ungroup()
    
    stackingOrder <-
      p_data %>%
      mutate_(.dots = setNames(str_c("abs(", yVar, ")"), yVar)) %>%
      filter(Trade.Flow %in% c(imp, exp)) %>%
      filter_(.dots = str_c(
        "as.numeric(",
        xVar,
        ")!=max(as.numeric(",
        xVar,
        "),na.rm = TRUE)"
      )) %>%
      group_by_(.dots = c(stackVar)) %>%
      filter_(.dots = str_c(yVar, "==max(", yVar, ",na.rm = TRUE)& ", yVar, "!= 0")) %>%
      ungroup() %>%
      anti_join(stackingOrder, by = stackVar) %>%
      bind_rows(stackingOrder) %>%
      mutate(stackOrder = row_number(desc(Value))) %>%
      arrange(Value) %>%
      select_(.dots = c(stackVar, "stackOrder"))
    
    
    # Define colours for categories.
    # myPal <-
    #   colorRampPalette(brewer.pal(max(3, min(8, nStacks)), name = brewPalName), bias = 2)
    myPal <-
      colorRampPalette(brewer_pal(type = brewScaleType, 
                                  palette = brewPalName,
                                  direction = 1)(max(3, min(8, nrow(stackingOrder)))), bias = 1)
    
    # Determine colour scale with the names for each colour
    if (revertColours) {
      palLegend <-
        setNames(rev(myPal(nrow(stackingOrder))),
                 stackingOrder %>%
                   arrange(stackOrder) %>%
                   .[[stackVar]])
    } else {
      palLegend <-
        setNames(myPal(nrow(stackingOrder)),
                 stackingOrder %>%
                   arrange(stackOrder) %>%
                   .[[stackVar]])
    }
    
    # Adding order to plotting data
    p_data[stackVar] <- eval(parse(text = str_c("factor(p_data$", stackVar,", levels = stackingOrder$", stackVar, ")")))
    
    # Initializing plot
    p <-
      ggplot(p_data) +
      aes_string(x = xVar, y = yVar, fill = stackVar, group = groupVar) +
      geom_hline(aes(yintercept = 0))
    
    # Imp
    p <-
      p +
      geom_bar(
        data = p_data %>% filter(Trade.Flow == imp) %>% arrange_(xVar, yVar),
        colour = "black",
        stat = "identity",
        position = "stack"
      )
    
    # Exp
    p <-
      p +
      geom_bar(
        data = p_data %>% filter(Trade.Flow == exp) %>% arrange_(xVar, str_c("-",yVar)),
        colour = "black",
        stat = "identity",
        position = "stack"
      )
    
    # Trade balance
    p <-
      p +
      geom_point(
        mapping = aes_string(
          x = xVar,
          y = yVar,
          group = groupVar,
          colour = colourVar
        ),
        data = p_data %>% filter(Trade.Flow == horizontalLine) ,
        inherit.aes = FALSE
      ) +
      geom_line(
        mapping = aes_string(
          x = xVar,
          y = yVar,
          group = groupVar,
          colour = colourVar
        ),
        data = p_data %>% filter(Trade.Flow == horizontalLine),
        inherit.aes = FALSE
      ) 
    
    # Adding legend
    p <-
      p +
      labs(
        x = xVarName,
        y = yVarName,
        colour = "",
        fill = stackVarName
      ) +
      scale_color_grey()
    
    # Adding scale 
    if(brewScale) {
      p <- 
      p + 
      scale_fill_manual(values = palLegend)
    }
    
    p
    
  }


