

xVar <- "Period"
yVar <- "Value"  
stackVar <- "Commodity.Code"
# srackMax <- 5
otherCompulsoryVars <- c("Reporter.Code", "Trade.Flow")
exp <- "Export"
imp <- "Import"
horizontalLine <- "Trade balance"
p_dataName <- c(xVar, yVar, stackVar, otherCompulsoryVars, horizontalLine, exp, imp)

# Extracting plotting data and calculating trade balance
p_data <-
  df %>%
  join_lables() %>%
  select_(.dots = names(.)[names(.) %in% p_dataName]) %>%
  spread(., Trade.Flow, Value, fill = 0) %>%
  select_(.dots = names(.)[names(.) %in% p_dataName]) %>%
  mutate_(.dots = setNames(str_c("-", imp), imp))%>%
  mutate_(.dots = setNames(str_c(imp, "+", exp), horizontalLine)) %>%
  gather(Trade.Flow, Value, 4:length(.))

# Calculatgin how many stacks are present
nStacks <- 
  p_data %>% 
  group_by_(.dots = stackVar) %>% 
  distinct %>% 
  nrow()

# Implement later Rank stacks and group them into other groups

# Ordering the stacking variable propperly.
# Here we may loose some categoreis which are only available in the past years.
stackingOrder <-
  p_data %>%
  mutate_(.dots = setNames(str_c("abs(", yVar,")"), yVar)) %>%
  filter(Trade.Flow %in% c(imp, exp)) %>%
  filter_(.dots = str_c("as.numeric(", xVar, ")==max(as.numeric(", xVar, "),na.rm = TRUE)")) %>%
  group_by_(.dots = c(xVar, stackVar)) %>%
  filter_(.dots = str_c(yVar, "==max(", yVar,",na.rm = TRUE)")) %>%
  ungroup() %>%
  mutate(stackOrder = row_number(desc(Value))) %>% 
  arrange(Value) %>%
  select_(.dots = c(stackVar, "stackOrder")) 

# Determine colour scale
palitra <-
  setNames(stackingOrder %>% 
           arrange(stackOrder) %>% 
             mutate(stackOrder = stackOrder + 1) %>% 
           .[["stackOrder"]], 
         stackingOrder %>% 
           arrange(stackOrder) %>% 
           .[["Commodity.Code"]])


# Adding order to plotting data
p_data <-
  p_data %>% 
  left_join(stackingOrder, by = stackVar) %>%
    mutate(Commodity.Code = factor(Commodity.Code, levels = stackingOrder[[stackVar]]))
  
  
  
p <- 
  ggplot(p_data) +
  aes_string(x = xVar, y = yVar, fill = stackVar) +
  geom_hline(aes(yintercept = 0))

# Imp
p <-
  p + 
    geom_bar(
    data = p_data %>% filter(Trade.Flow == imp),
    colour = "black",
    stat = "identity",
    position = "stack"
  )

# Exp
p <- 
  p +
  geom_bar(
    data = p_data %>% filter(Trade.Flow == exp),
    colour = "black",
    stat = "identity",
    position = "stack"
  )

p + 
  scale_fill_brewer(palette = length(palitra), breaks = names(palitra), labels = names(palitra), limits = names(palitra))

p + 
  discrete_scale("fill", "brewer", brewer_pal(type, palette, direction), breaks = palitra)

+ 
  scale_fill_manual(values = palitra)





