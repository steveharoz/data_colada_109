library(tidyverse)
library(glue)
library(xml2)

PATH = "~/Relook/2023/datacolada/109"

#### parse calcChain ####

calcChain = xml2::read_xml(glue("{PATH}/calcChain.xml"))

# get the order of calculations
calcChain = calcChain %>%
  xml_children() %>% 
  xml_attr("r")

# put it in a table
data_calc_order = tibble(
  cell = calcChain
)

data_calc_order = data_calc_order %>% 
  # split the column and row
  separate_wider_regex(cell, patterns=c(column="[A-Z]+", row="\\d+")) %>% 
  # Excel data starts at row 2 (row 1 is the header), so subtract 1
  mutate(row = as.numeric(row) - 1) %>% 
  # only look at the fully calculated columns M and P-T
  #filter(column %in% c("M", "P", "Q", "R", "S", "T")) %>% 
  filter(nchar(column) == 1) %>% 
  # calc order within each column
  mutate(
    .by = column,
    column_calc_order = 1:n()
  ) %>% 
  # sort by column
  arrange(column)

# pivot wide to have one column for each column's order
data_calc_order = data_calc_order %>% 
  mutate(column = glue("order_{column}")) %>% 
  pivot_wider(names_from = column, values_from = column_calc_order)



#### load data ####
data = read_csv(glue("{PATH}/S1_flagged_2023_06_09.csv"))

# better names
data = data %>% 
  rename(
    participant = `P#`,
    flag_DC = flag
  )

# set row id for joining
data = data %>% 
  mutate(row = 1:n()) %>% 
  # make row the first column
  relocate(row)




#### combine data and calcchain ####

data = data %>% left_join(data_calc_order) 


# rank the orders by column
data = data %>% 
  mutate(
    across(starts_with("order_"), rank, na.last="keep")
  )
write_csv(data, glue("{PATH}/output/all_columns_order.csv"))


# rank the orders by condition, so jumps can be more easily spotted
data = data %>% 
  mutate(
    .by = Cond,
    across(starts_with("order_"), rank, na.last="keep")
  )
write_csv(data, glue("{PATH}/output/all_columns_order_by_condition.csv"))


#### duplicated rows ####

data = data %>% 
  # duplicated
  mutate(
    .by = c(participant, Major, Age),
    duplicated = n() > 1
  )


#### non-calculated column in calcchain ####

# does any cell from a non-calculated column appear in calcChain?
data = data %>% 
  mutate(
    .by = row,
    manipulated = any(!across(order_C:order_K, is.na))
  )


#### mark what's out of order ####

# if the calcChain order is one more than the previous row, it's fine
# if the calcChain order is one less than the next row, it's fine
# if the calcChain order has a jump, but it's both bigger than the previous and smaller than the next, it's fine
data = data %>% 
  mutate(
    .by = Cond,
    increment_previous = order_R == lag(order_R, default=0) + 1,
    decrement_next = order_R == lead(order_R, default=max(order_R)+1) - 1,
    bigger_than_previous = order_R > lag(order_R, default=0),
    smaller_than_next = order_R < lead(order_R, default=999),
    out_of_order = !increment_previous & !decrement_next & !(bigger_than_previous & smaller_than_next)
  )



# simple view
data %>% 
  #filter(Cond!=0) %>% 
  mutate(flag_SH = 0 + (out_of_order | duplicated)) %>% 
  mutate(out_of_order = ifelse(out_of_order, "XXX", "")) %>% 
  mutate(duplicated = ifelse(duplicated, "XXX", "")) %>% 
  select(row, flag_DC, flag_SH, participant, Cond, SumDeduction, order_R, out_of_order, duplicated) %>% 
  view() %>% 
  write_csv(glue("{PATH}/output/simple_order_R.csv"))


#### plot it ####

# make a custom beeswarm layout to also shows participant number
data %>% 
  filter(Cond!=0) %>% 
  arrange(SumDeduction) %>% 
  mutate(y = round(SumDeduction)) %>% 
  mutate(
    .by = c(Cond, y),
    row = 1:n() - 1,
    x = Cond + (row - max(row)/2) / 8
  ) %>% 
  mutate(flag = ifelse(duplicated, "Duplicated   ", 
                  ifelse(out_of_order, "Out of Order   ", 
                    ifelse(manipulated, "Manipulated (H-K)", 
                      "As expected   ")))) %>% 
  ggplot() + 
    aes(x=-x, y=round(SumDeduction), fill=flag) + 
    geom_label(aes(label=participant), size=4, color="white") + 
    scale_x_continuous(breaks = c(-2, -1), limits = c(-2.5,-0.5), labels = c("Sign-Bottom", "Sign-Top")) +
    scale_fill_manual(values = c("#377EB8", "#666666", "#E41A1C", "#FF7F00"), 
                      breaks = c("As expected   ", "Duplicated   ", "Out of Order   ", "Manipulated (H-K)")) +
    guides(fill = guide_legend(override.aes = list(label = "#" ))) +
    theme_classic(15) + theme(legend.position = "top") +
    labs(x = NULL, y = "Expenses claimed ($)", fill = NULL, title = "order_R")

ggsave(glue("{PATH}/output/steve plot order_R manipulated.png"), width = 2200, height = 2200, units = "px")

