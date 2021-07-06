# read in MLB script
source("packages.R")

MLB_files = read_excel_allsheets(list.files(pattern = '.xls.*', full.names = TRUE))


MLB_full = MLB_files[[1]] %>% 
  .[-1:-3,] %>% 
  setNames(., c("sample_name", "perc_labile","perc_recalcitrant","ecosystem")) %>%
  dplyr::mutate(across(matches('perc'), ~as.numeric(.x)))

MLB_full%>%
  ggplot() +
  geom_density(aes(perc_labile, ..count..), size = 1.2, fill = 'blue',  color = 'blue', alpha = 0.5) +
  scale_x_continuous(name = "Percent of labile ", limits = c(0,100), expand = c(0.01,0.01))+
  scale_y_continuous(name = "# of observations", limits = c(0,5), expand = c(0.01,0.01))+
  theme_minimal()
