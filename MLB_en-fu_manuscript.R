# read in MLB script
source("packages.R")

MLB_files = read_excel_allsheets(list.files(pattern = '.xls.*', full.names = TRUE))

MLB_full = MLB_files[[1]] %>% 
  .[-1:-3,] %>% 
  setNames(., c("sample_name", "perc_labile","perc_recalcitrant","ecosystem")) %>%
  dplyr::mutate(across(matches('perc'), ~as.numeric(.x)))

HDInterval::hdi(MLB_full$perc_labile, credMass = 0.75)

MLB_full%>%
  ggplot(aes(x = perc_labile, ..density..)) +
  ggridges::geom_density_ridges_gradient( size = 1.2, quantile_lines = TRUE, quantile_fun = HDInterval::hdi,  color = 'blue', vline_linetype = 2) +
  scale_x_continuous(name = "Percent of labile ", limits = c(0,100), expand = c(0.01,0.01))+
  scale_y_continuous(name = "# of observations", limits = c(0,NA), expand = c(0.001,0.01))+
  theme_minimal()


ggplot(df, aes(x = dat, y = 0, fill = stat(quantile))) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) +
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none"