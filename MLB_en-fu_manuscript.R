# read in MLB script
source("packages.R")

MLB_files = read_excel_allsheets(list.files(pattern = '.xls.*', full.names = TRUE))


MLB_full = MLB_files[[1]] %>% 
  .[-1:-3,] %>% 
  setNames(., c("sample_name", "perc_labile","perc_recalcitrant","ecosystem")) %>%
  dplyr::mutate(across(matches('perc'), ~as.numeric(.x)))

MLB_full%>%
  ggplot() +
  geom_density(aes(perc_labile, ..count..), size = 1.5, fill = 'purple',  color = 'purple', alpha = 0.075) +
  scale_x_continuous(name = "% Biolabile ", limits = c(0,100), expand = c(0.01,0.01))+
  scale_y_continuous(name = "No. of observations", limits = c(0,5), expand = c(0.01,0.01))+
  geom_vline(aes(xintercept=quantile(perc_labile,0.9)), size=1.5, color='darkgreen') +
  geom_vline(aes(xintercept=quantile(perc_labile,0.1)), size=1.5, color='darkgreen') +
  geom_vline(aes(xintercept=mean(perc_labile)), color = "mediumblue", size=1.75, linetype='longdash') +
  geom_vline(aes(xintercept=median(perc_labile)), color = "orangered", size=2, linetype='twodash') +
  #theme(legend.background = element_rect('transparent'),legend.title = element_blank(), legend.position=c(0.9, 0.15)) +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid")) +
  theme(axis.text.x = element_text(colour="black",size=15,face="bold"),
        axis.text.y = element_text(colour="black",size=15,face="bold"),  
        axis.title.x = element_text(colour="black",size=20,face="bold"),
        axis.title.y = element_text(colour="black",size=20,face="bold")) +
  theme(legend.text=element_text(size=12,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
  

  #theme_minimal()
