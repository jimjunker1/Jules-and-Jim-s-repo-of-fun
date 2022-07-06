# read in MLB script
source("packages.R")

MLB_excel_files = list.files(path = './data/', pattern = '*.xls*', full.names = TRUE)
                         
MLB_files = lapply(MLB_excel_files, function(x) read_excel_allsheets(x, skip = 2))

MLB_SERMACS_dfs = MLB_files[[2]] %>%
  setNames(., c("full", "new", "old"))

MLB_SERMACS_final = MLB_SERMACS_dfs %>%
  bind_rows(.id = 'type') %>%
  setNames(., c("type", "sample", "perc_labile", "perc_recal", "ecosystem"))
  
## histograms for SERMACS talk
# full histogram
MLB_SERMACS_final%>%
  dplyr::filter(type == "full") %>%
  ggplot() +
  geom_density(aes(perc_labile, ..count..), size = 1.5, fill = 'purple',  color = 'purple', alpha = 0.2) +
  scale_x_continuous(name = "% Biolabile ", limits = c(0,100), expand = c(0.03,0.03))+
  scale_y_continuous(name = "No. of observations", limits = c(0,5), expand = c(0.03,0.03))+
  # geom_vline(aes(xintercept=quantile(perc_labile,0.9)), size=1.5, color='darkgreen') +
  # geom_vline(aes(xintercept=quantile(perc_labile,0.1)), size=1.5, color='darkgreen') +
  # geom_vline(aes(xintercept=mean(perc_labile)), color = "mediumblue", size=1.75, linetype='longdash') +
  # geom_vline(aes(xintercept=median(perc_labile)), color = "orangered", size=2, linetype='twodash') +
  #theme(legend.background = element_rect('transparent'),legend.title = element_blank(), legend.position=c(0.9, 0.15)) +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid")) +
  theme(axis.text.x = element_text(colour="black",size=15,face="bold"),
        axis.text.y = element_text(colour="black",size=15,face="bold"),  
        axis.title.x = element_text(colour="black",size=20,face="bold"),
        axis.title.y = element_text(colour="black",size=20,face="bold")) +
  theme(legend.text=element_text(size=10,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) -> MLB_SERMACS_full
ggsave("./figures/MLB_SERMACS_full.svg", MLB_SERMACS_full, width = 6, height =  6, units = "in")


# new histogram
MLB_SERMACS_final%>%
  dplyr::filter(type == "new") %>%
  ggplot() +
  geom_density(aes(perc_labile, ..count..), size = 1.5, fill = 'darkgreen',  color = 'darkgreen', alpha = 0.2) +
  scale_x_continuous(name = "% Biolabile ", limits = c(0,100), expand = c(0.03,0.03))+
  scale_y_continuous(name = "No. of observations", limits = c(0,5), expand = c(0.03,0.03))+
  # geom_vline(aes(xintercept=quantile(perc_labile,0.9)), size=1.5, color='darkgreen') +
  # geom_vline(aes(xintercept=quantile(perc_labile,0.1)), size=1.5, color='darkgreen') +
  # geom_vline(aes(xintercept=mean(perc_labile)), color = "mediumblue", size=1.75, linetype='longdash') +
  # geom_vline(aes(xintercept=median(perc_labile)), color = "orangered", size=2, linetype='twodash') +
  #theme(legend.background = element_rect('transparent'),legend.title = element_blank(), legend.position=c(0.9, 0.15)) +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid")) +
  theme(axis.text.x = element_text(colour="black",size=15,face="bold"),
        axis.text.y = element_text(colour="black",size=15,face="bold"),  
        axis.title.x = element_text(colour="black",size=20,face="bold"),
        axis.title.y = element_text(colour="black",size=20,face="bold")) +
  theme(legend.text=element_text(size=10,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) -> MLB_SERMACS_new
ggsave("./figures/MLB_SERMACS_new.svg", MLB_SERMACS_new, width = 6, height =  6, units = "in")

# old histogram

MLB_SERMACS_final%>%
  dplyr::filter(type == "old") %>%
  ggplot() +
  geom_density(aes(perc_labile, ..count..), size = 1.5, fill = 'orange',  color = 'orange', alpha = 0.2) +
  scale_x_continuous(name = "% Biolabile ", limits = c(0,100), expand = c(0.03,0.03))+
  scale_y_continuous(name = "No. of observations", limits = c(0,5), expand = c(0.03,0.03))+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid")) +
  theme(axis.text.x = element_text(colour="black",size=15,face="bold"),
        axis.text.y = element_text(colour="black",size=15,face="bold"),  
        axis.title.x = element_text(colour="black",size=20,face="bold"),
        axis.title.y = element_text(colour="black",size=20,face="bold")) +
  theme(legend.text=element_text(size=10,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) -> MLB_SERMACS_old
ggsave("./figures/MLB_SERMACS_old.svg", MLB_SERMACS_old, width = 6, height =  6, units = "in")
## old and new

MLB_SERMACS_final%>%
  dplyr::filter(type == "old") %>%
  ggplot() +
  geom_density(aes(perc_labile, ..count..), size = 1.5, fill = 'orange',  color = 'orange', alpha = 0.2) +
  scale_x_continuous(name = "% Biolabile ", limits = c(0,100), expand = c(0.03,0.03))+
  scale_y_continuous(name = "No. of observations", limits = c(0,5), expand = c(0.03,0.03))+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid")) +
  theme(axis.text.x = element_text(colour="black",size=15,face="bold"),
        axis.text.y = element_text(colour="black",size=15,face="bold"),  
        axis.title.x = element_text(colour="black",size=20,face="bold"),
        axis.title.y = element_text(colour="black",size=20,face="bold")) +
  geom_density(data = MLB_SERMACS_final %>% dplyr::filter(type == "new"),
               aes( perc_labile, ..count..), size = 1.5, fill = 'darkgreen', color = 'darkgreen', alpha = 0.2, inherit.aes = FALSE )+ 
  theme(legend.text=element_text(size=10,face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) -> MLB_SERMACS_comb;MLB_SERMACS_comb
ggsave("./figures/MLB_SERMACS_comb.svg", MLB_SERMACS_comb, width = 6, height =  6, units = "in")


####### full figure for publication ------
MLB_full = MLB_files[[1]] %>% 
  .[-1:-3,] %>% 
  setNames(., c("sample_name", "perc_labile","perc_recalcitrant","ecosystem")) %>%
  dplyr::mutate(across(matches('perc'), ~as.numeric(.x)))

HDInterval::hdi(MLB_full$perc_labile, credMass = 0.75)
quantile(MLB_full$perc_labile, c(0.1,0.9))


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
  
MLB_full%>%
  ggplot(aes(x = perc_labile, ..density..)) +
  ggridges::geom_density_ridges_gradient( size = 1.2, quantile_lines = TRUE, quantile_fun = HDInterval::hdi,  color = 'blue', vline_linetype = 2) +
  scale_x_continuous(name = "Percent of labile ", limits = c(0,100), expand = c(0.01,0.01))+
  scale_y_continuous(name = "# of observations", limits = c(0,NA), expand = c(0.001,0.01))+
  theme_minimal()

MLB_full%>%
  ggplot(df, aes(x = dat, y = 0, fill = stat(quantile))) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) +
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none")