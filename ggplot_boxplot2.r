library(tidyverse)

df <- read::read_csv('data.csv')

df %>%
	dplyr:select(diameter, wattage) %>%
        geom_boxplot(aes(x=diameter, y=maxfil)) +
	geom_jitter(position=position_jitter(width=0.1), alpha=0.5,
		    mapping=aes(color=diameter)) +
	xlim(1.0, 100.0) +
	xlab('Diameter[um]') +
	ylab('Wattage')
ggsave('diameter_vs_wattage.png', dpi=100, width=7, height=4)
	
	
		                
