library(tidyverse)

colnames <- c('datetime', 't2_x', 't2_y', 't3_x', 't3_y', 't4_x', 't4_y', 't5_x', 't5_y')
df <- readr::read_table('rel.log', col_names = colnames)
df$datetime <- strptime(df$datetime, "%Y%m%d-%H:%M:%S")
df$datetime <- as.POSIXct(df$datetime)
df2 <- df %>%
	tidyr::gather(key=type, value=value, 't2_x':'t5_y')
df3 <- df2 %>%
	tidyr::separate(type, c('type', 'direction'), sep='_', remove=TRUE)

ggplot(data=df3) +
	geom_line(mapping=aes(x=datetime, y=value, color=direction)) + 
	geom_point(mapping=aes(x=datetime, y=value, color=direction)) +
	facet_wrap( ~ type, ncol=2, nrow=2, scales='free')

ggsave('rel.png', dpi=100, width=9.6, height=7.2)

