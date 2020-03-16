
library(tidyverse)

df <- read.csv('blur.csv')

head(df)

df_mod <- df %>% gather(key=dir, value = value, x, y, avg)

df_mod

g <- ggplot(df, aes(x=dktype, y=x, color=dktype))
g <- g + geom_boxplot()
plot(g)

ggplot(data = df) +
    geom_boxplot(mapping = aes(x = dktype, y = x, color=dktype)) +
    ggtitle('Beam blur X')
ggsave('beam_blur_x.png')

ggplot(data=df) +
    geom_boxplot(mapping = aes(x = dktype, y = y, color=dktype)) +
    ggtitle('Beam blur Y')
ggsave('beam_blur_y.png')

ggplot(data = df_mod) +
    geom_boxplot(mapping = aes(x = dktype, y = value, color=dir))
ggsave('all.png')

ggplot(data = df_mod) +
    geom_boxplot(mapping = aes(x = dktype, y = value, color = dktype)) +
    facet_wrap(~dir)
ggsave("all.png", width=7, height=4)
