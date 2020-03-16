library(tidyverse)

df <- readr::read_csv('201812.a.test')

df0 <- df %>%
    dplyr::filter(df$mode == 0) %>%
    dplyr::select(lens, reso)
    
df1 <- df %>%
    dplyr::filter(df$mode == 1) %>%
    dplyr::select(lens, reso)

df1

x0 <- df0$lens
y0 <- df0$reso
x1 <- df1$lens
y1 <- df1$reso
xx0 = seq(5.775, 6.0, length=101)
xx1 = seq(5.725, 5.85, length=101)
result0 <- nls(y0 ~ (a*x0^2 + b*x0 + c),
               start = c(a=1, b=1, c=15))
               #start = c(a=2000, b=-30000, c=90000))
result1 <- nls(y1 ~ (a*x1^2 + b*x1 + c),
               start = c(a=1, b=1, c=100))
               #start = c(a=2000, b=-30000, c=90000))

yfit0 = predict(result0)
yfit1 = predict(result1)
sd(y0 - yfit0)
sd(y1 - yfit1)
mean(y0 - yfit0)
mean(y1 - yfit1)
rnorm(length(df0$lens),mean=0, sd=sd(y0-yfit0))

ggplot() +
    geom_line(mapping=aes(x=df0$lens, y=(y0-yfit0))) +
    geom_point(mapping=aes(x=df0$lens, y=(y0-yfit0))) +
    ylim(-0.5, 0.5)

yy0 = predict(result0, list(x0=xx0))
yy1 = predict(result1, list(x1=xx1))

ggplot() +
    geom_line(data = df0, mapping = aes(x = lens, y = reso, color='rough raw')) +
    geom_point(data = df0, mapping = aes(x = lens, y = reso)) +
    geom_line(mapping=aes(x=xx0, y=yy0, color='rough fit')) +
    #geom_line(mapping=aes(x=xx, y=predict(result0, list(x=xx)))
    #geom_line(mapping = aes(x=xx, y=predict(result0, list(x=xx), color='pink'))) +
    #geom_point(data = df1, mapping = aes(x = lens, y = reso, color='blue')) +
    xlim(5.75, 5.9) + ylim(40, 80)

ggplot() +
    geom_line(data = df1, mapping = aes(x = lens, y = reso, color='rough raw')) +
    geom_point(data = df1, mapping = aes(x = lens, y = reso)) +
    geom_line(mapping=aes(x=xx1, y=yy1, color='rough fit')) +
    ggsave('brabra.png', dpi=100, width=7.2, height=4.8)
    #xlim(5.75, 5.9) + ylim(40, 80)

rnorm(5)


