
library(tidyverse, verbose = FALSE)
library(plotly)

dk190 <- read.csv('181113_190_diagk.csv')
dk190$datetime <- strptime(dk190$datetime, "%Y%m%d-%H:%M:%S")
dk190$datetime <- as.POSIXct(dk190$datetime)

dkmod <- dk190 %>%
    filter(-2 < ave & ave < 2) %>%
    #filter(-15 < xy & xy < 0) %>%
    select(datetime, xy) %>%
    #select(datetime, ave, xy) %>%
    gather(key = type, value = value, xy)
    #gather(key = type, value = value, ave, xy)

head(dkmod)

lims <- as.POSIXct(strptime(c('2018/10/01', '2018/12/01'), format="%Y/%m/%d"))
ggplot(data = dkmod) +
    geom_line(mapping = aes(x = dkmod$datetime, y = dkmod$value, color=type)) +
    geom_point(mapping = aes(x = dkmod$datetime, y = dkmod$value, color=type)) +
    ylim(-40, +40) +
    ggtitle('9k w/o CL+') +
    xlab('datetime')  +
    ylab('diagk average/xy diff') +
    scale_x_datetime(limits=lims, date_breaks='5 days', date_labels="%m-%d") +
    #scale_x_datetime(date_breaks='90 days', date_labels="%y-%m") +
    #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    facet_wrap( ~ type, nrow=2, ncol=1) +
    geom_vline(xintercept = as.POSIXct(strptime('2018/10/24', format='%Y/%m/%d')),
               color='pink', linetype='dashed', alpha=0.8) +
    geom_vline(xintercept = as.POSIXct(strptime('2018/10/31', format='%Y/%m/%d')),
               color='orange', linetype='dashed', alpha=0.8) +
    geom_vline(xintercept = as.POSIXct(strptime('2018/11/05', format='%Y/%m/%d')),
               color='red', linetype='dashed', alpha=0.8)
ggsave('181113_190_diagk_trend.png')

lims <- as.POSIXct(strptime(c('2018/10/19', '2018/11/15'), format="%Y/%m/%d"))
p <- ggplot(data=dkmod) +
    geom_line(mapping = aes(x = 1:nrow(dkmod), y = value, color=type)) +
    geom_point(mapping = aes(x = 1:nrow(dkmod), y = value, color=type)) +
    #geom_point(mapping = aes(x = datetime, y = value, color=type)) +
    xlim(1025, nrow(dkmod)) +
    ylim(-40, 40) +
    ggtitle('9k w/o CL+') +
    xlab('data') +
    ylab('diagk xy diff[nm/Z10um]') +
    geom_vline(xintercept = 1033.5, linetype='dashed') +
    geom_vline(xintercept = 1040.5, linetype='dashed') +
    geom_vline(xintercept = 1044.5, linetype='dashed') +
    geom_vline(xintercept = 1061.5, linetype='dashed')
    #scale_x_datetime(limits=lims, date_breaks = '2 days', date_labels='%m/%d') +
    #geom_vline(xintercept=as.POSIXct(strptime('2018/10/15', format='%Y/%m/%d')), linetype='dashed') +
    #geom_vline(xintercept=as.POSIXct(strptime('2018/10/16', format='%Y/%m/%d')), linetype='dashed') +
    #geom_vline(xintercept=as.POSIXct(strptime('2018/10/22', format='%Y/%m/%d')), linetype='dashed') +
    #geom_vline(xintercept=as.POSIXct(strptime('2018/11/05', format='%Y/%m/%d')), linetype='dashed')

ggsave('181114_190_diagk_trend.png', plot = p, dpi = 100, width=9.6, height=4.8)
