
library(tidyverse)

col1 <- c('datetime', 'x', 'y', 'ave', 'alpha', 'raw_xy')
col2 <- c('datetime', 'cl1', 'cl2', 'cl3', 'sl', 'ol1', 'ol2', 'ol3', 'ol4',
         'slr', 'olf', 'olr1', 'olr2', 'a1x', 'a1y', 'a2x', 'a2y', 'a3x', 'a3y',
         'a4x', 'a4y', 'a5x', 'a5y', 'a6x', 'a6y', 'a7x', 'a7y', 'clsx', 'clsy',
         'a9x', 'a9y', 'sx', 'sy')

coefx <- 5.291
coefy <- 2.985

dfdk <- readr::read_csv('all_diagk.csv', skip=0, col_names=col1)
dfeos <- readr::read_csv('all_eos.csv', skip=0, col_names=col2)

dfdk$datetime <- strptime(dfdk$datetime, "%Y%m%d-%H:%M:%S")
dfdk$datetime <- as.POSIXct(dfdk$datetime)
dk <- dfdk %>%
    dplyr::select(datetime, ave, raw_xy) %>%
    dplyr::filter(-2.0 < ave & ave < 2.0) %>%
    tidyr::gather(key=type, value=value, ave, raw_xy) %>%
    dplyr::filter(type == "raw_xy")

dfeos$datetime <- strptime(dfeos$datetime, "%Y%m%d-%H:%M:%S")
dfeos$datetime <- as.POSIXct(dfeos$datetime)
de <- dfeos %>%
    dplyr::select(datetime, clsx, clsy) %>%
    dplyr::mutate(simu_xy = -(clsx*coefx + clsy*coefy)) %>%
    tidyr::gather(key=type, value=value, clsx, clsy, simu_xy) %>%
    dplyr::filter(type == "simu_xy")


conc <- dplyr::full_join(dk, de)
diff = dk$datetime - de$datetime[1]

index <- c()
for (i in 1:length(dk$datetime)) {
    index <- c(index, which.min(abs(de$datetime - dk$datetime[i])))
}

de_sliced <- dplyr::slice(de, index)

ee <- de_sliced %>%
    dplyr::filter(de_sliced$type == 'simu_xy') %>%
    dplyr::select(value)

kk <- dk %>%
    dplyr::filter(type == 'raw_xy') %>%
    dplyr::select(value)

ff <- ee + kk

df <- dplyr::bind_cols(dk, de_sliced, ff)
colnames(df) <- c('datetime', 'type', 'raw_xy_error', 'datetime1', 'type1', 'corrected_xy_error', 'total_xy_error')

df_gathered <- df %>%
    tidyr::gather(key=vtype, value=allvalue, raw_xy_error, corrected_xy_error, total_xy_error) %>%
    tidyr::gather(key=dtype , value=alltype, type, type1)

p <- ggplot(data=df_gathered) +
    ggtitle("Total diagk XY error") +
    xlab('datetime') +
    ylab('Total diagk XY difference[nm/Z10um]') +
    scale_x_datetime(
        date_labels = '%y-%m-%d',
        limits = c(
            as.POSIXct('2019/07/16'),
            as.POSIXct('2019/11/05')
        )
    ) +
    ylim(-20, 20) +
    geom_line(mapping=aes(x=datetime, y=allvalue, color=vtype)) +
    geom_point(mapping=aes(x=datetime, y=allvalue, color=vtype)) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/04/20', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/05/15', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/07/16', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/09/09', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) + # dismounted all EOS blocks
    geom_vline(xintercept = as.POSIXct(strptime('2019/09/18', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) + # mounted OL/SL
    geom_vline(xintercept = as.POSIXct(strptime('2019/09/19', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) + # mounted CL
    geom_vline(xintercept = as.POSIXct(strptime('2019/09/27', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) + # beam down
    geom_vline(xintercept = as.POSIXct(strptime('2019/10/24', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    facet_wrap( ~ vtype, nrow=3, ncol=1)
show(p)
ggsave('191111_229_raw_and_simulation_trend.png', plot=p, dpi=100, width=9.6, height=7.2)

p <- ggplot(data=conc) +
    ggtitle("S1AP: Sample #1 & #2 & #3") +
    xlab('datetime') +
    ylab('diagk XY difference[nm/Z10um]') +
    scale_x_datetime(
        date_labels="%y-%m-%d",
        limits = c(
            as.POSIXct("2019/07/16"),
            as.POSIXct("2019/11/05")
        )
    ) +
    ylim(-40, 40) +
    geom_line(mapping = aes(x=datetime, y=value, color=type)) +
    geom_point(mapping = aes(x=datetime, y=value, color=type)) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/04/20', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/05/15', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/07/16', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/10/24', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    facet_wrap( ~ type, nrow = 2, ncol = 1)
    #scale_x_datetime(l)
show(p)
ggsave('191105_229_raw_and_simulation_trend_closedup.png', plot=p, dpi=100, width=9.6, height=7.2)

p <- ggplot(data=conc) +
    ggtitle("S1AP: Sample #1 & #2 & #3") +
    xlab('datetime') +
    ylab('diagk XY difference[nm/Z10um]') +
    scale_x_datetime(date_labels="%y-%m-%d") +
    ylim(-20, 20) +
    geom_line(mapping = aes(x=datetime, y=value, color=type)) +
    geom_point(mapping = aes(x=datetime, y=value, color=type)) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/04/20', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/05/15', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/07/16', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = as.POSIXct(strptime('2019/10/24', format='%Y/%m/%d')), linetype='dashed', alpha=0.5) +
    facet_wrap( ~ type, nrow = 2, ncol = 1)
    #scale_x_datetime(l)
show(p)
ggsave('191105_229_raw_and_simulation_trend.png', plot=p, dpi=100, width=9.6, height=7.2)


