
library('tidyverse', verbose=FALSE)

df1 <- readr::read_csv('mbm1_eos_data.csv')
df3 <- readr::read_csv('mbm3_eos_data.csv')
df4 <- readr::read_csv('mbm4_eos_data.csv')
df5 <- readr::read_csv('mbm5_eos_data.csv')

preprocess <- function(df, machine)
{
    df$date <- strptime(df$date, "%Y%m%d-%H:%M:%S")
    df$date <- as.POSIXct(df$date)
    df <- df %>%
        dplyr::mutate(machine=rep(machine, nrow(df))) %>%
        dplyr::select(date, cl3a, machine) %>%
        tidyr::gather(key=type, value=tool_id, machine) 
    return(df)
}

preprocess_a13 <- function(df, machine) {
    df$date <- strptime(df$date, "%Y%m%d-%H:%M:%S")
    df$date <- as.POSIXct(df$date)
    df <- df %>%
        dplyr::mutate(machine=rep(machine, nrow(df))) %>%
        dplyr::select(date, a13x, a13y, machine) %>%
        tidyr::gather(key=type, value=tool_id, machine) %>%
        tidyr::gather(key=xy, value=value, a13x, a13y)
    return(df)
}

df1p <- preprocess(df1, '1')
df3p <- preprocess(df3, '3')
df4p <- preprocess(df4, '4')
df5p <- preprocess(df5, '5')

all <- plyr::join_all(list(df1p, df3p, df4p, df5p), type='full')

df1a <- preprocess_a13(df1, '1')
df3a <- preprocess_a13(df3, '3')
df4a <- preprocess_a13(df4, '4')
df5a <- preprocess_a13(df5, '5')

all_a13 <- plyr::join_all(list(df1a, df3a, df4a, df5a), type='full')

head(all_a13)

ggplot(data = all) +
    geom_line(mapping=aes(x = date, y = cl3a, color=tool_id)) +
    geom_point(mapping=aes(x = date, y = cl3a, color=tool_id)) +
    ylim(8.0, 9.0) +
    ylab('CL3A[nml]') +
    xlab('date')
    

ggsave('190115_MBMCL_trend.png', dpi=100, width=9.6, height=7.2)


ggplot(data = all_a13) +
    geom_line(mapping=aes(x =date, y = value, color=tool_id)) +
    geom_point(mapping=aes(x = date, y = value, color=tool_id)) +
    facet_wrap(~ xy, ncol=1, nrow=2) +
    ylab('A13X/Y[NML]') +
    xlab('date')

ggsave('190115_MBM_A13_trend.png', dpi=100, width=9.6, height=7.2)


