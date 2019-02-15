
library(tidyverse, verbose=FALSE)

coefx <- 4.942667
coefy <- 3.265333

# Add column names
col1 <- c('date', 'x', 'y', 'ave', 'alpha', 'xy')
col2 <- c('date', 'cl1', 'cl2', 'cl3', 'sl', 'ol1', 'ol2', 'ol3', 'ol4',
         'slr', 'olf', 'olr1', 'olr2', 'a1x', 'a1y', 'a2x', 'a2y', 'a3x', 'a3y',
         'a4x', 'a4y', 'a5x', 'a5y', 'a6x', 'a6y', 'a7x', 'a7y', 'clsx', 'clsy',
         'a9x', 'a9y', 'sx', 'sy')

# SC fleet 
eb21 <- readr::read_csv('161-eb21_all_diagk.csv')
eb22 <- readr::read_csv('163-eb22_all_diagk.csv')
eb23 <- readr::read_csv('168-eb23_all_diagk.csv')
eb24 <- readr::read_csv('177-eb24_all_diagk.csv')
eb25 <- readr::read_csv('175-eb25_all_diagk.csv')
eb26 <- readr::read_csv('182-eb26_all_diagk.csv')

# RA fleet
eb203 <- readr::read_csv('173-e203_all_diagk.csv')
eb204 <- readr::read_csv('174-e204_all_diagk.csv')
eb205 <- readr::read_csv('176-e205_all_diagk.csv')
eb206 <- readr::read_csv('179-e206_all_diagk.csv')

# H.M.
df229dk <- readr::read_csv('181126_229_diagk.csv', skip=0, col_names=col1)
df229eos <- readr::read_csv('181126_229_eos.csv', skip=0, col_names=col2)
#df229dk <- readr::read_csv('181126_229_diagk.csv', header=FALSE)
#df229eos <- readr::read_csv('181126_229_eos.csv', header=FALSE)

head(df229dk)

# Add column names
#colnames(df229dk) <- c('date', 'x', 'y', 'ave', 'alpha', 'xy')
#colnames(df229eos) <- c('date', 'cl1', 'cl2', 'cl3', 'sl', 'ol1', 'ol2', 'ol3', 'ol4',
#         'slr', 'olf', 'olr1', 'olr2', 'a1x', 'a1y', 'a2x', 'a2y', 'a3x', 'a3y',
#         'a4x', 'a4y', 'a5x', 'a5y', 'a6x', 'a6y', 'a7x', 'a7y', 'clsx', 'clsy',
#         'a9x', 'a9y', 'sx', 'sy')

preprocess1 <- function(df, machine)
{
    df$date <- strptime(df$date, "%Y%m%d-%H:%M:%S")
    df$date <- as.POSIXct(df$date)
    df <- df %>%
        dplyr::mutate(machine=rep(machine, nrow(df))) %>%
        dplyr::filter(-2.0 < ave & ave < 2.0) %>%
        dplyr::select(date, xy, machine) %>%
        gather(key=type, value=value,xy)
    return(df)
}

preprocess2 <- function(df, machine)
{
    df$date <- strptime(df$date, "%Y%m%d-%H:%M:%S")
    df$date <- as.POSIXct(df$date)
    df <- df %>%
        dplyr::mutate(xy = -(clsx * coefx + clsy * coefy)) %>%
        dplyr::mutate(machine=rep(machine, nrow(df))) %>%
        dplyr::select(date, xy, machine) %>%
        tidyr::gather(key=type, value=value, xy)
    return(df)
}

# SC
eb21p <- preprocess1(eb21, "EB21")
eb22p <- preprocess1(eb22, "EB22")
eb23p <- preprocess1(eb23, "EB23")
eb24p <- preprocess1(eb24, "EB24")
eb25p <- preprocess1(eb25, "EB25")
eb26p <- preprocess1(eb26, "EB26")

# RA
eb203p <- preprocess1(eb203, "EB203")
eb204p <- preprocess1(eb204, "EB204")
eb205p <- preprocess1(eb205, "EB205")
eb206p <- preprocess1(eb206, "EB206")

# HM
df229dkp <- preprocess1(df229dk, "HMR")
df229eosp <- preprocess2(df229eos, "HMS")

head(eb21p, n = 3)
head(eb203p, n = 3)
head(df229dkp, n = 3)
head(df229eosp, n = 3)

#all <- dplyr::full_join(eb21p, eb22p, eb23p, eb24p, eb25p, eb26p,
#                        eb203p, eb204p, eb205p, eb206p,
#                        df229dkp, df229eosp)

plyr::join_all(list(eb21p, df229dkp), type='full')

all <- plyr::join_all(list(eb21p, eb22p, eb23p, eb24p, eb25p, eb26p,
                    eb203p, eb204p, eb205p, eb206p,
                    #), type='full')
                    df229dkp, df229eosp), type='full')

p <- ggplot(data = all) +
    geom_boxplot(mapping=aes(x = machine, y = value, color=machine)) +
    ylim(-60, 60) +
    xlab("Tool") +
    ylab("diagk XY difference[nm/Z10um]")

ggsave('181214_boxplot.png', plot=p, dpi=100, width=9.6, height=7.2)

ggplot(data = df229dkp) +
    geom_line(mapping=aes(x = date, y = value, color=type)) +
    geom_point(mapping=aes(x = date, y = value, color=type))

ggplot(data = eb206p) +
    geom_line(mapping=aes(x = date, y=value, color=type)) +
    geom_point(mapping=aes(x = date, y = value, color = type))


