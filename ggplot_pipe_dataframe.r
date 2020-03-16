library(tidyverse, verbose=FALSE)
# options(repr.plot.width=4, repr.plot.height=4)

tibble::tibble(x=seq(-3, 3, by=0.1), y=plogis(x)) %>%
	ggplot2::ggplot() +
	ggplot2::geom_point(aes(x=x, y=y)) +
	ggplot2::geom_line(aes(x=x, y=y))


