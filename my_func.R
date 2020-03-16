cout <- function(x)
{
	sum(!is.na(x))
}

mean2 <- function(x)
{
	mean(x, na.rm=TRUE)
}

sd2 <- function(x)
{
	sd(x, na.rm=TRUE)
}

var2 <- function(x)
{
	var(x, na.rm=TRUE)
}

median2 <- function(x)
{
	median(x, na.rm=TRUE)
}

min2 <- function(x)
{
	min(x, na.rm=TRUE)
}

max2 <- function(x)
{
	max(x, na.rm=TRUE)
}

range2 <- function(x)
{
	range(x, na.rm=TRUE)
}

set_proxy <- function()
{
	Sys.setenv("http_proxy"="http://proxy.nuflare.co.jp:8080")
	options(repos=local({r <- getOption("repos"); r["CRAN"] <- "http://cran.ism.ac.jp"; r}))
	Sys.getenv("http_propxy")
	getOoption("repos")
}
