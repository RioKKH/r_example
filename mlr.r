library('MASS') # for MLR
library('tidyverse') # for data preparation

#dfall <- readr::read_csv(datafile, header=TRUE)
#df <- dfall %>%
#	dplyr::select(4, 8, 11:22)
#df <- subset(df, select=c(4, 8, 11:22))
df <- readr::read_csv(datafile, header=TRUE) %>%
	dplyr::select(4, 8, 11:22)

ans <- lm(XY ~ . , df)
ans2 <- stepAIC(ans)
summary(ans2)

# Decide model to use for MLR based on the result above.
ansMLR <- lm(XY ~ a + b + c + d + e, df)
summary(ansMLR)

