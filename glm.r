options(repl.plot.width=4, repr.plot.height=4)
library(tidyverse)

df <- readr::read_csv('data.csv')

# In case of 2nd-order fitting
lm2.1 <- glm(xl ~ lens + I(lens^2), data=df)
lm2.2 <- glm(xl ~ poly(lens, degree=2, raw=TRUE), data=df)

# In case of 3rd-order fitting
lm3.1 <- glm(xl ~ lens + I(lens^2) + I(lens^3), data=df)
lm3.2 <- glm(xl ~ poly(lens, degree=3, raw=TRUE), data=df)

# In case of mixed function
lm.mix <- glm(xl ~ lens + I(lens^2) + I(cosh(lens)), data=TRUE)

dfpred <- data.frame(lens = seq(from=range(df$lens)[1], to=range(df$lens)[2], length.out=100))

dfpred$xlp <- predict(lm2.1, newdata=dfpred)
dfpred$coshp <- predict(lm.mix, newdata=dfpred)


dfpred %>%
	gather(key=type, value=value, xlp, coshp) %>%
	ggplot() +
		geom_point(aes(x=lens, y=value, color=type)) +
		geom_line(aes(x=lens, y=vlaue, color=type))

