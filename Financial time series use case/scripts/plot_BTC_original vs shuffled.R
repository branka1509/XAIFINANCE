library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

load("~/GitHub/XAI_Marc 2/Data/bitcoin.Rdata")
data <- as.data.table(dat)

ggplot(data,
       aes(x = index,
           y = VWAP)) +
  geom_line(color = "steelblue") +
  xlab("Time") +
  ylab("Volume-weighted average price (VWAP)") +
  scale_x_date(date_labels = "%m-%Y") +
  theme(axis.text.x = element_text(angle=60, hjust=1))



data_changed <- data[,c(1,8)]
data_changed <- transform(data_changed, shuffled_VWAP = sample(VWAP))

plotdata <- data_changed %>%
  gather(key = "variable", value = "value", -index )

# Plot
ggplot(plotdata, aes(x = index,
                     y = value,
                     color = variable)) +
  geom_line() +
  scale_color_manual(values = c("lightcyan3", "steelblue")) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Time") +
  ylab("Value")


