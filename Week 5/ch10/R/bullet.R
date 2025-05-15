# 
# Listing 10-1
#
# Bulletgraphs in R

library(ggplot2)

# make a bullet graph (retuns a ggplot2 object)
#
# expects a data frame with columns: measure|high|mean|low|target|value 
#
# which equates to:
#   measure: label of what's being measured
#      high: the high value for the measure
#      mean: the mean value for the measure
#       low: the low value for the measure
#    target: the target value for the measure
#     value: the actual value of the measure
#
# NOTE: you *can* put multiple rows in the data frame, but they should all be at the same
#       scale. That either means normalizing the values or representing them as pecentages.
#       you are better off making multiple, invididual bullet graphs if the scales are
#       very different.
# 
# Adapted from: http://bit.ly/1fs6ooC
#

bullet.graph <- function(bg.data){
  
  # compute max and half for the ticks and labels
  max.bg <- max(bg.data$high)
  mid.bg <- max.bg / 2

  gg <- ggplot(bg.data) 
  gg <- gg + geom_bar(aes(measure, high),  fill="goldenrod2", stat="identity", width=0.5, alpha=0.2) 
  gg <- gg + geom_bar(aes(measure, mean),  fill="goldenrod3", stat="identity", width=0.5, alpha=0.2) 
  gg <- gg + geom_bar(aes(measure, low),   fill="goldenrod4", stat="identity", width=0.5, alpha=0.2) 
  gg <- gg + geom_bar(aes(measure, value), fill="black",  stat="identity", width=0.2) 
  gg <- gg + geom_errorbar(aes(y=target, x=measure, ymin=target, ymax=target), color="red", width=0.45) 
  gg <- gg + geom_point(aes(measure, target), colour="red", size=2.5) 
  gg <- gg + scale_y_continuous(breaks=seq(0,max.bg,mid.bg))
  gg <- gg + coord_flip()
  gg <- gg + theme(axis.text.x=element_text(size=5),
                   axis.title.x=element_blank(),
                   axis.line.y=element_blank(), 
                   axis.text.y=element_text(hjust=1, color="black"), 
                   axis.ticks.y=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="none",
                   panel.background=element_blank(), 
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())

  return(gg)

}

# test it out!

incidents <- data.frame(
  measure=c("Total Events (K)", "Security Events (K)"),
  high=c(3200,2000),
  mean=c(2170,1500),
  low=c(1500,500), 
  target=c(2500,1750),
  value=c(2726,1600)
)

# 5/1 seems to be a good ratio for individual bullet graphs but you
# can change it up to fit your dashboard needs

incidents.bg <- bullet.graph(incidents)
ggsave("incident-total-events.png", incidents.bg, width=5, height=2)

incidents.pct <- data.frame(
  measure=c("Total Events (%)", "Security Events (%)", "Filtered (%)", "Tickets (%)"),
  high=c(100,100,100,100),
  mean=c(45,40,50,30),
  low=c(25,20,10,5), 
  target=c(55,40,45,35),
  value=c(50,45,60,25)
)
incidents.pct.bg <- bullet.graph(incidents.pct)
ggsave("incident-total-events-pct.png", incidents.pct.bg, width=10, height=5)

print(incidents.bg)
print(incidents.pct.bg)


# 1. Define the Main Measure (e.g., current performance)
current_value <- 75

# 2. Define the Target Marker (e.g., goal)
target_value <- 90

# 3. Define the Comparative Measure(s) (e.g., previous year's performance)
comparative_value <- 68

# 4. Define the Qualitative Ranges (e.g., poor, satisfactory, good)
ranges <- data.frame(
  start = c(0, 60, 80),
  end = c(60, 80, 100),
  label = c("Poor", "Satisfactory", "Good"),
  color = c("firebrick", "goldenrod", "forestgreen") # Optional colors
)

# You can now use these elements to build a bullet graph using a plotting library

# Example using ggplot2 (conceptual - requires manual construction):
library(ggplot2)

ggplot() +
  # Background ranges
  geom_rect(data = ranges, aes(xmin = start, xmax = end, ymin = -0.4, ymax = 0.4, fill = color), alpha = 0.5) +
  scale_fill_identity() +
  # Current value bar
  geom_rect(aes(xmin = 0, xmax = current_value, ymin = -0.2, ymax = 0.2), fill = "steelblue") +
  # Target marker
  geom_vline(xintercept = target_value, linetype = "dashed", color = "black") +
  # Comparative measure marker
  geom_vline(xintercept = comparative_value, color = "gray50", linewidth = 1.5) +
  # Labels and annotations (optional)
  geom_text(data = ranges, aes(x = (start + end) / 2, y = 0.6, label = label), size = 3) +
  geom_text(aes(x = current_value, y = -0.6, label = current_value), hjust = 1, size = 3) +
  geom_text(aes(x = target_value, y = 0.8, label = "Target"), hjust = 0, size = 3) +
  geom_text(aes(x = comparative_value, y = -0.8, label = "Previous"), hjust = 0, size = 3) +
  # Theme and aesthetics
  xlim(0, 100) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  labs(title = "Sample Bullet Graph")

