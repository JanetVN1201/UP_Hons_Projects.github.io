library(ggplot2)
library(gganimate)
library(gifski)
# Simulate data
set.seed(123)
n <- 10
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x
df <- data.frame(x, y)

# Generate lines for animation
#slopes <- seq(0, 1, length.out = 50)      # varying slope
#intercepts <- seq(0, 4, length.out = 50)  # varying intercept

anim_data <- expand.grid(slope = slopes, intercept = intercepts, x = x)
anim_data$y <- anim_data$intercept + anim_data$slope * anim_data$x

# Use a simplified animation: only vary slope for clarity
anim_data <- data.frame(
  frame = rep(1:50, each = n),
  x = rep(x, 50),
  y = rep(y, 50)+ rnorm(n*50, mean = 0, sd = 1),
  intercept = 2
)
for (i in 1:50){
  yy =  anim_data$y[(i-1)*n+1:n]
  xx =  anim_data$x[(i-1)*n+1:n]
  anim_data$slope[(i-1)*n+1:n] = as.numeric(coef(lm(yy ~ xx))[2])
  anim_data$intercept[(i-1)*n+1:n] = as.numeric(coef(lm(yy ~ xx))[1])
  
}
anim_data$y_line <- anim_data$intercept + anim_data$slope * anim_data$x

# Plot with gganimate
p <- ggplot(anim_data, aes(x = x, y = y)) +
  geom_point(color = "steelblue", size = 2) +
  geom_abline(aes(intercept = intercept, slope = slope), color = "red", size = 1) +
  labs(title = '', x = "X", y = "Y") +
  transition_states(frame, transition_length = 2, state_length = 2) +
  ease_aes('linear')

# p <- ggplot(anim_data, aes(x = x, y = y)) +
#   geom_point(color = "steelblue", size = 2) +
#   geom_abline(aes(intercept = intercept, slope = slope), color = "red", size = 1) +
#   labs(title = 'Slope = {round(frame_along, 2)}', x = "X", y = "Y") +
#   transition_along(frame, range = c(1, 50)) +
#   ease_aes('linear')

# Render as GIF
animate(p, nframes = 50, fps = 3, width = 600, height = 400, renderer = gifski_renderer("regression.gif"))
