# Roy Gross
# Adapted from: https://willhipson.netlify.app/post/markov-sim/markov_chain/
# Modeling NBA team "tanking"
#
# Markov_Binary_State_Example_1 must be run first
# click on "viewer" in the panel to the right, it may take 20s
# or longer to load in

#Red is A
#Blue is B

df_long <- df %>%
  rowid_to_column(var = "iter") %>%
  pivot_longer(cols = V1:V100) %>%
  group_by(name) %>%
  mutate(x_jitter = rnorm(n(), 0, .025),
         x_jitter = x_jitter + lag(x_jitter, default = 0),
         x = value + x_jitter,
         y = rnorm(1, 0, .01),
         y = lag(y, default = 0) + rnorm(n(), 0, .01)) %>%
  group_map(~ {
    value = .$value
    name = .y
    x = spline(.$iter, .$x)$y
    y = spline(.$iter, .$y)$y
    data.frame(name, value, x, y)
  })

df_full <- do.call(rbind, df_long) %>%
  group_by(name) %>%
  mutate(initial = value[1],
         iter = row_number()) %>%
  group_by(iter) %>%
  mutate(prop1 = mean(value),
         prop0 = 1 - prop1)

anim1 <- df_full %>%
  plot_ly(
    x = ~x,
    y = ~y,
    color = ~factor(initial),
    size = 2,
    colors = c("#ff0000", "#1C86EE"),
    frame = ~iter,
    type = 'scatter',
    mode = 'markers',
    showlegend = FALSE
  )

anim1 <- anim1 %>%
  add_text(x = 1, y = .025, text = ~prop1, textfont = list(color = "#1C86EE", opacity = .6)) %>%
  add_text(x = 0, y = .025, text = ~prop0, textfont = list(color = "#ff0000", opacity = .6))

ax <- list(
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  title = ""
)

anim1 <- anim1 %>%
  layout(xaxis = ax, yaxis = ax) %>%
  animation_opts(redraw = FALSE) %>%
  animation_slider(hide = FALSE) %>%
  animation_button(x = .6, y = .10, showactive = TRUE, label = "Run Simulation") %>%
  config(displayModeBar = TRUE, scrollZoom = FALSE, showTips = FALSE)

anim1