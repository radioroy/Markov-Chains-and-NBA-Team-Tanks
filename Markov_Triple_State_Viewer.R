# Roy Gross
# Adapted from: https://willhipson.netlify.app/post/markov-sim/markov_chain/
# Modeling NBA team "tanking"
#

df_long2 <- df2 %>%
  rowid_to_column(var = "iter") %>%
  pivot_longer(cols = V1:V100) %>%
  group_by(name) %>%
  mutate(x = value + rnorm(1, 0, .10) + rnorm(n(), 0, .01),
         y = value %% 2 + rnorm(1, 0, .10) + rnorm(n(), 0, .01),
         initial = value[1],
         is_0 = ifelse(value == 0, TRUE, FALSE),
         is_1 = ifelse(value == 1, TRUE, FALSE)) %>%
  group_by(iter) %>%
  mutate(prop0 = mean(is_0),
         prop1 = mean(is_1),
         prop2 = 1 - prop0 - prop1) %>%
  ungroup()

anim2 <- df_long2 %>%
  plot_ly(
    x = ~x,
    y = ~y,
    color = ~factor(initial),
    size = 2,
    colors = c("#32CD32", "#1C86EE", "#FF7F00"),
    frame = ~iter,
    type = 'scatter',
    mode = 'markers',
    showlegend = FALSE
  )

anim2 <- anim2 %>%
  add_text(x = 0, y = .35, text = ~prop0, textfont = list(color = "#32CD32", size = 24, opacity = .6)) %>%
  add_text(x = 1, y = 1.25, text = ~prop1, textfont = list(color = "#1C86EE", size = 24, opacity = .6)) %>%
  add_text(x = 2, y = .35, text = ~prop2, textfont = list(color = "#FF7F00", size = 24, opacity = .6))

ax2 <- list(
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  title = ""
)

anim2 <- anim2 %>%
  layout(xaxis = ax2, yaxis = ax2) %>%
  animation_opts(redraw = FALSE) %>%
  animation_slider(hide = FALSE) %>%
  animation_button(x = .6, y = .10, showactive = TRUE, label = "Run Simulation") %>%
  config(displayModeBar = TRUE, scrollZoom = FALSE, showTips = FALSE)

anim2