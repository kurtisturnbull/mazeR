

plot_maze <- function(maze, show_distance = TRUE, palette = NULL){

  axes_ops <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    tickvals=NULL
  )

  if (show_distance == FALSE){
    maze[!is.na(distance)]$distance <- 0
  }

  plot_ly() %>%
    add_trace(
      x = maze$x,
      y = maze$y,
      z = maze$distance,
      type = "heatmap",
      colors = palette
    ) %>%
    layout(
      paper_bgcolor= "#00000000",
      plot_bgcolor = "#282828",
      xaxis = axes_ops,
      yaxis = axes_ops
    ) %>%
    hide_colorbar()
}
