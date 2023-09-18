source("header.R")

sbf_set_sub("temperature")

dag <- dagify(y ~ x + z,
              labels = c(y = "Y",
                         x = "X",
                         z = "Z"),
              coords = tribble(
                ~x, ~y, ~name,
                1, 1, "y",
                0, 1.5, "x",
                0, 0.5, "z")
              ) %>%
  tidy_dagitty() %>%
  fortify()

gp <- ggplot(dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(show.legend = FALSE) +
  geom_dag_edges_link() +
  geom_dag_label_repel(aes(label = label)) +
  geom_dag_edges_link(edge_alpha = 1/5) +
  scale_x_continuous(expand = expansion(add = 0.2)) +
  scale_y_continuous(expand = expansion(add = 0.2)) +
   theme_dag(panel.background = element_rect(color = "white")) +
  NULL

sbf_open_window(3)
sbf_print(gp)

sbf_save_plot(x_name = "dag", caption = "A Directed Acyclic Graph of the relationships between X, Z and Y.")
