library(tidyverse)
library(psych)
library(e1071)
library(factoextra)
library(patchwork)
library(cowplot)

# Load the combined dataset
df <- read.csv('wine_quality_data.csv')

# Separate features, target, and wine type
feature_cols <- c('fixed_acidity', 'volatile_acidity', 'citric_acid',
                  'residual_sugar', 'chlorides', 'free_sulfur_dioxide',
                  'total_sulfur_dioxide', 'density', 'pH', 'sulphates', 
                  'alcohol')

cat("Total samples:", nrow(df), "\n")

sum(duplicated(df))

sum(duplicated(df %>% select(-quality)))

df <- unique(df)

# Univariate #####

table(df$wine_type)

table(df$quality)

partial_X <- df[, feature_cols]

X <- partial_X %>% mutate(wine_type = df$wine_type)

X_long <- pivot_longer(partial_X, everything(), names_to = "variable", values_to = "value")

var_info <- tribble(
  ~variable,               ~unit,
  "fixed_acidity",         "g/dm³   ",
  "volatile_acidity",      "g/dm³   ",
  "citric_acid",           "g/dm³   ",
  "residual_sugar",        "g/dm³   ",
  "chlorides",             "g/dm³   ",
  "free_sulfur_dioxide",   "mg/dm³",
  "total_sulfur_dioxide",  "mg/dm³",
  "density",               "g/cm³   ",
  "pH",                    "-         ",
  "sulphates",             "g/dm³   ",
  "alcohol",               "% vol   "
)

X_long <- partial_X %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  left_join(var_info, by = "variable") %>%
  group_by(variable, unit) %>%
  mutate(
    skew  = moments::skewness(value, na.rm = TRUE),
    kurt  = moments::kurtosis(value, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(X_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  
  # Text annotation: top-left, aligned properly
  geom_text(
    data = X_long %>% distinct(variable, skew, kurt, unit),
    aes(
      x = -Inf, y = Inf,
      label = paste0(
"skew = ", round(skew, 2), "
kurt = ", sprintf("%.2f", kurt),  "
unit: ", unit
      )
    ),
    hjust = -1.2,      
    vjust = 1.2,       
    size = 3.5
  ) +
  
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.y = element_blank(),      
    axis.title.x = element_blank()
  )

# Correlation heatmaps #####

plot_corr_heatmap <- function(data, title = "Correlation Heatmap") {
  
  corr_matrix <- cor(data)
  
  corr_tibble <- corr_matrix %>%
    as_tibble(rownames = "var_a") %>%
    pivot_longer(
      -var_a,
      names_to = "var_b",
      values_to = "correlation"
    )
  
  var_names <- unique(corr_tibble$var_a)
  
  corr_tibble_factored <- corr_tibble %>%
    mutate(
      var_a = factor(var_a, levels = var_names),
      var_b = factor(var_b, levels = rev(var_names))
    )
  
  filtered_corr_rev <- corr_tibble_factored %>%
    mutate(
      lvl_a = as.numeric(var_a),
      lvl_b = as.numeric(var_b %>% fct_rev()),
      correlation = if_else(lvl_a <= lvl_b, correlation, NA)
    )
  
  filtered_corr_rev %>%
    ggplot(aes(var_a, var_b)) + 
    geom_tile(aes(fill = correlation), color = "black") +
    geom_text(
      aes(label = round(correlation, 2)),
      size = 3.5,
      fontface = "bold"
    ) +
    theme_minimal(base_size = 12) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Correlation",
      title = title
    ) +
    scale_fill_gradient2(
      high = "brown3",
      mid = "white",
      low = "dodgerblue4",
      limits = c(-1, 1),
      na.value = "black"
    ) +
    guides(
      x = guide_axis(n.dodge = 2)
    )
}

# Red = 1, white = 0
df$wine_type <- as.numeric(df$wine_type == "red")

df_red <- df[df$wine_type == 1, ]

df_white <- df[df$wine_type == 0, ]

# Quality: Bad <= 4, Average <= 7, Amazing <= 10
df$quality_group <- case_when(
  df$quality <= 4 ~ 0,
  df$quality <= 7 ~ 1,
  df$quality <= 10 ~ 2
)

df$quality_group <- factor(
  df$quality_group,
  levels = c(0, 1, 2),
  labels = c("bad", "average", "great")
)

table(df$quality_group)

# Extract Legend Function
get_legend <- function(myplot) {
  tmp <- ggplot_gtable(ggplot_build(myplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}

# Top Plot (A)
p_all_raw <- plot_corr_heatmap(df %>% select(-quality_group), "All Wines") +
  theme(
    axis.text.y = element_text(angle = 30),
    legend.position = "right"
  ) +
  guides(fill = guide_colorbar(
    barheight = unit(8, 'cm'),  
    label.position = "right")
  )

shared_legend <- get_legend(p_all_raw)

p_all <- p_all_raw + theme(legend.position = "none")

p_all <- ggdraw(p_all) +
  draw_label("A", x = 0, y = 1, hjust = -0.3, vjust = 1, 
             fontface = "bold", size = 16)


# Bottom Left (B): Red Wines
p_red0 <- plot_corr_heatmap(
  df %>% filter(wine_type == 1) %>% select(-wine_type, -quality_group),
  "    Red Wines"
) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p_red <- ggdraw() +
  draw_plot(p_red0, x = 0, y = 0, width = 1, height = 1) +
  draw_label("B", x = 0, y = 1, hjust = -0.3, vjust = 1, 
             fontface = "bold", size = 16)

# Bottom Right (C): White Wines
p_white0 <- plot_corr_heatmap(
  df %>% filter(wine_type == 0) %>% select(-wine_type, -quality_group),
  "    White Wines"
) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p_white <- ggdraw() +
  draw_plot(p_white0, x = 0, y = 0, width = 1, height = 1) +
  draw_label("C", x = 0, y = 1, hjust = -0.3, vjust = 1, 
             fontface = "bold", size = 16)

# Bottom Row 
bottom_row <- plot_grid(
  p_red, p_white,
  ncol = 2,
  align = "v"
)

# Combine A over B+C
main_plots <- plot_grid(
  p_all, bottom_row,
  ncol = 1,
  rel_heights = c(1, 0.8),
  align = "v"
)

# Add Back Legend
figure_2 <- plot_grid(
  main_plots, shared_legend,
  ncol = 2,
  rel_widths = c(1, 0.13)
)

figure_2

df %>% group_by(wine_type) %>% summarise(mean_pH = mean(pH, na.rm = TRUE))

# Scree Plot #####

partial_X_scaled <- scale(partial_X, center = TRUE, scale = TRUE)

pca <- prcomp(partial_X_scaled)

eigenvalues <- pca$sdev^2

sum(eigenvalues[1:3])/sum(eigenvalues)

n_gaps <- 5

eigenvalue_ratios <- numeric(n_gaps)

for(i in 1:n_gaps) {
  eigenvalue_ratios[i] <- eigenvalues[i]/eigenvalues[i + 1]
}

eigenvalue_ratios
eigenvalue_ratios > 2

eig_values <- get_eigenvalue(pca)

ymax <- max(eig_values$eigenvalue)

# Rescale cumulative variance to primary axis (0 - ymax)
eig_values <- eig_values %>%
  mutate(cumvar_scaled = cumulative.variance.percent / 100 * ymax)

ggplot(eig_values, aes(x = 1:nrow(eig_values))) +
  
  # Bars: eigenvalues
  geom_col(aes(y = eigenvalue, fill = "Eigenvalue"), width = 0.6) +
  
  # Line: cumulative variance (rescaled to primary axis)
  geom_line(aes(y = cumvar_scaled, color = "Cumulative Variance (%)"), size = 1) +
  geom_point(aes(y = cumvar_scaled, color = "Cumulative Variance (%)"), size = 2) +
  
  # Horizontal line for Kaiser criterion
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  
  # Axes
  scale_x_continuous(breaks = 1:nrow(eig_values), name = "Principal Component") +
  scale_y_continuous(
    name = NULL,
    limits = c(0, ymax),
    sec.axis = sec_axis(~ . / ymax * 100, name = NULL, breaks = seq(0, 100, 20))
  ) +
  scale_fill_manual("", values = "skyblue") +
  scale_color_manual("", values = "blue") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

# Biplot by wine_type with loading arrows #####

scores <- as.data.frame(pca$x[, 1:ncol(partial_X)])
scores$wine_type <- factor(X$wine_type)

loadings <- as.data.frame(pca$rotation[, 1:2])  # first 2 PCs
loadings$var <- rownames(loadings)
loadings$var

arrow_scale <- 10

loadings_scaled <- loadings
loadings_scaled[, 1:2] <- loadings_scaled[, 1:2] * arrow_scale

selected_loadings <- loadings_scaled %>% filter(!var %in% c("free_sulfur_dioxide"))

centroids <- scores %>%
  group_by(wine_type) %>%
  summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2)
  )

fig4a <- ggplot() +
  geom_point(data = scores, aes(x = PC1, y = PC2, color = wine_type), 
             alpha = 0.2, size = 3) +
  # Centroids
  geom_point(data = centroids, aes(x = PC1, y = PC2, color = wine_type),
             size = 4, shape = 21, fill = "white", stroke = 2) + 
  
  # Arrows
  geom_segment(data = selected_loadings, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", size = 1) +
  geom_text(data = selected_loadings,
            aes(x = PC1, y = PC2, label = var),
            vjust = 1.5, hjust = 0.5, size = 4) +
  labs(
    x = paste0("PC1 (", round(summary(pca)$importance[2, 1]*100,1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2, 2]*100,1), "%)"),
    color = "Wine Type \n Midpoints"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 11))

# Biplot by quality AND type (partial) #####

scores$quality_group <- df$quality_group
scores$group_type <- interaction(scores$quality_group, scores$wine_type, sep = " ")

table(scores$group_type)

centroids <- scores %>%
  filter(quality_group %in% c("bad", "great")) %>%
  group_by(group_type) %>%
  summarize(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  )

group_variances <- scores %>%     
  group_by(group_type) %>%
  summarize(
    var_PC1 = var(PC1),
    var_PC2 = var(PC2),
    total_variance = var(PC1) + var(PC2),
    .groups = "drop"
  )

variance_text <- group_variances %>%
  mutate(
    label = paste0(
      group_type, ":  ",
      sprintf("%.2f", var_PC1), ", ",
      sprintf("%.2f", var_PC2)
    )
  ) %>%
  pull(label) %>%
  paste(collapse = "\n")

fig4b <- ggplot(scores %>% filter(quality_group %in% c("bad", "great")),
             aes(x = PC1, y = PC2, color = group_type)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_point(
    data = centroids,
    aes(x = PC1, y = PC2, color = group_type),
    size = 4, shape = 21, fill = "white", stroke = 2
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("Group                         \n",
                          "Variances | PC1 | PC2\n", variance_text),
           hjust = 1.1, vjust = -0.1,
           size = 3.6, color = "black",
           lineheight = 0.9) +
  labs(
    x = paste0("PC1 (", round(summary(pca)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2,2] * 100, 1), "%)"),
    color = " Group \n Midpoints"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 11))

figure_4 <- plot_grid(
  fig4a, fig4b,
  ncol = 1,
  labels = c("A", "B"),
  label_size = 16,
  align = "v"
)

figure_4

df %>%
  group_by(wine_type, quality_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(wine_type) %>%
  mutate(prop = n / sum(n))

# Run Mardia's test #####
mardia_results <- mardia(partial_X_scaled)
print(mardia_results)