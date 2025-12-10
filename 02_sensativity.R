


invisible(lapply(list.files("R", full.names = TRUE), source))


td <- import_tidal("data/naacc-tidal-crossings.csv")

weights <- read.csv("data/weights.csv")
names(weights) <- gsub("\\.", "_", names(weights)) |> tolower()
weights$variable <- gsub("[\\.[:blank:]]", "_", weights$variable) |> tolower()

n_to_drop <- 1:4
vars <- weights$variable
n_vars <- nrow(weights)

k_vals <- 1:4

res <- vector(mode = "list", length(k_vals))
for(i in seq_along(k_vals)) {
  k <- k_vals[i]
  combinations  <- combn(vars, k)  |>
    apply( 2, list) |>
    lapply( \(x) x[[1]])
  d <- data.frame(k = rep(k, length(combinations)))
  d$exclude <- combinations
  res[[i]] <- d
}

d <- do.call(rbind, res)

d$correlation <- NA

full_model_scores <- score_tidal(td)
sv <- !is.na(full_model_scores)
sel <- td[sv, ]
full_model_scores <- score_tidal(sel)
mean_full_score <- mean(full_model_scores)

for(i in seq_len(nrow(d))) {
  score <- score_tidal(sel, exclude = d$exclude[[i]])
  d$correlation[i] <- cor(score, full_model_scores)
  d$bias[i] <- mean(score) - mean_full_score
  d$max_diff[i] <- max(abs(score - full_model_scores))
}

d$k <- as.factor(d$k)
ggplot(d, aes(x = correlation, group = k, color = k)) +  geom_density()

long <- tidyr::pivot_longer(d, c(correlation, bias, max_diff),
                            values_to = "correlation",
                            names_to = "metric")

ggplot(long, aes(x = correlation, group = interaction(metric, k), color = k)) +
  geom_density(adjust = 0.75) +
  facet_wrap(vars(metric), scales = "free") +
  labs(x = element_blank())


filter(d, bias > 0.02, k == 1)


