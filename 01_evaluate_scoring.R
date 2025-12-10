#



library(readr)
library(dplyr)

a <- lapply(list.files("R", full.names = TRUE), source)

td <- import_tidal("data/naacc-tidal-crossings.csv")


res <- score_tidal(td, return = "everything")



cross <- res$cross
scores <- res$scores

vars <- c(
  "crossing_condition", "crossing_type",
  "downstream_channel_width",
  "downstream_pool_width", "downstream_tidal_range",
  "flow_condition",  "no_crossing",
  "number_of_culverts", "road_fill_height",
  "tide_chart",
  "upstream_channel_width", "upstream_pool_width",
  "upstream_tidal_range", "vegetation",
  "barrier_severity",
  "crossing_structure_length", "dry_passage_for_wildlife",
  "height_above_dry_passage", "inlet_abutment_height",
  "inlet_armoring",
  "inlet_height", "inlet_high_tide_water_depth", "inlet_materials",
  "inlet_openness", "inlet_perch_high_tide", "inlet_perch_low_tide",
  "inlet_spring_tidewater_depth", "inlet_structure_type", "inlet_substrate_water_width",
  "inlet_type", "inlet_water_depth", "inlet_width", "other_barrier_type",
  "outlet_abutment_height", "outlet_armoring", "outlet_height",
  "outlet_high_tide_water_depth", "outlet_materials", "outlet_openness",
  "outlet_perch_high_tide", "outlet_perch_low_tide", "outlet_spring_tidewater_depth",
  "outlet_structure_type", "outlet_substrate_water_width", "outlet_water_depth",
  "outlet_width", "relative_water_depth", "structure_comment",
  "structure_substrate_type", "substrate_coverage", "substrate_matches_stream",
  "substrate_slope", "tide_gate_barrier_severity", "tide_gate_type",
  "crossing_constriction", "tidal_constriction", "downstream_scour",
  "upstream_scour", "different")


used_vars <- c(
  # "survey_id",
  "inlet_width",
  "outlet_width",
  "downstream_channel_width",
  "upstream_channel_width",
  "upstream_tidal_range",
  "downstream_tidal_range",
  "upstream_pool_width",
  "downstream_pool_width",
  "vegetation",
  "relative_water_depth",
  "inlet_perch_low_tide",
  "outlet_perch_low_tide",
  "inlet_armoring",
  "outlet_armoring",
  "inlet_openness",
  "outlet_openness",
  "substrate_matches_stream",
  "substrate_coverage",
  "inlet_perch_high_tide",
  "outlet_perch_high_tide",
  "tide_gate_barrier_severity",
  "barrier_severity",
  "crossing_constriction",
  "tidal_constriction",
  "upstream_scour",
  "downstream_scour"
)




# For each survey set the crossing score as the max of the individual
# culvert scores.
scores <- scores |> group_by(survey_id) |> mutate(max_score = max(score)) |> ungroup()


sv <- !near(scores$max_score,  cross$aquatic_score_tidal_stream, tol = 1.5e-2)
na_differs <- (( is.na(scores$max_score) + is.na(cross$aquatic_score_tidal_stream)) == 1 )
sv[na_differs] <- FALSE
sv[is.na(sv)] <- FALSE  # both NA
cross$different <- sv


plot(scores$max_score, cross$aquatic_score_tidal_stream, col = rgb(0, 0, 0, 0.6))
abline(a = 0, b = 1, col = rgb(0, 0, .8, 0.5))

sv <- cross$different
points(scores$max_score[sv], cross$aquatic_score_tidal_stream[sv], col = "red")
sv <- cross$inlet_perch_low_tide != 0
points(scores$max_score[sv], cross$aquatic_score_tidal_stream[sv], col = "blue")
sv <- cross$outlet_perch_low_tide != 0
points(scores$max_score[sv], cross$aquatic_score_tidal_stream[sv], col = "green")


sv <- cross$inlet_perch_low_tide > 0
sv[is.na(sv)] <- FALSE
points(scores$max_score[sv], cross$aquatic_score_tidal_stream[sv], col = "hotpink", pch = 19)

cross$max_score <- scores$max_score


library(ggplot2)
library(rlang)

for (var in used_vars) {
  p <- ggplot2::ggplot(cross, aes(x = max_score,
                                  y = aquatic_score_tidal_stream,
                                  color = .data[[var]])) +
    ggplot2::geom_point()

  if (!is.factor(cross[[var]]) && !is.character(cross[[var]])) {
    p <- p + scale_color_viridis_c()
  } else {
    p <- p
  }
  print(p)
}


sv <- cross$different
for(i in seq_along(used_vars)){
  cat(i, "  ", used_vars[i], "\n")
  print(cross[sv, used_vars[i]])
  cat("\n\n")

}



sv <- !near(scores$max_score,  cross$aquatic_score_tidal_stream, tol = 1.5e-2)
na_differs <- (( is.na(scores$max_score) + is.na(cross$aquatic_score_tidal_stream)) == 1 )
sv[na_differs] <- FALSE
sv[is.na(sv)] <- FALSE  # both NA

comparison <- cbind(
  select(cross, survey_id, naacc = aquatic_score_tidal_stream, n = number_of_culverts ),
  select(scores, all_of(c("score", "max_score", limiting_variables))))


factor_vars <- used_vars[sapply(cross[, used_vars], is.character)]
for(v in factor_vars)cross[[v]] <- as.factor(cross[[v]])

cross$difference <- scores$score - cross$aquatic_score_tidal_stream

f <- ranger::ranger(different~., data = cross[, c(used_vars, "different")], importance = "impurity")
imp <- ranger::importance(f)
imp[which.max(imp)]
sort(imp, decreasing = TRUE)[1:10]

cross$difference <- scores$score - cross$aquatic_score_tidal_stream
f <- ranger::ranger(difference~., data = cross[, c(used_vars, "difference")][!is.na(cross$difference), ], importance = "impurity")
imp <- ranger::importance(f)
imp[which.max(imp)]
sort(imp, decreasing = TRUE)[1:10]
