if(FALSE){
  library(readr)
  library(dplyr)
  td <- import_tidal("data/naacc-tidal-crossings.csv")
}


score_tidal <- function(td,  exclude = NA, return = "score") {



  stopifnot(length(return) == 1 && return %in% c("score", "everything"))
  # td = tidal data and includes multiple rows per survey

  #----------------------------------------------------------------------------#
  #
  #  Calculate derived variables
  #
  #----------------------------------------------------------------------------#

  ### Multi structure calculations

  # Calculate crossing constriction (at crossing level accross all culverts)
  crossing_constriction <- td |> group_by(survey_id) |>
    summarize(total_inlet_width = sum(inlet_width, na.rm = TRUE),
              total_outlet_width = sum(outlet_width, na.rm = TRUE),
              downstream_channel_width = first(downstream_channel_width),
              upstream_channel_width = first(upstream_channel_width)
    ) |>
    mutate(outlet_constriction = total_outlet_width/downstream_channel_width,
           inlet_constriction = total_inlet_width/upstream_channel_width)  |>
    mutate(crossing_constriction = pmin(inlet_constriction, outlet_constriction)) |>
    select(survey_id, crossing_constriction) |>
    as.data.frame()

  cross <- left_join(td, crossing_constriction, join_by(survey_id) )



  ####  Crossing level calculations


  # Tidal Constriction  function
  calc_tidal_constriction <- function(upstream_tidal_range, downstream_tidal_range) {
    tc <- upstream_tidal_range/downstream_tidal_range
    tc[upstream_tidal_range %in% 0 | downstream_tidal_range %in% 0] <- 0
    tc
  }


  cross <- cross |>
    mutate(
      tidal_constriction = calc_tidal_constriction(upstream_tidal_range, downstream_tidal_range),
      downstream_scour = downstream_pool_width / downstream_channel_width,
      upstream_scour = upstream_pool_width / upstream_channel_width)



  #----------------------------------------------------------------------------#
  #
  #  Calculate component scores
  #
  #----------------------------------------------------------------------------#

  # Setup

  # Use new dataframe but with a 1:1 correspondence to rows in cross
  scores <- cross |> select(survey_id)

  # Define scaling function using
  # von Bertalanffy functional form (Bolker pg 97)
  scaling_curve <- function(x, a = -5, b = 0.6) {
    1 / (1 + exp(a * (x - b)))
  }

  # Scores

  # 1. Constriction Ratio
  scores$constriction_ratio <- cross$crossing_constriction |> scaling_curve(a = -5, b = 0.6)

  # 2. Tidal Constriction
  scores$tidal_constriction <- cross$tidal_constriction |> scaling_curve(a = -10, b = 0.6)

  # 3. Vegetation change
  vegetation_scores <- data.frame(
    vegetation =c("Comparable",
                  "Slightly different",
                  "Moderately different",
                  "Very different",
                  "No data",
                  "Unknown"),
    score = c(1, 0.8, 0.4, 0, NA, NA))
  scores$vegetation_change <- left_join(cross, vegetation_scores, join_by(vegetation)) |>
    pull(score)

  # 4. High Tide Relative Water Depth
  a <-
    c( `< 0.10` = 0,
       `0.10-0.24` = 0.2,
       `0.25-0.49` = 0.4,
       `0.50-0.74` = 0.6,
       `0.75-0.99` = 0.8,
       `>= 1.0` = 1,
       `No data` = NA,
       `NA` = NA)
  water_depth_scores <- data.frame(relative_water_depth = names(a), score = as.vector(a))
  water_depth_scores$relative_water_depth[water_depth_scores$relative_water_depth  == "NA"] <- NA
  scores$high_tide_water_depth <- left_join(cross, water_depth_scores, join_by(relative_water_depth)) |>
    pull(score)

  # 5 & 6 Downstream and Upstream Scour
  # a = 4.0, b = 2.0
  # curve(scaling_curve(x, a = 4, b = 2), xlim = c(1, 6))
  scores$downstream_scour <- scaling_curve(cross$downstream_scour, a = 4, b = 2)
  scores$upstream_scour <- scaling_curve(cross$upstream_scour, a = 4, b = 2)

  # 7 & 8 Inlet and outlet perch at low tide
  # a = 10.0, b = .5
  # curve(scaling_curve(x, a = 10, b = 0.5), xlim = c(0, 2))
  scores$low_tide_inlet_perch <- cross$inlet_perch_low_tide |>
    scaling_curve(a = 10, b = 0.5)
  scores$low_tide_outlet_perch <- cross$outlet_perch_low_tide |>
    scaling_curve(a = 10, b = 0.5)

  # 9 & 10 Inlet and Outlet Armoring
  armoring_scores <- data.frame(
    armoring = c( "None", "Not Extensive", "Extensive", "No data", NA),
    score = c(1, 0.5, 0, NA, NA))

  scores$inlet_armoring <- left_join(cross, armoring_scores,
                                     join_by(inlet_armoring == armoring)) |>
    pull(score)
  scores$outlet_armoring <- left_join(cross, armoring_scores,
                                     join_by(outlet_armoring == armoring)) |>
    pull(score)

  # 11 Openness
  # curve(scaling_curve(x, a = -10, b = 0.6), xlim = c(0, 1.4))
  scores$openness <- pmin(cross$inlet_openness, cross$outlet_openness, na.rm = TRUE) |>
    scaling_curve(a = -10, b = 0.6)

  # 12 Substrate comparability
  # dput(unique(cross$substrate_matches_stream))
  substrate_comparability_scores <- data.frame(
    comp = c("Comparable",
             "Contrasting",
             "Not appropriate (e.g. rip rap)",
             "None",
             "Unknown",
            "No data",
            NA),
    score = c(1, 0.5, 0, 0, NA, NA, NA)
  )
  scores$substrate_comparability <-
    left_join(cross, substrate_comparability_scores,
              join_by(substrate_matches_stream == comp)) |>
    pull(score)

  # 13 Substrate Coverage
  substrate_coverage_scores <- data.frame(
    coverage = c("100%", "75%-99%", "50%-75%", "25%-50%",
                 "None", "Unknown", "No data",  NA),
    score = c(1, 0.8, 0.5, 0.2, 0, NA, NA, NA))
  scores$substrate_coverage <-
    left_join(cross, substrate_coverage_scores,
              join_by(substrate_coverage == coverage)) |>
    pull(score)

  # 14  & 15 High Tide Inlet and Outlet Perch
  # curve(scaling_curve(x, 4, 1.25), xlim = c(0, 4))
  scores$high_tide_inlet_perch <- scaling_curve(cross$inlet_perch_high_tide, a = 4, 1.25)
  scores$high_tide_outlet_perch <- scaling_curve(cross$outlet_perch_high_tide, a = 4, 1.25)


  # 16 Tide gates
  tide_gate_severity_scores <- data.frame(
    severity = c("No tide gate", "Minor", "Moderate", "Severe", "No aquatic passage","No data", NA),
    score = c(1, 0.8, 0.5, 0.2, 0, NA, NA)
    )
  scores$tide_gate <-
    left_join(cross,  tide_gate_severity_scores,
              join_by( tide_gate_barrier_severity == severity)) |>
    pull(score)

  # 17 Other physical barriers
  barrier_severity_scores <- data.frame(
    severity = c( "None", "Minor", "Moderate","Severe", "No aquatic passage",
      "No data", NA),
    score = c(1, 0.8, 0.5, 0.2, 0, NA, NA)
  )
  scores$physical_barrier <-  left_join(cross,  barrier_severity_scores,
                                        join_by( barrier_severity == severity)) |>
    pull(score)


  #----------------------------------------------------------------------------#
  # Calculate weighted preliminary score
  #  (prior to applying limits)
  #----------------------------------------------------------------------------#

  # Read weights table
  all_weights <- read.csv("data/weights.csv")
  names(all_weights) <- (gsub("\\.", "_", names(all_weights)) |> tolower() )
  all_weights$variable <- (gsub("[\\.[:blank:]]", "_", all_weights$variable) |> tolower() )
  all_weights[ , -1] <- all_weights[ , -1] / 100 # % to proportion

  # Define the stream types that each have a different weighting scheme
  stream_types <- c("Salt/brackish flow-through stream",
                    "Freshwater tidal",
                    "Salt marsh creek")
  # Lower case with no blanks
  lc_stream_types <- stream_types |>
    gsub("[//.////[:blank:]-]+", "_", x = _) |>
    tolower()

  if (!all(lc_stream_types %in% names(all_weights)))
    stop("Not all lower case stream types appear in the weights table")

  if (!all(stream_types %in% cross$stream_type))
    stop("Not all stream_types occur in the dataset - likely a spelling or formatting issue")


  if(!is.na(exclude)[1]) {
    exclude <- gsub("[[:blank:]]", "_", exclude)
    stopifnot(all(exclude %in% all_weights$variable))

    all_weights <- all_weights[!all_weights$variable %in% exclude, , drop = FALSE]


    for(col in 2:ncol(all_weights)){
      all_weights[ , col] <- all_weights[ , col] / sum(all_weights[, col])
    }

  }

  # Calculate weighted average of scores
  # Three stream types have different weighting schemes
  scores$weighed_score <- NA
  for (i in seq_along(stream_types)) {
    type_w <- lc_stream_types[i]  # for weights table column
    type_c <- stream_types[i]   # for crossing$stream_type values
    sv <- cross$stream_type %in% type_c
    weights <- all_weights[, c("variable", type_w)] |> rename(weight = 2)

    components <- scores[  , weights$variable] |> t()
    weighted_components <- components * weights$weight
    weighted_scores <- colSums(weighted_components)
    scores$weighed_score[sv] <- weighted_scores[sv]
  }


  #----------------------------------------------------------------------------#
  #  Apply limits
  #----------------------------------------------------------------------------#


  # Calculate limit (min of limiting varibles)
  limiting_variables <- c("high_tide_inlet_perch",
                          "high_tide_outlet_perch",
                          "tide_gate",
                          "physical_barrier")

  scores$limit <-scores[, limiting_variables] |>
    as.list() |>
    do.call(pmin, args = _)

  # Calculate final score (min of weighted score and limit)
  scores$score <- pmin(scores$weighed_score, scores$limit)

  # Force passability to 1 when there is no crossing or "bridge adequate"
  scores$score[cross$crossing_type == "No Crossing"] <- 1
  scores$score[cross$crossing_type == "Bridge Adequate"] <- 1


  if(return == "score")
    return(scores$score)

  # "everything" this is here to support debugging of scoring
  return(list(cross = cross, scores = scores))

}



