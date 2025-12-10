
import_tidal <- function(file) {

  td <- read_csv(file, na = c("NULL", "NA", "")) |> as.data.frame()

  # Redefine -1 as NA when appropriate
  if(FALSE) {
    # code used to define the neg_1_cols I ended up using all but
    # defined them manually so I could review and make sure -1 couldn't be
    # a real value for any of them
    sv <- sapply(td, function(x) is.numeric(x) && any(x == -1))
    neg_1_cols <- names(td)[sv]
    dput(neg_1_cols)
  }
  neg_1_cols <- c(
    "Aquatic_Score_Tidal_Stream",
    "Downstream_Channel_Width",
    "Downstream_Pool_Width",
    "Downstream_Tidal_Range",
    "Number_Of_Culverts",
    "Road_Fill_Height",
    "Salinity",
    "Upstream_Channel_Width",
    "Upstream_Pool_Width",
    "Upstream_Tidal_Range",
    "Crossing_Structure_Length",
    "Height_Above_Dry_Passage",
    "Inlet_Abutment_Height",
    "Inlet_Height",
    "Inlet_High_Tide_Water_Depth",
    "Inlet_Openness",
    "Inlet_Perch_High_Tide",
    "Inlet_Perch_Low_Tide",
    "Inlet_Spring_TideWater_Depth",
    "Inlet_Substrate_Water_Width",
    "Inlet_Water_Depth",
    "Inlet_Width",
    "Outlet_Abutment_Height",
    "Outlet_Height",
    "Outlet_High_Tide_Water_Depth",
    "Outlet_Openness",
    "Outlet_Perch_High_Tide",
    "Outlet_Perch_Low_Tide",
    "Outlet_Spring_TideWater_Depth",
    "Outlet_Substrate_Water_Width",
    "Outlet_Water_Depth",
    "Outlet_Width"
  )
  # Replace -1 with NA in selected cols
  for(col in neg_1_cols) {
    td[[col]][td[[col]] %in% -1] <- NA
  }


  names(td) <- tolower(names(td))
  return(td)

}
