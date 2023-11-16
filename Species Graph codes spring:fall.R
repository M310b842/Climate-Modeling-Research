biomassbyLat <- function(data, species){
  # new df of your species only
  df <- FULLDATA[FULLDATA[, "spnm"] == species, ]
  # add a column of the Latitude rounded to a degree
  df[, "latr"] <- round(df[, "DECDEG_BEGLAT"])
  # change a EXPCATCHWT to log(1+EXPCATCHWT) column
  df[, "logEXPCATCHWT"] <- log(1 + df[, "EXPCATCHWT"])
  # density and biomass aggregated by year and Latitude
  biomass_yl <- serialAgg(df, AggCats = c("GMT_YEAR", "latr", "fall"), AggTarg = "logEXPCATCHWT")
  # create a list of numerical and character Latitudes except for 35
  Lat <- sort(unique(biomass_yl[, 2]))
  Lat <- Lat[-1]
  Lat_chr <- as.character(Lat)
  # split the screen
  par(mfrow = c(3, 3))
  # for() loop that plots the biomass over time for each Latitude between 36 and 44
  for (i in seq_along(Lat)) {
    # create a character main title for the plot
    main_title <- paste(species, " - Lat =", Lat_chr[i])
    # plot biomass in the fall for Latitude i
    plot(biomass_yl[biomass_yl[, 2] == Lat[i] & biomass_yl[, 3] == 1, c(1, 4)], type = "l", lwd = 2, 
         ylim = c(min(biomass_yl[, 4]), max(biomass_yl[, 4])),
         main = main_title,
         xlab = "Year",
         ylab = "Biomass")
    # add biomass in the spring for latitude i
    lines(biomass_yl[biomass_yl[, 2] == Lat[i] & biomass_yl[, 3] == 0, c(1, 4)], lwd = 2, col = 2)
    # add a horizontal line at year 2007
    abline(v = 2007.5, col = 4, lty = 2)
  }
}

# list of species
ListofSpecies <- c("BLACKBELLY ROSEFISH", "STRIPED SEAROBIN", "FAWN CUSK-EEL", 
                   "WINTER SKATE", "CLEARNOSE SKATE", "ALEWIFE", 
                   "AMERICAN SHAD", "LONGHORN SCULPIN", "BUTTERFISH", "SMOOTH DOGFISH")

# plot all the species
for (species in ListofSpecies){
  biomassbyLat(data, species)
  print(species)
}