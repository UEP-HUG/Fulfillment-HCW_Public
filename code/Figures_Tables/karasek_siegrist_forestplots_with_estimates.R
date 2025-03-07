# first run the first chunks from "Current_snapshot_analyses_final.Rmd" to generate the data

# Karasek measures  ####
tiff(filename = here("output", "Publication figures", paste0("karasek_occupation_forest.tif")),
     width=12, height=12, units = "in", res = 300, compression = "lzw")
karasek_forest
dev.off()


# Siegrist measures  ####
tiff(filename = here("output", "Publication figures", paste0("siegrist_occupation_forest.tif")),
     width=12, height=12, units = "in", res = 300, compression = "lzw")
siegrist_forest
dev.off()
