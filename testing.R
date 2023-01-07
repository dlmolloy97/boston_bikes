neighbourhood_quoted <- sqldf::read.csv.sql("..//BostonBikeShare/data/november_enriched.csv", "SELECT DISTINCT start_neighbourhood as neighbourhood FROM file")
