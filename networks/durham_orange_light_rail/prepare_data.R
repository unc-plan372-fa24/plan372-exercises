# remove Route 400 from the GoTriangle GTFS, as it would presumably be replaced by the light rail
library(gtfstools)

gtfs = read_gtfs(here::here("networks/durham_orange_light_rail/data/baseline/gotriangle.zip"))

route_id = gtfs$routes[route_short_name == "400", route_id]

gtfs = gtfs |>
  filter_by_route_id(route_id, keep = FALSE)

write_gtfs(gtfs, here::here("networks/durham_orange_light_rail/data/scenario/gotriangle.zip"))
