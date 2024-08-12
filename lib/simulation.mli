type density = Low | Medium | High
type simulation_map

val create_simluation_map :
  map_height:int -> map_width:int -> density:density -> simulation_map

val start : simulation_map -> unit
