let simulation =
  Simulation.create_simluation_map ~map_height:10 ~map_width:10
    ~density:Simulation.Low
in

Simulation.start simulation
