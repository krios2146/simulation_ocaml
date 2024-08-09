let simulation =
  Simulation.create_simluation ~map_height:10 ~map_width:10
    ~density:Simulation.Low
in

Simulation.render_from_start simulation
