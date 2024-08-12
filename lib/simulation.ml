type herbivore = { health : int; speed : int }
type predator = { speed : int; attack_rating : int }
type creature = Herbivore of herbivore | Predator of predator
type entity = Tree | Grass | Rock | Creature of creature
type coordinates = { x : int; y : int }
type density = Low | Medium | High

module CoordinatesMap = Map.Make (struct
  type t = coordinates

  let compare (this : t) (that : t) =
    match compare this.x that.x with 0 -> compare this.y that.y | n -> n
end)

(* ensuring only entity is allowed to be values in the map *)
type coordinates_entity_map = entity CoordinatesMap.t
type simulation_map = { map : coordinates_entity_map; max_x : int; max_y : int }

let find coordinates_entity_map coordinates =
  CoordinatesMap.find_opt coordinates coordinates_entity_map

let get_entity_sprite entity =
  match entity with
  | Grass -> "🍀"
  | Rock -> "🪨"
  | Tree -> "🌳"
  | Creature (Herbivore _) -> "🐑"
  | Creature (Predator _) -> "🐺"

let empty_sprite = "  "

let rec render_from_with simulation coordinates (to_render : string) =
  match (coordinates.y, simulation.max_y) with
  | y, max_y when y >= max_y ->
      print_string to_render;
      flush stdout
  | _ -> (
      match (coordinates.x, simulation.max_x) with
      | x, max_x when x >= max_x ->
          render_from_with simulation
            { x = 0; y = coordinates.y + 1 }
            (to_render ^ "\n")
      | _ ->
          let entity = find simulation.map coordinates in
          let sprite =
            match entity with
            | Some entity -> get_entity_sprite entity
            | None -> empty_sprite
          in
          let to_render = to_render ^ sprite in
          render_from_with simulation
            { coordinates with x = coordinates.x + 1 }
            to_render)

let render simulation = render_from_with simulation { x = 0; y = 0 } ""

let gen_random_coordinates (max_x : int) (max_y : int) =
  let x = Random.int max_x in
  let y = Random.int max_y in
  { x; y }

let is_occupied coordinates coordinates_entity_map =
  CoordinatesMap.mem coordinates coordinates_entity_map

let generate amout entity = List.init amout (fun _ -> entity)

let rec place coordinates_entity_map (entities : entity list) max_x max_y =
  match entities with
  | [] -> coordinates_entity_map
  | head :: tail ->
      let coordinates = gen_random_coordinates max_x max_y in
      if is_occupied coordinates coordinates_entity_map then
        place coordinates_entity_map entities max_x max_y
      else
        let updated_map =
          CoordinatesMap.add coordinates head coordinates_entity_map
        in
        place updated_map tail max_x max_y

let percent value area = area * value / 100

let create_simluation_map ~(map_height : int) ~(map_width : int)
    ~(density : density) =
  let place coordinates_entity_map entities =
    place coordinates_entity_map entities map_width map_height
  in

  let map : coordinates_entity_map = CoordinatesMap.empty in
  let area = map_width * map_height in
  let percent value = percent value area in

  let predator = Creature (Predator { speed = 2; attack_rating = 50 }) in
  let herbivore = Creature (Herbivore { speed = 2; health = 100 }) in

  let map =
    match density with
    | High ->
        let map = place map (generate (percent 40) Grass) in
        let map = place map (generate (percent 10) Tree) in
        let map = place map (generate (percent 5) Rock) in
        let map = place map (generate (percent 3) predator) in
        place map (generate (percent 5) herbivore)
    | Medium ->
        let map = place map (generate (percent 20) Grass) in
        let map = place map (generate (percent 5) Tree) in
        let map = place map (generate (percent 2) Rock) in
        let map = place map (generate (percent 2) predator) in
        place map (generate (percent 4) herbivore)
    | Low ->
        let map = place map (generate (percent 10) Grass) in
        let map = place map (generate (percent 4) Tree) in
        let map = place map (generate (percent 2) Rock) in
        let map = place map (generate (percent 1) predator) in
        place map (generate (percent 2) herbivore)
  in
  { map; max_x = map_width; max_y = map_height }

let exists coordinates_entity_map predicate =
  CoordinatesMap.exists (fun _ e -> predicate e) coordinates_entity_map

let find_all coordinates_entity_map predicate =
  CoordinatesMap.filter (fun _ e -> predicate e) coordinates_entity_map
  |> CoordinatesMap.to_seq |> List.of_seq

let is_herbivore entity =
  match entity with Creature (Herbivore _) -> true | _ -> false

let is_predator entity =
  match entity with Creature (Predator _) -> true | _ -> false

let is_grass entity = match entity with Grass -> true | _ -> false

let is_over simulation =
  (not (exists simulation.map is_grass))
  || not (exists simulation.map is_herbivore)

module CoordinatesSet = Set.Make (struct
  type t = coordinates

  let compare (this : t) (that : t) =
    match compare this.x that.x with 0 -> compare this.y that.y | n -> n
end)

let is_valid coordinates max_x max_y =
  if coordinates.x >= max_x then false
  else if coordinates.y >= max_y then false
  else true

let rec reconstruct_path_rec predecessors origin path =
  match CoordinatesMap.find_opt origin predecessors with
  | Some parent ->
      let path = parent :: path in
      let origin = parent in
      reconstruct_path_rec predecessors origin path
  | None -> (
      match path with
      | _ :: t -> if List.length t >= 1 then t else path
      | [] -> [])

let reconstruct_path predecessors origin =
  reconstruct_path_rec predecessors origin [ origin ]

let find_neighbours simulation_map coordinates =
  let neighbours =
    [
      { x = coordinates.x - 1; y = coordinates.y - 1 };
      { x = coordinates.x; y = coordinates.y - 1 };
      { x = coordinates.x + 1; y = coordinates.y - 1 };
      { x = coordinates.x - 1; y = coordinates.y };
      { x = coordinates.x + 1; y = coordinates.y };
      { x = coordinates.x - 1; y = coordinates.y + 1 };
      { x = coordinates.x; y = coordinates.y + 1 };
      { x = coordinates.x + 1; y = coordinates.y + 1 };
    ]
  in
  let is_valid = function
    | coordinates ->
        is_valid coordinates simulation_map.max_x simulation_map.max_y
  in
  List.filter is_valid neighbours

let is_visited coordinates visited = CoordinatesSet.mem coordinates visited

let rec find_path_bfs simulation_map q visited predecessors target_predicate =
  let not_visited c = not (is_visited c visited) in
  try
    let current_coordinates = Queue.take q in
    let to_predecessor c = (c, current_coordinates) in
    let current_entity = find simulation_map.map current_coordinates in

    if Option.value ~default:false (Option.map target_predicate current_entity)
    then reconstruct_path predecessors current_coordinates
    else
      let neighbours = find_neighbours simulation_map current_coordinates in
      let neighbours = List.filter not_visited neighbours in
      let neighbours_seq = List.to_seq neighbours in
      let predecessors =
        CoordinatesMap.add_seq
          (List.map to_predecessor neighbours |> List.to_seq)
          predecessors
      in
      let visited = CoordinatesSet.add_seq neighbours_seq visited in
      Queue.add_seq q neighbours_seq;
      find_path_bfs simulation_map q visited predecessors target_predicate
  with Queue.Empty -> []

let find_path simulation_map coordinates target_predicate =
  let q = Queue.create () in
  let visited = CoordinatesSet.empty in
  let predecessors = CoordinatesMap.empty in

  Queue.add coordinates q;
  let visited = CoordinatesSet.add coordinates visited in

  find_path_bfs simulation_map q visited predecessors target_predicate

let move_to target origin simulation_map entity =
  let map = CoordinatesMap.remove origin simulation_map.map in
  CoordinatesMap.add target entity map

let last list = List.nth list (List.length list - 1)

let nearest list =
  if List.length list < 2 then last list
  else List.nth list (List.length list - 2)

let make_move simulation_map coordinates entity path =
  try
    match path with
    | [] -> simulation_map
    | _ -> (
        match entity with
        | Creature (Herbivore h) ->
            if h.speed > List.length path then
              let map =
                move_to (nearest path) coordinates simulation_map entity
              in
              let grass = find map (last path) in
              match grass with
              | Some _ ->
                  let map = CoordinatesMap.remove coordinates map in
                  { simulation_map with map }
              | None -> simulation_map
            else
              let map =
                move_to (List.nth path h.speed) coordinates simulation_map
                  entity
              in
              { simulation_map with map }
        | Creature (Predator p) ->
            if p.speed > List.length path then
              let map =
                move_to (nearest path) coordinates simulation_map entity
              in
              let herbivore = find map (last path) in
              match herbivore with
              | Some (Creature (Herbivore h)) ->
                  let herbivore =
                    { h with health = h.health - p.attack_rating }
                  in
                  let map =
                    CoordinatesMap.add (last path)
                      (Creature (Herbivore herbivore)) map
                  in
                  { simulation_map with map }
              | Some _ -> simulation_map
              | None -> simulation_map
            else
              let map =
                move_to (List.nth path p.speed) coordinates simulation_map
                  entity
              in
              { simulation_map with map }
        | _ -> simulation_map)
  with Failure _ | Invalid_argument _ -> simulation_map

let rec move_creatures (creatures : (coordinates * entity) list) simulation_map
    =
  match creatures with
  | [] -> simulation_map
  | (c, e) :: t ->
      let target_predicate =
        match e with
        | Creature (Herbivore _) -> is_grass
        | Creature (Predator _) -> is_herbivore
        | _ -> fun _ -> false
      in
      let path = find_path simulation_map c target_predicate in
      let simulation_map = make_move simulation_map c e path in
      move_creatures t simulation_map

let iterate simulation_map =
  let herbivores = find_all simulation_map.map is_herbivore in
  let predators = find_all simulation_map.map is_predator in

  let simulation_map = move_creatures herbivores simulation_map in
  let simulation_map = move_creatures predators simulation_map in

  simulation_map

let rec simulation_cycle simulation_map =
  Unix.sleep 1;
  render simulation_map;
  match is_over simulation_map with
  | true -> exit 0
  | false ->
      let simulation_map = iterate simulation_map in
      simulation_cycle simulation_map

let start simulation_map = simulation_cycle simulation_map
