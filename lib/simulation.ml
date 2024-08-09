type creature = Herbivore | Predator
type entity = Tree | Grass | Rock | Creature of creature
type coordinates = { x : int; y : int }
type density = High | Medium | Low

module CoordinatesMap = Map.Make (struct
  type t = coordinates

  let compare (this : t) (that : t) =
    match compare this.x that.x with 0 -> compare this.y that.y | n -> n
end)

(* ensuring only entity is allowed to be values in the map *)
type coordinates_entity_map = entity CoordinatesMap.t
type simulation = { map : coordinates_entity_map; max_x : int; max_y : int }

let find_entity_by_coordinates coordinates_entity_map coordinates =
  CoordinatesMap.find_opt coordinates coordinates_entity_map

let get_entity_sprite entity =
  match entity with
  | Grass -> "🍀"
  | Rock -> "🪨"
  | Tree -> "🌳"
  | Creature Herbivore -> "🐑"
  | Creature Predator -> "🐺"

let empty_sprite = "  "

let rec render simulation coordinates (to_render : string) =
  match (coordinates.y, simulation.max_y) with
  | y, max_y when y >= max_y -> print_string to_render
  | _ -> (
      match (coordinates.x, simulation.max_x) with
      | x, max_x when x >= max_x ->
          render simulation { x = 0; y = coordinates.y + 1 } (to_render ^ "\n")
      | _ ->
          let entity = find_entity_by_coordinates simulation.map coordinates in
          let sprite =
            match entity with
            | Some entity -> get_entity_sprite entity
            | None -> empty_sprite
          in
          let to_render = to_render ^ sprite in
          render simulation { coordinates with x = coordinates.x + 1 } to_render
      )

let render_from_start simulation = render simulation { x = 0; y = 0 } ""

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

let create_simluation ~(map_height : int) ~(map_width : int)
    ~(density : density) =
  let place coordinates_entity_map entities =
    place coordinates_entity_map entities map_width map_height
  in

  let map : coordinates_entity_map = CoordinatesMap.empty in
  let area = map_width * map_height in
  let percent value = percent value area in

  let map =
    match density with
    | High ->
        let map = place map (generate (percent 40) Grass) in
        let map = place map (generate (percent 10) Tree) in
        let map = place map (generate (percent 5) Rock) in
        let map = place map (generate (percent 3) (Creature Predator)) in
        place map (generate (percent 5) (Creature Herbivore))
    | Medium ->
        let map = place map (generate (percent 20) Grass) in
        let map = place map (generate (percent 5) Tree) in
        let map = place map (generate (percent 2) Rock) in
        let map = place map (generate (percent 2) (Creature Predator)) in
        place map (generate (percent 4) (Creature Herbivore))
    | Low ->
        let map = place map (generate (percent 10) Grass) in
        let map = place map (generate (percent 4) Tree) in
        let map = place map (generate (percent 2) Rock) in
        let map = place map (generate (percent 1) (Creature Predator)) in
        place map (generate (percent 2) (Creature Herbivore))
  in
  { map; max_x = map_width; max_y = map_height }
