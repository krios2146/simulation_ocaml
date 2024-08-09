type creature = Herbivore | Predator
type entity = Tree | Grass | Rock | Creature of creature
type coordinates = { x : int; y : int }

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
          render simulation
            { coordinates with y = coordinates.y + 1 }
            (to_render ^ "\n")
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
