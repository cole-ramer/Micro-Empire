open! Core

module Level = struct
  type t = int [@@deriving sexp]
end

type t = Nutrient_absorption | Decay_reduction | Movement | Strength | Size
[@@deriving sexp]

let list_of = [ Nutrient_absorption; Decay_reduction; Movement; Strength; Size ]

module Effect = struct
  let get_num_of_added_cells current_size =
    let open Float in
    let s = float_of_int current_size in

    let a = 1.0 in
    let b = 1.0 in
    int_of_float (Float.round_up (a *. log (b +. s))) 

  let nutrient_absorption_gain (level : Level.t) =
    10 + int_of_float (8. *. log (float_of_int (level + 1)))

  let movement_cost ~size ~level =
    let base_cost = 5.0 +. log (float_of_int (size + 1)) in
    let reduction = float_of_int level *. 0.15 in
    int_of_float (base_cost *. (1.0 -. reduction))

  let get_strength_power ~size ~level =
    let open Float in
    let s = float_of_int level in
    let z = float_of_int size in

    (* Core power components *)
    let strength_component = s ** 1.3 in
    let size_component = 0.5 *. (z ** 0.8) in

    (* Randomness: uniform float between -2.0 and 2.0 *)
    let randomness = Random.float 4.0 -. 2.0 in

    (* Final power *)
    Float.round_nearest (strength_component +. size_component +. randomness)
    |> Int.of_float

  let get_decay_amount ~size ~level = size + (level / 100000)
end

module Cost = struct
  let nutrient_absorption_cost (level : Level.t) =
    match level >= 3 with
    | true -> 15 + (level * level * 3)
    | false -> 15 + (level * 10)

  let decay_reduction_cost (level : Level.t) =
    40 + (level * 20) + (level * level * 25)

  let movement_reduction_cost level = 20 + (level * 15) + (level * level * 10)
  let strength_increase_cost level = 15 * int_of_float (2. ** float_of_int level)

  let size_upgrade_cost current_size =
    let open Float in
    let s = float_of_int current_size in

    let cells_added = Effect.get_num_of_added_cells current_size in

    let base = 10.0 in
    let k = 5.0 in
    let cost_per_cell = base /. log (k +. s) in

    Float.round (cost_per_cell *. float_of_int cells_added) |> Int.of_float
end

let upgrade_cost ?level ?size (upgrade : t) =
  match (upgrade, level, size) with
  | Nutrient_absorption, Some lev, None -> Cost.nutrient_absorption_cost lev
  | Decay_reduction, Some lev, None -> Cost.decay_reduction_cost lev
  | Movement, Some lev, None -> Cost.movement_reduction_cost lev
  | Strength, Some lev, None -> Cost.strength_increase_cost lev
  | Size, None, Some s -> Cost.size_upgrade_cost s
  | _, _, _ ->
      raise_s
        [%message
          (level : int option)
            (size : int option)
            (upgrade : t)
            "invalid arguments to upgrade cost. Getting cost for size increase \
             should only pass in the size optional argument. For all other \
             upgrades only pass in the level optional argument "]

let upgrade_effect ?level ?size (upgrade : t) =
  match (upgrade, level, size) with
  | Nutrient_absorption, Some lev, None -> Effect.nutrient_absorption_gain lev
  | Decay_reduction, Some lev, Some s ->
      Effect.get_decay_amount ~size:s ~level:lev
  | Movement, Some lev, Some s -> Effect.movement_cost ~size:s ~level:lev
  | Strength, Some lev, Some s -> Effect.get_strength_power ~size:s ~level:lev
  | Size, None, Some s -> Effect.get_num_of_added_cells s
  | _, _, _ ->
      raise_s
        [%message
          (level : int option)
            (size : int option)
            (upgrade : t)
            "invalid arguments to upgrade effect. Getting effect for size \
             increase should only pass in the size optional argument. Getting \
             effect for nutrient absoprtion shoud only pass in level. All \
             other should pass in both\n\
            \              "]
