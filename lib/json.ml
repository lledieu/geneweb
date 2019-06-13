open Def
open Config
open Gwdb
open Util

let json_of_iper p =
  let s = Printf.sprintf "%d" (Adef.int_of_iper (get_key_index p)) in
  "id", `String s

let json_of_first_name base p =
  let s = sou base (get_first_name p) in
  "fn", `String s

let json_of_surname base p particle_at_the_end =
  let s = sou base (get_surname p) in
  let s =
    if particle_at_the_end then surname_without_particle base s ^ surname_particle base s
    else s
  in
  "sn", `String s

let json_of_julian_day dmy =
  let s = Printf.sprintf "%d" (Calendar.sdn_of_julian dmy) in
  "jd", `String s

let json_of_event_date conf t_event dmy =
  let s = Printf.sprintf "%s%s" (Date.prec_text conf dmy) (string_of_int dmy.year) in
  let d = "d", `String s in
  t_event, `Assoc [ d ; json_of_julian_day dmy ]

let json_of_birth_date conf p =
  match Adef.od_of_cdate (get_birth p) with
  | Some d -> begin match d with
    | Dgreg (dmy, _) -> [json_of_event_date conf "bid" dmy]
    | _ -> []
    end
  | _ -> []

let json_of_death_date conf p =
  match get_death p with
  | Death (_, cd) -> begin match Adef.od_of_cdate cd with
    | Some (Dgreg (dmy, _)) -> [json_of_event_date conf "ded" dmy]
    | _ -> []
    end
  | _ -> []

let json_of_parents conf base p =
  match get_parents p with
  | Some ifam ->
      let cpl = foi base ifam in
      let json =
        let moth = pget conf base (get_mother cpl) in
        if p_first_name base moth = "?" then []
        else ["mo", `Assoc [json_of_first_name base moth ;
                            json_of_surname base moth true] ]
      in
      let fath = pget conf base (get_father cpl) in
      if p_first_name base fath = "?" then json
      else ("fa", `Assoc [json_of_first_name base fath]) :: json
  | None -> []

let json_of_spouses conf base p =
  let jd =
    if Array.length (get_family p) > 0 then
      let fam = foi base (get_family p).(0) in
      match Adef.od_of_cdate (get_marriage fam) with
      | Some d -> begin match d with
          | Dgreg (dmy, _) -> [json_of_julian_day dmy]
          | _ -> []
          end
      | None -> []
    else []
  in
  let text =
    let rec loop i res =
      let sep = if i = 0 then "" else "<br>" in
      if i < Array.length (get_family p) then
        let fam = foi base (get_family p).(i) in
        let prec_year =
          match Adef.od_of_cdate (get_marriage fam) with
          | Some d -> begin match d with
            | Dgreg (dmy, _) -> (Date.prec_text conf dmy) ^ (string_of_int dmy.year)
            | _ -> ""
            end
          | _ -> ""
        in
        let conjoint = Gutil.spouse (get_key_index p) fam in
        let conjoint = pget conf base conjoint in
        let fn = sou base (get_first_name conjoint) in
        let sn = sou base (get_surname conjoint) in
        if know base conjoint then
          loop (i + 1)
           (res ^ sep ^ prec_year ^ " " ^ fn ^ " " ^ sn)
        else loop (i + 1) (res ^ sep)
      else res
    in
    loop 0 ""
  in
  let e = "d", `String text in
  "sp", `Assoc ( e :: jd )

let json_of_result_person conf base get_fn get_sn p =
  `Assoc (
      [json_of_iper p]
    @ (if get_fn then [json_of_first_name base p] else [])
    @ (if get_sn then [json_of_surname base p true] else [])
    @ (json_of_birth_date conf p)
    @ (json_of_death_date conf p)
    @ (json_of_parents conf base p)
    @ [json_of_spouses conf base p]
  )

let print_results conf base truncated request_text list =
  let get_fn =
    match p_getenv conf.env "first_name" with
    | Some "" | None -> true
    | _ -> false
  in
  let get_sn =
    match p_getenv conf.env "surname" with
    | Some "" | None -> true
    | _ -> false
  in
 let json = `Assoc [
   "truncated", `Bool truncated ;
   "request_text", `String request_text ;
   "data", `List (List.map
      (fun p ->
        json_of_result_person conf base get_fn get_sn p)
      list) ]
 in
 Yojson.to_string json
