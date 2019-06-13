(* Copyright (c) 2001 Ludovic LEDIEU *)

open Geneweb
open Def
open Gutil
open Gwdb

let test = ref false

let person_year p =
  match
    (Adef.od_of_cdate p.birth, Adef.od_of_cdate p.baptism, p.death,
     CheckItem.date_of_death p.death)
  with
    (_, _, NotDead, _) -> None
  | (Some (Dgreg (d, _)), _, _, _) -> Some d.year
  | (_, Some (Dgreg (d, _)), _, _) -> Some d.year
  | (_, _, _, Some (Dgreg (d, _))) -> Some d.year
  | _ -> None

let is_old_person lim_year p =
  match person_year p with
  | Some d -> d < lim_year
  | None -> false

let family_year f =
  match (Adef.od_of_cdate f.marriage, f.divorce) with
    (Some (Dgreg (d, _)), _) -> Some d.year
  | (None, Divorced d) ->
      begin match Adef.od_of_cdate d with
        Some (Dgreg (d, _)) -> Some d.year
      | _ -> None
      end
  | _ -> None

let is_old_family lim_year f =
  match family_year f with
    Some d -> d < lim_year
  | None -> false

let no_date p =
  p.access <> Public &&
  Adef.od_of_cdate p.birth = None &&
  Adef.od_of_cdate p.baptism = None &&
  CheckItem.date_of_death p.death = None

let set_visible base cnt p =
  if no_date p then begin
    if !test then
      Printf.printf "%s.%d %s\n" (sou base p.first_name) (p.occ)
        (sou base p.surname)
    else ();
    cnt := !cnt + 1;
    patch_person base p.key_index {(p) with access = Public}
  end
  else ()

let rec propagate_nb_gen_asc base fam_tab nb_gen ifam =
  if nb_gen <= (Array.get fam_tab (Adef.int_of_ifam ifam)) then ()
  else
    let _ = Array.set fam_tab (Adef.int_of_ifam ifam) nb_gen in
    let f = foi base ifam in
    let fath = poi base (get_father f) in
    let moth = poi base (get_mother f) in
    begin
      match (get_parents fath) with
        Some af -> propagate_nb_gen_asc base fam_tab (nb_gen + 1) af
      | None -> ();
      match (get_parents moth) with
        Some am -> propagate_nb_gen_asc base fam_tab (nb_gen + 1) am
      | None -> ()
    end

let rec propagate_nb_gen base fam_tab nb_gen ifam =
  if nb_gen <= (Array.get fam_tab (Adef.int_of_ifam ifam)) then ()
  else begin
    let _ = Array.set fam_tab (Adef.int_of_ifam ifam) nb_gen in
    let to_unions iper =
      let p = poi base iper in
      let u = Array.to_list (get_family p) in
      List.iter (propagate_nb_gen base fam_tab (nb_gen - 1)) u
    in
    let f = foi base ifam in
    let d = Array.to_list (get_children f) in
    List.iter to_unions d;
    let fath = poi base (get_father f) in
    match (get_parents fath) with
      Some af -> propagate_nb_gen_asc base fam_tab (nb_gen + 1) af
    | None -> () ;
    let moth = poi base (get_mother f) in
    match (get_parents moth) with
      Some am -> propagate_nb_gen_asc base fam_tab (nb_gen + 1) am
    | None -> ()
  end

let years_per_gen = 60

let nb_gen_from_fam base fam_tab lim_year f =
  match family_year f with
    Some y ->
      let nb_gen = max 0 ((lim_year - (y + 20)) / years_per_gen) in
      propagate_nb_gen base fam_tab nb_gen f.fam_index
  | None -> ()

let nb_gen_from_per base fam_tab lim_year p =
  let nb_gen =
    match person_year (gen_person_of_person p) with
      Some y -> max 0 ((lim_year - y) / years_per_gen)
    | None -> 0
  in
  List.iter (propagate_nb_gen base fam_tab nb_gen)
    (Array.to_list (get_family p))

(* Rendre visible toute l'ascendance d'une personne *)
let rec p_public_action base per_tab lim_year cnt p =
  let iper = get_key_index p in
  let iper_int = Adef.int_of_iper iper in
  if Array.get per_tab iper_int || (* déjà passé *)
     (is_quest_string (get_surname p) && is_quest_string (get_first_name p)) (* personne fictive *)
  then ()
  else begin
    (* mémorisation du passage *)
    Array.set per_tab iper_int true;
    (* rendre visible *)
    set_visible base cnt (gen_person_of_person p);
    (* on enchaîne avec les époux *)
    List.iter
      (fun ifam ->
         let f = foi base ifam in
         let sp = poi base (spouse iper f) in
         p_public_action base per_tab lim_year cnt sp)
      (Array.to_list (get_family p));
    (* on enchaîne avec l'ascendance *)
    match get_parents p with
      Some ifam -> begin
        let f = foi base ifam in
        let fath = get_father f in
        let moth = get_mother f in
        p_public_action base per_tab lim_year cnt (poi base fath);
        p_public_action base per_tab lim_year cnt (poi base moth)
      end
    | None -> ()
  end

let public_action base lim_year =
  let cnt = ref 0 in
  (* traitement pour mémoriser le traitement des personnes *)
  let per_tab = Array.make (nb_of_persons base) false in
  (* traitement pour mémoriser le traitement des famille (nb générations remontées) *)
  let fam_tab = Array.make (nb_of_families base) 0 in
  begin
    (* Initialisation pour chaque personne *)
    for i = 0 to nb_of_persons base - 1 do
      let p = poi base (Adef.iper_of_int i) in
      begin
        if is_old_person lim_year (gen_person_of_person p) then
          p_public_action base per_tab lim_year cnt p
        else ();
        nb_gen_from_per base fam_tab lim_year p
      end
    done;
    (* Initialisation pour chaque famille *)
    for i = 0 to nb_of_families base - 1 do
      let f = foi base (Adef.ifam_of_int i) in
      let gen_f = gen_family_of_family f in
      if is_deleted_family f then ()
      else begin
        if is_old_family lim_year gen_f then begin
          p_public_action base per_tab lim_year cnt (poi base (get_father f));
          p_public_action base per_tab lim_year cnt (poi base (get_mother f))
        end
        else ();
        nb_gen_from_fam base fam_tab lim_year gen_f
      end
    done;
    (* Propagation vers le bas *)
    for i = 0 to nb_of_families base - 1 do
      if Array.get fam_tab i <> 0 then
        let f = foi base (Adef.ifam_of_int i) in
        let d = Array.to_list (get_children f) in
        List.iter
          (fun iper -> p_public_action base per_tab lim_year cnt (poi base iper))
          d
      else ()
    done;
    (* Compte-rendu *)
    Printf.printf "%d persons updated.\n" !cnt
  end

(* main *)

let fname = ref ""
let limit_year = ref 1850

let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <database>"
let speclist =
  [("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock data base");
   ("-year", Arg.String (fun s -> limit_year := int_of_string s),
    ": limit year (default is 1850)");
   ("-test", Arg.Set test, ": test mode.")]
let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several data bases")

let main () =
  Argl.parse speclist anonfun errmsg;
  if !fname = "" then begin
    Printf.eprintf "Missing file name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2
  end
  else ();
  let f () =
    let base = open_base !fname in
    begin
      let () = load_ascends_array base in
      let () = load_couples_array base in
      let () = load_unions_array base in
      let () = load_descends_array base in
      public_action base !limit_year;
      if !test then
        Printf.printf "Test mode: nothing changed in database.\n"
      else commit_patches base
    end
  in
  Lock.control_retry
    (Mutil.lock_file !fname)
    ~onerror:Lock.print_error_and_exit @@ fun () ->
    f ()

let _ = Printexc.catch main ()
