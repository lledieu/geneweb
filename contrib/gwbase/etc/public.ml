(* camlp4r *)
(* $Id: public.ml,v 5.00 2018-07-04 09:03:02 hg Exp $ *)

open Def;
open Gwdb;
open Printf;

value year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | _ -> None ]
;

value most_recent_year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | _ -> None ]
;

value is_old lim_year p =
  match year_of p with
  [ Some y -> y < lim_year
  | None -> False ]
;

value nb_gen_by_century = 3;

value nb_desc_gen lim_year p =
  match most_recent_year_of p with
  [ Some year -> (lim_year - year) * nb_gen_by_century / 100
  | None -> 0 ]
;

value changes = ref 0;
value trace = ref False;
value execute = ref True;
value ascend = ref True;
value age_death = ref 80;
value age_sp = ref 20;

value today = ref 2018;
value lim_year = ref 0;
value lim_b = ref 120;
value lim_d = ref 20;
value lim_m = ref 100;
value ind = ref "";
value bname = ref "";
value everybody = ref False;

(*
type death =
  [ NotDead
  | Death of death_reason and cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead ]
;
*)

value get_b_dates base p =
  let (reason, d, d2) =
    match
      (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p))
    with
    [ (_, _, NotDead, _) -> ("not dead", 0, 0)
    | (_, Some (Dgreg d _), _, _) -> ("baptized on", d.year, d.year+lim_b.val)
    | (Some (Dgreg d _), _, _, _) -> ("born on", d.year, d.year+lim_b.val)
    | (_, _, _, _) -> ("unknown", 0, 0) ]
  in
  (reason, d, d2)
;

value get_d_dates base p =
  let (reason, d, d2) =
    match
      (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p))
    with
    [ (_, _, NotDead, _) -> ("not dead", 0, 0)
    | (_, _, (Death reasn d1), Some (Dgreg d _)) -> ("dead on", d.year, (d.year+lim_d.val))
    | (_, _, DeadYoung, _) -> ("dead young", 0, -1)
    | (_, _, OfCourseDead, _) -> ("of course dead", 0, -2)
    | (_, _, DeadDontKnowWhen, _) -> ("dead, but dont know when", 0, -3)
    | (_, _, DontKnowIfDead, _) -> ("dont know if dead", 0, -4)
    | (_, _, (Death _ _), _) -> ("dead, no reason (d), no date", 0, -5) ]
  in
  (reason, d, d2)
;

value mark_old base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) &&
    old.(Adef.int_of_iper (get_key_index p)) = 0 &&
    ((get_access p) <> Public )
  then do {
    let (reason, y, old_p) = get_b_dates base p in
    if old_p > today.val || y = 0 then 
      old.(Adef.int_of_iper (get_key_index p)) := 1
    else do {
      (* birth date + lim_b > today *)
      old.(Adef.int_of_iper (get_key_index p)) := y;
      printf "Old (birth): %s, %s %d %d\n" (Gutil.designation base p) reason y old_p;
      flush stdout
      };
    let (reason, y, old_p) = get_d_dates base p in
    if old_p > today.val || y = 0 then 
      old.(Adef.int_of_iper (get_key_index p)) := 1
    else do {
      (* death date + lim_d > today *)
      old.(Adef.int_of_iper (get_key_index p)) := y;
      printf "Old (death): %s, %s %d %d\n" (Gutil.designation base p) reason y old_p;
      flush stdout
      };
      
    for i = 0 to Array.length (get_family p) - 1 do {
      let ifam = (get_family p).(i) in
      let fam = foi base ifam in
      let m_date = 
        match Adef.od_of_codate (get_marriage fam) with
        [ Some (Dgreg d _) -> Some (d.year+lim_m.val)
        | _ -> None ]
      in
      let isp = Gutil.spouse (get_key_index p) fam in
      let sp = poi base isp in do {
        match m_date with
        [ Some d -> 
            if d < today.val then do {
              (* marriage date + lim_m < today *)
              printf "Old (marriage): %s, marriage %d\n" (Gutil.designation base p) (d-lim_m.val);
              printf "Old (marriage): %s, marriage %d\n" (Gutil.designation base sp) (d-lim_m.val);
              flush stdout;
              old.(Adef.int_of_iper (get_key_index p)) := d-lim_m.val;
              old.(Adef.int_of_iper (get_key_index sp)) := d-lim_m.val;
            }
            else ()
        | None -> ()];
        if not scanned.(Adef.int_of_iper isp) &&
          old.(Adef.int_of_iper (get_key_index sp)) = 0
        then do {
          scanned.(Adef.int_of_iper (get_key_index sp)) := True;
          let (reason_sp, y_sp, old_sp) = get_b_dates base sp in
          if old_sp > today.val then (* mark spouse itself as old *)
            old.(Adef.int_of_iper (get_key_index sp)) := 1
          else
            old.(Adef.int_of_iper (get_key_index sp)) := y_sp;
          let (reason_sp, y_sp, old_sp) = get_d_dates base sp in
          if old_sp > today.val then (* mark spouse itself as old *)
            old.(Adef.int_of_iper (get_key_index sp)) := 1
          else
            old.(Adef.int_of_iper (get_key_index sp)) := y_sp;
        }
        else ();
      }
    }
  }
  else ()
;

value rec mark_ancestors base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) then do {
    scanned.(Adef.int_of_iper (get_key_index p)) := True;
    if not (is_quest_string (get_first_name p)) &&
       not (is_quest_string (get_surname p))
    then 
      if ((get_access p) <> Public) then do {
        if trace.val  then do {
          let (reason, d, date) = get_b_dates base p in
          printf "Anc: %s, %s %d\n" (Gutil.designation base p) reason d;
          flush stdout; 
          }
        else ();
        let gp = {(gen_person_of_person p) with access = Public} in
        if execute.val then patch_person base gp.key_index gp 
        else ();
        incr changes;
        }
      else ()
    else ();
    if ascend.val then 
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            mark_ancestors base scanned old (poi base (get_father cpl));
            mark_ancestors base scanned old (poi base (get_mother cpl));
          }
      | None -> () ]
    else ();
  }
  else ()
;

value public_everybody old bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> Public then
        let gp = {(gen_person_of_person p) with access = Public} in
        if execute.val then patch_person base gp.key_index gp else ()
      else ();
    };
    if changes.val > 0 then do {
      commit_patches base;
      printf "Patches applied\n"; flush stdout;
      }
    else ();
  }
;

value cnt = ref 0;

value public_all old bname lim_year =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    let scanned = Array.make (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      (*
      let p = poi base (Adef.iper_of_int i) in
      let _ = printf "Pers: %s\n" (Gutil.designation base p) in
      let _ = flush stdout in
      *)
      if not scanned.(i) then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_old base scanned old p 
      }
      else ();
    };
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      if old.(i) > 1 then incr cnt else ();
    };
    let scanned = Array.make (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      if old.(i) > 1 then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_ancestors base scanned old p
      }
      else ();
    };
    if changes.val > 0 then do {
      commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
  }
;

value public_some old bname lim_year key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      let _ = printf "Pers: %s\n" (Gutil.designation base p) in
      let _ = flush stdout in
      let scanned = Array.make (nb_of_persons base) False in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      do {
        mark_ancestors base scanned old p;
        if changes.val > 0 then do {
          commit_patches base;
          printf "Patches applied\n"; flush stdout;
        }
        else ();
        
      }
  | _ ->
      do {
        Printf.eprintf "Bad key %s\n" key;
        flush stderr;
        exit 2
      } ]
;


value speclist =
  [("-lb", Arg.Int (fun i -> lim_b.val := i),
    "limit birth (default = " ^ string_of_int lim_b.val ^ ")");
   ("-ld", Arg.Int (fun i -> lim_d.val := i),
    "limit death (default = " ^ string_of_int lim_d.val ^ ")");
   ("-lm", Arg.Int (fun i -> lim_m.val := i),
    "limit marriage (default = " ^ string_of_int lim_m.val ^ ")");
   ("-everybody", Arg.Set everybody, "set flag public to everybody");
   ("-ind", Arg.String (fun x -> ind.val := x), "individual key");
   ("-tr", Arg.Set trace, "trace changed persons");
   ("-ma_no", Arg.Clear ascend, "mark ascendants");
   ("-tst", Arg.Clear execute, "do not perform changes (test only)")]
;
value anonfun i = bname.val := i;
value usage = "Usage: public [-lb #] [-ld #] [-lm #] [-everybody] [-ind key] [-ma] [-tr] [-tst] base.\n";
  
value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    lim_year.val := today.val-lim_b.val;
    let base = Gwdb.open_base bname.val in
    let old = Array.make (nb_of_persons base) 0 in
    if everybody.val then public_everybody old bname.val
    else if ind.val = "" then public_all old bname.val lim_year.val
    else public_some old bname.val lim_year.val ind.val;
    printf "Set %d persons to old\n" cnt.val; flush stdout;
    printf "Changed %d persons\n" changes.val; flush stdout;
  }
;

main ();
