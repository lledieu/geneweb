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
value trace_old = ref False;
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
value testhg = ref False;
value testhg1 = ref False;
value set_friends = ref False;
value cnt = ref 0;
value rgpd_files = ref ".";

value nb_ift = ref 0;
value nb_pub = ref 0;
value nb_ami = ref 0;
value nb_amm = ref 0;
value nb_prv = ref 0;
value nb_oth = ref 0;
value nbf_ift = ref 0;
value nbf_pub = ref 0;
value nbf_ami = ref 0;
value nbf_amm = ref 0;
value nbf_prv = ref 0;
value nbf_oth = ref 0;


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

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value tr c1 c2 s =
  match rindex s c1 with
  [ Some _ ->
      String.init (String.length s) convert_char
        where convert_char i =
          if s.[i] = c1 then c2 else s.[i]
  | None -> s ]
;

value get_b_dates base p =
  let (reason, d, d2) =
    match
      (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p))
    with
    [ (_, Some (Dgreg d _), _, _) -> ("baptized in", d.year, d.year+lim_b.val)
    | (Some (Dgreg d _), _, _, _) -> ("born in", d.year, d.year+lim_b.val)
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
    | (_, _, (Death reasn d1), Some (Dgreg d _)) -> ("dead in", d.year, (d.year+lim_d.val))
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
      if trace_old.val then do {
        printf "Old (birth): %s, %s %d\n" (Gutil.designation base p) reason y;
        flush stdout
      } else ();
    };
    let (reason, y, old_p) = get_d_dates base p in
    if old_p > today.val || y = 0 then 
      old.(Adef.int_of_iper (get_key_index p)) := 1
    else do {
      (* death date + lim_d > today *)
      old.(Adef.int_of_iper (get_key_index p)) := y;
      if trace_old.val then do {
        printf "Old (death): %s, %s %d\n" (Gutil.designation base p) reason y;
        flush stdout
      } else ();
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
              if trace_old.val then do {
                printf "Old (marriage): %s, in %d\n" (Gutil.designation base p) (d-lim_m.val);
                printf "Old (marriage): %s, in %d\n" (Gutil.designation base sp) (d-lim_m.val);
                flush stdout;
              } else ();
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
      (* dont change access if Private *)
      if ((get_access p) <> Public && (get_access p) <> Private) then do {
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
            if old.(Adef.int_of_iper (get_father cpl)) > 1 then
              mark_ancestors base scanned old (poi base (get_father cpl)) 
            else ();
            if old.(Adef.int_of_iper (get_mother cpl)) > 1 then
              mark_ancestors base scanned old (poi base (get_mother cpl))
            else ();
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

value test_public old bname =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let (reason, bd, bd2) = 
        match
          (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
          get_death p, CheckItem.date_of_death (get_death p))
        with
        [ (Some (Dgreg d _), _, NotDead, _) -> ("born in", d.year, d.year+lim_b.val)
        | (_, Some (Dgreg d _),  NotDead, _) -> ("baptized in", d.year, d.year+lim_b.val)
        | (Some (Dgreg d _), _, DontKnowIfDead, _) -> ("born in", d.year, d.year+lim_b.val)
        | (_, Some (Dgreg d _),  DontKnowIfDead, _) -> ("baptized in", d.year, d.year+lim_b.val)
        | (_, _, _, _) -> ("other", 0, 0) ]
      in
      if bd2 > today.val && (get_access p) = Public then do {
        incr cnt;
        printf "Public: %s, %s: %d (%d)\n" (Gutil.designation base p) reason bd bd2;
        let gp = {(gen_person_of_person p) with access = IfTitles} in
        if execute.val then do { 
          patch_person base gp.key_index gp;
          incr changes;
        }
        else ();
      }
      else ();
    };
    if cnt.val > 0 then
      printf "Nb of persone: %d\n" cnt.val
    else ();
    if changes.val > 0 then do {
      commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
  }
;

value test_dead_child old bname =
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
      let p = poi base (Adef.iper_of_int i) in
      let (pdreason, dd, dd2) = get_d_dates base p in
      (* dd2 <> 0 -> dead *)
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            let fa = poi base (get_father cpl) in
            let (fbreason, fbd, fbd2) = get_b_dates base fa in
            let (fdreason, fdd, fdd2) = get_d_dates base fa in
            let f_not_old = not (fbd2 < today.val || (fdd2 <> 0 && fdd2 < today.val) ) in
            if dd2 <> 0 && f_not_old && (get_access fa = Public) then do {
              incr cnt;
              printf "Father of: %s, %s: %d; born: %d, dead: %d\n" (Gutil.designation base p)
                pdreason dd fbd fdd;
              let gp = {(gen_person_of_person fa) with access = IfTitles} in
              if execute.val then patch_person base gp.key_index gp else ();
              incr changes;
            }
            else ();
            let mo = poi base (get_mother cpl) in
            let (mbreason, mbd, mbd2) = get_b_dates base mo in
            let (mdreason, mdd, mdd2) = get_d_dates base mo in
            let mdd2 = if mdd2 = 0 then today.val+1 else mdd2 in
            let m_not_old = not (mbd2 < today.val || mdd2 < today.val ) in
            if dd2 <> 0 && m_not_old && (get_access mo = Public) then do {
              incr cnt;
              printf "Mother of: %s, %s: %d; born: %d, dead: %d\n" (Gutil.designation base p)
                pdreason dd mbd mdd;
              let gp = {(gen_person_of_person mo) with access = IfTitles} in
              if execute.val then patch_person base gp.key_index gp else ();
              incr changes;
            }
            else ();
          }
      | None -> () ]
    };
    if changes.val > 0 then do {
      commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
   if cnt.val > 0 then
      printf "Nb of persone: %d\n" cnt.val
    else ();
  }
;

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

value set_friend base p =
  let old_access = get_access p in
  let old_as = if old_access = IfTitles then "IfTitles"
    else if old_access = Public then "Public"
    else if old_access = Friend then "Friend"
    else if old_access = Friend_m then "Friend_m"
    else if old_access = Private then "Private"
    else "Other"
  in
  let fns = tr ' ' '_' (sou base (get_first_name p)) in
  let sns = tr ' ' '_' (sou base (get_surname p)) in
  let ocs = string_of_int (get_occ p) in
  let new_access =
    let d_sep = Filename.dir_sep in
    let rgpd_file = 
       rgpd_files.val ^ d_sep ^ fns ^ "." ^ ocs ^ "." ^ sns
    in
      (* if one of the files exist, set the Friend or Friend_m value *)
    if Sys.file_exists (rgpd_file ^ "-et-mineurs.pdf") then Friend_m
    else if Sys.file_exists (rgpd_file ^ ".pdf") then Friend
      (* if none of the file exist and person was Friend, then it becomes Private *)
    else if old_access = Friend || old_access = Friend_m then Private
      (* otherwise keep thee current value *)
    else old_access
  in do {
    if old_access = IfTitles then incr nb_ift
    else if old_access = Public then incr nb_pub
    else if old_access = Friend then incr nb_ami
    else if old_access = Friend_m then incr nb_amm
    else if old_access = Private then incr nb_prv
    else incr nb_oth;
    if new_access = Friend || new_access = Friend_m then do {
      if old_access = IfTitles then incr nbf_ift
      else if old_access = Public then incr nbf_pub
      else if old_access = Friend then incr nbf_ami
      else if old_access = Friend_m then incr nbf_amm
      else if old_access = Private then incr nbf_prv
      else incr nbf_oth;
      printf "Friend: %s.%s.%s, %s\n" fns ocs sns old_as; flush stdout;
    }
    else ();
    let gp = {(gen_person_of_person p) with access = new_access} in
    if execute.val && new_access <> old_access then do { 
      patch_person base gp.key_index gp;
      incr changes;
    }
    else ();
  }
;

value set_friend_all bname =
  let _ =printf "Set_friend_all: %s\n" bname in
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      set_friend base (poi base (Adef.iper_of_int i))
    };
    if changes.val > 0 then do {
      commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
    printf "Totals: IfTitle %d, Public %d, Friend %d, Friend_m %d, Private %d, Other %d\n"
      nb_ift.val nb_pub.val nb_ami.val nb_amm.val nb_prv.val nb_oth.val; flush stdout;
    printf "Totals Friends: IfTitle %d, Public %d, Friend %d, Friend_m %d, Private %d, Other %d\n"
      nbf_ift.val nbf_pub.val nbf_ami.val nbf_amm.val nbf_prv.val nbf_oth.val; flush stdout;
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
   ("-testhg", Arg.Set testhg, "test for dead child and still young");
   ("-testhg1", Arg.Set testhg1, "test for born < 120 and public");
   ("-set_fr", Arg.Set set_friends, "set friends");
   ("-ind", Arg.String (fun x -> ind.val := x), "individual key");
   ("-tr", Arg.Set trace, "trace changed persons");
   ("-tro", Arg.Set trace_old, "trace set to old");
   ("-ma_no", Arg.Clear ascend, "do not mark ascendants");
   ("-rgpd", Arg.String (fun x -> rgpd_files.val := x), "Set RGPD folder");
   ("-tst", Arg.Clear execute, "do not perform changes (test only)")
   ]
;
value anonfun i = bname.val := i;
value usage = "Usage: public [-lb #] [-ld #] [-lm #] [-everybody] [-ind key] [-ma] [-tr] [-tst] base.\n";
  
value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    printf "Executing public today (%d) on %s with -lb %d -ld %d -lm %d\n\n" 
      today.val bname.val lim_b.val lim_d.val lim_m.val;
    flush stdout;
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    lim_year.val := today.val-lim_b.val;
    let base = Gwdb.open_base bname.val in
    let old = Array.make (nb_of_persons base) 0 in
    if testhg.val then test_dead_child old bname.val
    else if testhg1.val then test_public old bname.val
    else if set_friends.val then set_friend_all bname.val
    else if everybody.val then public_everybody old bname.val
    else if ind.val = "" then public_all old bname.val lim_year.val
    else public_some old bname.val lim_year.val ind.val;
    if trace_old.val then do {
      printf "Set %d persons to old\n" cnt.val; flush stdout;
    } else ();
    printf "Changed %d persons\n" changes.val; flush stdout;
  }
;

main ();
