(* Copyright (c) 2001 Ludovic LEDIEU *)

open Geneweb
open Def
open Gwdb

let param =
  [
    [| "vAD02"    ; "1" ; "1500" ; "1907" ; ", Aisne, France" |] ;
    [| "vAD03"    ; "1" ; "1600" ; "1902" ; ", Allier, France" |] ;
    [| "vAD12"    ; "1" ; "1600" ; "1902" ; ", Aveyron, France" |] ;
    [| "vAD34"    ; "1" ; "1500" ; "1907" ; ", Hérault, France" |] ;
    [| "vAM44"    ; "1" ; "1793" ; "1902" ; "Nantes, Loire-Atlantique, France" |] ;
    [| "vAD44"    ; "1" ; "1500" ; "1880" ; ", Loire-Atlantique, France" |] ;
    [| "vAD48"    ; "1" ; "1500" ; "1902" ; ", Lozère, France" |] ;
    [| "vAD54"    ; "1" ; "1500" ; "1902" ; ", Meurthe-et-Moselle, France" |] ;
    [| "vAD55"    ; "1" ; "1500" ; "1902" ; ", Meuse, France" |] ;
    [| "vAC59465" ; "2" ; "1740" ; "1935" ; "Pommereuil, Nord, France" |] ;
    [| "vAD59"    ; "1" ; "1500" ; "1900" ; ", Nord, France" |] ;
    [| "vAD60"    ; "1" ; "1500" ; "1907" ; ", Oise, France" |] ;
    [| "vAM62119" ; "2" ; "1737" ; "1908" ; "Béthune, Pas-de-Calais, France" |] ;
    [| "vAD62"    ; "1" ; "1500" ; "1900" ; ", Pas-de-Calais, France" |] ;
    [| "vAD75"    ; "1" ; "1860" ; "1902" ; "Paris, France" |] ;
    [| "vAD77"    ; "1" ; "1500" ; "1902" ; ", Seine-et-Marne, France" |] ;
    [| "vAD80"    ; "1" ; "1500" ; "1902" ; ", Somme, France" |] ;
    [| "vAD95"    ; "1" ; "1500" ; "1902" ; ", Val-d'Oise, France" |] ;
    [| "vAEB"     ; "1" ; "1500" ; "1800" ; ", Belgique" |]
  ]

let cnt02 = ref 0
let cnt03 = ref 0
let cnt12 = ref 0
let cnt34 = ref 0
let cnt44 = ref 0
let cnt44000 = ref 0
let cnt48 = ref 0
let cnt54 = ref 0
let cnt55 = ref 0
let cnt59 = ref 0
let cnt59465 = ref 0
let cnt60 = ref 0
let cnt62 = ref 0
let cnt62119 = ref 0
let cnt75 = ref 0
let cnt77 = ref 0
let cnt80 = ref 0
let cnt95 = ref 0
let cntBE = ref 0

let t_cnt02 = ref 0
let t_cnt03 = ref 0
let t_cnt12 = ref 0
let t_cnt34 = ref 0
let t_cnt44 = ref 0
let t_cnt44000 = ref 0
let t_cnt48 = ref 0
let t_cnt54 = ref 0
let t_cnt55 = ref 0
let t_cnt59 = ref 0
let t_cnt59465 = ref 0
let t_cnt60 = ref 0
let t_cnt62 = ref 0
let t_cnt62119 = ref 0
let t_cnt75 = ref 0
let t_cnt77 = ref 0
let t_cnt80 = ref 0
let t_cnt95 = ref 0
let t_cntBE = ref 0

let string_incl x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then true
        else if
          j < String.length y &&
          String.unsafe_get x i = String.unsafe_get y j then
          loop1 (i + 1) (j + 1)
        else loop (j_ini + 1)
      in
      loop1 0 j_ini
  in
  loop 0

let check_event_src1 str_p str_s deb fin place src year =
  if (year = 0 || (year >= deb && year <= fin)) &&
      string_incl str_p place && not (string_incl str_s src) then
    let _ =
      match str_s with
        "vAD02" -> t_cnt02 := !t_cnt02 + 1
      | "vAD03" -> t_cnt03 := !t_cnt03 + 1
      | "vAD12" -> t_cnt12 := !t_cnt12 + 1
      | "vAD34" -> t_cnt34 := !t_cnt34 + 1
      | "vAD44" -> t_cnt44 := !t_cnt44 + 1
      | "vAM44" -> t_cnt44000 := !t_cnt44000 + 1
      | "vAD48" -> t_cnt48 := !t_cnt48 + 1
      | "vAD54" -> t_cnt54 := !t_cnt54 + 1
      | "vAD55" -> t_cnt55 := !t_cnt55 + 1
      | "vAD59" -> t_cnt59 := !t_cnt59 + 1
      | "vAC59465" -> t_cnt59465 := !t_cnt59465 + 1
      | "vAD60" -> t_cnt60 := !t_cnt60 + 1
      | "vAD62" -> t_cnt62 := !t_cnt62 + 1
      | "vAM62119" -> t_cnt62119 := !t_cnt62119 + 1
      | "vAD75" -> t_cnt75 := !t_cnt75 + 1
      | "vAD77" -> t_cnt77 := !t_cnt77 + 1
      | "vAD80" -> t_cnt80 := !t_cnt80 + 1
      | "vAD95" -> t_cnt95 := !t_cnt95 + 1
      | "vAEB" -> t_cntBE := !t_cntBE + 1
      | _ -> ()
    in false
  else if string_incl str_s src then
    let _ =
      match str_s with
        "vAD02" -> cnt02 := !cnt02 + 1
      | "vAD03" -> cnt03 := !cnt03 + 1
      | "vAD12" -> cnt12 := !cnt12 + 1
      | "vAD34" -> cnt34 := !cnt34 + 1
      | "vAD44" -> cnt44 := !cnt44 + 1
      | "vAM44" -> cnt44000 := !cnt44000 + 1
      | "vAD48" -> cnt48 := !cnt48 + 1
      | "vAD54" -> cnt54 := !cnt54 + 1
      | "vAD55" -> cnt55 := !cnt55 + 1
      | "vAD59" -> cnt59 := !cnt59 + 1
      | "vAC59465" -> cnt59465 := !cnt59465 + 1
      | "vAD60" -> cnt60 := !cnt60 + 1
      | "vAD62" -> cnt62 := !cnt62 + 1
      | "vAM62119" -> cnt62119 := !cnt62119 + 1
      | "vAD75" -> cnt75 := !cnt75 + 1
      | "vAD77" -> cnt77 := !cnt77 + 1
      | "vAD80" -> cnt80 := !cnt80 + 1
      | "vAD95" -> cnt95 := !cnt95 + 1
      | "vAEB" -> cntBE := !cntBE + 1
      | _ -> ()
    in false
  else true

let check_event_src2 str_p str_s deb fin place src year =
  if 0 = String.compare str_p place then
    (* Exact match for place *)
    check_event_src1 str_p str_s deb fin place src year
  else
    let str_p = ", " ^ str_p in
    check_event_src1 str_p str_s deb fin place src year

let check_event_src place src year ptest param =
  if ptest then
    match param with
      [| str_s ; t ; deb ; fin ; str_p |] ->
         let deb = int_of_string deb in
         let fin = int_of_string fin in
         let check_type t =
           match t with
             "1" -> check_event_src1 str_p str_s deb fin place src year
           | "2" -> check_event_src2 str_p str_s deb fin place src year
           | _ -> true
         in
         check_type t
    | _ -> true
  else false

let check_event_src_all place src year =
  let poursuivre = true in
  let _ = List.fold_left (check_event_src place src year) poursuivre param in
  ()

let check_baptism base p =
  let bp = sou base (get_birth_place p) in
  let bbp = sou base (get_baptism_place p) in
  match bp, bbp with
    "", ""
  | _, "" -> ()
  | "", _ ->
    Printf.printf "Warning %s.%d %s (baptism)\n" (sou base (get_first_name p)) (get_occ p)
      (sou base (get_surname p))
  | _, _ -> ()

let check_src base =
  (* pour chaque personne *)
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let b_year =
      match Adef.od_of_cdate (get_birth p) with
        Some (Dgreg (d, _)) -> d.year
      | _ -> 0
    in
    let d_year =
      match CheckItem.date_of_death (get_death p) with
        Some (Dgreg (d, _)) -> d.year
      | _ -> 0
    in
    check_event_src_all (sou base (get_birth_place p))
      (sou base (get_birth_src p)) b_year;
    check_event_src_all (sou base (get_death_place p))
       (sou base (get_death_src p)) d_year;
    check_baptism base p
  done;
  (* pour chaque famille *)
  for i = 0 to nb_of_families base - 1 do
    let f = foi base (Adef.ifam_of_int i) in
    if is_deleted_family f then ()
    else begin
      let m_year =
        match (Adef.od_of_cdate (get_marriage f)) with
          Some (Dgreg (d, _)) -> d.year
        | _ -> 0
      in
      check_event_src_all (sou base (get_marriage_place f))
        (sou base (get_marriage_src f)) m_year
    end
  done;
  Printf.eprintf "Nombre d'acte aux AD02 : %d / %d\n" !cnt02 !t_cnt02;
  Printf.eprintf "Nombre d'acte aux AD03 : %d / %d\n" !cnt03 !t_cnt03;
  Printf.eprintf "Nombre d'acte aux AD12 : %d / %d\n" !cnt12 !t_cnt12;
  Printf.eprintf "Nombre d'acte aux AD34 : %d / %d\n" !cnt34 !t_cnt34;
  Printf.eprintf "Nombre d'acte aux AD44 : %d / %d\n" !cnt44 !t_cnt44;
  Printf.eprintf "Nombre d'acte aux AM44 : %d / %d\n" !cnt44000 !t_cnt44000;
  Printf.eprintf "Nombre d'acte aux AD48 : %d / %d\n" !cnt48 !t_cnt48;
  Printf.eprintf "Nombre d'acte aux AD54 : %d / %d\n" !cnt54 !t_cnt54;
  Printf.eprintf "Nombre d'acte aux AD55 : %d / %d\n" !cnt55 !t_cnt55;
  Printf.eprintf "Nombre d'acte aux AD59 : %d / %d\n" !cnt59 !t_cnt59;
  Printf.eprintf "Nombre d'acte aux AC59465 (Pommereuil) : %d / %d\n" !cnt59465 !t_cnt59465;
  Printf.eprintf "Nombre d'acte aux AD60 : %d / %d\n" !cnt60 !t_cnt60;
  Printf.eprintf "Nombre d'acte aux AD62 : %d / %d\n" !cnt62 !t_cnt62;
  Printf.eprintf "Nombre d'acte aux AM62119 (Béthune) : %d / %d\n" !cnt62119 !t_cnt62119;
  Printf.eprintf "Nombre d'acte aux AD75 : %d / %d\n" !cnt75 !t_cnt75;
  Printf.eprintf "Nombre d'acte aux AD77 : %d / %d\n" !cnt77 !t_cnt77;
  Printf.eprintf "Nombre d'acte aux AD80 : %d / %d\n" !cnt80 !t_cnt80;
  Printf.eprintf "Nombre d'acte aux AD95 : %d / %d\n" !cnt95 !t_cnt95;
  Printf.eprintf "Nombre d'acte aux AE Belgique : %d / %d\n" !cntBE !t_cntBE

(* main *)

let fname = ref ""

let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <database>"

let speclist =
  [("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock data base")]

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
  else
    let base = open_base !fname in
    let () = load_strings_array base in
    check_src base

let _ = Printexc.catch main ()
