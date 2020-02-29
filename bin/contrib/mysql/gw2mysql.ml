(* Copyright (c) 2020 Ludovic LEDIEU *)

open Mysql
open Geneweb
open Def
open Gwdb

exception Impossible of string

let db = quick_connect ~database:"geneweb" ~user:"gw" ~password:"gw_pw"  ()

let insert debug t v =
  if debug then Printf.printf "%s\n%!" v ;
  ignore (exec db ( "insert into " ^ t ^ " values " ^ v))

let insert_new_text debug t v =
  insert debug t (values [
	"null" ;
	ml2rstr db v ;
  ]) ;
  insert_id db

let event base i_p1 i_p2 t n date place note source reason witnesses =
  let place = sou base place in
  let note = sou base note in
  let source = sou base source in
  if date <> Adef.cdate_None || place <> "" || note <> "" || source <> "" then
    let date = Adef.od_of_cdate date in
    let go_insert d =
	let n_id =
	  if note <> "" then ml642int (insert_new_text false "notes" note)
	  else "null"
	in
	let s_id =
	  if source <> "" then ml642int (insert_new_text false "sources" source)
	  else "null"
	in
        insert false "events" (values [
	  "null" ;
	  ml2rstr db (if t = "Name" then "EVEN" else t) ;
	  ml2rstr db (if t = "Name" then n else "")  ;
	  ml2rstr db (match d with
	  | Some (Dgreg (dmy, _)) ->
		begin match dmy.prec with
		| Sure -> ""
		| About -> "ABT"
		| Maybe -> "Maybe" (* FIXME: mauvais usage dans GeneWeb => comment ajuster ? *)
		| Before -> "BEF"
		| After -> "AFT"
		| OrYear _ -> raise (Impossible "OrYear")
		| YearInt _ -> raise (Impossible "YearInt")
		end
	  | _ -> ""
	  ) ;
	  ml2rstr db (match d with
	  | Some (Dgreg (_, calendar)) -> 
		begin match calendar with
		| Dgregorian -> "Gregorian"
		| Djulian -> "Julian"
		| Dfrench -> "French"
		| Dhebrew -> "Hebrew"
		end
	  | _ -> "Gregorian"
          ) ;
	  ml2int (match d with
	  | Some (Dgreg (dmy, _)) -> dmy.day
	  | _ -> 0
	  ) ;
	  ml2int (match d with
	  | Some (Dgreg (dmy, _)) -> dmy.month
	  | _ -> 0
	  ) ;
	  ml2int (match d with
	  | Some (Dgreg (dmy, _)) -> dmy.year
	  | _ -> 0
	  ) ;
	  ml2rstr db (match d with
	  | Some Dtext s -> s
	  | _ -> ""
          ) ;
	  ml2rstr db reason ;
	  ml2rstr db place ;
	  "null" ;
	  n_id ;
	  s_id ;
        ]);
	let e_id = ml642int (insert_id db) in
	insert false "person_event" (values [
		"null" ;
		e_id ;
	  	ml2int i_p1 ;
		ml2rstr db "Main" ;
	]) ;
	begin match i_p2 with
	| Some i ->
	    insert false "person_event" (values [
		"null" ;
		e_id ;
	  	ml2int i ;
		ml2rstr db "Main" ;
	    ])
	| _ -> ()
	end ;
	Array.iter (fun (iper, role) ->
	  insert false "person_event" (values [
		"null" ;
		e_id ;
		ml2int (Adef.int_of_iper iper) ;
		ml2rstr db (match role with
		| Witness -> "Witness"
		| Witness_GodParent -> "Godparent"
		| Witness_Officer -> "Official"
		) ;
	  ])
        ) witnesses
    in
    match date with
    | Some (Dgreg (dmy, calendar)) ->
	begin match dmy.prec with
	| OrYear dmy2 ->
            begin
		go_insert (Some (Dgreg ({dmy with prec = Sure ; day = dmy2.day2 ; month = dmy2.month2 ; year = dmy2.year2}, calendar)));
		go_insert (Some (Dgreg ({dmy with prec = Sure}, calendar)))
	    end
	| YearInt dmy2 ->
            begin
		go_insert (Some (Dgreg ({dmy with prec = Before ; day = dmy2.day2 ; month = dmy2.month2 ; year = dmy2.year2}, calendar)));
		go_insert (Some (Dgreg ({dmy with prec = After}, calendar)))
	    end
	| _ -> go_insert date
	end
    | _ -> go_insert date

let gw_to_mysql base =
  (* For each persons *)
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let sn = p_surname base p in
    let fn = p_first_name base p in
    let death = get_death p in
    let isDead d =
	match d with
	| NotDead -> "NotDead"
	| Death _ -> "Dead"
	| DeadYoung -> "DeadYoung"
	| DeadDontKnowWhen -> "DeadDontKnowWhen"
	| DontKnowIfDead -> "DontKnowIfDead"
	| OfCourseDead -> "OfCourseDead"
    in
    let n_id =
      let notes = sou base (get_notes p) in
      if notes <> "" then ml642int (insert_new_text false "notes" notes)
      else "null"
    in
    let s_id =
      let psources = sou base (get_psources p) in
      if psources <> "" then ml642int (insert_new_text false "sources" psources)
      else "null"
    in
    insert false "persons" (values [
	ml2int i ;
	ml2int (get_occ p) ;
	ml2rstr db (isDead death) ;
	n_id ;
	s_id ;
	"null" ; (* FIXME get consang *)
	(* FIXME image useless for me *)
	ml2rstr db (match get_sex p with
	| Male -> "M"
	| Female -> "F"
	| Neuter -> ""
	) ;
	ml2rstr db (match get_access p with
	| IfTitles -> "IfTitles"
	| Public -> "Public"
	| Private -> "Private"
	) ;
	(* FIXME titles *)
	(* FIXME rparents *)
	(* FIXME dispatch occupation on events *)
    ]);
    if sn <> "?" || fn <> "?" then
    insert false "names" (values [
	"null" ;
	ml2int i ;
	ml2rstr db fn ;
	ml2rstr db sn ;
	ml2rstr db "True" ;
	(* FIXME public_name *)
	(* FIXME qualifiers *)
	(* FIXME aliases *)
	(* FIXME first_names_aliases *)
	(* FIXME surnames_aliases *)
    ]);
    (* Ces événements sont en doublon dans pevents en v7 / à conserver pour la v6
    event base i "BIRT" "" (get_birth p) (get_birth_place p) (get_birth_note p) (get_birth_src p) "" [];
    event base i "BAPM" "" (get_baptism p) (get_baptism_place p) (get_baptism_note p) (get_baptism_src p) "" [];
    begin match death with
    | Death (r, d) ->
	let reason = match r with
          | Killed -> "Killed"
          | Murdered -> "Murdered"
          | Executed -> "Executed"
          | Disappeared -> "Disappeared"
          | Unspecified -> ""
	in
        event base i "DEAT" "" d (get_death_place p) (get_death_note p) (get_death_src p) reason [];
    | _ -> ()
    end ;
    begin match get_burial p with
    | Buried d ->
        event base i "BURI" "" d (get_burial_place p) (get_burial_note p) (get_burial_src p) "" [];
    | Cremated d ->
        event base i "CREM" "" d (get_burial_place p) (get_burial_note p) (get_burial_src p) "" [];
    | UnknownBurial -> ()
    end ;
    *)
    List.iter (fun evt ->
	let (e_t, e_n) =
	  match evt.epers_name with
	  | Epers_Birth -> "BIRT", ""
	  | Epers_Baptism -> "BAPM", ""
	  | Epers_Death -> "DEAT", ""
	  | Epers_Burial -> "BURI", ""
	  | Epers_Cremation -> "CREM", ""
	  | Epers_Accomplishment -> "EVEN", "Accomplishment"
	  | Epers_Acquisition -> "EVEN", "Acquisition"
	  | Epers_Adhesion -> "EVEN", "Membership"
	  | Epers_BaptismLDS -> "BAPM", "" (* BAPL removed from GEDCOM 5.5.5 *)
	  | Epers_BarMitzvah -> "BARM", ""
	  | Epers_BatMitzvah -> "BASM", ""
	  | Epers_Benediction -> "EVEN", "BLES" (* BLES removed from GEDCOM 5.5.5 *)
	  | Epers_ChangeName -> "EVEN", "Change name"
	  | Epers_Circumcision -> "EVEN", "Circumcision"
	  | Epers_Confirmation -> "CONF", ""
	  | Epers_ConfirmationLDS -> "CONF", "" (* CONL removed from GEDCOM 5.5.5 *)
	  | Epers_Decoration -> "EVEN", "Award"
	  | Epers_DemobilisationMilitaire -> "EVEN", "Military discharge"
	  | Epers_Diploma -> "EVEN", "Degree"
	  | Epers_Distinction -> "EVEN", "Distinction"
	  | Epers_Dotation -> "EVEN", "ENDL" (* ENDL removed from GEDCOM 5.5.5 *)
	  | Epers_DotationLDS -> "EVEN", "DotationLDS"
	  | Epers_Education -> "EDUC", ""
	  | Epers_Election -> "EVEN", "Election"
	  | Epers_Emigration -> "EMIG", ""
	  | Epers_Excommunication -> "EVEN", "Excommunication"
	  | Epers_FamilyLinkLDS -> "EVEN", "Family link LDS"
	  | Epers_FirstCommunion -> "FCOM", ""
	  | Epers_Funeral -> "EVEN", "Funeral"
	  | Epers_Graduate -> "GRAD", ""
	  | Epers_Hospitalisation -> "EVEN", "Hospitalization"
	  | Epers_Illness -> "EVEN", "Illness"
	  | Epers_Immigration -> "IMMI", ""
	  | Epers_ListePassenger -> "EVEN", "Passenger list"
	  | Epers_MilitaryDistinction -> "EVEN", "Military distinction"
	  | Epers_MilitaryPromotion -> "EVEN", "Military promotion"
	  | Epers_MilitaryService -> "EVEN", "Military service"
	  | Epers_MobilisationMilitaire -> "EVEN", "Military mobilization"
	  | Epers_Naturalisation -> "NATU", ""
	  | Epers_Occupation -> "OCCU", ""
	  | Epers_Ordination -> "EVEN", "ORDN" (* ORDN removed from GEDCOM 5.5.5 *)
	  | Epers_Property -> "PROP", ""
	  | Epers_Recensement -> "CENS", ""
	  | Epers_Residence -> "RESI", ""
	  | Epers_Retired -> "RETI", ""
	  | Epers_ScellentChildLDS -> "EVEN", "SLGC" (* SLGC removed from GEDCOM 5.5.5 *)
	  | Epers_ScellentParentLDS -> "EVEN", "Scellent parent LDS"
	  | Epers_ScellentSpouseLDS -> "EVEN", "SLGS" (* SLGS removed from GEDCOM 5.5.5 *)
	  | Epers_VenteBien -> "EVEN", "Property sale"
	  | Epers_Will -> "WILL", ""
	  | Epers_Name s -> "EVEN", (sou base s)
        in
	let reason = (* do not use evt.epers_reason (empty !) *)
	  if e_t = "DEAT" then
	    match death with
	    | Death (r, _) -> begin match r with
	        | Killed -> "Killed"
	        | Murdered -> "Murdered"
	        | Executed -> "Executed"
	        | Disappeared -> "Disappeared"
	        | Unspecified -> ""
	        end
	    | _ -> ""
	  else ""
	in
	event base i None e_t e_n evt.epers_date evt.epers_place evt.epers_note evt.epers_src reason evt.epers_witnesses
    ) (get_pevents p)
  done ;
  let create_group n_id s_id o =
    insert false "groups" (values [
	"null" ;
	n_id ;
	s_id ;
	ml2rstr db o ;
    ]) ;
    ml642int (insert_id db)
  in
  let populate_group g p_i role =
    ignore (insert false "person_group" (values [
	"null" ;
	g ;
	ml2int p_i ;
        ml2rstr db role ;
    ]))
  in
  (* For each families *)
  for i = 0 to nb_of_families base - 1 do
    let fam = foi base (Adef.ifam_of_int i) in
    if is_deleted_family fam then ()
    else begin
      let n_id =
        let note = sou base (get_comment fam) in
        if note <> "" then ml642int (insert_new_text false "notes" note)
        else "null"
      in
      let s_id =
        let source = sou base (get_fsources fam) in
        if source <> "" then ml642int (insert_new_text false "sources" source)
        else "null"
      in
      let g = create_group n_id s_id (sou base (get_origin_file fam)) in
      let ip_father = Adef.int_of_iper (get_father fam) in
      let ip_mother = Adef.int_of_iper (get_mother fam) in
      populate_group g ip_mother "Father" ;
      populate_group g ip_mother "Mother" ;
      Array.iter
        (fun c -> populate_group g (Adef.int_of_iper c) "Child")
        (get_children fam) ;
      List.iter (fun evt ->
	let (e_t, e_n) =
	  match evt.efam_name with
          | Efam_Marriage -> "MARR", ""
          | Efam_NoMarriage -> "EVEN", "unmarried" (* FIXME not an event ! *)
          | Efam_NoMention -> "EVEN", "nomen" (* FIXME not an event ! *)
          | Efam_Engage -> "ENGA", ""
          | Efam_Divorce -> "DIV", ""
          | Efam_Separated -> "EVEN", "SEP"
          | Efam_Annulation -> "ANUL", ""
          | Efam_MarriageBann -> "MARB", ""
          | Efam_MarriageContract -> "MARC", ""
          | Efam_MarriageLicense -> "MARL", ""
          | Efam_PACS -> "EVEN", "pacs"
          | Efam_Residence -> "RESI", ""
          | Efam_Name n -> "EVEN", (sou base n)
        in
	event base ip_father (Some ip_mother) e_t e_n evt.efam_date evt.efam_place evt.efam_note evt.efam_src "" evt.efam_witnesses
      ) (get_fevents fam)
    end
  done
  (* FIXME process linked_notes *)
  (* FIXME process history *)

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
    let () = load_ascends_array base in
    let () = load_couples_array base in
    let () = load_descends_array base in begin
	set_charset db "utf8" ;
	gw_to_mysql base ;
	disconnect db
    end

let _ = Printexc.catch main ()
