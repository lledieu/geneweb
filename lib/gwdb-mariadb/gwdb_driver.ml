open Def

(* Traces *)
let failwithstack msg n =
  failwith @@ msg ^ "\n" ^ Printexc.(raw_backtrace_to_string @@ get_callstack n)

let failexit () = failwithstack "" 5

let counters = Hashtbl.create 100

let trace_in f =
  match Hashtbl.find_opt counters f with
  | Some (v, c, a, t) ->
      Hashtbl.remove counters f ;
      Hashtbl.add counters f (v+1, c, a, t)
  | None -> Hashtbl.add counters f (1, 0, 0, None)

let trace_in_start f =
  trace_in f ;
  Unix.gettimeofday ()

let trace_out f start =
  match Hashtbl.find_opt counters f with
  | Some (v, c, a, t) -> begin
      let new_t = Unix.gettimeofday () -. start in
      let new_t = match t with
        | Some t -> t +. new_t
        | None -> new_t
      in
      Hashtbl.remove counters f ;
      Hashtbl.add counters f (v, c, a, Some new_t)
    end
  | None -> failexit ()

let trace_in_cache f =
  match Hashtbl.find_opt counters f with
  | Some (v, c, a, t) ->
      Hashtbl.remove counters f ;
      Hashtbl.add counters f (v, c+1, a, t)
  | None -> Hashtbl.add counters f (0, 1, 0, None)

let trace_in_array f =
  match Hashtbl.find_opt counters f with
  | Some (v, c, a, t) ->
      Hashtbl.remove counters f ;
      Hashtbl.add counters f (v, c, a+1, t)
  | None -> Hashtbl.add counters f (0, 0, 1, None)

let trace_in_dummy f = trace_in ("(dummy)" ^ f)

let trace_in_close start =
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) counters [] in
  let l = List.sort (fun (key1, _) (key2, _) -> String.compare key1 key2) l in
  Printf.eprintf "= gwdb_driver ==============================nbr=====time===cache===array=\n" ;
  List.iter (fun (key, (v, c, a, t)) ->
    match t with
    | Some t when v <> 0 -> Printf.eprintf "%40s%8d %f%8d%8d\n" key v (t /. (Float.of_int v)) c a
    | _ -> Printf.eprintf "%40s%8d         %8d%8d\n" key v c a
  ) l ;
  Printf.eprintf "=> close_base(%d) %f\n%!" (Unix.getpid ()) (Unix.gettimeofday () -. start)

let todo f =
  trace_in f ;
  failwith f

(* Database tools *)
module M = Mariadb.Blocking

type iper = int
type ifam = int

type istr =
  | DbNote of int
  | DbPlace of int
  | DbSource of int
  | DbOrigin of int
  | DbGivn of int
  | DbNick of int
  | DbSurn of int
  | DbSurnP of int (* with particle at the end *)
  | DbString of string
  | Empty
  | Quest

let empty_string = Empty
let quest_string = Quest

type person
type family

type base =
 { cnx : M.t
 ; bname : string
 ; start : float
 ; queries : (string, M.Stmt.t) Hashtbl.t
 ; mutable iper_nbr : int option
 ; mutable iper_len : int option
 ; mutable iper_array : iper array option
 ; mutable ifam_len : int option
 ; mutable ifam_array : ifam array option
 ; mutable cache_strings : (string * int, string) Hashtbl.t option
 ; mutable cache_persons : (iper, (iper, iper, istr) Def.gen_person) Hashtbl.t option
 ; mutable cache_ascends : (iper, ifam Def.gen_ascend) Hashtbl.t option
 ; mutable cache_unions : (iper, ifam Def.gen_union) Hashtbl.t option
 ; mutable cache_families : (ifam, (iper, ifam, istr) Def.gen_family) Hashtbl.t option
 ; mutable cache_couples : (ifam, iper Def.gen_couple) Hashtbl.t option
 ; mutable cache_descends : (ifam, iper Def.gen_descend) Hashtbl.t option
 ; mutable cache_particles : string list option
 ; mutable array_ascends : ifam Def.gen_ascend array option
 ; mutable array_couples : iper Def.gen_couple array option
 }

type string_person_index =
 { db : base
 ; field_id : string
 ; query_one : string
 ; query_all : string
 ; query_start : string
 ; mutable cursor : M.Res.t option
 }

let stream res =
  let module F = struct exception E of M.error end
  in
  let next _ =
    match M.Res.fetch (module M.Row.Array) res with
    | Ok (Some _ as row) -> row
    | Ok None -> None
    | Error e -> raise (F.E e)
  in
  try Ok (Stream.from next)
  with F.E e -> Error e

let or_die prefix = function
  | Ok x -> x
  | Error (num, msg) -> failwithstack (Printf.sprintf "%s\n(%d) %s" prefix num msg) 4

let get_statement db query =
  let f = "(internal)get_statement" in
  match Hashtbl.find_opt db.queries query with
  | Some s -> trace_in_cache f ; s
  | None ->
      let start = trace_in_start f in
      let s = M.prepare db.cnx query |> or_die "prepare" in
      Hashtbl.add db.queries query s ;
      trace_out f start ;
      s

let query_one_row db query params parse_row =
  let s = get_statement db query in
  let e = M.Stmt.execute s params |> or_die "execute" in
  let r = M.Res.fetch (module M.Row.Array) e |> or_die "fetch" in
  parse_row r

let query_with_init_and_fetch db query params init_res parse_res =
  let s = get_statement db query in
  let e = M.Stmt.execute s params |> or_die "execute" in
  let l = M.Res.num_rows e in
  init_res l ;
  let r = stream e |> or_die "stream" in
  Stream.iter parse_res r ;
  l

let no_init _ = ()

let query_with_fetch db query params parse_res =
  ignore @@ query_with_init_and_fetch db query params no_init parse_res

let insert_cache db name v =
  let query =
    "insert into caches (name, object) \
     values (?, ?) \
     on duplicate key update object = value(object)"
  in
  let params =
    [| `String name
     ; `Bytes (Marshal.to_bytes v [Marshal.No_sharing])
    |]
  in
  let s = M.prepare db.cnx query |> or_die "prepare" in
  ignore (M.Stmt.execute s params |> or_die "execute") ;
  M.Stmt.close s |> or_die "close"

let get_cache_opt db name =
  let parse_row row =
    match row with
    | Some [| f |] -> Some (M.Field.bytes f)
    | None -> None
    | _ -> failexit ()
  in
  let query =
    "select SQL_NO_CACHE object \
     from caches \
     where name = ?"
  in
  query_one_row db query [| `String name |] parse_row

let get_int = M.Field.int

let get_int_opt = M.Field.int_opt

let get_string = M.Field.string

let get_string_opt = M.Field.string_opt

let get_prec field get_dmy2 =
  match get_string field with
  | "" -> Sure
  | "ABT" -> About
  | "Maybe" -> Maybe
  | "BEF" -> Before
  | "AFT" -> After
  | "FROM" -> Sure
  | "TO" -> Sure
  | "FROM-TO" -> Sure
  | "OrYear" -> OrYear (get_dmy2 ())
  | "YearInt" -> YearInt (get_dmy2 ())
  | _ -> failexit ()

let get_cal field =
  match get_string field with
  | "Gregorian" -> Dgregorian
  | "Julian" -> Djulian
  | "French" -> Dfrench
  | "Hebrew" -> Dhebrew
  | _ -> failexit ()

let get_place_opt field =
  match get_int_opt field with
  | None -> empty_string
  | Some i -> DbPlace i

let get_note_opt field =
  match get_int_opt field with
  | None -> empty_string
  | Some i -> DbNote i

let get_source_opt field =
  match get_int_opt field with
  | None -> empty_string
  | Some i -> DbSource i

let get_origin_opt field =
  match get_int_opt field with
  | None -> empty_string
  | Some i -> DbOrigin i

let get_sex field =
  match get_string field with
  | "M" -> Male
  | "F" -> Female
  | "" -> Neuter
  | _ -> failexit ()

let get_access field =
  match get_string field with
  | "IfTitles" -> IfTitles
  | "Public" -> Public
  | "Private" -> Private
  | _ -> failexit ()

(* Implementation *)
    
let string_of_iper = string_of_int
let string_of_ifam = string_of_int

let string_of_istr _ = todo "string_of_istr"

let iper_of_string = int_of_string
let ifam_of_string = int_of_string

let istr_of_string _ = todo "istr_of_string"

let open_base bname =
  Printf.eprintf "=> open_base(%d)\n%!" (Unix.getpid ()) ;
  let db =
    M.connect
      ~host:"localhost"
      ~db:"geneweb"
      ~user:"gw"
      ~pass:"gw_pw" () |> or_die "connect"
  in
  ignore @@ M.set_character_set db "utf8" ;
  { cnx = db
  ; bname = Filename.(remove_extension @@ basename bname)
  ; start = Unix.gettimeofday ()
  ; queries = Hashtbl.create 50
  ; iper_nbr = None
  ; iper_len = None
  ; iper_array = None
  ; ifam_len = None
  ; ifam_array = None
  ; cache_strings = None
  ; cache_persons = None
  ; cache_ascends = None
  ; cache_unions = None
  ; cache_families = None
  ; cache_couples = None
  ; cache_descends = None
  ; cache_particles = None
  ; array_ascends = None
  ; array_couples = None
  }

let close_base db = trace_in_close db.start ;
  Hashtbl.iter (fun query s -> M.Stmt.close s |> or_die ("close\n" ^ query) ) db.queries ;
  M.close db.cnx

let dummy_iper = -1
let dummy_ifam = -1

let eq_istr istr1 istr2 = trace_in "eq_istr" ;
  match istr1, istr2 with
  | Empty, Empty -> true
  | Quest, Quest -> true
  | DbString s1, DbString s2 -> s1 = s2
  | DbGivn nag_id1, DbGivn nag_id2 -> nag_id1 = nag_id2
  | DbNick nan_id1, DbNick nan_id2 -> nan_id1 = nan_id2
  | DbSurn nas_id1, DbSurn nas_id2 -> nas_id1 = nas_id2
  | DbSurnP nas_id1, DbSurnP nas_id2 -> nas_id1 = nas_id2
  | DbSurn nas_id1, DbSurnP nas_id2 -> nas_id1 = nas_id2 (* strange but needed ! *)
(*  | DbSurnP nas_id1, DbSurn nas_id2 -> nas_id1 = nas_id2 => not needed so far *)
  | DbPlace pl_id1, DbPlace pl_id2 -> pl_id1 = pl_id2
  | DbNote n_id1, DbNote n_id2 -> n_id1 = n_id2
  | DbSource s_id1, DbSource s_id2 -> s_id1 = s_id2
  | DbOrigin o_id1, DbOrigin o_id2 -> o_id1 = o_id2
  | _ -> false

let is_empty_string istr = trace_in "is_empty_string" ;
  istr = empty_string

let is_quest_string istr = trace_in "is_quest_string" ;
  istr = quest_string

let sou2 db field table fid id =
  let cache =
    match db.cache_strings with
    | Some h -> h
    | None ->
        let h = Hashtbl.create 100 in
        db.cache_strings <- Some h ;
        h
  in
  let f = "sou->" ^ field in
  match Hashtbl.find_opt cache (field, id) with
  | Some s -> trace_in_cache f ; s
  | None ->
      let start = trace_in_start f in
      let parse_row row =
        match row with
        | Some [| f1 |] -> begin match M.Field.value f1 with
            | `String s -> s
            | `Bytes b -> Bytes.to_string b
            | _ -> failexit ()
            end
        | _ -> failexit ()
      in
      let query = "select " ^ field ^ " from " ^ table ^ " where " ^ fid ^ " = ?" in
      let text = query_one_row db query [| `Int id |] parse_row in
      Hashtbl.add cache (field, id) text ;
      trace_out f start ;
      text

let sou db =
  function
  | Empty -> trace_in "sou->empty" ; ""
  | Quest -> trace_in "sou->quest" ; "?"
  | DbString s -> trace_in "sou->string" ; s
  | DbGivn nag_id -> sou2 db "givn" "names_givn" "nag_id" nag_id
  | DbNick nan_id -> sou2 db "nick" "names_nick" "nan_id" nan_id
  | DbSurn nas_id -> sou2 db "surn" "names_surn" "nas_id" nas_id
  | DbSurnP nas_id -> sou2 db "surn_p" "names_surn" "nas_id" nas_id
  | DbPlace pl_id -> sou2 db "place" "places" "pl_id" pl_id
  | DbNote n_id -> sou2 db "note" "notes" "n_id" n_id
  | DbSource s_id -> sou2 db "source" "sources" "s_id" s_id
  | DbOrigin o_id -> sou2 db "origin" "origins" "o_id" o_id

let bname db = trace_in "bname" ;
  db.bname

let nb_of_persons db =
  let f = "nb_of_persons" in
  match db.iper_len with
  | Some len -> trace_in_cache f ; len
  | None ->
      let start = trace_in_start f in
      let parse_row row =
        match row with
        | Some [| f |] -> 1 + get_int f
        | _ -> failexit ()
      in
      let len =
        query_one_row db
          "select max(p_id) from persons"
          [| |]
          parse_row
      in
      db.iper_len <- Some len ;
      trace_out f start ;
      len

let nb_of_real_persons db =
  let f = "nb_of_real_persons" in
  match db.iper_nbr with
  | Some len -> trace_in_cache f ; len
  | None ->
      let start = trace_in_start f in
      let parse_row row =
        match row with
        | Some [| f |] -> get_int f
        | _ -> failexit ()
      in
      let len =
        query_one_row db
          "select count(n_type) \
           from person_name \
           where n_type = 'Main'"
          [| |]
          parse_row
      in
      db.iper_nbr <- Some len ;
      trace_out f start ;
      len

let nb_of_families db =
  let f = "nb_of_families" in
  match db.ifam_len with
  | Some len -> trace_in_cache f ; len
  | None ->
      let start = trace_in_start f in
      let parse_row row =
        match row with
        | Some [| f |] -> 1 + get_int f
        | _ -> failexit ()
      in
      let len =
        query_one_row db
          "select max(g_id) from groups"
          [| |]
          parse_row
      in
      db.ifam_len <- Some len ;
      trace_out f start ;
      len

let new_iper _ = todo "new_iper"
let new_ifam _ = todo "new_ifam"
let insert_person _ = todo "insert_person"
let insert_ascend _ = todo "insert_ascend"
let insert_union _ = todo "insert_union"
let insert_family _ = todo "insert_family"
let insert_descend _ = todo "insert_descend"
let insert_couple _ = todo "insert_couple"
let patch_person _ = todo "patch_person"
let patch_ascend _ = todo "patch_ascend"
let patch_union _ = todo "patch_union"
let patch_family _ = todo "patch_family"
let patch_descend _ = todo "patch_descend"
let patch_couple _ = todo "patch_couple"
let delete_person _ = todo "delete_person"
let delete_ascend _ = todo "delete_ascend"
let delete_union _ = todo "delete_union"
let delete_family _ = todo "delete_family"
let delete_descend _ = todo "delete_descend"
let delete_couple _ = todo "delete_couple"
let insert_string _ = todo "insert_string"
let commit_patches _ = todo "commit_patches"
let commit_notes _ = todo "commit_notes"

let person_of_key db fn sn oc =
  let f = "person_of_key" in
  let start = trace_in_start f in
  let fn = String.trim fn in
  let sn = String.trim sn in
  let key = Mutil.tr ' ' '_' (fn ^ "." ^ string_of_int oc ^ "." ^ sn) in
  let parse_row row =
    match row with
    | Some [| f |] -> Some (get_int f)
    | None -> None
    | _ -> failexit ()
  in
  let iper_opt = query_one_row db
    "select p_id \
     from persons \
     where pkey = ?"
    [| `String key |]
    parse_row
  in
  trace_out f start ;
  iper_opt

let persons_of_name db s =
  let f = "persons_of_name" in
  let start = trace_in_start f in
  let s = Name.crush_lower s in
  let query =
    "select p_id from person_name \
     inner join names using(na_id) \
     inner join names_crush using (nac_id) \
     where crush = ?"
  in
  let l = ref [] in
  let parse_res = function
    | [| f1 |] -> l := get_int f1 :: !l
    | _ -> failexit ()
  in
  query_with_fetch db query [| `String s |] parse_res ;
  trace_out f start ;
  !l

let persons_of_first_name db = trace_in "persons_of_first_name" ;
  { db = db
  ; field_id = "nag_id"
  ; query_one =
      "select p_id from person_name \
       inner join names using(na_id) \
       where nag_id = ?"
  ; query_all =
      "select distinct nag_id \
       from person_name \
       inner join names using (na_id) \
       where n_type = 'Main'"
  ; query_start =
      "select distinct names.nag_id \
       from person_name \
       inner join names using (na_id) \
       inner join names_givn using (nag_id) \
       where n_type = 'Main' and givn rlike ?"
  ; cursor = None
  }

let persons_of_surname db = trace_in "persons_of_surname" ;
  { db = db
  ; field_id = "nas_id"
  ; query_one =
      "select p_id from person_name \
       inner join names using(na_id) \
       where nas_id = ?"
  ; query_all =
      "select distinct nas_id \
       from person_name \
       inner join names using (na_id) \
       where n_type = 'Main'"
  ; query_start =
      "select distinct names.nas_id \
       from person_name \
       inner join names using (na_id) \
       inner join names_surn using (nas_id) \
       where n_type = 'Main' \
         and surn_wp rlike ?"
  ; cursor = None
  }

let cursor_fetch cursor field_id =
  match M.Res.fetch (module M.Row.Array) cursor with
  | Ok (Some [| f1 |]) when field_id = "nag_id" -> DbGivn (get_int f1)
  | Ok (Some [| f1 |]) when field_id = "nas_id" -> DbSurnP (get_int f1)
  | Ok None -> raise Not_found
  | Error e -> or_die "fetch" (Error e)
  | _ -> failexit ()

let spi_first ind s = 
  let f = "spi_first" in
  let start = trace_in_start f in
  let query, params =
    if s = "" then ind.query_all, [| |]
    else
      (* FIXME not safe for utf8 *)
      let s = String.concat "[ _]" @@ String.split_on_char ' ' s in
      let s = String.concat "\\(" @@ String.split_on_char '(' s in
      let s = String.concat "\\)" @@ String.split_on_char ')' s in
      let s = String.concat "\\." @@ String.split_on_char '.' s in
      let s = "^" ^ s ^ ".*" in
      ind.query_start, [| `String s |]
  in
  let s = get_statement ind.db query in
  let e = M.Stmt.execute s params |> or_die "execute" in
  ind.cursor <- Some e ;
  let res = cursor_fetch e ind.field_id in
  trace_out f start ;
  res

let spi_next ind _ =
  let f = "spi_next" in
  let start = trace_in_start f in
  let e =
    match ind.cursor with
    | Some e -> e
    | None -> failexit ()
  in
  let res = cursor_fetch e ind.field_id in
  trace_out f start ;
  res

let spi_find ind istr =
  let f = "spi_find" in
  let start = trace_in_start f in
  let query =
    "select distinct p_id from person_name \
     inner join names using(na_id) \
     where " ^ ind.field_id ^ " = ?"
  in
  let l = ref [] in
  let parse_res = function
    | [| f1 |] -> l := get_int f1 :: !l
    | _ -> failexit ()
  in
  let id =
    match istr, ind.field_id with
    | DbGivn nag_id, "nag_id" -> nag_id
    | DbSurn nas_id, "nas_id" -> nas_id
    | DbSurnP nas_id, "nas_id" -> nas_id
    | _ -> failexit ()
  in
  query_with_fetch ind.db query [| `Int id |] parse_res ;
  trace_out f start ;
  !l

let base_particles db =
  let f = "base_particles" in
  match db.cache_particles with
  | Some l -> trace_in_cache f ; l
  | None ->
      let start = trace_in_start f in
      let query = "select particle from particles" in
      let l = ref [] in
      let parse_res = function
        | [| f1 |] -> l := get_string f1 :: !l
        | _ -> failexit ()
      in
      query_with_fetch db query [| |] parse_res ;
      db.cache_particles <- Some !l ;
      trace_out f start ;
      !l

let base_strings_of_first_name db s =
  let f = "base_strings_of_first_name" in
  let start = trace_in_start f in
  let query =
    "select nag_id \
     from names_givn \
     where givn_l = ? \
        or givn = ?"
  in
  let l = ref [] in
  let parse_res = function
    | [| f1 |] -> l := DbGivn (get_int f1) :: !l
    | _ -> failexit ()
  in
  let params = Array.make 2 (`String s) in
  query_with_fetch db query params parse_res ;
  if !l = [] then begin
    let s = Name.crush_lower s in
    let query =
      "select nag_id \
       from names_givn \
       inner join names_crush using(nac_id) \
       where crush = ?"
    in
    query_with_fetch db query [| `String s |] parse_res ;
  end ;
  trace_out f start ;
  !l

let base_strings_of_surname db s = 
  let f = "base_strings_of_surname" in
  let start = trace_in_start f in
  let query =
    "select 1, nas_id \
     from names_surn \
     where surn_l = ? \
        or surn = ? \
    union \
     select 2, nas_id \
     from names_surn \
     where surn_p = ?"
  in
  let l = ref [] in
  let parse_res = function
    | [| f1 ; f2 |] -> begin
        match get_int f1 with
        | 1 -> l := DbSurn (get_int f2) :: !l
        | 2 -> l := DbSurnP (get_int f2) :: !l
        | _ -> failexit ()
      end
    | _ -> failexit ()
  in
  let params = Array.make 3 (`String s) in
  query_with_fetch db query params parse_res ;
  if !l = [] then begin
    let s = Name.crush_lower s in
    let query =
      "select 1, nas_id \
       from names_surn \
       inner join names_crush using(nac_id) \
       where crush = ?"
    in
    query_with_fetch db query [| `String s |] parse_res ;
  end ;
  trace_out f start ;
  !l

let load_ascends_array db =
  let f = "load_ascend_array" in
  let start = Unix.gettimeofday () in
  let build_array () =
    trace_in f ;
    let size = nb_of_persons db in
    let a = Array.make size
      { parents = None
      ; consang = Adef.no_consang
      }
    in
    let query =
      "select SQL_NO_CACHE p.p_id, g_id, consang \
       from persons p \
       left join person_group pg on p.p_id = pg.p_id and role = 'Child'"
    in
    let parse_res = function
      | [| f1 ; f2 ; f3|] -> a.(get_int f1) <-
          { parents = get_int_opt f2
          ; consang =
              let f3 = Float.of_string @@ get_string f3 in
              if f3 = (-1.0) then Adef.no_consang
              else Adef.fix_of_float (f3 /. 100.0)
          }
      | _ -> failexit ()
    in
    query_with_fetch db query [| |] parse_res ;
    insert_cache db "ascends" a ;
    db.array_ascends <- Some a ;
    trace_out f start
  in
  match db.array_ascends with
  | Some _ -> trace_in_cache f
  | None -> begin
      match get_cache_opt db "ascends" with
      | Some a -> begin
          try
            let f = f ^ "(pre)" in
            trace_in f ;
            db.array_ascends <- Some (Marshal.from_bytes a 0) ;
            trace_out f start
          with _ -> build_array ()
        end
      | None -> build_array ()
    end

let load_unions_array _ = trace_in_dummy "load_unions_array" ;
  ()

let load_couples_array db =
  let f = "load_couples_array" in
  let start = Unix.gettimeofday () in
  let build_array () =
    trace_in f ;
    let a = Array.make (nb_of_families db) (Adef.couple dummy_iper dummy_iper) in
    let query =
      "select SQL_NO_CACHE g.g_id, pg1.p_id, pg2.p_id \
       from groups g \
       inner join person_group pg1 on g.g_id = pg1.g_id and pg1.role = 'Parent1' \
       inner join person_group pg2 on g.g_id = pg2.g_id and pg2.role = 'Parent2'"
    in
    let parse_res = function
      | [| f1 ; f2 ; f3 |] -> a.(get_int f1) <- Adef.couple (get_int f2) (get_int f3)
      | _ -> failexit ()
    in
    query_with_fetch db query [| |] parse_res ;
    insert_cache db "couples" a ;
    db.array_couples <- Some a ;
    trace_out f start
  in
  match db.array_couples with
  | Some _ -> trace_in_cache f
  | None -> begin
      match get_cache_opt db "couples" with
      | Some a -> begin
          try
            let f = f ^ "(pre)" in
            trace_in f ;
            db.array_couples <- Some (Marshal.from_bytes a 0) ;
            trace_out f start
          with _ -> build_array ()
        end
      | None -> build_array ()
    end

let load_descends_array _ = trace_in_dummy "load_descends_array" ;
  ()

let load_strings_array _ = trace_in_dummy "load_strings_array" ;
  ()

let load_persons_array _ = trace_in_dummy "load_persons_array" ;
  ()

let load_families_array _ = trace_in_dummy "load_families_array" ;
  ()

let clear_ascends_array db = trace_in "clear_ascends_array" ;
  match db.array_ascends with
  | Some _ -> db.array_ascends <- None
  | None -> ()

let clear_unions_array _ = trace_in_dummy "clear_unions_array" ;
  ()

let clear_couples_array db = trace_in "clear_couples_array" ;
  match db.array_couples with
  | Some _ -> db.array_couples <- None
  | None -> ()

let clear_descends_array _ = trace_in_dummy "clear_descends_array" ;
  ()

let clear_strings_array _ = trace_in_dummy "clear_strings_array" ;
  ()

let clear_persons_array _ = trace_in_dummy "clear_persons_array" ;
  ()

let clear_families_array _ = trace_in_dummy "clear_families_array" ;
  ()

let date_of_last_change _ = todo "date_of_last_change"

let get_death_reason db e_id =
  let step = "get_person->events->death_reason" in
  let start_step = trace_in_start step in
  let parse_row row =
    match row with
    | Some [| f1 |] -> begin match get_string f1 with
        | "Killed" -> Killed
        | "Murdered" -> Murdered
        | "Executed" -> Executed
        | "Disappeared" -> Disappeared
        | _ -> failexit ()
      end
    | None -> Unspecified
    | _ -> failexit ()
  in
  let res =
    query_one_row db
      "select reason \
       from death_details \
       where e_id = ?"
      [| `Int e_id |]
      parse_row
  in
  trace_out step start_step ;
  res

let get_event_values db e_id path =
  let step = "get_person->events->value" in
  let start_step = trace_in_start step in
  let parse_row row =
    match row with
    | Some [| f1 |] -> get_string f1
    | _ -> failexit ()
  in
  let query =
    "select json_value(attr, '" ^ path ^ "') \
     from event_values \
     where e_id = ?"
  in
  let res =
    query_one_row db query [| `Int e_id |] parse_row
  in
  trace_out step start_step ;
  res

let get_event_values_FB db e_id =
  let step = "get_person->events->valueFB" in
  let start_step = trace_in_start step in
  let parse_row row =
    match row with
    | Some [| f1 ; f2 |] -> begin
        match get_string_opt f1, get_string_opt f2 with
        | Some v1, None -> "%vFB:" ^ v1 ^ ";"
        | None, Some v2 -> "%vFB2:" ^ v2 ^ ";"
        | _ -> failexit ()
      end
    | _ -> failexit ()
  in
  let query =
    "select \
      json_value(attr, '$.ref_v1'), \
      json_value(attr, '$.ref_v2') \
     from event_values \
     where e_id = ?"
  in
  let res =
    query_one_row db query [| `Int e_id |] parse_row
  in
  trace_out step start_step ;
  res

let get_title db e_id =
  let step = "get_person->events->title" in
  let start_step = trace_in_start step in
  let parse_row row =
    match row with
    | Some [|f1;f2;f3;f4;f5;f6;f7;f8;f9;f10;f11;f12;f13;f14;f15;f16;f17;f18;f19;f20;f21;f22;f23|] ->
        let db_name =
          match get_string f4, get_string f5 with
          | "True", "" -> Tmain
          | "False", "" -> Tnone
          | _, s -> Tname (DbString s)
        in
        let db_ident =
          let s = get_string f1 in
          if s = "" then Empty
          else DbString s
        in
        let db_place =
          let s = get_string f2 in
          if s = "" then Empty
          else DbString s
        in
        let get_date f_prec f_cal f_dmy1_d f_dmy1_m f_dmy1_y f_dmy2_d f_dmy2_m f_dmy2_y f_text =
          let text = get_string f_text in
          let dmy1_y = get_int f_dmy1_y in
          if text = "" && dmy1_y = 0 then Adef.cdate_None
          else
            let date =
              let get_dmy2 () =
                { day2 = get_int f_dmy2_d
                ; month2 = get_int f_dmy2_m
                ; year2 = get_int f_dmy2_y
                ; delta2 = 0
                }
              in
              if text <> "" then Dtext text
              else Dgreg (
                { day = get_int f_dmy1_d
                ; month = get_int f_dmy1_m
                ; year = dmy1_y
                ; prec = get_prec f_prec get_dmy2
                ; delta = 0
                }, get_cal f_cal)
            in
            Adef.cdate_of_date date
        in
        { t_name = db_name
        ; t_ident = db_ident
        ; t_place = db_place
        ; t_date_start = get_date f6 f7 f8 f9 f10 f11 f12 f13 f14
        ; t_date_end = get_date f15 f16 f17 f18 f19 f20 f21 f22 f23
        ; t_nth = get_int f3
        }
    | _ -> failexit ()
  in
  let query =
    "select ident, place, nth, main, name, \
      d1_prec, d1_cal, d1_dmy1_d, d1_dmy1_m, d1_dmy1_y, d1_dmy2_d, d1_dmy2_m, d1_dmy2_y, d1_text, \
      d2_prec, d2_cal, d2_dmy1_d, d2_dmy1_m, d2_dmy1_y, d2_dmy2_d, d2_dmy2_m, d2_dmy2_y, d2_text \
     from title_details \
     where e_id = ?"
  in
  let res =
    query_one_row db query [| `Int e_id |] parse_row
  in
  trace_out step start_step ;
  res

let get_occupation db iper =
  let step = "get_person->occupation" in
  let start_step = trace_in_start step in
  let query =
    "select name, \
      group_concat(case d_prec \
                   when 'FROM-TO' then concat(dmy1_y, '-', dmy2_y) \
                   when 'FROM' then concat(dmy1_y, '-') \
                   when 'TO' then concat('-',dmy1_y) \
                   else dmy1_y \
                   end order by dmy1_y separator ', ') as period \
     from events \
     left join event_dmy2 using(e_id) \
     inner join occupation_details using(e_id) \
     inner join occupations using(o_id) \
     where e_type = 'OCCU' \
       and e_id in (select concat_ws(',',e_id) \
                    from person_event \
                    where p_id = ? \
                      and role = 'Main') \
     group by 1 order by period"
  in
  let occu = ref "" in
  let add_name s =
    let prefix =
      if !occu = "" then ""
      else ", "
    in
    occu := !occu ^ prefix ^ s
  in
  let parse_res = function
    | [| f1 ; f2 |] -> begin
        let name = get_string f1 in
        let period =
          match M.Field.value f2 with
          | `String s -> s
          | `Bytes b -> Bytes.to_string b
          | _ -> failexit ()
        in
        if period = "0" then add_name name
        else add_name (name ^ " (" ^ period ^ ")")
      end
    | _ -> failexit ()
  in
  query_with_fetch db query [| `Int iper |] parse_res ;
  trace_out step start_step ;
  !occu

let get_witnesses db e_id =
  let step = "get_person->events->witnesses" in
  let start_step = trace_in_start step in
  let a = ref [| |] in
  let parse_res = function
    | [| f1 ; f2 |] -> a := Array.append !a [| (
        get_int f1,
        match get_string f2 with
        | "Witness" -> Witness
        | "GodParent" -> Witness_GodParent
        | "Official" -> Witness_Officer
        | _ -> failexit ()
        ) |]
    | _ -> failexit ()
  in
  let query =
    "select p_id, role \
     from person_event \
     where e_id = ? \
       and role in ('Witness','GodParent','Official')"
  in
  query_with_fetch db query [| `Int e_id |] parse_res ;
  trace_out step start_step ;
  !a

let get_rparents db iper =
  let step = "get_person->rparents" in
  let start_step = trace_in_start step in
  let l = ref [] in
  let parse_res = function
    | [| f1 ; f2 ; f3 ; f4 ; f5 |] ->
        let s_id = get_source_opt f5 in
        let rel =
          match get_string_opt f2, get_string_opt f4 with
          | Some "AdoptionParent", _ -> Adoption
          | Some "RecognitionParent", _ -> Recognition
          | Some "CandidateParent", _ -> CandidateParent
          | Some "GodParent", _ -> GodParent
          | Some "FosterParent", _ -> FosterParent
          | _, Some "AdoptionParent" -> Adoption
          | _, Some "RecognitionParent" -> Recognition
          | _, Some "CandidateParent" -> CandidateParent
          | _, Some "GodParent" -> GodParent
          | _, Some "FosterParent" -> FosterParent
          | _ -> failexit ()
        in
        let r =
          { r_type = rel
          ; r_fath = get_int_opt f1
          ; r_moth = get_int_opt f3
          ; r_sources = s_id
          }
        in
        l := r :: !l
    | _ -> failexit ()
  in
  let query =
    "select p1.p_id, p1.role, p2.p_id, p2.role, e.s_id \
     from person_event pe \
     inner join events e using(e_id) \
     left join ( \
       select e_id, person_event.p_id, role \
       from person_event \
       inner join persons using (p_id) \
       where sex = 'M' \
         and role like '%Parent' \
     ) p1 using(e_id) \
     left join ( \
       select e_id, person_event.p_id, role \
       from person_event \
       inner join persons using (p_id) \
       where sex = 'F' \
         and role like '%Parent' \
     ) p2 using(e_id) \
     where pe.role = 'Main' \
       and (p1.p_id is not null or p2.p_id is not null) \
       and pe.p_id = ?"
  in
  query_with_fetch db query [| `Int iper |] parse_res ;
  trace_out step start_step ;
  !l

let get_related db iper =
  let step = "get_person->related" in
  let start_step = trace_in_start step in
  let l = ref [] in
  let parse_res = function
    | [| f1 |] -> l := get_int f1 :: !l
    | _ -> failexit ()
  in
  let query =
    "select pe2.p_id \
     from person_event pe1 \
     inner join person_event pe2 on pe1.e_id = pe2.e_id \
     where pe1.p_id = ? \
       and pe1.role <> 'Main' \
       and pe2.role = 'Main' \
    union \
     select pe.p_id \
     from person_group pg \
     inner join group_event using(g_id) \
     inner join person_event pe using(e_id) \
     where pg.p_id = ? \
       and pg.role in ('Parent1','Parent2') \
       and pe.role <> 'Main'"
  in
  let params = Array.make 2 (`Int iper) in
  query_with_fetch db query params parse_res ;
  trace_out step start_step ;
  !l

let get_event_dmy2 db e_id =
  let step = "get_person->events->dmy2" in
  let start_step = trace_in_start step in
  let parse_row row =
    match row with
    | Some [| f1 ; f2 ; f3 |] -> begin
        { day2 = get_int f1
        ; month2 = get_int f2
        ; year2 = get_int f3
        ; delta2 = 0
        }
      end
    | _ -> failexit ()
  in
  let res =
    query_one_row db
      "select dmy2_d, dmy2_m, dmy2_y from event_dmy2 where e_id = ?"
      [| `Int e_id |]
      parse_row
  in
  trace_out step start_step ;
  res

let get_person db iper =
  let f = "get_person" in
  let cache =
    match db.cache_persons with
    | Some h -> h
    | None ->
        let h = Hashtbl.create 100 in
        db.cache_persons <- Some h ;
        h
  in
  match Hashtbl.find_opt cache iper with
  | Some p -> trace_in_cache f ; p
  | None -> begin
      let start = trace_in_start f in
      let step = f ^ "->main" in
      let start_step = trace_in_start step in
      let parse_row row =
        match row with
        | Some [| f1 ; f2 ; f3 ; f4 ; f5; f6 |] ->
            get_int f1,
            get_string f2,
            get_note_opt f3,
            get_source_opt f4,
            get_sex f5,
            get_access f6
        | _ -> failexit ()
      in
      let db_occ, db_death_status, db_n_id, db_s_id, db_sex, db_access =
        query_one_row db
          "select occ, death, n_id, s_id, sex, access \
           from persons \
           where p_id = ?"
          [| `Int iper |]
          parse_row
      in
      trace_out step start_step ;
      let step = f ^ "->names" in
      let start_step = trace_in_start step in
      let query =
        "select n_type, nag_id, nan_id, nas_id  \
         from person_name \
         inner join names using(na_id) \
         where p_id = ?"
      in
      let db_first_name = ref quest_string in
      let db_surname = ref quest_string in
      let db_public_name = ref empty_string in
      let db_qualifiers = ref [] in
      let db_aliases = ref [] in
      let db_first_names_aliases = ref [] in
      let db_surnames_aliases = ref [] in
      let parse_res = function
        | [| f1 ; f2 ; f3 ; f4 |] ->
            begin match get_string f1, get_int_opt f2, get_int_opt f3, get_int_opt f4 with
            | "Main", Some nag_id, None, Some nas_id ->
                db_first_name := DbGivn nag_id ;
                db_surname := DbSurn nas_id
            | "PublicName", Some nag_id, None, None ->
                db_public_name := DbGivn nag_id
            | "Qualifier", Some _, Some nan_id, None ->
                db_qualifiers := DbNick nan_id :: !db_qualifiers
            | "Alias", Some nag_id, None, None ->
                db_aliases := DbGivn nag_id :: !db_aliases
            | "FirstNamesAlias", Some nag_id, None, Some _ ->
                db_first_names_aliases := DbGivn nag_id :: !db_first_names_aliases
            | "SurnamesAlias", Some _, None, Some nas_id ->
                db_surnames_aliases := DbSurn nas_id :: !db_surnames_aliases
            | _ -> failexit ()
            end ;
            ()
        | _ -> failexit ()
      in
      query_with_fetch db query [| `Int iper |] parse_res ;
      trace_out step start_step ;
      let step = f ^ "->events" in
      let start_step = trace_in_start step in
      let query =
        "select e_type, t_name, d_prec, d_cal1, dmy1_d, dmy1_m, dmy1_y, d_text, pl_id, n_id, s_id, e_id \
         from events \
         inner join person_event using(e_id) \
         where e_type <> 'OCCU' \
           and role = 'Main' \
           and p_id = ?"
      in
      let db_birth = ref Adef.cdate_None in
      let db_birth_place = ref empty_string in
      let db_birth_note = ref empty_string in
      let db_birth_src = ref empty_string in
      let db_baptism = ref Adef.cdate_None in
      let db_baptism_place = ref empty_string in
      let db_baptism_note = ref empty_string in
      let db_baptism_src = ref empty_string in
      let db_death =
        match db_death_status with
        | "NotDead" -> ref NotDead
        | "Dead" -> ref NotDead (* Updated later *)
        | "DeadYoung" -> ref DeadYoung
        | "DeadDontKnowWhen" -> ref DeadDontKnowWhen
        | "DontKnowIfDead" -> ref DontKnowIfDead
        | "OfCourseDead" -> ref OfCourseDead
        | _ -> failexit ()
      in
      let db_death_place = ref empty_string in
      let db_death_note = ref empty_string in
      let db_death_src = ref empty_string in
      let db_burial = ref UnknownBurial in
      let db_burial_place = ref empty_string in
      let db_burial_note = ref empty_string in
      let db_burial_src = ref empty_string in
      let db_pevents = ref [] in
      let db_titles = ref [] in
      let parse_res = function
        | [| f1 ; f2 ; f3 ; f4 ; f5 ; f6 ; f7 ; f8 ; f9 ; f10 ; f11 ; f12 |] -> begin
            let dmy1_y = get_int f7 in
            let d_text = get_string f8 in
            let pl_id = get_place_opt f9 in
            let n_id = get_note_opt f10 in
            let s_id = get_source_opt f11 in
            let e_id = get_int f12 in
            let get_dmy2 () = get_event_dmy2 db e_id in
            let e_cdate =
              Adef.cdate_of_date (
                if d_text <> "" then Dtext d_text
                else Dgreg (
                  { day = get_int f5
                  ; month = get_int f6
                  ; year = dmy1_y
                  ; prec = get_prec f3 get_dmy2
                  ; delta = 0
                  },
                  get_cal f4
                )
              )
            in
            let e_cdate2 =
              if dmy1_y = 0 && d_text = "" then Adef.cdate_None
              else e_cdate
            in
            let add_event name date place note src witnesses =
              db_pevents := 
                { epers_name = name
                ; epers_date = date
                ; epers_place = place
                ; epers_reason = empty_string
                ; epers_note = note
                ; epers_src = src
                ; epers_witnesses = witnesses
                }
                :: !db_pevents
            in
            let db_witnesses = get_witnesses db e_id in
            match get_string f1, get_string f2 with
            | "BIRT", "" ->
                db_birth := e_cdate2 ;
                db_birth_place := pl_id ;
                db_birth_note := n_id ;
                db_birth_src := s_id ;
                add_event Epers_Birth e_cdate2 pl_id n_id s_id db_witnesses
            | "BAPM", "" ->
                db_baptism := e_cdate2 ;
                db_baptism_place := pl_id ;
                db_baptism_note := n_id ;
                db_baptism_src := s_id ;
                add_event Epers_Baptism e_cdate2 pl_id n_id s_id db_witnesses
            | "DEAT", "" ->
                if db_death_status = "Dead" then begin
                  db_death := Death (get_death_reason db e_id, e_cdate)
                end ;
                db_death_place := pl_id ;
                db_death_note := n_id ;
                db_death_src := s_id ;
                add_event Epers_Death e_cdate pl_id n_id s_id db_witnesses
            | "BURI", "" ->
                db_burial :=
                  if dmy1_y = 0 then Buried Adef.cdate_None
                  else Buried e_cdate
                ;
                db_burial_place := pl_id ;
                db_burial_note := n_id ;
                db_burial_src := s_id ;
                add_event Epers_Burial e_cdate pl_id n_id s_id db_witnesses
            | "CREM", "" ->
                db_burial :=
                  if dmy1_y = 0 then Cremated Adef.cdate_None
                  else Cremated e_cdate
                ;
                db_burial_place := pl_id ;
                db_burial_note := n_id ;
                db_burial_src := s_id ;
                add_event Epers_Cremation e_cdate pl_id n_id s_id db_witnesses
            | "TITL", "" ->
                db_titles := get_title db e_id :: !db_titles
            | "FACT", "FS" ->
                add_event (Epers_Name (DbString "FS")) Adef.cdate_None empty_string
                  (DbString ("%vFSp:" ^ get_event_values db e_id "$.ref" ^ ";"))
                  empty_string [| |]
            | "FACT", "FB" ->
                add_event (Epers_Name (DbString "Facebook")) Adef.cdate_None empty_string
                  (DbString (get_event_values_FB db e_id))
                  empty_string [| |]
            | "ADOP", "" -> ()
            | "FACT", _ -> ()
            | "EVEN", _ -> ()
            | s1, s2 -> failwith @@ Printf.sprintf "unexpected pevent %s(%s)" s1 s2
          end
        | _ -> failexit ()
      in
      query_with_fetch db query [| `Int iper |] parse_res ;
      trace_out step start_step ;
      let db_occupation =
        let s = get_occupation db iper in
        if s = "" then empty_string
        else DbString s
      in
      let p =
        { first_name = !db_first_name
        ; surname = !db_surname
        ; occ = db_occ
        ; image = empty_string
        ; public_name = !db_public_name
        ; qualifiers = !db_qualifiers
        ; aliases = !db_aliases
        ; first_names_aliases = !db_first_names_aliases
        ; surnames_aliases = !db_surnames_aliases
        ; titles = !db_titles
        ; rparents = get_rparents db iper
        ; related = get_related db iper
        ; occupation = db_occupation
        ; sex = db_sex
        ; access = db_access
        ; birth = !db_birth
        ; birth_place = !db_birth_place
        ; birth_note = !db_birth_note
        ; birth_src = !db_birth_src
        ; baptism = !db_baptism
        ; baptism_place = !db_baptism_place
        ; baptism_note = !db_baptism_note
        ; baptism_src = !db_baptism_src
        ; death = !db_death
        ; death_place = !db_death_place
        ; death_note = !db_death_note
        ; death_src = !db_death_src
        ; burial = !db_burial
        ; burial_place = !db_burial_place
        ; burial_note = !db_burial_note
        ; burial_src = !db_burial_src
        ; pevents = !db_pevents
        ; notes = db_n_id
        ; psources = db_s_id
        ; key_index = iper
        }
      in
      Hashtbl.add cache iper p ;
      trace_out f start ;
      p
  end

let get_ascend db iper =
  let f = "get_ascend" in
  match db.array_ascends with
  | Some a -> trace_in_array f ; a.(iper)
  | None -> begin
      let cache =
        match db.cache_ascends with
        | Some h -> h
        | None ->
            let h = Hashtbl.create 100 in
            db.cache_ascends <- Some h ;
            h
      in
      match Hashtbl.find_opt cache iper with
      | Some a -> trace_in_cache f ; a
      | None -> begin
          let start = trace_in_start f in
          let parse_row row =
            match row with
            | Some [| f1 |] -> Some (get_int f1)
            | None -> None
            | _ -> failexit ()
          in
          let db_ifam =
            query_one_row db 
              "select g_id \
               from person_group \
               where role = 'Child' \
                 and p_id = ?"
              [| `Int iper |]
              parse_row
          in
          let a =
            { parents = db_ifam
            ; consang = Adef.no_consang
            }
          in
          Hashtbl.add cache iper a ;
          trace_out f start ;
          a
        end
    end

let get_union db iper =
  let f = "get_union" in
  let cache =
    match db.cache_unions with
    | Some h -> h
    | None ->
        let h = Hashtbl.create 100 in
        db.cache_unions <- Some h ;
        h
  in
  match Hashtbl.find_opt cache iper with
  | Some u -> trace_in_cache f ; u
  | None -> begin
      let start = trace_in_start f in
      let query =
        "select g_id \
         from person_group \
         where role in ('Parent1','Parent2') \
           and p_id = ? \
         order by seq asc"
      in
      let db_union = ref [| |] in
      let parse_res = function
        | [| f1 |] -> db_union := Array.append !db_union [| get_int f1 |]
        | _ -> failexit ()
      in
      query_with_fetch db query [| `Int iper |] parse_res ;
      let u = { family = !db_union } in
      Hashtbl.add cache iper u ;
      trace_out f start ;
      u
    end

let no_family =
  { marriage = Adef.cdate_None
  ; marriage_place = empty_string
  ; marriage_note = empty_string
  ; marriage_src = empty_string
  ; witnesses = [| |]
  ; relation = NoMention
  ; divorce = NotDivorced
  ; fevents = []
  ; comment = empty_string
  ; origin_file = empty_string
  ; fsources = empty_string
  ; fam_index = dummy_ifam
  }

let get_family db ifam =
  if ifam = dummy_ifam then no_family else
  let f = "get_family" in
  let cache =
    match db.cache_families with
    | Some h -> h
    | None ->
        let h = Hashtbl.create 100 in
        db.cache_families <- Some h ;
        h
  in
  match Hashtbl.find_opt cache ifam with
  | Some fam -> trace_in_cache f ; fam
  | None -> begin
      let start = trace_in_start f in
      let parse_row row =
        match row with
        | Some [| f1 ; f2 ; f3 |] -> Some (
            get_note_opt f1,
            get_source_opt f2,
            get_origin_opt f3 )
        | Some _ -> failexit ()
        | None -> None
      in
      let res =
        query_one_row db
          "select n_id, s_id, o_id \
           from groups \
           where g_id = ?"
          [| `Int ifam |]
          parse_row
      in
      match res with
      | None ->
          let fam = no_family in
          Hashtbl.add cache ifam fam ;
          fam
      | Some (db_n_id, db_s_id, db_o_id) -> begin
          let query =
            "select e_type, t_name, d_prec, d_cal1, \
              dmy1_d, dmy1_m, dmy1_y, d_text, pl_id, n_id, s_id, e_id \
             from events \
             inner join group_event using(e_id) \
             where g_id = ?"
          in
          let db_marriage = ref Adef.cdate_None in
          let db_marriage_place = ref empty_string in
          let db_marriage_note = ref empty_string in
          let db_marriage_src = ref empty_string in
          let db_relation = ref NotMarried in
          let db_divorce = ref NotDivorced in
          let db_fevents = ref [] in
          let db_witnesses = ref [| |] in
          let parse_res = function
            | [| f1 ; f2 ; f3 ; f4 ; f5 ; f6 ; f7 ; f8 ; f9 ; f10 ; f11 ; f12 |] -> begin
                let dmy1_y = get_int f7 in
                let d_text = get_string f8 in
                let pl_id = get_place_opt f9 in
                let n_id = get_note_opt f10 in
                let s_id = get_source_opt f11 in
                let e_id = get_int f12 in
                let get_dmy2 () = get_event_dmy2 db e_id in
                let e_cdate =
                  if dmy1_y = 0 && d_text = "" then Adef.cdate_None
                  else Adef.cdate_of_date (
                    if d_text <> "" then Dtext d_text
                    else Dgreg (
                      { day = get_int f5
                      ; month = get_int f6
                      ; year = dmy1_y
                      ; prec = get_prec f3 get_dmy2
                      ; delta = 0
                      }, get_cal f4
                    )
                  )
                in
                let add_event name date place note src witnesses =
                  db_fevents :=
                    { efam_name = name
                    ; efam_date = date
                    ; efam_place = place
                    ; efam_reason = empty_string
                    ; efam_note = note
                    ; efam_src = src
                    ; efam_witnesses = witnesses
                    }
                    :: !db_fevents
                in
                let witnesses = get_witnesses db e_id in
                match get_string f1, get_string f2 with
                | "MARR", "" ->
                    db_marriage := e_cdate ;
                    db_marriage_place := pl_id ;
                    db_marriage_note := n_id ;
                    db_marriage_src := s_id ;
                    add_event Efam_Marriage e_cdate pl_id n_id s_id witnesses ;
                    db_witnesses := Array.map (fun (iper, _) -> iper) witnesses ;
                    db_relation := Married
                | "MARC", "" ->
                    add_event Efam_MarriageContract e_cdate pl_id n_id s_id witnesses
                | "DIV", "" ->
                    add_event Efam_Divorce e_cdate pl_id n_id s_id witnesses ;
                    db_divorce := Divorced e_cdate
                | "ENGA", "" ->
                    add_event Efam_Engage e_cdate pl_id n_id s_id witnesses ;
                    db_relation := Engaged
                | "EVEN", "unmarried" ->
                    db_relation := NotMarried
                | "EVEN", "nomen" ->
                    db_relation := NoMention
                | "EVEN", "SEP" ->
                    db_divorce := Separated
                | "FACT", "FSc" -> ()
                | s1, s2 -> failwith @@ Printf.sprintf "unexpected fevent %s(%s)" s1 s2
              end
            | _ -> failexit ()
          in
          query_with_fetch db query [| `Int ifam |] parse_res ;
          let fam =
            { marriage = !db_marriage
            ; marriage_place = !db_marriage_place
            ; marriage_note = !db_marriage_note
            ; marriage_src = !db_marriage_src
            ; witnesses = !db_witnesses
            ; relation = !db_relation
            ; divorce = !db_divorce
            ; fevents = !db_fevents
            ; comment = db_n_id
            ; origin_file = db_o_id
            ; fsources = db_s_id
            ; fam_index = ifam
            }
          in
          Hashtbl.add cache ifam fam ;
          trace_out f start ;
          fam
        end
    end

let get_couple db ifam =
  let f = "get_couple" in
  match db.array_couples with
  | Some a -> trace_in_array f ; a.(ifam)
  | None -> begin
      let cache =
        match db.cache_couples with
        | Some h -> h
        | None ->
            let h = Hashtbl.create 100 in
            db.cache_couples <- Some h ;
            h
      in
      match Hashtbl.find_opt cache ifam with
      | Some c -> trace_in_cache f ; c
      | None -> begin
          let start = trace_in_start f in
          let query =
            "select p_id, role \
             from person_group \
             where role in ('Parent1','Parent2') \
               and g_id = ?"
          in
          let db_father = ref dummy_iper in
          let db_mother = ref dummy_iper in
          let parse_res = function
            | [| f1 ; f2 |] -> begin match get_string f2 with
                | "Parent1" -> db_father := get_int f1
                | "Parent2" -> db_mother := get_int f1
                | _ -> failexit ()
              end
            | _ -> failexit ()
          in
          query_with_fetch db query [| `Int ifam |] parse_res ;
          let c = Adef.couple !db_father !db_mother in
          Hashtbl.add cache ifam c ;
          trace_out f start ;
          c
        end
    end

let get_descend db ifam =
  let f = "get_descend" in
  let cache =
    match db.cache_descends with
    | Some h -> h
    | None ->
        let h = Hashtbl.create 100 in
        db.cache_descends <- Some h ;
        h
  in
  match Hashtbl.find_opt cache ifam with
  | Some d -> trace_in_cache f ; d
  | None -> begin
      let start = trace_in_start f in
      let query =
        "select p_id \
         from person_group \
         where role = 'Child' \
           and g_id = ? \
         order by seq asc"
      in
      let db_children = ref [| |] in
      let parse_res = function
        | [| f1 |] -> db_children := Array.append !db_children [| get_int f1 |]
        | _ -> failexit ()
      in
      query_with_fetch db query [| `Int ifam |] parse_res ;
      let d = { children = !db_children } in
      Hashtbl.add cache ifam d ;
      trace_out f start ;
      d
    end

type 'a cursor = { length : int ; get : int -> 'a option }

(* 
let get_id_array db id_len set_len id_array set_array f table field =
  match id_len, id_array with
  | Some len, Some a ->
      trace_in_cache f ;
      { length = len ; get = fun i -> Some a.(i) }
  | _, None -> begin
      let start = trace_in_start f in
      let query = "select SQL_NO_CACHE " ^ field ^ " from " ^ table in
      let a = ref [| |] in
      let i = ref 0 in
      let init_res len =
        a := Array.make len dummy_iper
      in
      let parse_res = function
        | [| f1 |] ->
            !a.(!i) <- get_int f1 ;
            incr i
        | _ -> failexit ()
      in
      let len = query_with_init_and_fetch db query [| |] init_res parse_res in
      set_len len ;
      set_array !a ;
      trace_out f start ;
      { length = len ; get = fun i -> Some !a.(i) }
    end
  | _ -> failexit ()

let persons db =
  let set_len len = db.iper_len <- Some len in
  let set_array a = db.iper_array <- Some a in
  let res = get_id_array db db.iper_len set_len db.iper_array set_array "persons" "persons" "p_id" in
  res

let families db =
  let set_len len = db.ifam_len <- Some len in
  let set_array a = db.ifam_array <- Some a in
  get_id_array db db.ifam_len set_len db.ifam_array set_array "families" "groups" "g_id"
*)

let persons db = trace_in "persons" ;
  { length = nb_of_persons db
  ; get = fun i -> Some i
  }

let families db = trace_in "families" ;
  { length = nb_of_families db
  ; get = fun i -> Some i
  }

let make _ = todo "make"

let read_nldb _ = trace_in_dummy "read_nldb" ;
  []

let write_nldb _ = todo "write_nldb"
let sync ?scratch:_ _ = todo "sync"
let base_notes_origin_file _ = todo "base_notes_origin_file"

let base_notes_dir _ = trace_in_dummy "base_notes_dir" ;
  "DUMMY"

let base_wiznotes_dir _ = trace_in_dummy "base_wiznotes_dir" ;
  "DUMMY"

let base_notes_read _ = todo "base_notes_read"
let base_notes_read_first_line _ = todo "base_notes_read_first_line"

let base_notes_are_empty _ _ = trace_in_dummy "base_notes_are_empty" ;
  true
