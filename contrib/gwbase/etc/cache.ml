(* camlp4r *)
(* $Id: public.ml,v 5.00 2018-07-04 09:03:02 hg Exp $ *)

open Def;
open Gwdb;
open Printf;
open Util;


value index = ref 0;
value fn = ref "";
value sn = ref "";
value oc = ref 0;
value set_true = ref False;
value set_false = ref False;
value force = ref False;
value list = ref False;
value size = ref False;
value trace = ref False;
value test = ref False;
value bname = ref "";

(* copied from notes.ml, modified conf.bname to bname *)

type cache_person_linked_pages_t = Hashtbl.t Def.iper bool;

value (ht_cache_person_linked_pages : cache_person_linked_pages_t) =
  Hashtbl.create 1;

value cache_fname_person_linked_pages bname =
  let d_sep = Filename.dir_sep in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  "." ^ d_sep ^ bname ^ d_sep ^ "cache_person_linked_pages"
;

value read_cache_person_linked_pages bname =
  let fname = cache_fname_person_linked_pages bname in
  match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let ht : cache_person_linked_pages_t = input_value ic in
      do { close_in ic; ht }
  | None -> ht_cache_person_linked_pages ]
;

value write_cache_person_linked_pages bname ht =
  let fname = cache_fname_person_linked_pages bname in
  match try Some (Secure.open_out_bin fname) with [ Sys_error _ -> None ] with
  [ Some oc ->
      do {
        output_value oc ht;
        close_out oc
      }
  | None -> () ]
;

value patch_cache_person_linked_pages bname ht k v =
  if v then do {
    if not test.val then do {
      Hashtbl.replace ht k v;
      write_cache_person_linked_pages bname ht;
    }
    else ();
    if trace.val then printf "Adding person %d\n" (Adef.int_of_iper k) else ();
  }
  else do {
    if not test.val then do {
      Hashtbl.remove ht k;
      write_cache_person_linked_pages bname ht;
    }
    else ();
    if trace.val then printf "Removing person %d\n" (Adef.int_of_iper k) else ();
  }
;

value get_someone base i fn sn oc =
  if i >= 0 && i < nb_of_persons base then
    Some (Adef.iper_of_int index.val)
  else if fn <> "" || sn <> "" || oc <> 0 then
    match person_of_key base fn sn oc with
    [ Some p -> Some p
    | _ -> None ]
  else None
;

value has_linked_pages ht ip =
  try Hashtbl.find ht ip with
  [ Not_found -> False ]
;

value speclist =
  [("-i", Arg.Int (fun i -> index.val := i),
    "Index of person");
   ("-fn", Arg.String (fun i -> fn.val := i),
    "First name of person");
   ("-sn", Arg.String (fun i -> sn.val := i),
    "Surname of person");
   ("-oc", Arg.Int (fun i -> oc.val := i),
    "Occurence of person");
   ("-set", Arg.Set set_true, "Set to True");
   ("-reset", Arg.Set set_false, "Remove from table");
   ("-force", Arg.Set force, "Force removal of index");
   ("-list", Arg.Set list, "List of entries");
   ("-tr", Arg.Set trace, "Trace actions");
   ("-tst", Arg.Set test, "Test only");
   ("-size", Arg.Set size, "Size of cache table")
  ]
;

value anonfun i = bname.val := i;
value usage = "Usage: cache [-i #] [-set] [-reset] [-list] [-help] base.\n";

value main () =
  do {
    index.val := -1;
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let base = Gwdb.open_base bname.val in
    printf "Executing cache on %s with -i %d \n\n"
      bname.val index.val;
    flush stdout;
    let ht_cache = read_cache_person_linked_pages bname.val in
    let _ = printf "Cache %s of size %d\n"
      (cache_fname_person_linked_pages bname.val)
      (Hashtbl.length ht_cache)
    in
    if list.val = True then do {
      printf "Listing cache table\n";
      Hashtbl.iter
      (fun k v ->
        let index = (Adef.int_of_iper k) in
        if index >= 0 && index < nb_of_persons base then
          let p = poi base k in
          let fn = (sou base (get_first_name p)) in
          let sn = (sou base (get_surname p)) in
          let oc = (get_occ p) in
          let vv = if v then "True" else "False" in
          printf "Person %s.%d %s (i=%d) has linked pages (%s)\n" fn oc sn index vv
        else printf "*** Index out of bounds %d ***\n" index )
      ht_cache
      }
    else if size.val = True then
      printf "Size of cache table is %d\n" (Hashtbl.length ht_cache)
    else
      match get_someone base index.val fn.val sn.val oc.val with
        [ Some ip ->
            let p = poi base ip in
            let fn = (sou base (get_first_name p)) in
            let sn = (sou base (get_surname p)) in
            let oc = (get_occ p) in
            if has_linked_pages ht_cache ip
            then do {
              printf "Person %s.%d %s (i=%d) has linked pages\n" fn oc sn (Adef.int_of_iper ip);
              if set_false.val then
                patch_cache_person_linked_pages bname.val ht_cache ip False
              else ()
              }
            else do {
              printf "Person %s.%d %s (i=%d) does not have linked pages\n" fn oc sn (Adef.int_of_iper ip);
              if set_true.val then 
                patch_cache_person_linked_pages bname.val ht_cache ip True
              else ()
              }
        | None ->
            if force.val && index.val >= 0 then do {
              patch_cache_person_linked_pages bname.val ht_cache (Adef.iper_of_int index.val) False
              }
            else
              printf "*** Person not found %d ***\n" index.val ];
    if test.val then printf "Test only, nothing changed\n" else ();
    flush stdout;
  }
;

main ();
