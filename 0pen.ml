#!/usr/bin/env ocaml

Printexc.record_backtrace true;;

[@@@alert "-ocaml_deprecated_auto_include"]
#load "unix.cma"
#load "str.cma"

#use "topfind";;
Topfind.load_deeply ["batteries"; "dolog"; "magic-mime"; "ubase"; "uucp"; "uutf"];;

module Log = Dolog.Log
open Printf

let _ =
  Log.color_on();
  Log.set_log_level Log.DEBUG;
  Random.self_init ()


module Utils = struct

  module IntSet = Set.Make(Int)
  module StringMap = Map.Make(String)


  let (!++) r = incr r; !r
  and (!--) r = decr r; !r
  let (+=) r x = r := !r + x
  and (-=) r x = r := !r - x


  let crash msg =
    Log.fatal "%s" msg;
    exit 1


  module Files = struct

    let path_parts_no_dot path =
      let parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "" && s <> ".") in
      if path.[0] = '/' then
        "/" :: parts
      else
        parts

    (* Get inode number of file `f`. Dereferences symlinks. *)
    let ino f : int =
      Unix.((stat f).st_ino)

    let inode_set_of_file_list l =
      List.fold_left (fun s f -> IntSet.add (ino f) s) IntSet.empty l

    let inode_set_of_path p0 =
      let res = ref IntSet.empty in
      let rec aux p =
        if Sys.is_directory p then
          (let files = Sys.readdir p in
           BatArray.modify (fun f -> Filename.concat p f) files;
           Array.iter aux files)
        else
          res := IntSet.add (ino p) !res
      in
      aux p0;
      !res

  end


  module Floats = struct

    (* +. and -. are unreliable *)
    let floot = 1000000000.
    let intify x = int_of_float (x *. floot)
    let deintify n = float_of_int n /. floot

  end


  module Lists = struct

    let push lr x =
      lr := x :: !lr
  
    let pop lr =
      let res = List.hd !lr in
      lr := List.tl !lr;
      res

  end


  module Seqs = struct

    exception End_of_seq
  
    let pop seq_ref =
      match Seq.uncons !seq_ref with
      | None -> raise End_of_seq
      | Some (x, xs) ->
          seq_ref := xs;
          x
  
    let float_sum s =
      Seq.fold_left (fun accu x -> accu + Floats.intify x) 0 s |> Floats.deintify
  
    let int_sum s =
      Seq.fold_left (fun accu x -> accu + x) 0 s

  end


  module Strings = struct

    (* Compare 2 UTF8 strings, assuming they are symbolically equal iff. their characters are equal *)
    let utf8_compare s1 s2 =
      let rec loop d1 d2 =
        match Uutf.decode d1, Uutf.decode d2 with
        | `End, `End -> 0
        | `End, _ -> -1
        | _, `End -> 1
        | whatever1, whatever2 ->
          let c = compare whatever1 whatever2 in
          if c <> 0 then
            c
          else
            loop d1 d2
      in
      loop (Uutf.decoder ~encoding:`UTF_8 (`String s1)) (Uutf.decoder ~encoding:`UTF_8 (`String s2))

    let exists test s =
      let rec loop d =
        match Uutf.decode d with
        | `Uchar u when test u -> true
        | `Uchar _ | `Malformed _ -> loop d
        | `End -> false
        | `Await -> assert false
      in
      loop (Uutf.decoder ~encoding:`UTF_8 (`String s))

    module FileManagerSort = struct

      type key_part = Low of string | Int of int | High of string

      type key = key_part array

      let key_part_is_int = function
      | Int _ -> true
      | _ -> false

      let key_part_to_string = function
      | Low s | High s -> s
      | Int n -> string_of_int n

      let filename_maybe_tag_group_regexp = Str.regexp ("^\\([^[]*\\)" ^ "\\([[][^]]*[]]\\)?" ^ "\\(.*\\)$")
      let remove_tag_group file_name =
        Str.string_match filename_maybe_tag_group_regexp file_name 0 |> (fun b -> assert b);
        Str.matched_group 1 file_name ^ Str.matched_group 3 file_name

      let digits_regexp = Str.regexp "[0-9]+"

      let key file_name : key =
        let open Str in
        file_name
        |> remove_tag_group
        |> Filename.remove_extension
        |> Ubase.from_utf8
        |> String.lowercase_ascii
        |> full_split digits_regexp
        |> List.to_seq
        |> Seq.map (function
          | Delim digits when String.length digits <= 18 -> Int (int_of_string digits)
          | Delim text
          | Text text ->
            assert (text <> "");
            if Char.code text.[0] < 0x30 then Low text else High text
          )
        |> Array.of_seq

      let compare_parts p1 p2 =
        match p1, p2 with
        | High t1, High t2 -> utf8_compare t1 t2
        | High _, _ -> 1
        | _, High _ -> -1
        | Low t1, Low t2 -> utf8_compare t1 t2
        | Low _, _ -> -1
        | _, Low _ -> 1
        | Int n1, Int n2 -> compare n1 n2

      let compare_keys k1 k2 =
        BatArray.compare compare_parts k1 k2

      let compare_with_keys s1 key1 s2 key2 =
        let key_result = compare_keys key1 key2 in
        if key_result <> 0 then
          key_result
        else
          utf8_compare s1 s2

      let compare s1 s2 =
        compare_with_keys s1 (key s1) s2 (key s2)

      let number_separator_regexp = Str.regexp "^\\([ex]\\|[^a-z]+\\)" (* For lowercase strings *)

      (** Test whether 2 keys only differ by one number or one sequence of numbers *)
      let keys_are_in_sequence k1 k2 =
        if Array.length k1 <> Array.length k2 then
          false
        else
          let i = ref 0 and j = ref (Array.length k1) in
          (* Common prefix *)
          while !i < !j && k1.(!i) = k2.(!i) do
            i += 1
          done;
          (* Common suffix *)
          while !j > !i && k1.(!j - 1) = k2.(!j - 1) do
            j -= 1
          done;
          if !i = !j then
            (* k1 = k2 *)
            true
          (* Differing parts must begin and end with numbers (note: !j - 1 may = !i) *)
          else if not (Array.for_all key_part_is_int [|k1.(!i); k2.(!i); k1.(!j - 1); k2.(!j - 1)|]) then
            false
          else begin
            while
              !i < !j &&
                match k1.(!i), k2.(!i) with
                | Int _, Int _ -> true
                | part1, part2 ->
                  let s1 = key_part_to_string part1 and s2 = key_part_to_string part2 in
                  s1 = s2 && Str.string_match number_separator_regexp s1 0
            do
              i += 1
            done;
            !i = !j
          end
    end
  end
end

open Utils


(* = Files list = *)

module FileTree = struct

  let default_ignored_first_chars = [|'.'; '_'; '~'; '+'|]
  let ignored_first_chars = ref default_ignored_first_chars

  let set_dont_ignore_plus () =
    ignored_first_chars := [|'.'; '_'; '~'|]

  let file_is_ignored name =
    Array.mem name.[0] !ignored_first_chars && not (Array.mem name [|"."; ".."|])

  let path_is_ignored path =
    List.exists file_is_ignored (Files.path_parts_no_dot path)

  let dir_flattening_characters = [|':'; '='|]

  let dir_should_be_flattened name =
    (* =foo *)
    Array.mem name.[0] dir_flattening_characters
    (* +=foo *)
    || (Array.mem name.[0] default_ignored_first_chars
        && String.length name >= 2 && Array.mem name.[1] dir_flattening_characters)
    (* FOO *)
    || (Strings.exists Uucp.Case.is_cased name && not @@ Strings.exists Uucp.Case.is_lower name)

  let path_should_be_flattened path =
    List.for_all dir_should_be_flattened (Files.path_parts_no_dot path)

  module T = struct
    type tree =
      | Dir of {
          name : string;
          name_key : Strings.FileManagerSort.key lazy_t;
          weight : float; (* By default this is the number of files *)
          sum_of_local_weights : float; (* sum of the local weights in [children] *)
          children : (tree * float) list; (* children trees with their local weights *)
          sorted_leaf_children : tree Seq.t lazy_t (* for number series *)
        }
      | File of {
          name : string;
          name_key : Strings.FileManagerSort.key lazy_t;
          path : string;
          weight : float;
        }
      | Void
  end

  open T

  let name = function
    | Dir d -> d.name
    | File f -> f.name
    | Void -> assert false

  let name_key = function
    | Dir d -> Lazy.force d.name_key
    | File f -> Lazy.force f.name_key
    | Void -> assert false

  let compare_names t1 t2 =
    Strings.FileManagerSort.compare_with_keys (name t1) (name_key t1) (name t2) (name_key t2)

  let weight = function
    | Dir d -> d.weight
    | File f -> f.weight
    | Void -> 0.

  let rec num_files = function
    | Dir d -> d.children |> List.to_seq |> Seq.map (fun (t, _) -> num_files t) |> Seqs.int_sum
    | File _ -> 1
    | Void -> 0

  (* Apply f to each file path in t *)
  let rec iter (f : string -> unit) = function
    | Dir d -> List.iter (fun (t, _) -> iter f t) d.children
    | File file -> f file.path
    | Void -> ()

  let rec first_path_parts = function
  | Dir { name; children } ->
    begin match
      children |> List.to_seq
               |> Seq.map (fun (t, _) -> first_path_parts t)
               |> Seq.drop_while ((=) None)
               |> Seq.uncons
    with
    | Some (Some parts, _) -> Some (name :: parts)
    | Some (None, _) -> assert false
    | None -> None
    end
  | File { name } -> Some [name]
  | Void -> None

  let sorted_leaf_seq trees =
    trees |> List.filter (function File _ -> true | _ -> false)
          |> List.sort compare_names
          |> List.to_seq

  let of_tree_list name trees =
    let total_weight = trees |> List.to_seq |> Seq.map weight |> Seqs.float_sum in
    if total_weight > 0. then
      let non_void = List.filter ((<>) Void) trees in
      let children = List.map (fun x -> x, nan) non_void in
      Dir { name; name_key = lazy (Strings.FileManagerSort.key name); weight = total_weight; sum_of_local_weights = nan;
        children; sorted_leaf_children = lazy (sorted_leaf_seq non_void) }
    else
      Void

  let of_tree_seq name trees =
    of_tree_list name (List.of_seq trees)

  let rec shuffle = function
    | File _ as f -> f
    | Dir d ->
      let children = BatList.shuffle (List.map (fun (t, w) -> shuffle t, w) d.children) in
      Dir { d with children }
    | Void -> Void

  let rec sort_by_name = function
    | File _ as f -> f
    | Dir d ->
      let children =
        List.sort
          (fun (t1, _) (t2, _) -> compare_names t1 t2)
          (List.map (fun (t, w) -> sort_by_name t, w) d.children)
      in Dir { d with children }
    | Void -> Void

  let rec remove test tree =
    match tree with
    | Dir d ->
        let children1 = d.children |> List.map (fun (t, _) -> remove test t) |> List.filter ((<>) Void) in
        let total_weight = children1 |> List.to_seq |> Seq.map weight |> Seqs.float_sum in
        if total_weight = 0. then
          Void
        else
          Dir { d with weight = total_weight; sum_of_local_weights = nan;
            children = List.map (fun t -> t, nan) children1; sorted_leaf_children = lazy (sorted_leaf_seq children1) }
    | File f when test f.name -> Void
    | File _ -> tree
    | Void -> assert false

  let to_array t =
    let a = Array.make (num_files t) "" in
    let i = ref ~-1 in
    iter (fun s -> a.(!++i) <- s) t;
    a

  let rec to_seq = function
    | Dir d -> d.children |> List.to_seq |> Seq.map (fun (t, _) -> to_seq t) |> Seq.concat
    | File file -> Seq.return file.path
    | Void -> Seq.empty


  module OfPathList = struct

    type file_map_value =
      | MFile of { path : string }
      | MDir of { contents : file_map }

    and file_map = file_map_value StringMap.t

    let add_path_to_file_map file_map path =
      let rec aux path parts file_map =
        match parts with
        | [] -> failwith "Invalid argument (parts=[])"
        | name::_ when file_is_ignored name -> file_map
        | [file_name] -> StringMap.add file_name (MFile { path }) file_map
        | dir_name::rest when dir_should_be_flattened dir_name ->
          (* Directly add path to ancestor map *)
          aux path rest file_map
        | dir_name::rest ->
          (* Get or create dir contents map *)
          let contents0 =
            match StringMap.find_opt dir_name file_map with
            | None -> StringMap.empty
            | Some (MDir { contents }) -> contents
            | Some (MFile _) -> failwith (dir_name ^ " can't be both a file and a dir")
          in
          (* Add path to it *)
          let contents = aux path rest contents0 in
          StringMap.add dir_name (MDir { contents }) file_map
      in
      aux path (Files.path_parts_no_dot path) file_map

    let file_map_of_file_path_list (l : string list) =
      List.fold_left add_path_to_file_map (StringMap.empty) l

    let rec filetree_of_file_map_value (name, value) =
      match value with
      | MFile { path } -> File { name; name_key = lazy (Strings.FileManagerSort.key name); path; weight = 1. }
      | MDir { contents } -> of_tree_list name (filetree_list_of_map contents)

    and filetree_list_of_map m =
      StringMap.to_seq m |> Seq.map filetree_of_file_map_value |> List.of_seq

    let build l =
      l |> List.filter (fun f -> f <> "" && Unix.((stat f).st_kind <> S_DIR)) (* Remove dirs *)
        |> file_map_of_file_path_list
        |> filetree_list_of_map
        |> of_tree_list "[root]"

  end


  module OfFileSystem = struct

    let rec tree_seq_of_path full_path name_only =
      if Sys.is_directory full_path then
        let child_names =
          try
          	Sys.readdir full_path
          with Sys_error msg ->
            Log.warn "Sys_error: %s" msg;
            [||]
        in
        let trees =
          child_names
            |> Array.to_seq
            |> Seq.filter (fun child_name -> not (file_is_ignored child_name))
            |> Seq.map (fun child_name -> tree_seq_of_path (Filename.concat full_path child_name) child_name)
            |> Seq.concat
        in
        if path_should_be_flattened full_path then
          trees
        else
          Seq.return (of_tree_seq name_only trees)

      else
        Seq.return
          (File { name = name_only; name_key = lazy (Strings.FileManagerSort.key name_only); path = full_path;
            weight = 1. })

    let build roots =
      roots
        |> List.to_seq
        |> Seq.filter (fun root -> not (path_is_ignored root))
        |> Seq.map (fun root -> tree_seq_of_path root (Filename.basename root) |> of_tree_seq root)
        |> of_tree_seq "[root]"

  end


  (* == Postprocessing == *)

  module Postproc = struct

    let file_timestamp_half_life_days = ref 0.

    let file_creation_and_modification_timestamps path =
      let command = [| "/usr/bin/stat"; "-c"; "%W,%Y"; path |] in
      let channel = Unix.open_process_args_in command.(0) command in
      let output = input_line channel in
      ignore (Unix.close_process_in channel);
      match String.split_on_char ',' output with
      | [creation; modification] -> float_of_string creation, float_of_string modification
      | _ -> failwith ("Invalid stat output: " ^ output)

    let halflife_weight half_life_seconds timestamp =
      let decay_rate = Stdlib.log 2. /. half_life_seconds in
      exp (-. decay_rate *. (Unix.gettimeofday () -. timestamp))

    let file_weight path =
      if !file_timestamp_half_life_days = 0. then
        1.
      else
        (* Get whichever is latest between creation and modification timestamp, in seconds *)
        let creation_time, modification_time = file_creation_and_modification_timestamps path in
        halflife_weight (!file_timestamp_half_life_days *. 86400.) (max creation_time modification_time)

    let tree_weigh weight_list tree =
      if weight_list = [] then
        invalid_arg "tree_weigh: weight_list = []";
      assert (match tree with Dir _ -> true | _ -> false);
      let rec aux first_weight next_weights = function
      | Dir d ->
        let fw, nw =
          match next_weights with
          | [] -> first_weight, []
          | w::ws -> w, ws
        in
        let children = d.children |> List.map (fun (t, _) -> aux fw nw t) |> List.filter (fun (t, _) -> t <> Void) in
        let total_weight = children |> List.to_seq |> Seq.map (fun (t, _) -> weight t) |> Seqs.float_sum in
        let sum_of_local_weights = children |> List.to_seq |> Seq.map snd |> Seqs.float_sum in
        if total_weight < 0.0000001 then
          Void, 0.
        else
          (Dir { d with weight = total_weight; sum_of_local_weights; children }, first_weight total_weight)
      | File f ->
        let weight = file_weight f.path in
        if weight < 0.0000001 (* sanity check because floats *) then
          Void, 0.
        else
          (File { f with weight }, first_weight weight)
      | Void -> assert false
      in
      fst (aux (fun _ -> nan) weight_list tree)

  end
end

open FileTree.T


module FileTypes = struct

  type file_type = Audio | Image | Subtitles | Video | Other

  let subtitles_extensions =
    [| "ass"; "smi"; "srt"; "ssa"; "sub"; "sup"; "vtt" |]
  let noise_extensions =
    Array.append subtitles_extensions [| "exe"; "idx" |]

  let file_extension f =
    let dot_ext = Filename.extension f in
    if dot_ext <> "" then
      String.sub dot_ext 1 (String.length dot_ext - 1)
    else
      ""

  let is_avnoise file_name =
    let ext = file_extension file_name in
    if Array.mem ext noise_extensions then
      true
    else if ext = "gif" then
      false
    else begin
      match Mime_types.map_extension ext with
      | t when String.starts_with ~prefix:"image/" t -> true
      | t when String.starts_with ~prefix:"text/" t -> true
      | _ -> false
    end

  let is_image_noise file_name =
    let ext = file_extension file_name in
    Array.mem ext noise_extensions
    || String.starts_with ~prefix:"text/" @@ Mime_types.map_extension ext

  let main_type = ref None
  let mode_music = ref None (* None = default = auto *)

  let count_files_by_type tree =
    let num_audio_files = ref 0 in
    let num_image_files = ref 0 in
    let num_video_files = ref 0 in
    FileTree.iter (fun file ->
      match Magic_mime.lookup file with
      | t when String.starts_with ~prefix:"audio/" t -> num_audio_files += 1
      | t when String.starts_with ~prefix:"image/" t -> num_image_files += 1
      | t when String.starts_with ~prefix:"video/" t -> num_video_files += 1
      | _ -> ()
    ) tree;
    main_type := Some(
      if !num_image_files >= !num_audio_files + !num_video_files then
        Image
      else if !num_audio_files > !num_video_files then
        Audio
      else
        Video
    );
    if !mode_music = None then
      mode_music := Some (!main_type = Some Audio)

end


(* = Next file selection = *)

module Selection = struct

  module Types = struct

    type weight = Constant | Linear | Sqrt | SqrtSqrt

    type order =
    | Alpha
    | Pick of { pre_order : order; golden : bool; weight_list : weight list}
    | Raw
    | Shuffle

  end

  open Types

  let order_is_random = function
    | Alpha | Raw -> false
    | Pick _ | Shuffle -> true


  (* == Deterministic == *)

  let build_alpha_seq filetree : string Seq.t =
    FileTree.to_seq (FileTree.sort_by_name filetree)

  let build_raw_seq filetree : string Seq.t =
    FileTree.to_seq filetree


  (* == Random == *)

  let build_shuffle_seq filetree : string Seq.t =
    let files = FileTree.to_array filetree in
    let next_seq () =
      BatArray.shuffle files;
      Array.to_seq files
    in
    Seq.concat (Seq.forever next_seq)


  (* === Picking === *)

  let max_series_length = ref 1

  (** Pick file at abscissa x0 in t *)
  (* Superseded by pick_series below but kept for future reference because simpler to understand *)
  (*
  let pick_file x0 tree =
    assert (0. <= x0 && x0 <= 1.);
    let rec aux path x0 = function
      | Dir (name, _, sum, l0) ->
        let x = ref (x0 *. sum) in
        let l = ref l0 in
        while !x > snd (hd !l) do
          x := !x -. snd (hd !l);
          l := tl !l
        done;
        let subtree, subtree_weight = hd! l in
        aux (Filename.concat path name) (!x /. subtree_weight) subtree
      | File f -> Filename.concat path f
      | Void -> assert false
    in
    aux "" x0 tree
  *)

  (** Pick the file at abscissa x0 and the following files whose name differ only by a number *)
  let pick_series x0 tree : string list =
    assert (0. <= x0 && x0 <= 1.);
    let rec aux x0 = function
      | Dir d ->
        let x = ref (Floats.intify (x0 *. d.sum_of_local_weights)) in
        let c = ref d.children in
        while !x > Floats.intify (snd (List.hd !c)) do
          x := !x - Floats.intify (snd (List.hd !c));
          c := List.tl !c
        done;
        (match List.hd !c with
         | Dir _ as subtree, subtree_weight -> aux (Floats.deintify !x /. subtree_weight) subtree
         | File f as tf, _ ->
           (* Return f and the following files that differ only by a number *)
           if !max_series_length > 1 then begin
             let series =
               Lazy.force d.sorted_leaf_children
               |> Seq.drop_while (fun f1 -> FileTree.name f1 <> f.name)
               |> Seq.take !max_series_length
               |> Seq.filter_map (function
                 | File f1 as tf1 when
                     Strings.FileManagerSort.keys_are_in_sequence (FileTree.name_key tf) (FileTree.name_key tf1)
                   -> Some f1.path
                 | _ -> None
               )
               |> List.of_seq
             in
             match series with
             | [path] -> [path; path] (* If no series after f, return it twice *)
             | _ -> series
           end else
             [f.path]
         | Void, _ -> assert false)
      | File f -> [f.path]
      | Void -> assert false
    in
    aux x0 tree

  (** Stateless pick *)
  let build_stateless_seq weight_list tree : string Seq.t =
    let t = FileTree.Postproc.tree_weigh weight_list tree in
    Seq.concat
      (Seq.forever
         (fun () ->
            List.to_seq (pick_series (Random.float 1.) t) ))

  (** Circular pick with golden angle increment *)
  let build_golden_seq pre_order weight_list tree : string Seq.t =
    let pre_ordered_tree =
      match pre_order with
      | Alpha -> FileTree.sort_by_name tree
      | Raw -> tree
      | Shuffle -> FileTree.shuffle tree
      | _ -> failwith "Wrong preorder"
    in
    let weighed_tree = FileTree.Postproc.tree_weigh weight_list pre_ordered_tree in
    let golden_angle = (3. -. sqrt 5.) /. 2. in
    let mymod x = if x < 1. then x else x -. 1. in
    let x = ref (Random.float 1.) in
    Seq.concat
      (Seq.forever
         (fun () ->
            x := mymod (!x +. golden_angle);
            List.to_seq (pick_series !x weighed_tree) ))

  let fun_of_weight = function
    | Constant -> (fun _ -> 1.)
    | Linear -> (fun x -> x)
    | Sqrt -> sqrt
    | SqrtSqrt ->
        let y = 1. /. sqrt 2. in
        (fun x -> x ** y)

  let build_pick_seq pre_order golden weight_list tree : string Seq.t =
    let builder_function = if golden then build_golden_seq pre_order else build_stateless_seq in
    builder_function (List.map fun_of_weight weight_list) tree


  (* == Main == *)

  let build_seq order : tree -> string Seq.t =
    match order with
      (* Deterministic *)
      | Alpha -> build_alpha_seq
      | Raw -> build_raw_seq

      (* Random *)
      | Pick p -> build_pick_seq p.pre_order p.golden p.weight_list
      | Shuffle -> build_shuffle_seq

end

open Selection.Types


(* = UI = *)
(* == Command line parsing == *)

module Params = struct

  module PickModes = struct

    (* Weights *)

    let parse_weight = function
      | 'c' -> Constant
      | 'l' -> Linear
      | 'q' -> Sqrt
      | 'Q' -> SqrtSqrt
      | _ -> invalid_arg "parse_weight"

    let string_of_weight = function
    | Constant -> "c"
    | Linear -> "l"
    | Sqrt -> "q"
    | SqrtSqrt -> "Q"

    let parse_submode s =
      if String.length s <> 1 then
        invalid_arg "parse_submode";
      parse_weight s.[0]


    (* First mode *)

    let pre_order = ref Shuffle
    let golden = ref false
    let first_weight = ref Linear

    let parse_first_mode_char c =
      match c with
        | 'a' -> pre_order := Alpha
        | 'h' -> pre_order := Shuffle
        | 'r' -> pre_order := Raw
        | 'c' | 'l' | 'q' | 'Q' -> first_weight := parse_weight c
        | 'g' -> golden := true
        | 'u' -> (* Uniform: stateless linear pick *)
            golden := false;
            first_weight := Linear
        | _ -> assert false

    let parse_positive s =
      if s <> "_" then
        String.iter parse_first_mode_char s

    let parse_negative _ =
      assert false

    let parse_first_pick_mode s =
      match Str.split_delim (Str.regexp_string "-") s with
        | [pos] ->
            parse_positive s
        | [pos; neg] ->
            parse_positive pos;
            parse_negative neg
        | [] -> ()
        | _ ->
            crash ("More than one '-' in pick mode " ^ s)


    (* Main *)

    let parse s =
      let next_weights = ref [] in
      (match Str.split_delim (Str.regexp_string "/") s with
        | h::t ->
            parse_first_pick_mode h;
            next_weights := List.map parse_submode t
        | [] -> ());
      Pick { pre_order = !pre_order; golden = !golden; weight_list = !first_weight :: !next_weights }

  end


  (* === Main === *)

  let first_file = ref ""
  let input_list = ref ""
  let roots = ref []

  let max_files_to_open = ref None
  let order = ref Shuffle

  let break_delay = ref 0.7 (* float, in seconds *)
  let command = ref `MPlayer
  let list_mode = ref false
  let v_mode = ref false

  let make_order_pick () =
    match !order with
    | Pick _ -> ()
    | _ -> order := PickModes.parse ""

  let myspeclist =
    let open Arg in
  [
    ["--input-list"; "-i"], Set_string input_list, "FILE Use the given file list instead of browsing the filesystem";
    ["--first"; "-f"], Set_string first_file, "FILE First file to open";
    ["--more"; "-+"], Unit FileTree.set_dont_ignore_plus, " Don't exclude files starting with +, only ._~";

    ["-n"], Int (fun n -> max_files_to_open := Some n), "N Open/output at most N files";
    ["--alpha"; "-a"], Unit (fun () -> order := Alpha),
      " Read all files once in file-manager order, i.e., in lexical order except '9' < '10'";
    ["--pick"; "-p"], String (fun s -> order := PickModes.parse s), "MODE Pick files according to the given pick mode";
    ["--raw"; "-r"], Unit (fun () -> order := Raw), " Read all files once without ordering or shuffling them";
    ["--shuffle"], Unit (fun () -> order := Shuffle), " (default) Shuffle all files then loop through them";
    ["--half-life"],
      Float (fun half_life -> make_order_pick (); FileTree.Postproc.file_timestamp_half_life_days := half_life),
      "DAYS (float) Decrease file weight since creation/modification (default = 0. = don't)";
    ["--series"], Int (fun n -> make_order_pick (); Selection.max_series_length := n),
      "N Max length of a number series (default = 1)\n";

    ["--list"; "-l"], Set list_mode, " Print a list of files, don't open them or interact";
    ["--command"; "-c"], String (fun s -> command := `Custom s), "string Command with which to open files";
    ["--mplayer"], Unit (fun () -> command := `MPlayer), " (default)";
    ["--vlc"], Unit (fun () -> command := `VLC), " ";
    ["--xdg"], Unit (fun () -> command := `XDG), " (xdg-open)";
    ["--break"; "-b"], Set_float break_delay,
      Printf.sprintf "DELAY Delay to interact between playing two files, in seconds (default = %F)" !break_delay;
    ["--music"; "-m"], Unit (fun () -> FileTypes.mode_music := Some true),
      " Only play audio even for video files (MPlayer only)";
    ["--not-music"; "-M"], Unit (fun () -> FileTypes.mode_music := Some false),
      " Don't do that (default = follow main file type)";
    ["-V"], Set v_mode, " Shift video colors (MPlayer only)"
  ]

  let speclist =
    Arg.align
      (List.flatten
         (List.map
            (fun (l, s, d) ->
              let k = List.hd l in
              (k, s, d) :: List.map (fun k' -> k', s, " ") (List.tl l))
            myspeclist))

  let usage_msg =
    "Usage: 0pen (option|path)*\n" ^
    "Ex: 0pen -m toto -f toto/africa.ogg"

  let parse_command_line () =
    Arg.parse speclist (Lists.push roots) usage_msg;

    (match !input_list, !roots with
     | "", [] -> input_list := "-"
     | "", _ -> roots := List.rev !roots
     | _, [] -> ()
     | _, _ -> crash "Can't provide both an input list file and roots as command-line arguments");

    if !FileTree.Postproc.file_timestamp_half_life_days <> 0. then
      (match !order with Pick _ -> () | _ -> Log.warn "Half-life incompatible with non-pick order");
    if !Selection.max_series_length <> 1 then
      (match !order with Pick _ -> () | _ -> Log.warn "Series incompatible with non-pick order")

end


module Play = struct

  (* == Command wrappers  #data == *)

  module Players = struct

    let use_playtag : bool = Sys.command "which playtag >/dev/null" = 0

    let subs_of_video vid =
      let vid_file_name_dot_noext = Filename.(remove_extension (basename vid)) ^ "." in
      let dir_name = Filename.dirname vid in
      Sys.readdir dir_name
      |> Array.to_seq
      |> Seq.filter_map (fun file_name ->
          if
            String.starts_with ~prefix:vid_file_name_dot_noext file_name
            && Array.mem (FileTypes.file_extension file_name) FileTypes.subtitles_extensions
          then
            Some (Filename.concat dir_name file_name)
          else
            None
      )
      |> List.of_seq


    let protect_file path =
      Filename.quote (if use_playtag && path.[0] = '-' then "./" ^ path else path)

    let command start files =
      let s = String.concat " " (start :: (List.map protect_file files)) in
      Sys.command s


    let mplayer ?(fs = true) ?(mirror = false) ?(sub = []) ?(vid = true) ?(v_mode = false) file =
      let escape_subfile s =
        Str.global_replace (Str.regexp_string ",") "\\\\," s
      in
      let args = ref ["-msglevel all=0:statusline=6"] in
      if vid then begin (* vid-only options *)
        if fs then
          Lists.push args "-fs";
        if mirror then
          (* Buggy output in mplayer and can't find a CLI parameter that does anything for VLC *)
          Lists.push args "-vf mirror -af channels=2:2:0:1:1:0";
        (let sub = if sub <> [] then sub else subs_of_video file in
         if sub <> [] then
           let l = List.map (fun f -> Filename.quote (escape_subfile f)) sub in
           Lists.push args ("-sub " ^ String.concat "," l));
        if v_mode then
          Lists.push args "-vf hue=180"
      end else begin
        Lists.push args "-vo null"
      end;
      Lists.push args "mplayer 2>/dev/null";
      if use_playtag then
        Lists.push args "playtag";
      command (String.concat " " !args) [file]


    let vlc ?(fs = true) file =
      let args = ref ["--play-and-exit"] in
      if fs then
        Lists.push args "-f";
      args := List.rev !args;
      Lists.push args "vlc 2>/dev/null";
      if use_playtag then
        Lists.push args "playtag";
      command (String.concat " " !args) [file]


    let xdg file =
      command "xdg-open" [file]


    let custom_player name file =
      command name [file]


    let player command ?(fs = true) ?(mirror = false) ?(sub = []) ?(vid = true) ?(v_mode = false) file =
      match command with
        | `MPlayer -> mplayer ~fs ~mirror ~sub ~vid ~v_mode file
        | `VLC -> vlc ~fs file
        | `XDG -> xdg file
        | `Custom c -> custom_player c file

  end

  (* == Interactivity == *)

  let history = ref []
  and history_pointer = ref 0
  and candidate = ref ""

  let seq = ref Seq.empty


  (* === Echoing === *)

  let get_terminal_size () =
    (* from http://pleac.sourceforge.net/ ; fair use (license = GNU FDL) *)
    let in_channel = Unix.open_process_in "stty size" in
    try
      begin
        try
          Scanf.bscanf (Scanf.Scanning.from_channel in_channel) "%d %d"
            (fun rows cols ->
              ignore (Unix.close_process_in in_channel);
              (rows, cols))
        with End_of_file ->
          ignore (Unix.close_process_in in_channel);
          (0, 0)
      end
    with e ->
      ignore (Unix.close_process_in in_channel);
      raise e

  let blank_line () : string =
    String.make (snd (get_terminal_size ())) ' '

  (** Replace the current line with the given string *)
  let echo s =
    Printf.printf "\r%s\r%s%!" (blank_line ()) s


  (* === File selection === *)

  let select_previous_file () =
    if !history_pointer < List.length !history - 1 then
      (incr history_pointer;
       candidate := List.nth !history !history_pointer;
       echo ("^ " ^ !candidate) )

  let select_next_file () =
    if !history_pointer = 0 then
      (candidate := Seqs.pop seq;
       history := !candidate :: !history;
       echo ("> " ^ !candidate);
       print_newline () )
    else
      (decr history_pointer;
       candidate := List.nth !history !history_pointer;
       echo ("^ " ^ !candidate) )


  (* === Playing === *)

  let play_candidate () =
    let choose_mirror () =
      false && (not (Selection.order_is_random !Params.order) || Random.bool ())
    in
    let v_mode = !Params.v_mode && (not (Selection.order_is_random !Params.order) || Random.bool ()) in
    if !history_pointer <> 0 then
      print_newline ();
    ignore (Players.player !Params.command ~mirror:(choose_mirror ())
              ~vid:(!FileTypes.mode_music = Some false)  ~v_mode:v_mode  !candidate )


  (* === Keyboard input === *)

  module KbInput = struct

    let init_interactive_mode () =
      let open Unix in
      let tcattr = tcgetattr stdin in
      tcattr.c_echo <- false;
      tcattr.c_icanon <- false;
      tcattr.c_vmin <- 1;
      tcsetattr stdin TCSANOW tcattr

    let on_pause = ref false

    let handle_ansi_code = function
      | 'A' (* up *) -> select_previous_file ()
      | 'B' (* down *) -> select_next_file ()
      | _ -> ()

    let handle_input_char =
      let stdin = Stdlib.stdin in
      function
        | '\027' ->
          ignore (input_char stdin);
          handle_ansi_code (input_char stdin)
        | 'j' -> handle_ansi_code 'B'
        | 'k' -> handle_ansi_code 'A'
        | 'q' -> raise Sys.Break
        | ' ' -> on_pause := not !on_pause
        | '\n' when !on_pause -> on_pause := false
        | _ -> ()

    let rec interact () =
      (* If we're on pause or if the user gives input before break_delay *)
      if !on_pause || Unix.select [Unix.stdin] [] [] !Params.break_delay <> ([], [], []) then begin
        (* wait for and react to input *)
        handle_input_char (input_char Stdlib.stdin);
        interact ()
      end

  end

  let play (seq0 : string Seq.t) =
    seq := seq0;
    Sys.catch_break true;
    try
      KbInput.init_interactive_mode ();
      let f () =
        select_next_file ();
        KbInput.interact ();
        play_candidate ()
      in
      for i = 1 to match !Params.max_files_to_open with Some n -> n | None -> max_int do
        f ()
      done
    with Seqs.End_of_seq | Sys.Break ->
      echo ""

end


(* == Main == *)

module Main = struct

  let print_list seq =
    let seq_ref = ref seq in
    try
      for i = 1 to match !Params.max_files_to_open with Some n -> n | None -> 1000 do
        print_endline (Seqs.pop seq_ref)
      done
    with Seqs.End_of_seq -> ()

  let main () =
    Params.parse_command_line ();

    (* Build file seq *)
    Log.debug "Building file tree";
    let file_tree =
      if !Params.input_list <> "" then
        let ic = if !Params.input_list = "-" then In_channel.stdin else In_channel.open_text !Params.input_list in
        let lines = ref [] in
        begin try
          while true do
            lines := input_line ic :: !lines;
          done
        with End_of_file ->
          close_in ic;
          lines := List.rev !lines
        end;
        ref (FileTree.OfPathList.build !lines)

      else begin
        begin match !Params.roots with
        | [s] when not (Sys.is_directory s) && !Params.first_file = "" && Selection.order_is_random !Params.order ->
            Params.first_file := s;
            Params.roots := [Filename.dirname s]
        | _ -> ()
        end;
        ref (FileTree.OfFileSystem.build !Params.roots)
      end
    in

    if FileTree.num_files !file_tree = 0 then
      crash "No files to open.";

    (* Determine main file type to set music mode and remove irrelevant files from the tree *)
    FileTypes.count_files_by_type !file_tree;
    begin match !FileTypes.main_type with
    | Some (Audio | Video) -> file_tree := FileTree.remove FileTypes.is_avnoise !file_tree
    | Some Image -> file_tree := FileTree.remove FileTypes.is_image_noise !file_tree
    | _ -> ()
    end;

    (* Log example weighing when using more than one weight *)
    begin match !Params.order with
    | Pick ({ weight_list = _::_::_ } as p) ->
      begin match FileTree.first_path_parts !file_tree with
      | Some parts ->
        let weights =
          p.weight_list |> List.to_seq |> Seq.map (fun w -> "." ^ Params.PickModes.string_of_weight w ^ ".")
        in
        Seq.interleave (List.to_seq parts) weights
        |> List.of_seq
        |> String.concat "/"
        |> Log.info "Example weighing: %s"
      | None -> assert false
      end
    | _ -> ()
    end;

    Log.debug "Initializing play sequence";
    let seq =
      let seq0 = Selection.build_seq !Params.order !file_tree in
      if !Params.first_file <> "" then
        Seq.cons !Params.first_file seq0
      else
        seq0
    in

    (* Print or play *)
    if !Params.list_mode || not Unix.(isatty stdin && isatty stdout) then
      print_list seq
    else
      Play.play seq

end


let _ =
  Unix.handle_unix_error Main.main ()
