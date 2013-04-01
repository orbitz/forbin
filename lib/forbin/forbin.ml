module String_map = Map.Make(String)

type t = { nick        : string
	 ; set_factoid : Re.re
	 ; get_factoid : Re.re
	 ; db          : string String_map.t
	 ; in_chan     : in_channel
	 ; db_path     : string
	 ; ctl         : string
	 }

let safe f =
  try
    Some (f ())
  with
    | _ ->
      None

let safe_input_line in_chan =
  safe (fun () -> input_line in_chan)

let safe_open_in path =
  safe (fun () -> open_in path)

let safe_index s ch =
  safe (fun () -> String.index s ch)

let safe_re_exec re s =
  safe (fun () -> Re.get_all (Re.exec re s))

let safe_map_get k db =
  safe (fun () -> String_map.find k db)

let lsplit2 ~on s =
  match safe_index s on with
    | Some idx ->
      let l = String.sub s 0 idx in
      let r = String.sub s (idx + 1) (String.length s - idx - 1) in
      Some (l, r)
    | None ->
      None

let make_set_factoid_re nick =
  Re.compile
    (Re.seq
       [ Re.bol
       ; Re.no_case (Re.str nick)
       ; Re.alt [Re.char ':'; Re.char ';'; Re.char ','; Re.char ' ']
       ; Re.rep Re.space
       ; Re.group (Re.rep1 (Re.alt [Re.alpha; Re.punct]))
       ; Re.rep1 Re.space
       ; Re.group (Re.alt [Re.str "is reply"; Re.str "is"])
       ; Re.rep1 Re.space
       ; Re.group (Re.rep1 Re.any)
       ])

let make_get_factoid_re nick =
  Re.compile
    (Re.seq
       [ Re.bol
       ; Re.no_case (Re.str nick)
       ; Re.alt [Re.char ':'; Re.char ';'; Re.char ','; Re.char ' ']
       ; Re.rep Re.space
       ; Re.group (Re.rep1 (Re.alt [Re.alpha; Re.punct]))
       ; Re.rep Re.space
       ; Re.group (Re.rep Re.any)
       ])

let cmd ctl c =
  let fout = open_out_gen [Open_append] 0o666 ctl in
  output_string fout (c ^ "\n");
  close_out fout

let send_msg t dst msg = cmd t.ctl ("MSG " ^ dst ^ " " ^ msg)

let rec read_db in_chan db =
  match safe_input_line in_chan with
    | Some l -> begin
      match lsplit2 ~on:'\t' l with
	| Some (name, value) ->
	  read_db
	    in_chan
	    (String_map.add name value db)
	| None ->
	  read_db in_chan db
    end
    | None ->
      db

let read_db_if_exists path =
  match safe_open_in path with
    | Some in_chan -> begin
      let ret = read_db in_chan String_map.empty in
      close_in in_chan;
      ret
    end
    | None ->
      String_map.empty

let write_db db db_path =
  let fout = open_out db_path in
  String_map.iter
    (fun k v ->
      Printf.fprintf fout "%s\t%s\n" k v)
    db;
  close_out fout

let set_factoid factoid value t =
  let t = { t with db = String_map.add factoid value t.db } in
  write_db t.db t.db_path;
  t

let parse_msg msg t =
  match safe_re_exec t.set_factoid msg with
    | Some [| _; factoid; "is reply"; value |] ->
      Some (`Set (factoid, value))
    | Some [| _; factoid; "is"; value |] ->
      Some (`Set (factoid, factoid ^ " is " ^ value))
    | _ -> begin
      match safe_re_exec t.get_factoid msg with
	| Some [| _; factoid; args |] ->
	  Some (`Get (factoid, args))
	| _ ->
	  None
    end

let parse_args args =
  match lsplit2 ~on:' ' args with
    | Some (src, rest) -> begin
      match lsplit2 ~on:' ' rest with
	| Some (dst, msg) ->
	  Some (src, dst, msg)
	| None ->
	  None
    end
    | None ->
      None

let update_nick nick t =
  { t with
    nick        = nick
  ; set_factoid = make_set_factoid_re nick
  ; get_factoid = make_get_factoid_re nick
  }

let get_factoid factoid args db =
  match safe_map_get factoid db with
    | Some v ->
      Some v
    | None ->
      None

let rec loop t =
  match safe_input_line t.in_chan with
    | Some l ->
      eval_input l t
    | None ->
      ()
and eval_input line t =
  match lsplit2 ~on:' ' line with
    | Some ("MSG", args) ->
      eval_msg args t
    | Some ("WHOAMI", nick) ->
      loop (update_nick nick t)
    | Some _ ->
      loop t
    | None ->
      loop t
and eval_msg args t =
  match parse_args args with
    | Some (src, dst, msg) -> begin
      interpret_msg src dst msg t
    end
    | None ->
      loop t
and interpret_msg src dst msg t =
  let dst =
    if dst.[0] = '#' then
      dst
    else
      src
  in
  match parse_msg msg t with
    | Some (`Set (factoid, value)) -> begin
      let t = set_factoid factoid value t in
      send_msg t dst (factoid ^ " set");
      loop t
    end
    | Some (`Get (factoid, args)) -> begin
      match get_factoid factoid args t.db with
	| Some resp -> begin
	  cmd t.ctl (Printf.sprintf "MSG %s %s, %s" dst src resp);
	  loop t
	end
	| None ->
	  loop t
    end
    | None ->
      loop t

let start ~ctl ~in_chan ~db_path =
  let rec read_whoami in_chan =
    match safe_input_line in_chan with
      | Some l -> begin
	match lsplit2 ~on:' ' l with
	  | Some ("WHOAMI", nick) ->
	    nick
	  | _ ->
	    read_whoami in_chan
      end
      | None ->
	failwith "ARGH"
  in
  cmd ctl "WHOAMI";
  let nick = read_whoami in_chan in
  loop
    { nick        = nick
    ; set_factoid = make_set_factoid_re nick
    ; get_factoid = make_get_factoid_re nick
    ; ctl         = ctl
    ; in_chan     = stdin
    ; db_path     = db_path
    ; db          = read_db_if_exists db_path
    }

let main () =
  start
    ~ctl:Sys.argv.(1)
    ~in_chan:stdin
    ~db_path:Sys.argv.(2)

let () = main ()
