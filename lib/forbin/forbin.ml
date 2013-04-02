module String_map = Map.Make(String)

type t = { nick        : string
	 ; set_factoid : Re.re
	 ; get_factoid : Re.re
	 ; db          : string String_map.t
	 ; in_chan     : in_channel
	 ; db_path     : string
	 ; ctl         : string
	 }

module Safe = struct
  let safe f =
    try
      Some (f ())
    with
      | _ ->
	None

  let input_line in_chan =
    safe (fun () -> input_line in_chan)

  let open_in path =
    safe (fun () -> open_in path)

  let index s ch =
    safe (fun () -> String.index s ch)

  let re_exec re s =
    safe (fun () -> Re.get_all (Re.exec re s))

  let re_exec_of re s =
    safe (fun () -> Re.get_all_ofs (Re.exec re s))

  let map_get k db =
    safe (fun () -> String_map.find k db)

  let lsplit2 ~on s =
    match index s on with
      | Some idx ->
	let l = String.sub s 0 idx in
	let r = String.sub s (idx + 1) (String.length s - idx - 1) in
	Some (l, r)
      | None ->
	None
end

module Word = struct
  let rec nth n s =
    match n with
      | n when n < 1 ->
	None
      | 1 -> begin
	match Safe.lsplit2 ~on:' ' s with
	  | Some (word, _) -> Some word
	  | None           -> Some s
      end
      | _ -> begin
	match Safe.lsplit2 ~on:' ' s with
	  | Some (_, rest) ->
	    nth (n - 1) rest
	  | None ->
	    None
      end

  let rec drop n s =
    match n with
      | n when n < 1 ->
	None
      | 1 ->
	Some s
      | n -> begin
	match Safe.lsplit2 ~on:' ' s with
	  | Some (_, rest) ->
	    drop (n - 1) rest
	  | None ->
	    None
      end
end

module Ctl = struct
  let cmd ctl c =
    let fout = open_out_gen [Open_append] 0o666 ctl in
    output_string fout (c ^ "\n");
    close_out fout

  let send_msg t dst msg = cmd t.ctl ("MSG " ^ dst ^ " " ^ msg)
end

module Db = struct
  let rec read_db in_chan db =
    match Safe.input_line in_chan with
      | Some l -> begin
	match Safe.lsplit2 ~on:'\t' l with
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
    match Safe.open_in path with
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
end

let make_set_factoid_re nick =
  Re.compile
    (Re.seq
       [ Re.bol
       ; Re.no_case (Re.str nick)
       ; Re.alt [Re.char ':'; Re.char ';'; Re.char ','; Re.char ' ']
       ; Re.rep Re.space
       ; Re.group (Re.rep1 (Re.alt [Re.alnum; Re.punct]))
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
       ; Re.group (Re.rep1 (Re.alt [Re.alnum; Re.punct]))
       ; Re.rep Re.space
       ; Re.group (Re.rep Re.any)
       ])

let set_factoid factoid value t =
  let factoid = String.lowercase factoid in
  let t = { t with db = String_map.add factoid value t.db } in
  Db.write_db t.db t.db_path;
  t

let parse_msg msg t =
  match Safe.re_exec t.set_factoid msg with
    | Some [| _; factoid; "is reply"; value |] ->
      Some (`Set (factoid, value))
    | Some [| _; factoid; "is"; value |] ->
      Some (`Set (factoid, factoid ^ " is " ^ value))
    | _ -> begin
      match Safe.re_exec t.get_factoid msg with
	| Some [| _; factoid; args |] ->
	  Some (`Get (factoid, args))
	| _ ->
	  None
    end

let parse_args args =
  match Safe.lsplit2 ~on:' ' args with
    | Some (src, rest) -> begin
      match Safe.lsplit2 ~on:' ' rest with
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

let var_re =
  Re.compile
    (Re.seq
       [ Re.char '{'
       ; Re.seq [Re.group (Re.rep1 Re.digit); Re.group (Re.opt (Re.char '-'))]
       ; Re.char '}'
       ])

let replace s e str n =
  String.sub str 0 s ^
    n ^
    String.sub str (e + 1) (String.length str - e - 1)

let rec replace_vars v args =
  let rpl ts te = function
    | Some w ->
      replace_vars (replace ts (te - 1) v w) args
    | None ->
      v
  in
  match Safe.re_exec_of var_re v with
    | Some [| (ts, te); (ws, we); (ms, me) |] when ms = me ->
      (* When there is no '-' *)
      let w_pos = int_of_string (String.sub v ws (we - ws)) in
      rpl ts te (Word.nth w_pos args)
    | Some [| (ts, te); (ws, we); _ |] ->
      let w_pos = int_of_string (String.sub v ws (we - ws)) in
      rpl ts te (Word.drop w_pos args)
    | Some _ ->
      v
    | None ->
      v

let get_factoid factoid args db =
  if Safe.re_exec var_re args = None then begin
    match Safe.map_get (String.lowercase factoid) db with
      | Some v ->
	Some (replace_vars v args)
      | None ->
	None
  end
  else
    None

let rec loop t =
  match Safe.input_line t.in_chan with
    | Some l ->
      eval_input l t
    | None ->
      ()
and eval_input line t =
  match Safe.lsplit2 ~on:' ' line with
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
      Ctl.send_msg t dst (factoid ^ " set");
      loop t
    end
    | Some (`Get (factoid, args)) -> begin
      match get_factoid factoid args t.db with
	| Some resp -> begin
	  Ctl.send_msg t dst (Printf.sprintf "%s, %s" src resp);
	  loop t
	end
	| None ->
	  loop t
    end
    | None ->
      loop t

let start ~ctl ~in_chan ~db_path =
  let rec read_whoami in_chan =
    match Safe.input_line in_chan with
      | Some l -> begin
	match Safe.lsplit2 ~on:' ' l with
	  | Some ("WHOAMI", nick) ->
	    nick
	  | _ ->
	    read_whoami in_chan
      end
      | None ->
	failwith "ARGH"
  in
  Ctl.cmd ctl "WHOAMI";
  let nick = read_whoami in_chan in
  loop
    { nick        = nick
    ; set_factoid = make_set_factoid_re nick
    ; get_factoid = make_get_factoid_re nick
    ; ctl         = ctl
    ; in_chan     = stdin
    ; db_path     = db_path
    ; db          = Db.read_db_if_exists db_path
    }

let main () =
  start
    ~ctl:Sys.argv.(1)
    ~in_chan:stdin
    ~db_path:Sys.argv.(2)

let () = main ()
