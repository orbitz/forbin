module String_map = Map.Make(String)

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

  let re_exec re s =
    safe (fun () -> Re.get_all (Re.exec re s))

  let re_exec_of re s =
    safe (fun () -> Re.get_all_ofs (Re.exec re s))

  let map_get k db =
    safe (fun () -> String_map.find k db)
end

module Word = struct
  let rec nth n s =
    match n with
      | n when n < 1 ->
	None
      | 1 -> begin
	match Losic.String_ext.lsplit2 ~on:' ' s with
	  | Some (word, _) -> Some word
	  | None           -> Some s
      end
      | _ -> begin
	match Losic.String_ext.lsplit2 ~on:' ' s with
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
	match Losic.String_ext.lsplit2 ~on:' ' s with
	  | Some (_, rest) ->
	    drop (n - 1) rest
	  | None ->
	    None
      end
end

module Ctl = struct
  let cmd ctl c =
    let module Cb = Losic.Ctl_builder in
    let fout = open_out_gen [Open_append] 0o666 ctl in
    output_string fout (Cb.build c);
    close_out fout
end

module Db = struct
  let rec read_db in_chan db =
    match Safe.input_line in_chan with
      | Some l -> begin
	match Losic.String_ext.lsplit2 ~on:'\t' l with
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


module Forbin = struct
  type t = { nick        : string
	   ; set_factoid : Re.re
	   ; get_factoid : Re.re
	   ; db          : string String_map.t
	   ; db_path     : string
	   ; ctl         : string
	   }

  let var_re =
    Re.compile
      (Re.seq
	 [ Re.char '{'
	 ; Re.seq [Re.group (Re.rep1 Re.digit); Re.group (Re.opt (Re.char '-'))]
	 ; Re.char '}'
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

  let update_nick nick t =
    { t with
      nick        = nick
    ; set_factoid = make_set_factoid_re nick
    ; get_factoid = make_get_factoid_re nick
    }

  let rec replace_vars v args =
    let rpl ts te = function
      | Some w ->
	replace_vars (Losic.String_ext.splice ts (te - 1) v w) args
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

  let interpret_msg msg t =
    let module Om = Losic.Out_message in
    let dst =
      if Om.Msg.is_to_channel msg then
	Om.Msg.dst msg
      else
	Om.Msg.src msg
    in
    let module Om = Losic.Out_message in
    let module Cm = Losic.Ctl_message in
    match parse_msg (Om.Msg.msg msg) t with
      | Some (`Set (factoid, value)) -> begin
	let t = set_factoid factoid value t in
	Ctl.cmd
	  t.ctl
	  (Cm.Message.Msg (Cm.Msg.create ~dst (factoid ^ " set")));
	t
      end
      | Some (`Get (factoid, args)) -> begin
	match get_factoid factoid args t.db with
	  | Some resp -> begin
	    Ctl.cmd
	      t.ctl
	      (Cm.Message.Msg
		 (Cm.Msg.create
		    ~dst
		    (Om.Msg.src msg ^ ", " ^ resp)));
	    t
	  end
	  | None ->
	    t
      end
      | None ->
	t

  let init () =
    let module Cm = Losic.Ctl_message in
    Ctl.cmd
      Sys.argv.(1)
      (Cm.Message.Whoami);
    { nick        = ""
    ; set_factoid = make_set_factoid_re ""
    ; get_factoid = make_get_factoid_re ""
    ; ctl         = Sys.argv.(1)
    ; db_path     = Sys.argv.(2)
    ; db          = Db.read_db_if_exists Sys.argv.(2)
    }

  let destroy _ = ()

  let handle m t =
    let module Om = Losic.Out_message in
    match Om.message m with
      | Om.Message.Whoami nick ->
	update_nick nick t
      | Om.Message.Msg msg ->
	interpret_msg msg t
      | _ ->
	t

end

module Loop = Losic.Loop.Make(Forbin)

let main () =
  Loop.run stdin

let () = main ()
