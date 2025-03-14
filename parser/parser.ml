open Links_core
(* open Links_core.Sugartypes *)

type position = {
    start_line: int;
    start_char: int;
    end_line: int;
    end_char: int;
} [@@deriving yojson]

type ast = 
    | Node of string * string * ast list
    | Leaf of string * string
    [@@deriving yojson]


(* Problems:  
    Problem: Parsing breaks when comments are in code
    
    Potential solution: From the LSP Server, only strip comments from submitted code

    Implemented: No
*)

(* let pos_to_position(pos: SourceCode.Position.t) : position =
    let open SourceCode.Position in
    let open Lexing in
    let mm = SourceCode.show pos in
    {
        start_line = pos.start.pos_lnum;
        start_char = pos.start_char;
        end_line = pos.end_line;
        end_char = pos.end_char;
    } *)


    
let rec convert_phrase_to_ast ?(offset=0) (phrase : Sugartypes.phrase) : ast =
    let open Links_core.Sugartypes in
    let open Links_core.SourceCode in
    let open Links_core.Operators in
    let open Links_core.CommonTypes in
    let pos_start = Position.start (WithPos.pos phrase) in
    let pos_end = Position.finish (WithPos.pos phrase) in
    (* Printf.printf "pos_start.pos_bol: %d\n" pos_start.pos_bol; *)
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (offset + pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
        
    
    match WithPos.node phrase with
        | Constant c -> 
            Leaf ("Constant: " ^ Constant.show c, pos_str)
        | Var v -> Leaf ("Variable: " ^ v, pos_str)
        | FreezeVar v -> Leaf ("FreezeVar: " ^ v, pos_str)
        | QualifiedVar vs -> Node ("QualifiedVar", pos_str, List.map (fun v -> Leaf (v, pos_str)) vs)
        | FunLit (_, _, fnlit, _) -> Node ("FunLit", pos_str, [convert_funlit_to_ast fnlit pos_str])
        | Spawn (_, _, p, _) -> Node ("Spawn", pos_str, [convert_phrase_to_ast p])
        | Query (p_opt, _, p3, _) -> 
            Node ("Query", pos_str, 
            match p_opt with
            | Some (p1, p2) -> 
                [convert_phrase_to_ast p1; convert_phrase_to_ast p2] @ [convert_phrase_to_ast p3]
            | None ->  [convert_phrase_to_ast p3])
        | RangeLit (p1, p2) -> Node ("RangeLit", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2])
        | ListLit (ps, _) -> Node ("ListLit", pos_str, List.map convert_phrase_to_ast ps)
        | Iteration (generators, p, where, orderby) ->
            let gen_nodes = List.map (fun gen ->
                match gen with
                | List (pat, source) ->
                    Node ("List", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast source])
                | Table (_, pat, source) ->
                    (* Node ("Table", pos_str, [convert_temporality_to_ast temp pos_str; convert_pattern_to_ast pat; convert_phrase_to_ast source]) *)
                    Node ("Table", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast source])
            ) generators in
            let where_node = match where with
                | Some w -> [Node ("Where", pos_str, [convert_phrase_to_ast w])]
                | None -> [] in
            let orderby_node = match orderby with
                | Some o -> [Node ("OrderBy", pos_str, [convert_phrase_to_ast o])]
                | None -> [] in
            Node ("Iteration", pos_str, gen_nodes @ where_node @ orderby_node @ [convert_phrase_to_ast p])
        | Escape (_, p) -> Node ("Escape", pos_str, [convert_phrase_to_ast p])
        | Section s -> Leaf ("Section: " ^ Section.show s, pos_str)
        | FreezeSection s -> Leaf ("FreezeSection: " ^ Section.show s, pos_str)
        | Conditional (p1, p2, p3) -> Node ("Conditional", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2; convert_phrase_to_ast p3])
        | Block (bindings, expr) -> 
            let child_positions = 
                (List.map (fun b -> WithPos.pos b) bindings) @
                [WithPos.pos expr] 
            in
            
            (* Find min start position and max end position using accessors *)
            let min_start = List.fold_left (fun acc pos -> 
                let start = Position.start pos in
                let start_line = start.Lexing.pos_lnum in
                let start_col = start.Lexing.pos_cnum - start.Lexing.pos_bol in
                let acc_line = (Position.start acc).Lexing.pos_lnum in
                let acc_col = (Position.start acc).Lexing.pos_cnum - (Position.start acc).Lexing.pos_bol in
                
                if start_line < acc_line || (start_line = acc_line && start_col < acc_col)
                then pos else acc
            ) (List.hd child_positions) (List.tl child_positions) in
            
            (* Similar changes for max_end calculation *)
            let max_end = List.fold_left (fun acc pos ->
                let end_pos = Position.finish pos in
                let end_line = end_pos.Lexing.pos_lnum in
                let end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
                let acc_line = (Position.finish acc).Lexing.pos_lnum in
                let acc_col = (Position.finish acc).Lexing.pos_cnum - (Position.finish acc).Lexing.pos_bol in
                
                if end_line > acc_line || (end_line = acc_line && end_col > acc_col)
                then pos else acc
            ) (List.hd child_positions) (List.tl child_positions) in
            
            (* Now generate position string using proper field references *)
            let start = Position.start min_start in
            let finish = Position.finish max_end in
            let block_pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
              start.Lexing.pos_lnum (start.Lexing.pos_cnum - start.Lexing.pos_bol + 2)
              finish.Lexing.pos_lnum (finish.Lexing.pos_cnum - finish.Lexing.pos_bol + 2)
            in
            Node ("Block", block_pos_str, List.map convert_binding_to_ast bindings @ [convert_phrase_to_ast expr])
        | InfixAppl ((_, op), lhs, rhs) -> Node ("InfixAppl", pos_str, [Leaf (BinaryOp.to_string op, pos_str); convert_phrase_to_ast lhs; convert_phrase_to_ast rhs])
        | Regex r -> Node("Regex", pos_str, [convert_regex_to_ast r])
        | UnaryAppl (_, p) -> Node ("UnaryAppl", pos_str, [convert_phrase_to_ast p])
        | FnAppl (fn, args) -> Node ("FnAppl", pos_str, convert_phrase_to_ast fn :: List.map convert_phrase_to_ast args)
        | TAbstr (_, p) -> Node ("TAbstr", pos_str, [convert_phrase_to_ast p])
        | TAppl (p, _) -> Node ("TAppl", pos_str, [convert_phrase_to_ast p])
        | TupleLit ps -> Node ("TupleLit", pos_str, List.map convert_phrase_to_ast ps)
        | RecordLit (fields, _) -> Node ("RecordLit", pos_str, List.map (fun (name, expr) -> Node (name, pos_str,[convert_phrase_to_ast expr])) fields)
        | Projection (p, name) -> Node ("Projection", pos_str, [convert_phrase_to_ast p; Leaf (name, pos_str)])
        | With (p, fields) -> Node ("With", pos_str, convert_phrase_to_ast p :: List.map (fun (name, expr) -> Node (name, pos_str, [convert_phrase_to_ast expr])) fields)
        | TypeAnnotation (p, _) -> Node ("TypeAnnotation", pos_str, [convert_phrase_to_ast p])
        | Upcast (p, _, _) -> Node ("Upcast", pos_str, [convert_phrase_to_ast p])
        | Instantiate p -> Node ("Instantiate", pos_str, [convert_phrase_to_ast p])
        | Generalise p -> Node ("Generalise", pos_str, [convert_phrase_to_ast p])
        | ConstructorLit (name, expr_opt, _) 
        -> 
        Node ("ConstructorLit", pos_str, 
            Leaf (name, pos_str) :: (
                match expr_opt with 
                    | Some expr -> [convert_phrase_to_ast expr] 
                    | None -> []
            )
        )
        | DoOperation (p, ps, _, _) -> Node ("DoOperation", pos_str, convert_phrase_to_ast p :: List.map convert_phrase_to_ast ps)
        | Operation name -> Leaf ("Operation: " ^ name, pos_str)
        | Handle h -> Node ("Handle", pos_str, [convert_handler_to_ast h])
        | Unlet p -> Node ("Unlet", pos_str, [convert_phrase_to_ast p])
        | Linlet p -> Node ("Linlet", pos_str, [convert_phrase_to_ast p])
        | Switch (p, cases, _) -> Node ("Switch", pos_str, convert_phrase_to_ast p :: List.map convert_case_to_ast cases)
        | Receive (cases, _) -> Node ("Receive", pos_str, List.map convert_case_to_ast cases)
        | DatabaseLit (p, _) -> Node ("DatabaseLit", pos_str, [convert_phrase_to_ast p])
        | TableLit t -> Node ("TableLit", pos_str, [convert_table_lit_to_ast t])
        (* | DBDelete (_, pat, p, _) -> Node ("DBDelete", pos_str,[convert_pattern_to_ast pat; convert_phrase_to_ast p]) *)
        | DBDelete (_, pat, p, where_opt) -> 
            let where_node = match where_opt with
                | Some where_clause -> [Node ("Where", pos_str, [convert_phrase_to_ast where_clause])]
                | None -> []
            in
            Node ("DBDelete", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast p] @ where_node)
        | DBInsert (_, p, columns, p_opt, popt) -> 
            Node ("DBInsert", pos_str, 
            [convert_phrase_to_ast p] @
            (* (List.map (fun col -> Node ("Column", WithPos.to_string (WithPos.position col), [Leaf (WithPos.node col, WithPos.to_string (WithPos.position col))])) columns) @ *)
            (* (List.map (fun col -> 
                (* let pos_start = Position.start (WithPos.pos col) in
                let pos_end = Position.finish (WithPos.pos col) in
                let col_pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
                    pos_start.pos_lnum
                    (pos_start.pos_cnum - pos_start.pos_bol+2)
                    pos_end.pos_lnum  
                    (pos_end.pos_cnum - pos_end.pos_bol+2) 
                in *)
                Node ("Column", "", [Leaf (WithPos.node col, "")])
            ) columns) @ *)
            (List.map (fun col -> Node ("Column", pos_str, List.map (fun c -> Leaf (c, pos_str)) [col])) columns) @
            [convert_phrase_to_ast p_opt] @
            (match popt with Some p -> [convert_phrase_to_ast p] | None -> [])
    )
        (* | DBUpdate (_, pat, p, _, fields) -> Node ("DBUpdate", pos_str, convert_pattern_to_ast pat :: convert_phrase_to_ast p :: List.map (fun (name, expr) -> Node (name, pos_str, [convert_phrase_to_ast expr])) fields) *)
        | DBUpdate (_, pat, p, where_opt, fields) -> 
            let where_node = match where_opt with
                | Some where_clause -> [Node ("Where", pos_str, [convert_phrase_to_ast where_clause])]
                | None -> []
            in
            let set_nodes = List.map (fun (name, expr) -> 
                Node ("SetField", pos_str, [Leaf (name, pos_str); convert_phrase_to_ast expr])
            ) fields in
            Node ("DBUpdate", pos_str, 
                convert_pattern_to_ast pat ::
                convert_phrase_to_ast p ::
                where_node @
                set_nodes
            )
    
        | DBTemporalJoin (_, p, _) -> Node ("DBTemporalJoin", pos_str, [convert_phrase_to_ast p])
        | LensLit (p, _) -> Node ("LensLit", pos_str, [convert_phrase_to_ast p])
        | LensSerialLit (p, _, _) -> Node ("LensSerialLit", pos_str, [convert_phrase_to_ast p])
        | LensKeysLit (p1, p2, _) -> Node ("LensKeysLit", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2])
        | LensFunDepsLit (p, _, _) -> Node ("LensFunDepsLit", pos_str, [convert_phrase_to_ast p])
        | LensDropLit (p1, _, _, p2, _) -> Node ("LensDropLit", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2])
        | LensSelectLit (p1, p2, _) -> Node ("LensSelectLit", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2])
        | LensJoinLit (p1, p2, p3, p4, p5, _) -> Node ("LensJoinLit", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2; convert_phrase_to_ast p3; convert_phrase_to_ast p4; convert_phrase_to_ast p5])
        | LensGetLit (p, _) -> Node ("LensGetLit", pos_str,[convert_phrase_to_ast p])
        | LensCheckLit (p, _) -> Node ("LensCheckLit", pos_str, [convert_phrase_to_ast p])
        | LensPutLit (p1, p2, _) -> Node ("LensPutLit", pos_str,  [convert_phrase_to_ast p1; convert_phrase_to_ast p2])
        (* | Xml (name, attrs, expr_opt, children) ->
            Node ("Xml", pos_str, Leaf (name, pos_str) ::
            List.map (fun (attr_name, exprs) ->
                (* Get overall attribute range *)
                let attr_pos_start = Position.start (WithPos.pos (List.hd exprs)) in
                let attr_pos_end = Position.finish (WithPos.pos (List.hd (List.rev exprs))) in
                let attr_pos_str = Printf.sprintf 
                    "{\"start\": {\"line\": %d, \"character\": %d}, \"end\": {\"line\": %d, \"character\": %d}}"
                    attr_pos_start.pos_lnum
                    (attr_pos_start.pos_cnum - attr_pos_start.pos_bol)
                    attr_pos_end.pos_lnum
                    (attr_pos_end.pos_cnum - attr_pos_end.pos_bol) in
                
                (* Process expressions with running position tracking *)
                let rec process_exprs remaining_exprs acc (current_pos : Lexing.position) =
                    match remaining_exprs with
                    | [] -> List.rev acc
                    | expr :: rest ->
                        (* Calculate expression position based on current running position *)
                        let f = Position.finish (WithPos.pos expr) in
                        let s = Position.start (WithPos.pos expr) in

                        let expr_len = f.pos_cnum - s.pos_cnum in
                        
                        let expr_pos_start = current_pos in
                        let expr_pos_end = { 
                            current_pos with 
                            pos_cnum = current_pos.pos_cnum + expr_len 
                        } in
                        
                        let expr_pos_str = Printf.sprintf 
                            "{\"start\": {\"line\": %d, \"character\": %d}, \"end\": {\"line\": %d, \"character\": %d}}"
                            expr_pos_start.pos_lnum
                            (expr_pos_start.pos_cnum - expr_pos_start.pos_bol)
                            expr_pos_end.pos_lnum
                            (expr_pos_end.pos_cnum - expr_pos_end.pos_bol) in
                        
                        (* Create node based on expression type *)
                        let node = match expr.node with
                        | Block (bindings, expr) when List.length bindings > 0 ->
                            (* Special handling for blocks to ensure correct positions for block contents *)
                            let block_pos_str = Printf.sprintf 
                                "{\"start\": {\"line\": %d, \"character\": %d}, \"end\": {\"line\": %d, \"character\": %d}}"
                                expr_pos_start.pos_lnum
                                (expr_pos_start.pos_cnum - expr_pos_start.pos_bol+2)
                                expr_pos_end.pos_lnum
                                (expr_pos_end.pos_cnum - expr_pos_end.pos_bol+2) in
                            
                            Node ("Expr", expr_pos_str, [
                                Node ("Block", block_pos_str, 
                                    List.map (fun binding -> convert_binding_to_ast binding) bindings @ [convert_phrase_to_ast expr])
                            ])
                        | _ ->
                            Node ("Expr", expr_pos_str, [convert_phrase_to_ast expr])
                        in
                        
                        (* Update the running position for the next expression *)
                        let next_pos = expr_pos_end in
                        process_exprs rest (node :: acc) next_pos
                in
                
                let expr_nodes = process_exprs exprs [] attr_pos_start in
                
                Node (("Attribute: " ^ attr_name), attr_pos_str, expr_nodes)
            ) attrs @
            (match expr_opt with
            | Some expr -> [convert_phrase_to_ast expr]
            | None -> []
            ) @
            List.map convert_phrase_to_ast children) *)
        (* | Xml (name, attrs, expr_opt, children) ->
            Node ("Xml", pos_str, Leaf (name, pos_str) ::
            List.map (fun (attr_name, exprs) ->
                (* Get overall attribute position from first and last expressions *)
                let attr_pos_start = Position.start (WithPos.pos (List.hd exprs)) in
                let attr_pos_end = Position.finish (WithPos.pos (List.hd (List.rev exprs))) in
                let attr_pos_str = Printf.sprintf 
                    "{\"start\": {\"line\": %d, \"character\": %d}, \"end\": {\"line\": %d, \"character\": %d}}"
                    attr_pos_start.pos_lnum
                    (attr_pos_start.pos_cnum - attr_pos_start.pos_bol+2)
                    attr_pos_end.pos_lnum
                    (attr_pos_end.pos_cnum - attr_pos_end.pos_bol+2) in
                
                (* Process each expression with its own correct position information *)
                let expr_nodes = List.map (fun expr ->
                    (* Get exact position for this specific expression *)
                    let expr_pos_start = Position.start (WithPos.pos expr) in
                    let expr_pos_end = Position.finish (WithPos.pos expr) in
                    
                    (* For debugging *)
                    Printf.printf "for attribute %s\n" attr_name;
                    Printf.printf "expr_pos_start.pos_cnum: %d\n" expr_pos_start.pos_cnum;
                    Printf.printf "expr_pos_start.pos_bol: %d\n" expr_pos_start.pos_bol;
                    
                    (* Create position string with correct character offsets *)
                    let expr_pos_str = Printf.sprintf 
                        "{\"start\": {\"line\": %d, \"character\": %d}, \"end\": {\"line\": %d, \"character\": %d}}"
                        expr_pos_start.pos_lnum
                        (expr_pos_start.pos_cnum - expr_pos_start.pos_bol+2)
                        expr_pos_end.pos_lnum
                        (expr_pos_end.pos_cnum - expr_pos_end.pos_bol+2) in
                    
                    (* Create expression node with correct position and properly convert its contents *)
                    match expr.node with
                    | Block (bindings, expr) when List.length bindings > 0 ->
                        (* Special handling for blocks to ensure correct positions for block contents *)
                        let block_pos_str = Printf.sprintf 
                            "{\"start\": {\"line\": %d, \"character\": %d}, \"end\": {\"line\": %d, \"character\": %d}}"
                            expr_pos_start.pos_lnum
                            (expr_pos_start.pos_cnum - expr_pos_start.pos_bol+2)
                            expr_pos_end.pos_lnum
                            (expr_pos_end.pos_cnum - expr_pos_end.pos_bol+2) in
                        
                        Node ("Expr", expr_pos_str, [
                            Node ("Block", block_pos_str, 
                                List.map (fun binding -> convert_binding_to_ast binding) bindings @ [convert_phrase_to_ast expr])
                        ])
                    | _ ->
                        Node ("Expr", expr_pos_str, [convert_phrase_to_ast expr])
                ) exprs in
                
                Node (("Attribute: " ^ attr_name), attr_pos_str, expr_nodes)
            ) attrs @
            (match expr_opt with
            | Some expr -> [convert_phrase_to_ast expr]
            | None -> []
            ) @
            List.map convert_phrase_to_ast children) *)
        | Xml (name, attrs, expr_opt, children) -> 
            Node ("Xml", pos_str, Leaf (name, pos_str) :: 
            (* (name, exprs) = attr *)
            List.map (fun (name, exprs) ->
                Printf.printf "starting expr_nodes construction\n";
                (* let expr_nodes = List.map (fun expr ->
                    let expr_pos_start = Position.start (WithPos.pos expr) in
                    let expr_pos_end = Position.finish (WithPos.pos expr) in
                    Printf.printf "for attribute %s\n" name;
                    Printf.printf "expr_pos_start.pos_cnum: %d\n" expr_pos_start.pos_cnum;
                    Printf.printf "expr_pos_start.pos_lnum: %d\n" expr_pos_start.pos_lnum;
                    Printf.printf "expr_pos_start.pos_bol: %d\n" expr_pos_start.pos_bol;
                    Printf.printf "actual column start pos: %d\n" (expr_pos_start.pos_cnum - expr_pos_start.pos_bol);


                    let expr_pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
                    expr_pos_start.pos_lnum  
                    (expr_pos_start.pos_cnum - expr_pos_start.pos_bol+2)
                    expr_pos_end.pos_lnum  
                    (expr_pos_end.pos_cnum - expr_pos_end.pos_bol+2)
                    in
                    Node ("Expr", expr_pos_str, [convert_phrase_to_ast expr])
                    ) exprs in *)
                let expr_nodes = List.map convert_phrase_to_ast exprs in
                Printf.printf "finished expr_nodes construction\n";

                let attr_pos_start = Position.start (WithPos.pos (List.hd exprs)) in
                let attr_pos_end = Position.finish (WithPos.pos (List.hd (List.rev exprs))) in
                let attr_pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
                  attr_pos_start.pos_lnum
                  (attr_pos_start.pos_cnum - attr_pos_start.pos_bol + 2)
                  attr_pos_end.pos_lnum
                  (attr_pos_end.pos_cnum - attr_pos_end.pos_bol + 2)
                in
                Node (("Attribute: " ^ name), attr_pos_str, expr_nodes)

            ) attrs @ 
            (match expr_opt with 
             | Some expr -> [convert_phrase_to_ast expr] 
             | None -> []
             ) @ 
            List.map convert_phrase_to_ast children)
            (* | Xml (name, attrs, expr_opt, children) -> 
                Node ("Xml", pos_str, Leaf (name, pos_str) :: 
                List.map (fun (name, exprs) ->
                    let head_expr = List.hd exprs in
                    let expr_pos_start = Position.start (WithPos.pos head_expr) in
                    let expr_pos_end = Position.finish (WithPos.pos head_expr) in
                    let expr_pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
                    expr_pos_end.pos_lnum  
                    (expr_pos_start.pos_cnum - expr_pos_start.pos_bol+2)
                    expr_pos_end.pos_lnum  
                    (expr_pos_start.pos_cnum - expr_pos_start.pos_bol+2+(String.length name))
                    in
                    Node (("Attribute: " ^ name), expr_pos_str, List.map convert_phrase_to_ast (List.tl exprs))
                ) attrs @ 
                (match expr_opt with 
                 | Some expr -> [convert_phrase_to_ast expr] 
                 | None -> []
                 ) @ 
                List.map convert_phrase_to_ast children)
                             *)
        | TextNode text -> Leaf ("TextNode: " ^ text, pos_str)
        | Formlet (p1, p2) -> Node ("Formlet", pos_str, [convert_phrase_to_ast p1; convert_phrase_to_ast p2])
        | Page p -> Node ("Page", pos_str, [convert_phrase_to_ast p])
        | FormletPlacement (p1, p2, p3) -> Node ("FormletPlacement", pos_str,[convert_phrase_to_ast p1; convert_phrase_to_ast p2; convert_phrase_to_ast p3])
        | PagePlacement p -> Node ("PagePlacement", pos_str, [convert_phrase_to_ast p])
        | FormBinding (p, pat) -> Node ("FormBinding", pos_str, [convert_phrase_to_ast p; convert_pattern_to_ast pat])
        | Select (name, p) -> Node ("Select", pos_str, [Leaf (name, pos_str); convert_phrase_to_ast p])
        | Offer (p, cases, _) -> Node ("Offer", pos_str, convert_phrase_to_ast p :: List.map convert_case_to_ast cases)
        | CP cp -> Node ("CP", pos_str, [convert_cp_phrase_to_ast cp])
        | TryInOtherwise (p1, pat, p2, p3, _) -> Node ("TryInOtherwise", pos_str, [convert_phrase_to_ast p1; convert_pattern_to_ast pat; convert_phrase_to_ast p2; convert_phrase_to_ast p3])
        | Raise -> Leaf ("Raise", pos_str)

    
  (* Commented out temporality code since wasn't useful *)
  (* and convert_temporality_to_ast (temp: CommonTypes.Temporality.t) (pos_str: string) : ast =
    let open CommonTypes in
    match temp with
    | Temporality.Current -> Node ("Current", pos_str, [])
    | Temporality.Transaction -> Node ("Transaction", pos_str, [])
    | Temporality.Valid -> Node ("Valid", pos_str, []) *)
  and convert_funlit_to_ast (fnlit : Sugartypes.funlit) (pos: string) : ast =
    let open Sugartypes in
    (* let open SourceCode in *)
    (* let temp = WithPos.make fnlit in *)
    (* let pos_str = Position.show (WithPos.pos pos) in *)
    match fnlit with
    | NormalFunlit (args, body) ->
        let arg_nodes = List.flatten (List.map (List.map convert_pattern_to_ast) args) in
        let body_node = convert_phrase_to_ast body in
        Node ("NormalFunlit", pos, arg_nodes @ [body_node])
        (* Node ("NormalFunlit", pos, List.flatten (List.map (List.map convert_pattern_to_ast) args) @ [convert_phrase_to_ast body]) *)
    | SwitchFunlit (args, body) ->
        Node ("SwitchFunlit", pos, List.flatten (List.map (List.map convert_pattern_to_ast) args) @ [convert_switch_body_to_ast body])
  
  (* Helper function to convert a pattern to an AST node *)
  and convert_pattern_to_ast (pattern : Sugartypes.Pattern.with_pos) : ast =
    let open Sugartypes.Pattern in
    let open Links_core.SourceCode in
    let open CommonTypes in
    (* let pos_str = Position.show (WithPos.pos pattern) in *)
    let pos_start = Position.start (WithPos.pos pattern) in
    let pos_end = Position.finish (WithPos.pos pattern) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    Printf.printf "Variable position: %s\n" pos_str;
    match WithPos.node pattern with
    | Any -> Leaf ("Any", pos_str)
    | Nil -> Leaf ("Nil", pos_str)
    | Cons (head, tail) -> Node ("Cons", pos_str, [convert_pattern_to_ast head; convert_pattern_to_ast tail])
    | List patterns -> Node ("List", pos_str, List.map convert_pattern_to_ast patterns)
    | Variable bndr -> 
        Leaf ("Variable: " ^ (Sugartypes.Binder.to_name bndr), pos_str)
    | Record (fields, None) ->
        let children = List.map (fun (_, var) ->
            convert_pattern_to_ast var
          ) fields in
          Node ("Record" , pos_str, children)
    | Variant (name, withpos_option) ->
    let name_node = Leaf (name, pos_str) in
    let children = match withpos_option with
        | Some var -> [convert_pattern_to_ast var]
        | None -> []
    in
    Node ("Variant", pos_str, name_node :: children)
    | Operation (label, args, result, _) ->
        Node ("Operation", pos_str, Leaf (label, pos_str) :: List.map convert_pattern_to_ast args @ [convert_pattern_to_ast result])
    | Negative names -> Node ("Negative", pos_str, List.map (fun name -> Leaf (name, pos_str)) names)
    | Record (fields, withpos_option) ->
        let field_nodes = List.map (fun (name, var) -> Node (name, pos_str, [convert_pattern_to_ast var])) fields in
        let withpos_nodes = match withpos_option with
          | Some var -> [convert_pattern_to_ast var]
          | None -> []
        in
        Node ("Record", pos_str, field_nodes @ withpos_nodes)
    | Tuple patterns -> Node ("Tuple", pos_str, List.map convert_pattern_to_ast patterns)
    | Constant c -> Leaf ("Constant: " ^ (Constant.show c), pos_str)
    | As (bndr, var) -> Node ("As", pos_str, [Leaf ("Binder: " ^ (Sugartypes.Binder.to_name bndr), pos_str); convert_pattern_to_ast var])
    | HasType (var, datatype) -> 
        let (actual, _) = datatype in
        Node ("HasType", pos_str, [convert_pattern_to_ast var; convert_datatype_to_ast actual])
  
  (* Helper function to convert a switch body to an AST node *)
  and convert_switch_body_to_ast (body : (Sugartypes.Pattern.with_pos * Sugartypes.phrase) list) : ast =
    let open SourceCode in
    Node ("SwitchBody", "", List.map (fun (pat, phr) -> 
        (* let pos_str = Position.show (WithPos.pos pat) in *)
        let pos_start = Position.start (WithPos.pos pat) in
        let pos_end = Position.finish (WithPos.pos pat) in
        let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
        pos_start.pos_lnum
        (pos_start.pos_cnum - pos_start.pos_bol+2)
        pos_end.pos_lnum  
        (pos_end.pos_cnum - pos_end.pos_bol+2) 
        in
        Node ("Case", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast phr])) body)
  
  (* Helper function to convert a binding to an AST node *)
  and convert_binding_to_ast (binding : Sugartypes.binding) : ast =
    let open Sugartypes in
    let open SourceCode in
    (* let pos = Position.start (WithPos.pos binding) in
    let pos_str = Printf.sprintf "line: %d, column: %d" pos.pos_cnum  pos.pos_lnum in *)
    let pos_start = Position.start (WithPos.pos binding) in
    let pos_end = Position.finish (WithPos.pos binding) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    match WithPos.node binding with
        | Val (pat, (_, rhs), _, _) -> 
            flush stdout;
            Node ("Val", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast rhs])
        | Fun { fun_binder; fun_definition = (_, fn); fun_signature; _ } ->
            (* let fun_start = Position.start (WithPos.pos fun_binder) in
            let fun_end = Position.finish (WithPos.pos fun_binder) in
            let fun_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
            fun_start.pos_lnum
            (fun_start.pos_cnum - fun_start.pos_bol+2)
            fun_end.pos_lnum  
            (fun_end.pos_cnum - fun_end.pos_bol+2) 
            in *)
            let sig_node = match fun_signature with
                | Some sig_ -> 
                    let (dt_w_pos, _) = sig_ in
                    Node ("Signature", pos_str, [convert_datatype_to_ast dt_w_pos])
                | None -> Leaf ("No signature", pos_str)
            in
            Node ("Fun", pos_str, [
                Leaf ("Binder: " ^ (Binder.to_name fun_binder), pos_str); 
                convert_funlit_to_ast fn pos_str;
                sig_node;
                ])
        | Funs funs ->
            Node ("Funs", pos_str, List.map (fun fun_def ->
                let { rec_binder; rec_definition = (_, fn); _ } = WithPos.node fun_def in
                let fun_pos_start = Position.start(WithPos.pos fun_def) in
                let fun_pos_end = Position.finish(WithPos.pos fun_def) in
                let fun_pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}"
                fun_pos_start.pos_lnum
                (fun_pos_start.pos_cnum - fun_pos_start.pos_bol+2)
                fun_pos_end.pos_lnum
                (fun_pos_end.pos_cnum - fun_pos_end.pos_bol+2)
                in
                Node ("Fun", fun_pos_str, [Leaf ("Binder: " ^ (Binder.to_name rec_binder), fun_pos_str); convert_funlit_to_ast fn fun_pos_str])
            ) funs)
        | Foreign alien ->
            let (binder, (_, datatype_opt)) = Alien.declaration alien in
            let datatype_str = 
                match datatype_opt with
                    | Some datatype -> Types.string_of_datatype datatype
                    | None -> "None"
                in
                Node ("Foreign", pos_str, [
                    Leaf ("Binder: " ^ (Binder.to_name binder), pos_str);
                    Leaf ("Datatype: " ^ datatype_str, pos_str)
                ])
        | Import { pollute; path } ->
            Node ("Import", pos_str,[Leaf ("Pollute: " ^ (string_of_bool pollute), pos_str); Node ("Path", pos_str, List.map (fun name -> Leaf (name, pos_str)) path)])
        | Open names ->
            Node ("Open", pos_str, List.map (fun name -> Leaf (name, pos_str)) names)
        | Aliases aliases -> 
            Node("Aliases", pos_str, List.map(fun alias ->
                let (name, _, alias_body) = WithPos.node alias in
                Node(name, pos_str, [convert_alias_body_to_ast alias_body])
            ) aliases)
        | Infix { assoc; precedence; name } ->
            let open Operators in
            Node ("Infix", pos_str, [Leaf ("Assoc: " ^ (Associativity.show assoc), pos_str); Leaf ("Precedence: " ^ (string_of_int precedence), pos_str); Leaf ("Name: " ^ name, pos_str)])
        | Exp phrase ->
            Node ("Exp", pos_str, [convert_phrase_to_ast phrase])
        | Module { binder; members } ->
            Node ("Module", pos_str, [Leaf ("Binder: " ^ (Binder.to_name binder), pos_str); Node ("Members", pos_str, List.map convert_binding_to_ast members)])
        | AlienBlock alien ->
            Node ("AlienBlock", pos_str, List.map (fun (binder, datatype) ->
                let datatype_str = match snd datatype with
                    | Some dt -> Types.string_of_datatype dt
                    | None -> "None"
                in
                Node ("Declaration", pos_str, [
                    Leaf ("Binder: " ^ (Binder.to_name binder), pos_str);
                    Leaf ("Datatype: " ^ datatype_str, pos_str)
                ])
            ) (Alien.declarations alien))
        (* | _ -> Leaf (show_binding binding) *)



    (* | Val (pat, (_, rhs), _, _) -> Node ("Val", [convert_pattern_to_ast pat; convert_phrase_to_ast rhs])
    | Fun { fun_binder; fun_definition = (_, fn); _} ->
        Node ("Fun", [Leaf ("Binder: " ^ (Binder.to_name fun_binder)); convert_funlit_to_ast fn])
    | Aliases aliases -> 
        Node("Aliases", List.map(fun alias ->
            let (name, _, alias_body) = WithPos.node alias in
            Node(name, [convert_alias_body_to_ast alias_body])
            ) aliases)
        
    | _ -> Leaf (show_binding binding) *)
  and convert_alias_body_to_ast (alias_body: Sugartypes.aliasbody) : ast = 
    let open Sugartypes in
    let open SourceCode in
    let tmp = WithPos.make alias_body in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    match alias_body with
    | Typename (datatype, _) -> Node ("Typename", pos_str, [convert_datatype_to_ast datatype])
    | Effectname (row, _) -> Node ("Effectname", pos_str, [convert_row_to_ast row])

  and convert_datatype_to_ast (datatype: Sugartypes.Datatype.with_pos) : ast =
    let open Sugartypes.Datatype in
    let open SourceCode in
    let open CommonTypes in
    let open Sugartypes in
    (* let pos_str = Position.show (WithPos.pos datatype) in *)
    let pos_start = Position.start (WithPos.pos datatype) in
    let pos_end = Position.finish (WithPos.pos datatype) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    match WithPos.node datatype with
        | TypeVar var -> Node ("TypeVar", pos_str, [Leaf ((SugarTypeVar.show var), pos_str)])
        | QualifiedTypeApplication (names, args) -> 
            Node("QualifiedTypeApplication", pos_str,
                List.map (fun name -> Leaf (name, pos_str)) names @ 
                List.map convert_type_arg_to_ast args)
        | Function (params, row, ret) ->
            Node ("Function", pos_str,
                List.map convert_with_pos_to_ast params @
                [convert_row_to_ast row; convert_with_pos_to_ast ret])
        | Lolli (params, row, ret) ->
            Node ("Lolli", pos_str,
                List.map convert_with_pos_to_ast params @
                [convert_row_to_ast row; convert_with_pos_to_ast ret])
        | Mu (var, body) -> 
            Node("Mu", pos_str,[Leaf ((SugarTypeVar.show var), pos_str); convert_with_pos_to_ast body])
        | Forall (quantifiers, body) ->
            Node("Forall", pos_str,
                List.map(fun q -> Leaf ((SugarQuantifier.show q), pos_str)) quantifiers @ [convert_with_pos_to_ast body])
        | Unit -> Leaf ("Unit", pos_str)
        | Tuple elements -> 
            Node ("Tuple", pos_str, List.map convert_with_pos_to_ast elements)
        | Record row -> 
            Node ("Record", pos_str, [convert_row_to_ast row])
        | Variant row -> 
            Node ("Variant", pos_str, [convert_row_to_ast row])
        | Effect row -> 
            Node ("Effect", pos_str, [convert_row_to_ast row])
        | Operation (params, ret, linearity) -> 
            Node ("Operation", pos_str,
                    List.map convert_with_pos_to_ast params @ 
                    [convert_with_pos_to_ast ret; Leaf ((DeclaredLinearity.show linearity), pos_str)])
        | Table (temporality, key, value, row) -> 
            Node ("Table", pos_str,
                    [Leaf (Temporality.show temporality, pos_str); 
                    convert_with_pos_to_ast key; 
                    convert_with_pos_to_ast value; 
                    convert_with_pos_to_ast row])
        | List element -> 
            Node ("List", pos_str, [convert_with_pos_to_ast element])
        | TypeApplication (name, args) -> 
            Node ("TypeApplication", pos_str,
                    Leaf (name, pos_str) :: List.map convert_type_arg_to_ast args)
        | Primitive p -> Leaf ("Primitive: " ^ Primitive.show p, pos_str)
        | DB -> Leaf ("DB", pos_str)
        | Input (input, output) -> 
            Node ("Input", pos_str,[convert_with_pos_to_ast input; convert_with_pos_to_ast output])
        | Output (input, output) -> 
            Node ("Output", pos_str,[convert_with_pos_to_ast input; convert_with_pos_to_ast output])
        | Select row -> 
            Node ("Select", pos_str,[convert_row_to_ast row])
        | Choice row -> 
            Node ("Choice", pos_str,[convert_row_to_ast row])
        | Dual typ -> 
            Node ("Dual", pos_str,[convert_with_pos_to_ast typ])
        | End -> Leaf ("End", pos_str)
        (* | Variant (fields, _) -> Node ("Variant", List.map (fun (name, spec) -> Node (name, [convert_fieldspec_to_ast spec])) fields) *)
        (* | Record (fields, _) -> Node ("Record", List.map (fun (name, spec) -> Node (name, [convert_fieldspec_to_ast spec])) fields) *)
        (* | TypeApplication (name, args) -> Node ("TypeApplication", Leaf name :: List.map convert_type_arg_to_ast args) *)
        (* | Primitive p -> Leaf ("Primitive: " ^ Primitive.show p) *)
        (* | node -> Leaf (show node) *)
  and convert_row_to_ast (row: Sugartypes.Datatype.row) : ast =
    (* let open Sugartypes in *)
    let open SourceCode in
    let tmp = WithPos.make row in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    match row with
    | (fields, _) -> Node ("Row", pos_str, List.map (fun (name, field) -> Node (name, pos_str, [convert_field_to_ast field])) fields)
  and convert_field_to_ast (field: Sugartypes.Datatype.fieldspec) : ast =
        let open Sugartypes.Datatype in
        let open SourceCode in
        (* let open Sugartypes.SugarTypeVar in *)
        let open Sugartypes in
        (* let open CommonTypes in *)
        (* let open Links_core.Types in *)
        let tmp = WithPos.make field in
        (* let pos_str = Position.show (WithPos.pos tmp) in *)
        let pos_start = Position.start (WithPos.pos tmp) in
        let pos_end = Position.finish (WithPos.pos tmp) in
        let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
        pos_start.pos_lnum
        (pos_start.pos_cnum - pos_start.pos_bol+2)
        pos_end.pos_lnum  
        (pos_end.pos_cnum - pos_end.pos_bol+2) 
        in
        match field with
        | Present expr -> Node ("Present", pos_str,[convert_with_pos_to_ast expr])
        | Absent -> Leaf ("Absent", pos_str)
        | Var var -> 
            let var_name = SugarTypeVar.show var in
            Node("Var", pos_str,[Leaf (var_name, pos_str)])
  and convert_with_pos_to_ast (with_pos: 'a SourceCode.WithPos.t) : ast =
    let open SourceCode in
    (* let node = WithPos.node with_pos in *)
    (* let pos = WithPos.pos with_pos in *)
    (* let pos_str = Position.show (WithPos.pos with_pos) in *)
    let pos_start = Position.start (WithPos.pos with_pos) in
    let pos_end = Position.finish (WithPos.pos with_pos) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    Node ("WithPos", pos_str, [convert_datatype_to_ast with_pos])
  and convert_type_arg_to_ast (type_arg: Sugartypes.Datatype.type_arg) : ast = 
    let open SourceCode in
    let tmp = WithPos.make type_arg in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    match type_arg with
        | Type typ -> Node ("Type", pos_str,[convert_datatype_to_ast typ])
        | Row row -> Node ("Row", pos_str,[convert_row_to_ast row])
        | Presence field -> Node ("Presence", pos_str, [convert_field_to_ast field])
  and convert_regex_to_ast(regex: Sugartypes.regex): ast =
    let open Sugartypes in
    let open SourceCode in
    let tmp = WithPos.make regex in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    match regex with
    | Range (c1, c2) -> Node ("Range", pos_str, [Leaf (String.make 1 c1, pos_str); Leaf (String.make 1 c2, pos_str)])
    | Simply s -> Leaf ("Simply: " ^ s, pos_str)
    | Quote r -> Node ("Quote", pos_str, [convert_regex_to_ast r])
    | Any -> Leaf ("Any", pos_str)
    | StartAnchor -> Leaf ("StartAnchor", pos_str)
    | EndAnchor -> Leaf ("EndAnchor", pos_str)
    | Seq rs -> Node ("Seq", pos_str, List.map convert_regex_to_ast rs)
    | Alternate (r1, r2) -> Node ("Alternate", pos_str, [convert_regex_to_ast r1; convert_regex_to_ast r2])
    | Group r -> Node ("Group", pos_str, [convert_regex_to_ast r])
    | Repeat (rep, r) -> Node ("Repeat", pos_str, [Leaf (Regex.show_repeat rep, pos_str); convert_regex_to_ast r])
    | Splice p -> Node ("Splice", pos_str, [convert_phrase_to_ast p])
    | Replace (r, rhs) -> Node ("Replace", pos_str, [convert_regex_to_ast r; Leaf (show_replace_rhs rhs, pos_str)])
  and convert_handler_to_ast(handler: Sugartypes.handler) : ast = 
    let open Sugartypes in
    let open SourceCode in
    let tmp = WithPos.make handler in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    Node ("Handler", pos_str,[
        Node ("Expr", pos_str,[convert_phrase_to_ast handler.sh_expr]);
        Node ("EffectCases", pos_str,List.map convert_case_to_ast handler.sh_effect_cases);
        Node ("ValueCases", pos_str,List.map convert_case_to_ast handler.sh_value_cases);
        Node ("Descriptor", pos_str,[convert_handler_descriptor_to_ast handler.sh_descr])
    ])
  and convert_handler_descriptor_to_ast(descr: Sugartypes.handler_descriptor) : ast =
    let open Sugartypes in
    let open SourceCode in
    let tmp = WithPos.make descr in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    let (row1, dat1, row2, dat2) = descr.shd_types in
    Node ("HandlerDescriptor", pos_str,[
        Leaf ("Depth: " ^ show_handler_depth descr.shd_depth, pos_str);
        Node ("Types", pos_str,[
            Leaf ("Row1: " ^ Types.string_of_datatype row1, pos_str);
            Leaf ("Dat1: " ^ Types.string_of_datatype dat1,pos_str);
            Leaf ("Row2: " ^ Types.string_of_datatype row2,pos_str);
            Leaf("Dat2: " ^ Types.string_of_datatype dat2,pos_str);
            (* Leaf ("Datatype: " ^ Types.show_datatype descr.shd_types); *)
            (* Leaf ("RawRow: " ^ show_typ descr.shd_raw_row); *)
        ]);
        Node ("Params", pos_str,
            match descr.shd_params with
                | Some params -> [convert_handler_parameterisation_to_ast params]
                | None -> [Leaf ("None", pos_str)]
        )
    ])
   and convert_handler_parameterisation_to_ast (params : Sugartypes.handler_parameterisation) : ast =
    let open Sugartypes in
    let open SourceCode in
    let tmp = WithPos.make params in
    (* let pos_str = Position.show (WithPos.pos tmp) in *)
    let pos_start = Position.start (WithPos.pos tmp) in
    let pos_end = Position.finish (WithPos.pos tmp) in
    let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
    pos_start.pos_lnum
    (pos_start.pos_cnum - pos_start.pos_bol+2)
    pos_end.pos_lnum  
    (pos_end.pos_cnum - pos_end.pos_bol+2) 
    in
    Node ("HandlerParameterisation", pos_str,[
        Node ("Bindings", pos_str,List.map (fun (pat, phr) -> Node ("Binding", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast phr])) params.shp_bindings);
        Node ("Types", pos_str, List.map (fun typ -> Leaf (Types.string_of_datatype typ, pos_str)) params.shp_types)
    ])
    and convert_case_to_ast (case : Sugartypes.clause) : ast =
        let open SourceCode in
        (* let tmp = WithPos.make case in *)
        (* let pos_str = Position.show (WithPos.pos tmp) in *)
        let (x, _) = case in
        let pos_start = Position.start (WithPos.pos x) in
        let pos_end = Position.finish (WithPos.pos x) in
        let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
        pos_start.pos_lnum
        (pos_start.pos_cnum - pos_start.pos_bol+2)
        pos_end.pos_lnum  
        (pos_end.pos_cnum - pos_end.pos_bol+2) 
        in
        let (pat, phr) = case in
        Node ("Case", pos_str, convert_pattern_to_ast pat :: [convert_phrase_to_ast phr])

            (* Node("Case statement", pos_str, [(convert_pattern_to_ast pat)]); 
            Node("Case body", pos_str, [(convert_phrase_to_ast phr)]);  *)
        (* ) *)

        (* Node ("Case", pos_str, [Node("Case phrase", pos_str, [convert_pattern_to_ast pat; convert_phrase_to_ast phr])]) *)

        (* Node ("Case", pos_str, [convert_pattern_to_ast pat] @ [convert_phrase_to_ast phr]) *)
    and convert_table_lit_to_ast (table_lit : Sugartypes.table_lit) : ast =
        let open Sugartypes in
        let open CommonTypes in
        let open SourceCode in
        let (_, t, _) = table_lit.tbl_type in
        (* let tmp = WithPos.make t in *)
        (* let pos_str = Position.show (WithPos.pos tmp) in *)
        let pos_start = Position.start (WithPos.pos t) in
        let pos_end = Position.finish (WithPos.pos t) in
        let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
        pos_start.pos_lnum
        (pos_start.pos_cnum - pos_start.pos_bol+2)
        pos_end.pos_lnum  
        (pos_end.pos_cnum - pos_end.pos_bol+2) 
        in
        let (temporality, datatype_with_pos, typ_option) = table_lit.tbl_type in
        Node ("TableLit", pos_str, [
            Node ("Name", pos_str, [convert_phrase_to_ast table_lit.tbl_name]);

            Node ("Type", pos_str,[
                Leaf ("Temporality: " ^ Temporality.show temporality, pos_str);
                convert_datatype_to_ast  datatype_with_pos;
                (match typ_option with
                | Some (typ1, typ2, typ3) -> Node ("Types", pos_str,[
                    Leaf ("Type1: " ^ Types.string_of_datatype typ1, pos_str);
                    Leaf ("Type2: " ^ Types.string_of_datatype typ2, pos_str);
                    Leaf ("Type3: " ^ Types.string_of_datatype typ3, pos_str)
                ])
                | None -> Leaf ("None", pos_str))
            ]);

            Node ("FieldConstraints", pos_str, List.map (fun (name, constraints) -> 
                Node (name, pos_str, List.map (fun (field_constraint) -> Leaf (show_fieldconstraint field_constraint, pos_str)) constraints)
            ) table_lit.tbl_field_constraints);

            Node ("Keys", pos_str, [convert_phrase_to_ast table_lit.tbl_keys]);

            Node ("TemporalFields", pos_str, [
                match table_lit.tbl_temporal_fields with
                    | Some (field1, field2) -> Node ("Fields", pos_str,[Leaf (field1, pos_str); Leaf (field2, pos_str)])
                    | None -> Leaf ("None", pos_str)
                ]
            );
            Node ("Database", pos_str, [convert_phrase_to_ast table_lit.tbl_database])

        ])
        and convert_cp_phrase_to_ast (cp_phrase : Sugartypes.cp_phrase) : ast =
            let open Sugartypes in
            let open SourceCode in
            let tmp = WithPos.make cp_phrase in
            (* let pos_str = Position.show (WithPos.pos tmp) in *)
            let pos_start = Position.start (WithPos.pos tmp) in
            let pos_end = Position.finish (WithPos.pos tmp) in
            let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
            pos_start.pos_lnum
            (pos_start.pos_cnum - pos_start.pos_bol+2)
            pos_end.pos_lnum  
            (pos_end.pos_cnum - pos_end.pos_bol+2) 
            in
            match WithPos.node cp_phrase with
                | CPUnquote (bindings, expr) -> Node ("CPUnquote", pos_str, [convert_block_to_ast (bindings, expr)])
                | CPGrab ((name, typ_opt), binder_opt, cp) -> Node ("CPGrab", pos_str, [
                    Leaf ("Name: " ^ name, pos_str);
                    (match typ_opt with
                    | Some (typ, args) -> Node ("Type", pos_str,[Leaf (Types.string_of_datatype typ, pos_str); Node ("Args", pos_str, List.map (fun arg -> Leaf (Types.string_of_type_arg arg, pos_str)) args)])
                    | None -> Leaf ("None", pos_str));
                    (match binder_opt with
                    | Some binder -> Node ("Binder", pos_str,[convert_binder_to_ast binder])
                    | None -> Leaf ("None", pos_str));
                    convert_cp_phrase_to_ast cp
                ])
                | CPGive ((name, typ_opt), expr_opt, cp) -> Node ("CPGive", pos_str,[
                    Leaf ("Name: " ^ name, pos_str);
                    (match typ_opt with
                    | Some (typ, args) -> Node ("Type", pos_str,[Leaf (Types.string_of_datatype typ, pos_str); Node ("Args", pos_str, List.map (fun arg -> Leaf (Types.string_of_type_arg arg, pos_str)) args)])
                    | None -> Leaf ("None", pos_str));
                    (match expr_opt with
                    | Some expr -> Node ("Expr", pos_str, [convert_phrase_to_ast expr])
                    | None -> Leaf ("None", pos_str));
                    convert_cp_phrase_to_ast cp
                ])
                | CPGiveNothing binder -> Node ("CPGiveNothing", pos_str, [convert_binder_to_ast binder])
                | CPSelect (binder, label, cp) -> Node ("CPSelect", pos_str, [
                    convert_binder_to_ast binder;
                    Leaf ("Label: " ^ label, pos_str);
                    convert_cp_phrase_to_ast cp
                ])
                | CPOffer (binder, cases) -> Node ("CPOffer", pos_str,[
                    convert_binder_to_ast binder;
                    Node ("Cases", pos_str, List.map (fun (label, cp) -> Node (label, pos_str, [convert_cp_phrase_to_ast cp])) cases)
                ])
                | CPLink (binder1, binder2) -> Node ("CPLink", pos_str,[
                    convert_binder_to_ast binder1;
                    convert_binder_to_ast binder2
                ])
                | CPComp (binder, cp1, cp2) -> Node ("CPComp", pos_str,[
                    convert_binder_to_ast binder;
                    convert_cp_phrase_to_ast cp1;
                    convert_cp_phrase_to_ast cp2
                ])
        
        and convert_block_to_ast (bindings, expr) : ast =
            let open SourceCode in
            let tmp = WithPos.make expr in
            (* let pos_str = Position.show (WithPos.pos tmp) in *)
            let pos_start = Position.start (WithPos.pos tmp) in
            let pos_end = Position.finish (WithPos.pos tmp) in
            let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
            pos_start.pos_lnum
            (pos_start.pos_cnum - pos_start.pos_bol+2)
            pos_end.pos_lnum  
            (pos_end.pos_cnum - pos_end.pos_bol+2) 
            in
            Node ("Block", pos_str, List.map convert_binding_to_ast bindings @ [convert_phrase_to_ast expr])
        
        and convert_binder_to_ast (binder : Sugartypes.Binder.with_pos) : ast =
            let open Sugartypes.Binder in
            let open SourceCode in
            let tmp = WithPos.make binder in
            (* let pos_str = Position.show (WithPos.pos tmp) in *)
            let pos_start = Position.start (WithPos.pos tmp) in
            let pos_end = Position.finish (WithPos.pos tmp) in
            let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
            pos_start.pos_lnum
                (pos_start.pos_cnum - pos_start.pos_bol+2)
                pos_end.pos_lnum  
                (pos_end.pos_cnum - pos_end.pos_bol+2) 
                in
            Node ("Binder", pos_str, [
                Leaf ("Name: " ^ to_name binder, pos_str);
                Leaf ("Type: " ^ Types.string_of_datatype (to_type binder), pos_str)
            ])
  
  (* Uses the parser to generate ASTs of Links code *)
  let parse_code file_location =
      let open SourceCode in
      (* let ((bindings, _phrase_opt), _pos) = Parse.parse_string Parse.program code in *)
      Printf.printf("[parser.parse_code] Sending file dir to parser\n");
      flush stdout;
      let ((bindings, _phrase_opt), _pos) = Parse.parse_file Parse.program file_location in
      Printf.printf("[parser.parse_code] Successfully retrieved bindings of Links code\n");
      flush stdout;
      (* let tmp = WithPos.make bindings in *)
      (* let pos_str = Position.show (WithPos.pos tmp) in *)
      let start_pos = Position.start (WithPos.pos (List.hd bindings)) in
      let end_pos = Position.finish (WithPos.pos (List.hd (List.rev bindings))) in
      (* Printf.printf "[parse_code (start_pos: cnum)] %d\n" start_pos.pos_cnum;
      Printf.printf "[parse_code (start_pos: lnum)] %d\n" start_pos.pos_lnum;
      Printf.printf "[parse_code (start_pos: bol)] %d\n" start_pos.pos_bol;
      Printf.printf "[parse_code (end_pos: cnum)] %d\n" end_pos.pos_cnum;
      Printf.printf "[parse_code (end_pos: lnum)] %d\n" end_pos.pos_lnum;
      Printf.printf "[parse_code (end_pos: bol)] %d\n" end_pos.pos_bol; *)

        (* let combined_pos = Position.make ~start:start_pos ~finish:end_pos ~code:None in *)
      (* let pos_str = Position.show combined_pos in *)
      (* Range: start: (line, col) end: (line, col) *)
      let pos_str = Printf.sprintf "{\"start\": {\"line\": %d, \"col\": %d}, \"finish\": {\"line\": %d, \"col\": %d}}" 
                start_pos.pos_lnum
                (start_pos.pos_cnum - start_pos.pos_bol+2)
                end_pos.pos_lnum  
                (end_pos.pos_cnum - end_pos.pos_bol+2)
                in
      let ast = Node ("root", pos_str, List.map convert_binding_to_ast bindings) in

        (* let rec ast_to_msgpack input_ast = 
            List.map(fun node ->
                match node with
                | Leaf (name, pos) -> Mparray[Mpstring name; Mpstring pos]
                | Node (name, pos, children) -> Mparray[Mpstring name; Mpstring pos; Mparray (List.map ast_to_msgpack children)]
                ) input_ast

        in
        let ast_msgpack = ast_to_msgpack ast in
        let ast_msgpack_str = Msgpack.to_string ast_msgpack in
        ast_msgpack_str *)
    (* Below is what WORKS *)
      Yojson.Safe.to_string (ast_to_yojson ast) 


(* Return AST as ast *)

let handle_request file_location =
    Printf.printf("[handle_request] called\n");
    flush stdout;
    let ast = parse_code file_location in
    ast
(* 
let () =
    let response = handle_request in
    Printf.printf "Response: %s\n" response;
    flush stdout; *)
  
(* let () =
Printf.printf "Starting server\n";
flush stdout;
let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8081) in
Unix.bind server addr;
Unix.listen server 10;
Printf.printf "Listening on Port 8081\n";
flush stdout;
while true do
    try
    let (client, _) = Unix.accept server in
    Printf.printf "Accepted connection\n";
    flush stdout;
    let in_channel = Unix.in_channel_of_descr client in
    let out_channel = Unix.out_channel_of_descr client in
    try
        let file_location = input_line in_channel in
        Printf.printf "Received code: %s\n" file_location;
        flush stdout;
        let response = handle_request file_location in
        (* let json_response = Yojson.Safe.to_string (`List (List.map (fun s -> `String s) response)) in *)
        (* output_string out_channel (json_response ^ "\n"); *)
        (* output_string out_channel (Yojson.Safe.to_string response ^ "\n"); *)
        output_string out_channel (response ^ "\n");
        flush out_channel;
        Unix.close client;
        Printf.printf "Client disconnected\n";
        flush stdout;
        close_in_noerr in_channel;
        close_out_noerr out_channel;
      with
      | End_of_file ->
        Printf.printf "Client closed connection\n";
        flush stdout;
        Unix.close client;
        close_in_noerr in_channel;
        close_out_noerr out_channel;
      | exn ->
        let open Errors in
        let detailed_error = format_exception exn in
        Unix.close client;
        close_in_noerr in_channel;
        close_out_noerr out_channel;
        Printf.printf "Error: %s\n" detailed_error;
        flush stdout;
      with 
      | exn ->
        
        Printf.printf "Server error: %s\n" (Printexc.to_string exn);
        flush stdout;
      
done *)


let () = 
  Printf.printf"[Main] Starting server\n"; 
  flush stdout; 
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8081) in 
  Unix.bind server addr; 
  Unix.listen server 10; 
  Printf.printf "[Main] Listening on Port 8081\n"; 
  flush stdout; 
  
  while true do
    try
      let (client, _) = Unix.accept server in
      (* Wrap everything in a function to ensure cleanup *)
      let process_client () =
        Printf.printf "[Main] Accepted connection\n";
        flush stdout;
        
        (* Use with_open_in and with_open_out to auto-close *)
        let in_channel = Unix.in_channel_of_descr client in
        let out_channel = Unix.out_channel_of_descr client in
        
        try
          let file_location = input_line in_channel in
          Printf.printf "[Main] Received code: %s\n" file_location;
          flush stdout;
          
          let response = handle_request file_location in
          output_string out_channel (response ^ "\n");
          flush out_channel;
          Printf.printf "[Main] Finished parsing code\n";
          flush stdout;
          Printf.printf "[Main] Response: %s\n" response;
          flush stdout;

          (* Printf.printf "AST: %s\n" response;
          flush stdout; *)
          (* close_in in_channel;
          close_out out_channel; *)
        
        (* Ensure cleanup in all error scenarios *)
        with 
        | End_of_file -> 
            Printf.printf "[Main] Client closed connection\n";
            flush stdout;
            (* close_in in_channel;
            close_out out_channel; *)
        | exn ->
            let detailed_error = Errors.format_exception exn in
            Printf.printf "[Main] Error: %s\n" detailed_error;
            flush stdout
      in
      
      (* Run client processing *)
      process_client ();
      
      (* Guaranteed cleanup *)
      Unix.close client
    
    with exn ->
      Printf.printf "[Main] Server error: %s\n" (Printexc.to_string exn);
      
      flush stdout
  done


















(* 
let () =
  Printf.printf "Starting server\n";
  flush stdout;
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8081) in
  Unix.bind server addr;
  Unix.listen server 10;
  Printf.printf "Listening on Port 8081\n";
  flush stdout;
  while true do
    let (client, _) = Unix.accept server in
    Printf.printf "Accepted connection\n";
    flush stdout;
    let in_channel = Unix.in_channel_of_descr client in
    let out_channel = Unix.out_channel_of_descr client in
    try
      let rec read_all_lines acc =
        try
          let line = input_line in_channel in
          read_all_lines (acc ^ line ^ "\n")
        with End_of_file -> acc
      in
      let code = read_all_lines "" in
      Printf.printf "Received code:\n%s\n" code;
      flush stdout;
      let response = handle_request code in
      output_string out_channel (response ^ "\n");
      flush out_channel;
    with
    | exn ->
      Printf.printf "Error: %s\n" (Printexc.to_string exn);
      flush stdout;
    Unix.close client;
    Printf.printf "Client disconnected\n";
    flush stdout;
  done *)