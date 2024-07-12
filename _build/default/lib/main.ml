open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe
  let empty_game_omok = Game.empty Game.Game_kind.Omok

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let map_positions (game : Game.t) ~f =
    let len = Game.Game_kind.board_length game.game_kind in
    List.init len ~f:(fun row ->
      List.init len ~f:(fun column -> f { Game.Position.row; column }))
  ;;

  let print_game (game : Game.t) =
    map_positions game ~f:(fun pos ->
      Map.find game.board pos
      |> Option.value_map ~default:" " ~f:Game.Piece.to_string)
    |> List.map ~f:(String.concat ~sep:" | ")
    |> String.concat ~sep:"\n---------\n"
    |> Core.print_endline
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let spots_list =
      map_positions game ~f:(fun pos ->
        match Map.find game.board pos with
        | None -> Some pos
        | Some _ -> None)
    in
    List.filter_opt (List.concat spots_list)
  ;;

  let in_bounds r c board_len =
    r >= 0 && r < board_len && c >= 0 && c < board_len
  ;;

  let filter_out_moves all_moves to_filt =
    (* List.filter all_moves ~f: (List.mem to_filt ~equal:(fun p1 p2 -> not
       (Game.Position.equal p1 p2))) *)
    Set.to_list
      (Set.diff
         (Game.Position.Set.of_list all_moves)
         (Game.Position.Set.of_list to_filt))
  ;;

  let get_neighor_pieces board_len x y =
    let dir =
      [ x - 1, y - 1
      ; x - 1, y
      ; x - 1, y + 1
      ; x, y - 1
      ; x, y + 1
      ; x + 1, y - 1
      ; x + 1, y
      ; x + 1, y + 1
      ]
    in
    List.filter dir ~f:(fun (x, y) -> in_bounds x y board_len)
  ;;

  let available_neighor_moves (game : Game.t) : Game.Position.t list =
    let occupied_spots =
      List.map (Map.to_alist game.board) ~f:(fun (pos, _p) -> pos)
    in
    let spots_list =
      List.map
        (List.concat
           (List.map occupied_spots ~f:(fun pos ->
              get_neighor_pieces
                (Game.Game_kind.board_length game.game_kind)
                pos.row
                pos.column)))
        ~f:(fun (row, column) -> { Game.Position.row; column })
    in
    filter_out_moves spots_list occupied_spots
  ;;

  let%expect_test "print_spaces_on_empty_board" =
    let moves = available_moves empty_game in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|
    (((row 0) (column 0)) ((row 0) (column 1)) ((row 0) (column 2)) 
     ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2)) 
     ((row 2) (column 0)) ((row 2) (column 1)) ((row 2) (column 2)))
    |}];
    return ()
  ;;

  let make_step start finish =
    if finish - start = 0
    then start
    else if finish - start < 0
    then start - 1
    else start + 1
  ;;

  let tup_equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2

  let check_same_piece (peice : Game.Piece.t) (game : Game.t) (row, column)
    : bool
    =
    match Map.find game.board { Game.Position.row; column } with
    | None -> false
    | Some p -> Game.Piece.equal peice p
  ;;

  let viable_win_dir r c win_len board_len =
    let all_dir =
      [ r + win_len - 1, c
      ; r, c + win_len - 1
      ; r + win_len - 1, c + win_len - 1
      ; r + win_len - 1, c - (win_len - 1)
      ]
    in
    List.filter all_dir ~f:(fun (r, c) -> in_bounds r c board_len)
  ;;

  let rec check_win piece start_x start_y ~end_x ~end_y game : bool =
    if tup_equal (start_x, start_y) (end_x, end_y)
    then check_same_piece piece game (start_x, start_y)
    else if not (check_same_piece piece game (start_x, start_y))
    then false
    else (
      let curr_x = make_step start_x end_x in
      let curr_y = make_step start_y end_y in
      check_win piece curr_x curr_y ~end_x ~end_y game)
  ;;

  let any_wins piece start_x start_y board_len (game : Game.t) win_len
    : Game.Piece.t option
    =
    let pos_directions = viable_win_dir start_x start_y win_len board_len in
    match pos_directions with
    | [] -> None
    | lis ->
      if List.exists lis ~f:(fun (end_x, end_y) ->
           check_win piece start_x start_y ~end_x ~end_y game)
      then Some piece
      else None
  ;;

  let game_board_continues game =
    match available_moves game with
    | [] -> Game.Evaluation.Game_over { winner = None }
    | _ -> Game_continues
  ;;

  let get_win_piece (wins : (Game.Position.t * Game.Piece.t) list) game =
    match wins with
    | (_, piece) :: [] -> Game.Evaluation.Game_over { winner = Some piece }
    | [] -> game_board_continues game
    | _ -> Illegal_move
  ;;

  (* Exercise 2 *)
  let evaluate ?(win_len : int option) (game : Game.t) : Game.Evaluation.t =
    let board_len = Game.Game_kind.board_length game.game_kind in
    if Map.for_alli game.board ~f:(fun ~key:pos ~data:_d ->
         in_bounds pos.row pos.column board_len)
    then (
      let len =
        match win_len with
        | None -> Game.Game_kind.win_length game.game_kind
        | Some l -> l
      in
      let wins =
        Map.to_alist
          (Map.filter_mapi game.board ~f:(fun ~key:pos ~data:piece ->
             any_wins piece pos.row pos.column board_len game len))
      in
      get_win_piece wins game)
    else Game.Evaluation.Illegal_move
  ;;

  let num_pieces_wins ~me wins ~len =
    match wins with
    | [] -> 0
    | lis ->
      let total = ref 0 in
      List.iter lis ~f:(fun (_pos, piece) ->
        match Game.Piece.equal me piece with
        | true -> total := !total + (len * len)
        | false -> total := !total - (len * len));
      !total
  ;;

  let num_pieces_in_row ~(len : int) (game : Game.t) ~(me : Game.Piece.t)
    : int
    =
    let board_len = Game.Game_kind.board_length game.game_kind in
    let wins =
      Map.to_alist
        (Map.filter_mapi game.board ~f:(fun ~key:pos ~data:piece ->
           any_wins piece pos.row pos.column board_len game len))
    in
    num_pieces_wins ~len ~me wins
  ;;

  (* iterate over all positions in the map check the right, down and diagonal
     down directions using neighbors when checking each coord see if it is in
     bounds *)

  (* If a position leads to a win the returns position else returns None*)
  let match_wins
    (piece : Game.Piece.t)
    (game_eval : Game.Evaluation.t)
    (pos : Game.Position.t)
    =
    match game_eval with
    | Game_over winner ->
      Option.bind winner.winner ~f:(fun w ->
        if Game.Piece.equal w piece then Some pos else None)
    | _ -> None
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let all_moves = available_moves game in
    List.dedup_and_sort
      (List.filter_map all_moves ~f:(fun pos ->
         let new_game = place_piece game ~piece:me ~position:pos in
         match_wins me (evaluate new_game) pos))
      ~compare:Game.Position.compare
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let opp_winning_moves = winning_moves ~me:(Game.Piece.flip me) game in
    match opp_winning_moves with
    | [] -> []
    | _ :: [] ->
      let all_moves = available_moves game in
      filter_out_moves all_moves opp_winning_moves
    | _ -> available_moves game
  ;;

  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    let loser_moves = losing_moves ~me game in
    let all_moves = available_moves game in
    filter_out_moves all_moves loser_moves
  ;;

  let intersect_lists
    (lis1 : Game.Position.t list)
    (lis2 : Game.Position.t list)
    =
    Set.to_list
      (Set.inter
         (Game.Position.Set.of_list lis1)
         (Game.Position.Set.of_list lis2))
  ;;

  let _available_neighoring_moves_dont_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    : Game.Position.t list
    =
    let dont_lose = available_moves_that_do_not_immediately_lose ~me game in
    let neighbors = available_neighor_moves game in
    intersect_lists dont_lose neighbors
  ;;

  let%expect_test "find legal nonlosing moves" =
    let win_for_x =
      let open Game in
      empty_game
      |> place_piece
           ~piece:Piece.X
           ~position:{ Position.row = 2; column = 0 }
      |> place_piece
           ~piece:Piece.X
           ~position:{ Position.row = 1; column = 1 }
    in
    let moves =
      available_moves_that_do_not_immediately_lose ~me:Game.Piece.O win_for_x
    in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect {|
  (((row 0) (column 2)))
  |}];
    return ()
  ;;

  let check_hot_spots (game : Game.t) (player : Game.Piece.t) =
    let bord = Game.Game_kind.board_length game.game_kind in
    let best_spots =
      [ { Game.Position.row = 0; column = 0 }
      ; { Game.Position.row = 0; column = bord - 1 }
      ; { Game.Position.row = bord - 1; column = 0 }
      ; { Game.Position.row = bord - 1; column = bord - 1 }
      ; { Game.Position.row = bord / 2; column = bord / 2 }
      ]
    in
    List.length
      (List.filter best_spots ~f:(fun spot ->
         Map.existsi game.board ~f:(fun ~key:k ~data:v ->
           Game.Position.equal spot k && Game.Piece.equal player v)))
  ;;

  let calc_chains_in_row (piece : Game.Piece.t) (game : Game.t) : int =
    num_pieces_in_row ~len:4 game ~me:piece
    + num_pieces_in_row ~len:3 game ~me:piece
    + num_pieces_in_row ~len:2 game ~me:piece
    + num_pieces_in_row ~len:1 game ~me:piece
  ;;

  let omok_score_calc
    (piece : Game.Piece.t)
    (game_result : Game.Evaluation.t)
    (game : Game.t)
    =
    match game_result with
    | Game_over winner ->
      (match winner.winner with
       | None -> 0
       | Some p ->
         if Game.Piece.equal p piece then Int.max_value else Int.min_value)
    | Game_continues -> calc_chains_in_row piece game
    | _ -> 0
  ;;

  let score
    (piece : Game.Piece.t)
    (game_result : Game.Evaluation.t)
    (game : Game.t)
    =
    match game.game_kind with
    | Omok -> omok_score_calc piece game_result game
    | Tic_tac_toe ->
      (match game_result with
       | Game_continues -> check_hot_spots game piece
       | Game_over winner ->
         (match winner.winner with
          | None -> check_hot_spots game piece
          | Some p ->
            if Game.Piece.equal p piece
            then (Int.max_value / 2) + check_hot_spots game piece
            else (Int.min_value / 2) + check_hot_spots game piece)
       | _ -> 0)
  ;;

  let is_game_over (evaluation : Game.Evaluation.t) : bool =
    match evaluation with Game.Evaluation.Game_over _ -> true | _ -> false
  ;;

  let check_alpha_beta
    ~(a : int ref)
    ~(b : int ref)
    (maximizing : bool)
    (value : int)
    : bool
    =
    match maximizing with true -> value > !b | false -> value < !a
  ;;

  let set_alpha_beta
    ~(a : int ref)
    ~(b : int ref)
    (maximizing : bool)
    (value : int)
    : unit
    =
    match maximizing with
    | true -> a := max !a value
    | false -> b := min !b value
  ;;

  let rec minimax
    ~(node : Game.t)
    ~(maximizing : bool)
    ?(depth = 9)
    ?(alpha = Int.min_value)
    ?(beta = Int.max_value)
    (player : Game.Piece.t)
    : int
    =
    let evaluated = evaluate node in
    if depth = 0 || is_game_over evaluated
    then (
      match maximizing with
      | true -> score (Game.Piece.flip player) evaluated node
      | false -> score player evaluated node)
    else (
      let value =
        if maximizing then ref Int.min_value else ref Int.max_value
      in
      let max_or_min = if maximizing then Int.max else Int.min in
      let moves = _available_neighoring_moves_dont_lose ~me:player node in
      let a = ref alpha in
      let b = ref beta in
      let _child_heurisitcs =
        List.fold_map ~init:true moves ~f:(fun continue pos ->
          if not continue
          then false, None
          else (
            let new_node = place_piece node ~piece:player ~position:pos in
            let new_player = Game.Piece.flip player in
            value
            := max_or_min
                 (minimax
                    ~depth:(depth - 1)
                    ~node:new_node
                    ~maximizing:(not maximizing)
                    new_player
                    ~alpha:!a
                    ~beta:!b)
                 !value;
            match check_alpha_beta ~a ~b maximizing !value with
            | false ->
              set_alpha_beta ~a ~b maximizing !value;
              true, Some !value
            | true -> false, Some !value))
      in
      !value)
  ;;

  let pos_hueristic_comp (_p1, h1) (_p2, h2) = h1 - h2

  let give_pos_of_tip (p, _h) =
    (* print_s [%sexp (_h : int)]; *)
    p
  ;;

  let make_turn ~(game : Game.t) ~(me : Game.Piece.t) : Game.Position.t =
    let winning_moves = winning_moves ~me game in
    if List.length winning_moves > 0
    then List.hd_exn winning_moves
    else (
      let poss_moves = _available_neighoring_moves_dont_lose ~me game in
      match poss_moves with
      | [] -> List.random_element_exn (available_moves game)
      | moves ->
        let best_move_list =
          List.map moves ~f:(fun pos ->
            ( pos
            , minimax
                ~node:(place_piece game ~piece:me ~position:pos)
                ~maximizing:false
                (Game.Piece.flip me) ))
        in
        give_pos_of_tip
          (Option.value_exn
             (List.min_elt best_move_list ~compare:pos_hueristic_comp)))
  ;;

  let%expect_test "test mini_max_1" =
    let best_move = make_turn ~game:non_win ~me:Game.Piece.X in
    print_s [%sexp (best_move : Game.Position.t)];
    [%expect {| ((row 1) (column 1))
    |}];
    return ()
  ;;

  let%expect_test "test mini_max_2" =
    let best_move =
      make_turn
        ~game:
          (empty_game
           |> place_piece
                ~piece:Game.Piece.X
                ~position:{ Game.Position.row = 0; column = 0 }
           |> place_piece
                ~piece:Game.Piece.X
                ~position:{ Game.Position.row = 2; column = 0 }
           |> place_piece
                ~piece:Game.Piece.O
                ~position:{ Game.Position.row = 0; column = 2 })
        ~me:Game.Piece.X
    in
    print_s [%sexp (best_move : Game.Position.t)];
    [%expect {| ((row 1) (column 0))
  |}];
    return ()
  ;;

  let%expect_test "test mini_max_3" =
    let best_move =
      make_turn
        ~game:
          (empty_game
           |> place_piece
                ~piece:Game.Piece.X
                ~position:{ Game.Position.row = 0; column = 1 }
           |> place_piece
                ~piece:Game.Piece.X
                ~position:{ Game.Position.row = 1; column = 0 }
           |> place_piece
                ~piece:Game.Piece.O
                ~position:{ Game.Position.row = 0; column = 0 })
        ~me:Game.Piece.O
    in
    print_s [%sexp (best_move : Game.Position.t)];
    [%expect {| ((row 1) (column 1))
|}];
    return ()
  ;;

  let%expect_test "test mini_max_4" =
    let best_move = make_turn ~game:empty_game ~me:Game.Piece.X in
    print_s [%sexp (best_move : Game.Position.t)];
    [%expect {| ((row 0) (column 0))
|}];
    return ()
  ;;

  let%expect_test "test mini_max_5" =
    let best_move =
      make_turn
        ~game:
          (empty_game
           |> place_piece
                ~piece:Game.Piece.X
                ~position:{ Game.Position.row = 0; column = 0 })
        ~me:Game.Piece.O
    in
    print_s [%sexp (best_move : Game.Position.t)];
    [%expect {| ((row 1) (column 1))
|}];
    return ()
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_five =
    Command.async
      ~summary:"Exercise 5: What are all legal non-losing moves?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let legal_moves =
           available_moves_that_do_not_immediately_lose ~me:piece non_win
         in
         print_s [%sexp (legal_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_six =
    Command.async
      ~summary:"Exercise 6: Test Gomoku"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let move = make_turn ~me:piece ~game:empty_game_omok in
         let ng = place_piece empty_game_omok ~piece ~position:move in
         print_game ng;
         return ())
  ;;

  let exercise_seven =
    Command.async
      ~summary:"Exercise 7: Test Gomok Against Self"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let turns = List.init 150 ~f:(fun a -> a) in
         let _ =
           List.fold
             turns
             ~init:(empty_game_omok, piece)
             ~f:(fun (g, p) _t ->
               let new_pos = make_turn ~game:g ~me:p in
               let ng = place_piece g ~piece:p ~position:new_pos in
               print_game ng;
               Core.print_endline "";
               ng, Game.Piece.flip p)
         in
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "five", exercise_five
      ; "six", exercise_six
      ; "seven", exercise_seven
      ]
  ;;
end

let handle_turn (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  print_s [%message "Received query" (query : Rpcs.Take_turn.Query.t)];
  let response =
    { Rpcs.Take_turn.Response.piece = query.you_play
    ; position = Exercises.make_turn ~game:query.game ~me:query.you_play
    }
  in
  return response
;;

(* let test_omok = let new_game = Exercises.empty_game_omok in let turns =
   List.init 9 ~f:(fun a -> a) in List.fold turns ~init:(new_game,
   Game.Piece.X) ~f:(fun (g, p) _t -> let new_pos = Exercises.make_turn
   ~game:g ~me:p in let ng = Exercises.place_piece g ~piece:p
   ~position:new_pos in Exercises.print_game ng; print_endline ""; ng,
   Game.Piece.flip p) ;;

   let%expect_test "testing against self" = let _ = test_omok in
   print_endline ""; [%expect {| |}]; return () ;; *)

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_turn ]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
