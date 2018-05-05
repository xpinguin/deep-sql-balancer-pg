(* from: https://gist.github.com/komamitsu/1362149 *)
open Lwt
let () = Lwt_log.add_rule "*" Lwt_log.Info

let (>>) a b = a >>= fun() -> b;;

let server_port = 12341
let so_timeout = Some 20
let backlog = 10

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())

let init_socket sockaddr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let () = Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true in
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket backlog;
  socket
  
let counter = ref 0

let handle_message msg =
    match msg with
    | "read" -> string_of_int !counter
    | "inc"  -> counter := !counter + 1; "Counter has been incremented"
    | _      -> "Unknown command"
    
let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg ->
            let reply = handle_message msg in
            Lwt_io.write_line oc reply >>= handle_connection ic oc
        | None -> Lwt_log.info "Connection closed" >>= return)

let accept_connection conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    Lwt_log.info "New connection" >>= return
    
let create_server sock =
  let rec serve () =
        Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

(*wtf*)
let process socket ~timeout ~callback =
  let rec _process () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
        let c = callback inchan outchan in
        let events =
          match timeout with
          | None -> [c]
          | Some t -> [c; Lwt_unix.sleep (float_of_int t) >> return ()]
        in
        ignore (Lwt.pick events >> try_close outchan >> try_close inchan);
        _process ()
      )
  in
  _process ()

let () =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  let socket = init_socket sockaddr in
  let serve = create_server socket in
  Lwt_main.run @@ serve ()
  (*Lwt_main.run (
    process
      socket
      ~timeout:so_timeout
      ~callback:
        (fun inchan outchan ->
          Lwt_io.read_line inchan >>= (fun msg -> Lwt_io.write_line outchan msg))
  )*)
