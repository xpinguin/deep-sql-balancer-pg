let establish_server server_fun sockaddr =
   let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 
   in Unix.bind sock sockaddr ;
      Unix.listen sock 3;
      while true do
        let (s, caller) = Unix.accept sock 
        in  let inchan = Unix.in_channel_of_descr s 
                    and outchan = Unix.out_channel_of_descr s 
                    in server_fun inchan outchan ; Unix.close s
                       (*close_in inchan ;
                       close_out outchan *)
      done ;;
  
let get_my_addr () =
   (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;

let main_server  serv_fun =
   if Array.length Sys.argv < 2 then Printf.eprintf "usage : serv_up port\n"
   else try
          let port =  int_of_string Sys.argv.(1) in 
          let my_address = get_my_addr() 
          in establish_server serv_fun  (Unix.ADDR_INET(my_address, port))
        with
          Failure (_) -> 
            Printf.eprintf "serv_up : bad port number\n" ;;

let uppercase_service ic oc =
   try while true do    
         let s = input_line ic in 
         let r = String.uppercase_ascii s 
         in output_string oc (r^"\n") ; flush oc
       done
   with _ -> () ;;
   
main_server  uppercase_service
