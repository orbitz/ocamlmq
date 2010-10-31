open Printf
open Mq_types
open Lwt

type msg_id = string


type msg_stored = { msg : message
		  ; ack_pending : bool
		  }

type t = { tbl : (msg_id, msg_stored) Hashtbl.t
	 ; debug : bool
	 }

let do_save tbl ?(ack_pending = false) msg =
  Hashtbl.replace tbl msg.msg_id { msg = msg; ack_pending = ack_pending }

let query tbl f =
  Hashtbl.fold (fun k v accum -> if f k v then v::accum else accum) tbl []

let get_msg tbl msg_id =
  (Hashtbl.find tbl msg_id).msg

let get_ack_pending_msg' tbl msg_id =
  query tbl (fun k v -> k == msg_id && v.ack_pending)

let create debug = { tbl = Hashtbl.create 100
		   ; debug = debug
		   }

let initialize q = return ()

let save_msg q ?(low_priority = false) msg = 
  eprintf "Saving message: %s\n" msg.msg_id;
  match msg.msg_destination with
    | Topic _ | Control _ -> return ()
    | Queue queue ->
      (* Ignoring priority *)
      return (do_save q.tbl msg)

let register_ack_pending_new_msg q msg =
  return (do_save q.tbl ~ack_pending:true msg)

let register_ack_pending_msg q msg_id =
  match get_ack_pending_msg' q.tbl msg_id with
    | [] -> return false
    | xs -> begin
      List.iter (fun m -> do_save q.tbl ~ack_pending:false m.msg) xs;
      return true
    end

let get_ack_pending_msg q msg_id =
  match get_ack_pending_msg' q.tbl msg_id with
    | [] -> return None
    | x::_ -> return (Some x.msg)

let ack_msg q msg_id = 
  return (Hashtbl.remove q.tbl msg_id)

let unack_msg q msg_id = 
  return (do_save q.tbl ~ack_pending:false (get_msg q.tbl msg_id))

let get_msg_for_delivery q queue =
  match List.map (fun m -> m.msg) 
    (query q.tbl 
       (fun k v -> v.msg.msg_destination = Queue queue && v.ack_pending = false)) with
    | [] -> return None
    | x::_ -> begin
      do_save q.tbl ~ack_pending:true x;
      return (Some x)
    end

let count_queue_msgs q queue = 
  return (Int64.of_int 
	    (List.length 
	       (query q.tbl 
		  (fun k v -> v.msg.msg_destination = Queue queue && v.ack_pending = false))))

let crash_recovery q = return ()
