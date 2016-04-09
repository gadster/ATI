open OUnit
open PortfolioHandler
open Core

let test_fixture = "PortfolioHandlerTest" >:::
		     [
		       "CreateOrderFromSignal" >::
			 (fun () ->
			  (* DO THE SETUP *)
			  let signal_event = AtiEvent.create_signal_event "MSFT" Position.BOT in
			  let order = PortfolioHandler.create_order_from_signal signal_event in
			  (* DO THE CHECKS *)
			  assert_equal (Order.create_suggested_order "MSFT" Position.BOT) order
			 );
		       "PlaceOrderOntoQueue" >:: 
			 (fun () ->
			  (* DO THE SETUP *)
			  let handler = create_portfolio_handler 50000000 in
			  let order_event = AtiEvent.create_order_event "MSFT" Position.BOT 100 in
			  let order_list = [order_event] in
			  let () = PortfolioHandler.place_orders_onto_queue handler order_list in
			  let ret_order = Queue.pop handler.eventsQueue in
			  (* DO THE CHECKS *)
			  assert_equal order_event ret_order
			 );
		       "ConvertFillToPortfolioUpdate" >::
			 (fun () ->
			  (* DO THE SETUP *)
			  let handler = create_portfolio_handler 50000000 in
			  let fill_event = AtiEvent.create_fill_event (Time.now ()) "MSFT" Position.BOT 100 1 5025 100 in
			  let handler = PortfolioHandler.convert_fill_to_portfolio_update handler fill_event in
			  let portfolio = handler.pfolio in
			  (* DO THE CHECKS *)
			  assert_equal 49497400 portfolio.cur_cash
			 );
		       "OnSignal" >::
			 (fun () ->
			  (* DO THE SETUP *)
			  let handler = create_portfolio_handler 50000000 in
			  let signal_event = AtiEvent.create_signal_event "MSFT" Position.BOT in
			  let () = PortfolioHandler.on_signal handler signal_event in
			  let ret_order = Queue.pop handler.eventsQueue in
			  let expected_ret_order = AtiEvent.create_order_event "MSFT" Position.BOT 100 in
			  assert_equal expected_ret_order ret_order
			 );
		     ]
		       
(* Test Runner *)
let _ = let return_list = run_test_tt ~verbose:true test_fixture in
	let final_ret_val = List.fold_left (fun ret_val test_result -> match test_result with
								       | RSuccess _ -> ret_val
								       | _ -> 1) 0 return_list in
	exit final_ret_val

