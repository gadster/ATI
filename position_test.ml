open OUnit
open Position
    
let test_fixture = "Position" >:::
		     [
		       "round_trip_1" >:: (fun () ->
					   (* DO A ROUND TRIP *)
					   let mypos = create_position BOT "XOM" 100 7478 100 in
					   let mypos = calculate_initial_value mypos in
					   let mypos = update_market_value mypos 7478 7480 in 
					   let mypos = transact_shares mypos BOT 100 7463 100 in
					   let mypos = transact_shares mypos BOT 250 7462 125 in
					   let mypos = transact_shares mypos SLD 200 7458 100 in
					   let mypos = transact_shares mypos SLD 250 7526 125 in
					   let mypos = update_market_value mypos 7775 7777 in
					   (* DO THE CHECKS *)
					   assert_equal
					     {action = BOT; ticker = "XOM"; quantity = 0; init_price = 7478; init_commission = 100; market_value = 0; realised_pnl = 12950; unrealised_pnl = 0; buys = 450; sells = 450; avg_bot = 7465; avg_sld = 7495; avg_price = 7469; total_bot = 3359250; total_sld = 3372750; total_commission = 550; cost_basis = 0; net = 0; net_total = 13500; net_incl_comm = 12950}
					     mypos
					  );
		       "round_trip_2" >:: (fun () ->
					   (* DO A ROUND TRIP *)
					   let mypos = create_position SLD "PG" 100 7769 100 in
					   let mypos = calculate_initial_value mypos in
					   let mypos = update_market_value mypos 7768 7770 in
					   let mypos = transact_shares mypos SLD 100 7768 100 in
					   let mypos = transact_shares mypos SLD 50 7770 100 in
					   let mypos = transact_shares mypos BOT 100 7777 100 in
					   let mypos = transact_shares mypos BOT 150 7773 100 in
					   let mypos = update_market_value mypos 7772 7772 in
					   (* DO THE CHECKS *)
					   assert_equal
					     {action = SLD; ticker = "PG"; quantity = 0; init_price = 7769; init_commission = 100; market_value = 0; realised_pnl = -2000; unrealised_pnl = 0; buys = 250; sells = 250; avg_bot = 7774; avg_sld = 7768; avg_price = 7768; total_bot = 1943500; total_sld = 1942000; total_commission = 500; cost_basis = 0; net = 0; net_total = -1500; net_incl_comm = -2000}
					     mypos
					  );
		     ]
(* Test Runner *)
let _ = run_test_tt test_fixture

