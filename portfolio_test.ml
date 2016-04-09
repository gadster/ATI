open OUnit
open Portfolio


(* DEFINE A CUSTOM PORTFOLIO COMPARISON FUNCTION
 THAT IGNORES THE INDIVIDUAL POSITIONS AND FOCUSES
 ON THE HIGH LEVEL VALUES*)
let portfolio_compare port1 port2 = 
  port1.init_cash == port2.init_cash
  && port1.cur_cash == port2.cur_cash
  && port1.equity == port2.equity
  && port1.unrealised_pnl == port2.unrealised_pnl
  && port1.realised_pnl == port2.realised_pnl
       
let expected_portfolio_1 = {init_cash = 50000000;
			  cur_cash = 49909650;
			  equity = 49909650;
			  unrealised_pnl = 0;
			  realised_pnl = -90350;
			  positions = PositionMap.empty}

let test_fixture = "PortfolioAmazonGooglePortfolio" >:::
		     [
		       "RoundTrip1" >:: (fun () ->
					   (* DO A ROUND TRIP *)
					   let portfolio = create_portfolio 50000000 in
					   let portfolio = transact_position portfolio Position.BOT "AMZN" 100 56656 100 in
					   let portfolio = transact_position portfolio Position.BOT "AMZN" 200 56640 100 in
					   let portfolio = transact_position portfolio Position.BOT "GOOG" 200 70750 100 in
					   let portfolio = transact_position portfolio Position.SLD "AMZN" 100 56583 100 in
					   let portfolio = transact_position portfolio Position.BOT "GOOG" 200 70555 100 in
					   let portfolio = transact_position portfolio Position.SLD "AMZN" 200 56559 100 in
					   let portfolio = transact_position portfolio Position.SLD "GOOG" 100 70492 100 in
					   let portfolio = transact_position portfolio Position.SLD "GOOG" 100 70490 100 in
					   let portfolio = transact_position portfolio Position.SLD "GOOG" 100 70492 50 in
					   let portfolio = transact_position portfolio Position.SLD "GOOG" 100 70478 100 in
			
					   (* DO THE CHECKS *)
					   assert_equal ~cmp:portfolio_compare portfolio expected_portfolio_1
					  );
		     ]
		       
(* Test Runner *)
let _ = let return_list = run_test_tt ~verbose:true test_fixture in
	let final_ret_val = List.fold_left (fun ret_val test_result -> match test_result with
								       | RSuccess _ -> ret_val
								       | _ -> 1) 0 return_list in
	exit final_ret_val

