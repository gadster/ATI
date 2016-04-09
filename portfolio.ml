open Position 
open PriceHandler

module PositionMap = Map.Make(String)

type portfolio = {
    init_cash : money ;
    cur_cash : money ;
    equity : money ;
    unrealised_pnl : money ;
    realised_pnl : money ;
    positions : position PositionMap.t
}

let create_portfolio cash = 
  {
    init_cash=cash;
    cur_cash=cash;
    equity=0;
    unrealised_pnl=0;
    realised_pnl=0;
    positions= PositionMap.empty
  }

(*
        This is called after every position addition or
        modification. It allows the calculations to be
        carried out "from scratch" in order to minimise
        errors.

        All cash is reset to the initial values and the
        PnL is set to zero.
 *)
let reset_values portfolio = 
  let aux = {portfolio with cur_cash = portfolio.init_cash} in
  let aux = {aux with equity = aux.cur_cash} in
  let aux = {aux with unrealised_pnl = 0} in
  let aux = {aux with realised_pnl = 0} in
  aux

let update_portfolio portfolio = 
  let individual_update key position portfolio = {portfolio with 
				     unrealised_pnl = portfolio.unrealised_pnl + position.unrealised_pnl; 
				     realised_pnl = portfolio.realised_pnl + position.realised_pnl;
				     cur_cash = portfolio.cur_cash - position.cost_basis + (position.realised_pnl - position.unrealised_pnl) ;
				     equity = portfolio.equity + (position.market_value - position.cost_basis + (position.realised_pnl - position.unrealised_pnl))}
  in   
  PositionMap.fold individual_update portfolio.positions portfolio

(*
        Adds a new Position object to the Portfolio. This
        requires getting the best bid/ask price from the
        price handler in order to calculate a reasonable
        "market value".

        Once the Position is added, the Portfolio values
        are updated.
 *)
let add_position portfolio action ticker quantity price commission =
  let portfolio = reset_values portfolio in
  if PositionMap.mem ticker portfolio.positions then
    portfolio
  else
    let (bid,ask) = PriceHandler.get_best_bid_ask ticker priceMap in
    let position = (Position.calculate_initial_value (Position.create_position action ticker quantity price commission)) in
    let position = Position.update_market_value position bid ask in
    update_portfolio {portfolio with positions = (PositionMap.add ticker position portfolio.positions)}

(*
        Modifies a current Position object to the Portfolio.
        This requires getting the best bid/ask price from the
        price handler in order to calculate a reasonable
        "market value".

        Once the Position is modified, the Portfolio values
        are updated.
 *)
let modify_position portfolio action ticker quantity price commission = 
  let portfolio = reset_values portfolio in
  if PositionMap.mem ticker portfolio.positions then
    let portfolio = {portfolio with positions = PositionMap.add ticker (Position.transact_shares (PositionMap.find ticker portfolio.positions) action quantity price commission) portfolio.positions} in
    let (bid,ask) = PriceHandler.get_best_bid_ask ticker priceMap in
    update_portfolio {portfolio with positions = PositionMap.add ticker (Position.update_market_value (PositionMap.find ticker portfolio.positions) bid ask) portfolio.positions}
  else
    portfolio
      
(*
        Handles any new position or modification to 
        a current position, by calling the respective
        _add_position and _modify_position methods. 

        Hence, this single method will be called by the 
        PortfolioHandler to update the Portfolio itself.
 *)
let transact_position portfolio action ticker quantity price commission = 
  if PositionMap.mem ticker portfolio.positions then
    modify_position portfolio action ticker quantity price commission
  else
    add_position portfolio action ticker quantity price commission
		 
