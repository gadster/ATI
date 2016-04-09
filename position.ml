type action = BOT | SLD

(* MONEY TYPE - Used to represent money. The chosen representation is to describe cents as ints *)
type money = int

type position = 
    { action : action ;
      ticker : string ;
      quantity : int ;
      init_price : money ; 
      init_commission : money ;
      market_value : money ;
      realised_pnl : money ;
      unrealised_pnl : money ;
      buys : int ;
      sells : int ;
      avg_bot : money ;
      avg_sld : money ;
      avg_price : money ;
      total_bot : money ;
      total_sld : money ;
      total_commission : money ;

      cost_basis : money ;
      net : money ;
      net_total : money ;
      net_incl_comm : money
    }

type t = position

let compare = Pervasives.compare

let create_position act symbol qty price commission =
  { action = act;
    ticker = symbol; 
    quantity = qty;
    init_price = price;
    init_commission = commission; 
    market_value=0;
    realised_pnl = 0;
    unrealised_pnl = 0; 
    buys = 0;
    sells = 0; 
    avg_bot = 0;
    avg_sld = 0;
    avg_price = 0;
    total_bot = 0;
    total_sld = 0; 
    total_commission = commission;
    cost_basis = 0;
    net = 0;
    net_total = 0;
    net_incl_comm = 0
 }

(*
        Depending upon whether the action was a buy or sell ("BOT"
        or "SLD") calculate the average bought cost, the total bought
        cost, the average price and the cost basis.

        Finally, calculate the net total with and without commission.
 *)
let calculate_initial_value position = 
  let calculate_dependable_values position =
    match position.action with 
    | BOT -> 
       let new_buys = position.quantity in
       let new_avg_bot = position.init_price in
       let new_total_bot = new_buys * new_avg_bot in
       let new_avg_price = (position.init_price * position.quantity + position.init_commission) / position.quantity in
       let new_cost_basis = position.quantity * new_avg_price in
       {position with buys = new_buys; avg_bot = new_avg_bot; total_bot = new_total_bot; avg_price = new_avg_price; cost_basis = new_cost_basis }
    | SLD -> 
       let new_sells = position.quantity in
       let new_avg_sld = position.init_price in
       let new_total_sld = new_sells * new_avg_sld in
       let new_avg_price = (position.init_price * position.quantity + position.init_commission) / position.quantity in
       let new_cost_basis = position.quantity * new_avg_price in
       {position with sells = new_sells; avg_sld = new_avg_sld; total_sld = new_total_sld; avg_price = new_avg_price; cost_basis = new_cost_basis }
  in
  let partial_position = calculate_dependable_values position in
  {partial_position with net = partial_position.buys - partial_position.sells; net_total = partial_position.total_sld - partial_position.total_bot; net_incl_comm = partial_position.total_sld - partial_position.total_bot - partial_position.init_commission}
    
(*
        The market value is tricky to calculate as we only have
        access to the top of the order book through Interactive
        Brokers, which means that the true redemption price is
        unknown until executed.

        However, it can be estimated via the mid-price of the
        bid-ask spread. Once the market value is calculated it
        allows calculation of the unrealised and realised profit
        and loss of any transactions.
*)
let update_market_value position bid ask = 
  let midpoint = (bid + ask) / 2 in
  let new_market_value = position.quantity * midpoint in
  let new_unrealised_pnl = new_market_value - position.cost_basis in
  let new_realised_pnl = new_market_value + position.net_incl_comm in
  {position with market_value = new_market_value; unrealised_pnl = new_unrealised_pnl; realised_pnl = new_realised_pnl}
    

(*
        Calculates the adjustments to the Position that occur
        once new shares are bought and sold.

        Takes care to update the average bought/sold, total
        bought/sold, the cost basis and PnL calculations,
        as carried out through Interactive Brokers TWS.
*)
let transact_shares position action quantity price commission =
  let transact_shares_aux =
    match action with
    | BOT ->
       let new_avg_bot = (position.avg_bot * position.buys + price * quantity) / (position.buys + quantity) in
       let new_buys = position.buys + quantity in
       let new_total_bot = new_buys * new_avg_bot in 
       begin
	 match position.action with
	 | BOT ->
	    let aux_avg_price = position.avg_price in
	    let aux_buys = new_buys in
	    {position with avg_bot = new_avg_bot; buys = new_buys; total_bot = new_total_bot; avg_price = (aux_avg_price * aux_buys + price * quantity+commission) / (new_buys + quantity)}
	 | SLD -> {position with avg_bot = new_avg_bot; buys = new_buys; total_bot = new_total_bot}
       end
    | SLD ->
       let new_avg_sld = (position.avg_sld * position.sells + price * quantity) / (position.sells + quantity) in
       let new_sells = position.sells + quantity in
       let new_total_sld = new_sells * new_avg_sld in 
       begin
	 match position.action with
	 | SLD ->
	    let aux_avg_price = position.avg_price in
	    let aux_sells = new_sells in
	    {position with avg_sld = new_avg_sld; sells = new_sells; total_sld = new_total_sld; avg_price = (aux_avg_price * aux_sells + price * quantity - commission) / (new_sells + quantity)}
	 | BOT -> {position with avg_sld = new_avg_sld; sells = new_sells; total_sld = new_total_sld}
       end       
  in
  let aux_position = transact_shares_aux in
  let new_total_commission = aux_position.total_commission + commission in
  let new_net = aux_position.buys - aux_position.sells in
  let new_quantity = new_net in
  let new_net_total = aux_position.total_sld - aux_position.total_bot in
  let new_net_incl_comm = new_net_total - new_total_commission in
  let new_cost_basis = new_quantity * aux_position.avg_price in
  {aux_position with net = new_net; quantity = new_quantity; net_total = new_net_total; net_incl_comm = new_net_incl_comm; cost_basis = new_cost_basis; total_commission = new_total_commission}
    
