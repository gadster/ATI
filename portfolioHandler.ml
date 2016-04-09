open Batteries 
open List
open Position
open Portfolio
open AtiEvent
open PriceHandler
open PositionSizer
open RiskManager
open Order       
       (*
        The PortfolioHandler is designed to interact with the 
        backtesting or live trading overall event-driven
        architecture. It exposes two methods, on_signal and
        on_fill, which handle how SignalEvent and FillEvent
        objects are dealt with.
        Each PortfolioHandler contains a Portfolio object,
        which stores the actual Position objects. 
        The PortfolioHandler takes a handle to a PositionSizer
        object which determines a mechanism, based on the current
        Portfolio, as to how to size a new Order.
        The PortfolioHandler also takes a handle to the 
        RiskManager, which is used to modify any generated 
        Orders to remain in line with risk parameters.
	*)

type portfolioHandler = {
  init_cash : money;
  pfolio :  Portfolio.portfolio;
  eventsQueue : AtiEvent.orderEvent Batteries.Queue.t
}
			  
let create_portfolio_handler in_cash = { 
  init_cash=in_cash; 
  pfolio=Portfolio.create_portfolio in_cash;
  eventsQueue = Batteries.Queue.create ()
}
					 
(*
        Take a SignalEvent object and use it to form a
        SuggestedOrder object. These are not OrderEvent objects,
        as they have yet to be sent to the RiskManager object.
        At this stage they are simply "suggestions" that the
        RiskManager will either verify, modify or eliminate.
 *)
let create_order_from_signal (signal_event : AtiEvent.signalEvent) = 
  Order.create_suggested_order signal_event.ticker signal_event.action
		
(*
        Once the RiskManager has verified, modified or eliminated
        any order objects, they are placed onto the events queue,
        to ultimately be executed by the ExecutionHandler.
 *)
let place_orders_onto_queue pHandler (orderList : AtiEvent.orderEvent list) =
  List.iter (fun event -> Batteries.Queue.push event pHandler.eventsQueue) orderList

(*
        Upon receipt of a FillEvent, the PortfolioHandler converts
        the event into a transaction that gets stored in the Portfolio
        object. This ensures that the broker and the local portfolio
        are "in sync".

        In addition, for backtesting purposes, the portfolio value can
        be reasonably estimated in a realistic manner, simply by 
        modifying how the ExecutionHandler object handles slippage,
        transaction costs, liquidity and market impact.
 *)
let convert_fill_to_portfolio_update pHandler (fill_event : AtiEvent.fillEvent) = 
  {pHandler with 
    pfolio = Portfolio.transact_position
	       pHandler.pfolio
	       fill_event.action
	       fill_event.ticker
	       fill_event.quantity
	       fill_event.price
	       fill_event.commission
  }

(*
        This is called by the backtester or live trading architecture
        to form the initial orders from the SignalEvent. 

        These orders are sized by the PositionSizer object and then
        sent to the RiskManager to verify, modify or eliminate.

        Once received from the RiskManager they are converted into
        full OrderEvent objects and sent back to the events queue.
 *)
let on_signal pHandler signal_event =
  let initial_order = create_order_from_signal signal_event in
  let sized_order = PositionSizer.size_order pHandler.pfolio initial_order in
  let order_events = RiskManager.refine_orders pHandler.pfolio sized_order in
  place_orders_onto_queue pHandler order_events

(*
        This is called by the backtester or live trading architecture
        to take a FillEvent and update the Portfolio object with new
        or modified Positions.

        In a backtesting environment these FillEvents will be simulated
        by a model representing the execution, whereas in live trading
        they will come directly from a brokerage (such as Interactive
        Brokers).
 *)
let on_fill pHandler fill_event = 
  convert_fill_to_portfolio_update pHandler fill_event
