open Order
open AtiEvent

(*
        This TestRiskManager object simply lets the
        sized order through, creates the corresponding
        OrderEvent object and adds it to a list.
 *)

type riskManager = unit

let refine_orders portfolio (sized_order : Order.suggestedOrder) = 
  [ AtiEvent.create_order_event sized_order.ticker sized_order.action sized_order.quantity ]
