open Order

type positionSizer = unit

let size_order portfolio (initial_order : Order.suggestedOrder) = 
  {initial_order with quantity = 100}
