open Position

module PriceHandlerMap = Map.Make(String)

let priceMap = PriceHandlerMap.add "MSFT" (5028,5031) 
				   (PriceHandlerMap.add "AMZN" (56414,56514) 
							(PriceHandlerMap.add "GOOG" (70546,70546) PriceHandlerMap.empty))
				   
let get_best_bid_ask ticker priceMap: (money * money) = 
  PriceHandlerMap.find ticker priceMap
		       
		       
