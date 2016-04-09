open Position

type suggestedOrder = {
  ticker : string;
  action : Position.action;
  quantity : int
}

let create_suggested_order ?quant:(quant=0) tick act = {
  ticker = tick;
  action = act;
  quantity = quant
}
