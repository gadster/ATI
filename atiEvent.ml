open Position
open Core

type eventType = TICK | SIGNAL | ORDER | FILL

type tickEvent = {
  eType : eventType;
  ticker : string;
  time : Time.t;
  bid : Position.money;
  ask : Position.money
}

let create_tick_event tic ?(tim = Time.now ()) b a = {
  eType = TICK;
  ticker = tic;
  time = tim;
  bid = b;
  ask = a
}

type signalEvent = {
  eType : eventType;
  ticker : string;
  action : Position.action
}

let create_signal_event tic act = {
  eType = SIGNAL;
  ticker = tic;
  action = act
}


type orderEvent = {
  eType : eventType;
  ticker : string;
  action : Position.action;
  quantity : int
}

let create_order_event tic act quant = {
  eType = ORDER;
  ticker = tic;
  action = act;
  quantity = quant;
}

type fillEvent = {
  eType : eventType;
  timestamp : Time.t;
  ticker : string;
  action : Position.action;
  quantity : int;
  (* TODO: FIX TYPE OF EXCHANGE *)
  exchange : int;
  price : Position.money;
  commission : Position.money
}

let create_fill_event time tic act quant exch pri com = {
  eType = FILL;
  timestamp = time;
  ticker = tic;
  action = act;
  quantity = quant;
  exchange = exch;
  price = pri;
  commission = com;
}
