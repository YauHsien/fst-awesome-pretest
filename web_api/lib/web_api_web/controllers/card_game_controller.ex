defmodule WebApiWeb.CardGameController do
  use WebApiWeb, :controller
  @moduledoc """
  Main API for the card-trading game.
  """
  def status(conn, %{}) do
    case WebApi.check_node() do
      {:pong, :pong} ->
        case {:rpc.call(WebApi.node_users(), :lobby, :status, []),
              :rpc.call(WebApi.node_trades(), :lobby, :status, [])} do
          {{:ok,result1}, {:ok,result2}} ->
            conn
            |> send_resp(200, "#{(result1+result2)/2}")
          {{:ok,result1}, {e,_}} when e !== :ok ->
            conn
            |> send_resp(200, "#{(result1+0)/2}")
          {{e,_}, {:ok,result2}} when e !== :ok ->
            conn
            |> send_resp(200, "#{(0+result2)/2}")
          _ ->
            conn
            |> send_resp(200, "0")
        end
      _ ->
        conn
        |> send_resp(503, "Service(s) NOT found")
    end
  end

  @doc """
  NAME claim_user/2
  ----
  To claim a user existing in the platform.

  Find a user by one's identity, a composition (timestamp, seq);
  make a new one if no user found;
  load the user info, accompanied with one's orders tracker, into the platform;
  eventually, return the collected user info.

  INPUT
  -----
  timestamp       The TS when the user first came.
  seq             A identical sequence number for a user within single timestamp.
  orders_count    Expected length of user orders tracker; 50 by default.

  RESPONSE
  --------
  {"user":{
    "timestamp": timestamp,
    "seq":       sequence-number,
    "orders":    [{
      "timestamp": timestamp,
      "seq":       sequence-number,
      "orderType": buy-or-sell-string,
      "currency":  currency-string,
      "price":     floating-number,
      "cardType":  card-type-string,
      "match":     undefined
      OR
      "match":     {
        // another order
        "timestamp": timestamp,
        "seq":       sequence-number,
        "orderType": buy-or-sell-string,
        "currency":  currency-string,
        "price":     floating-number,
        "cardType":  card-type-string
        }
      }, ... ]}}
  OR
  {"errors":{"detail":some-reason-string}}
  """
  def claim_user(conn, %{"orders_count" => orders_count}) when orders_count != "50" do
    conn
    |> send_resp(501, "Specifying expected count of orders to track is NOT supported in this version")
  end
  def claim_user(conn, %{"timestamp" => timestamp, "seq" => seq, "orders_count" => orders_count}) do
    case WebApi.check_node() do
      {:pong, :pong} ->
        case {WebApi.parse_int(timestamp), WebApi.parse_int(seq), WebApi.parse_int(orders_count)} do
          {{symbol1,msg1}, {symbol2,msg2}, {symbol3,msg3}}
          when symbol1 == :error or symbol2 == :error or symbol3 == :error  ->
            messages =
              Enum.filter(
                [{symbol1,msg1,"timestamp"},{symbol2,msg2,"seq"},{symbol3,msg3,"orders_count"}],
                fn {symbol,_,_} -> symbol == :error end)
            summary =
              Enum.join(
                Enum.map(messages, fn {_symbol,msg,field} -> "when parsing field #{field}: #{msg}" end),
                "; ")
            conn
            |> send_resp(500, summary)
          {{:ok,timestamp1}, {:ok,seq1}, {:ok,orders_count1}} ->
            case :rpc.call(WebApi.node_users(), :lobby, :find_user, [timestamp1, seq1, orders_count1]) do
              {:badrpc, reason} ->
                conn
                |> send_resp(502, "Bad RPC to node-users: #{reason}")
              {:ok, result} ->
                conn
                |> send_resp(200, result)
              {:error, {status_code, reason}} ->
                conn
                |> send_resp(status_code, reason)
            end
        end
      _ ->
        conn
        |> send_resp(503, "Service(s) NOT found")
    end
  end

  @doc """
  NAME draw_cards/2
  ----
  To draw cards for a user.

  Find the user;
  send draw-cards command to it;
  eventually, get result.

  INPUT
  -----
  timestamp       The TS when the user first came.
  seq             A identical sequence number for a user within single timestamp.
  count           Card count to draw.

  RESPONSE
  --------
  {"cards": [{
     "timestamp": timestamp,
     "seq":       sequence-number,
     "cardType":  card-type-string,
     "currency":  currency-string,
     "price":     floating-number,
     }, ... ]}
  OR
  {"errors":{"detail":some-reason-string}}
  """
  def draw_cards(conn, %{"count" => count}) when count != "1" do
    conn
    |> send_resp(501, "Specifying expected count of cards to draw is NOT supported in this version")
  end
  def draw_cards(conn, %{"timestamp" => timestamp, "seq" => seq, "count" => count}) do
    case WebApi.check_node() do
      {:pong, :pong} ->
        case {WebApi.parse_int(timestamp), WebApi.parse_int(seq), WebApi.parse_int(count)} do
          {{symbol1,msg1}, {symbol2,msg2}, {symbol3,msg3}}
          when symbol1 == :error or symbol2 == :error or symbol3 == :error  ->
            messages =
              Enum.filter(
                [{symbol1,msg1,"timestamp"},{symbol2,msg2,"seq"},{symbol3,msg3,"count"}],
                fn {symbol,_,_} -> symbol == :error end)
            summary =
              Enum.join(
                Enum.map(messages, fn {_symbol,msg,field} -> "when parsing field #{field}: #{msg}" end),
                "; ")
            conn
            |> send_resp(500, summary)
          {{:ok,timestamp1}, {:ok,seq1}, {:ok,count1}} ->
            case :rpc.call(WebApi.node_users(), :lobby, :draw_cards, [timestamp1, seq1, count1]) do
              {:badrpc, reason} ->
                conn
                |> send_resp(502, "Bad RPC to node-users: #{reason}")
              {:ok, result} ->
                conn
                |> send_resp(200, result)
              {:error, {status_code, reason}} ->
                conn
                |> send_resp(status_code, reason)
            end
        end
      _ ->
        conn
        |> send_resp(503, "Service(s) NOT found")
    end
  end

  @doc """
  NAME place_order/2
  ----
  A user places an order.

  Find the user;
  send place-order command to it;
  eventually, get the result :ok.

  INPUT
  -----
  userTs         The TS part in the user identity.
  userSeq        Teh Sequence part in the user identity.
  cardTs         The TS part in the card identity.
  cardSeq        The Sequence part in the card identity.
  cardType       Card type: "Pikachu", "Bulbasaur", "Charmander", "Squirtle".
  currency       Currency such as "USD".
  price          Floating number.
  orderType      Buy or sell string.

  RESPONSE
  --------
  {"orderId":{
     "timestamp": timestamp,
     "seq":       sequence-number
  }}
  OR
  {"errors":{"detail":some-reason-string}}
  """
  def place_order(conn, %{
        "userTs" => user_timestamp, "userSeq" => user_seq,
        "cardTs" => card_timestamp, "cardSeq" => card_seq,
        "cardType" => card_type,    "currency" => currency,
        "price" => price,           "orderType" => order_type }) do
    case WebApi.check_node() do
      {:pong, :pong} ->
        case {WebApi.parse_int(user_timestamp), WebApi.parse_int(user_seq),
              WebApi.parse_int(card_timestamp), WebApi.parse_int(card_seq),
              WebApi.parse_atom(card_type), WebApi.parse_atom(currency),
              WebApi.parse_float(price), WebApi.parse_atom(order_type)  } do
          {{s1,m1}, {s2,m2}, {s3,m3}, {s4,m4}, {s5,m5}, {s6,m6}, {s7,m7}, {s8,m8}}
          when s1 == :error or s2 == :error or s3 == :error or s4 == :error
          or s5 == :error or s6 == :error or s7 == :error or s8 == :error  ->
            messages =
              Enum.filter(
                [{s1,m1,"userTs"},{s2,m2,"userSeq"},{s3,m3,"cardTs"},{s4,m4,"cardSeq"},
                 {s5,m5,"cardType"},{s6,m6,"currency"},{s7,m7,"price"},{s8,m8,"orderType"}],
                fn {symbol,_,_} -> symbol == :error end)
            summary =
              Enum.join(
                Enum.map(messages, fn {_symbol,msg,field} -> "when parsing field #{field}: #{msg}" end),
                "; ")
            conn
            |> send_resp(500, summary)
          {{_,user_timestamp1}, {_,user_seq1}, {_,card_timestamp1}, {_,card_seq1},
           {_,card_type1},      {_,currency1}, {_,price1},          {_,order_type1}} ->
            case :rpc.call(WebApi.node_users(), :lobby, :place_order,
                  [user_timestamp1, user_seq1, card_timestamp1, card_seq1,
                   card_type1,      currency1, price1,          order_type1]) do
              {:badrpc, reason} ->
                conn
                |> send_resp(502, "Bad RPC to node-users: #{reason}")
              {:ok, result} ->
                conn
                |> send_resp(200, result)
              {:error, {status_code, reason}} ->
                conn
                |> send_resp(status_code, reason)
            end
        end
      _ ->
        conn
        |> send_resp(503, "Service(s) NOT found")
    end
  end

  @doc """
  NAME track_trades/2
  ----
  To get recent trades of some card type.

  INPUT
  -----
  card_type       "Pikachu", "Bulbasaur", "Charmander", or "Squirtle".
  count           Expected count of trades to get.

  RESPONSE
  --------
  {"trades":    [{
     "timestamp": timestamp,
     "seq":       sequence-number,
     "orderType": buy-or-sell-string,
     "currency":  currency-string,
     "price":     floating-number,
     "cardType":  card-type-string,
     "match":     {
        // another order
        "timestamp": timestamp,
        "seq":       sequence-number,
        "orderType": buy-or-sell-string,
        "currency":  currency-string,
        "price":     floating-number,
        "cardType":  card-type-string
        }
     }, ... ]}
  OR
  {"errors":{"detail":some-reason-string}}
  """
  def track_trades(conn, %{"count" => count}) when count != "50" do
    conn
    |> send_resp(501, "Specifying expected count of trades to track is NOT supported in this version")
  end
  def track_trades(conn, %{"card_type" => card_type, "count" => count}) do
    case WebApi.check_node() do
      {:pong, :pong} ->
        case {WebApi.parse_atom(card_type), WebApi.parse_int(count)} do
          {{symbol1,msg1}, {symbol2,msg2}}
          when symbol1 == :error or symbol2 == :error ->
            messages =
              Enum.filter(
                [{symbol1,msg1,"card_type"},{symbol2,msg2,"count"}],
                fn {symbol,_,_} -> symbol == :error end)
            summary =
              Enum.join(
                Enum.map(messages, fn {_symbol,msg,field} -> "when parsing field #{field}: #{msg}" end),
                "; ")
            conn
            |> send_resp(500, summary)
          {{:ok,card_type1}, {:ok,count1}} ->
            case :rpc.call(WebApi.node_trades(), :lobby, :track_trades, [card_type1, count1]) do
              {:badrpc, reason} ->
                conn
                |> send_resp(502, "Bad RPC to node-users: #{reason}")
              {:ok, result} ->
                conn
                |> send_resp(200, result)
              {:error, {status_code, reason}} ->
                conn
                |> send_resp(status_code, reason)
            end
        end
      _ ->
        conn
        |> send_resp(503, "Service(s) NOT found")
    end
  end
end
