%% Order operational semantic %% ------------------------------------------------------------------------------------ %%
%
%  1. In the begining an order is created with a `timestamp`, the natual sort factor, and a `sequence` which is a number
%     between 1 and `CommonRNGRange` (65535 by default);
%
%       these two numbers, `timestamp` and `sequence`, form the presedence of each order.
%
%  2. When it's a deck of infinite card and each card has some value, it's not important that some card must have fixed
%     color and dollar;
%
%       abstractly every trader holds a "Cards generation" device; just draw a card and give it attributes, including
%       `orderType`, `currency`, `price`, and `cardType`.
%
%  3. An order and card shell be set by its owner.
%
%  4. When an order is created, it is kept track in its owner's order tracker; when being added to the tracker, it causes
%     those cards out of limited number to keep track be poped out;
%
%       the order tracker of a trader must keep track all incomplete order, while keeping track some or no more complete
%       order, so that the size of the order tracker may euquals or be greater than the limit number (50 by default).
%
%  5. Then a new order is put into the global incomplete order tracker;
%
%       when a new order arrive into the tracker, it goes one of two ways, that either it matches another incomplete order
%       and then be both kept track in the trade tracker, or it just be kept track by the incomplete order tracker.
%
%  6. Finally an order may be kept track by either its owner's order tracker or the global trade tracker;
%
%       once it loses both track, then it can be removed from the computer memory.
%
%% ------------------------------------------------------------------------------------ %% Order operational semantic %%
%
-record(order, {timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?UnixEpoch, %% Unix timestamp
                sequence = rand:uniform(?CommonRNGRange),  %% Artificial sort factor.
                orderType, %% ?Buy or ?Sell.
                currency,  %% ?USD or the other one.
                price,     %% Between 1.00 and 10.00 USD.
                cardType,  %% ?Pikachu, ?Bulbasaur, ?Charmander, or ?Squirtle.
                owner,                        %% Pid; trader.
                incompleteOrderTracker = nil, %% Pid; tracker of all incomplete orders
                orderTracker = nil,           %% Pid; to keep track of recent orders of a trader.
                tradeTracker = nil}).         %% Pid; to keep track of trades of each kind of card.

