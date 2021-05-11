-record(
   % User information including one's identity, i.e., (timestamp, seq),
   % and orders tracking.
   user,
   {% Unix timestamp by default
    timestamp =
        calendar:datetime_to_gregorian_seconds(
                  calendar:universal_time()),
    % Artificial sort factor
    sequence = rand:uniform(?CommonRNGRange),
    % Orders tracking
    orders = []
   }).
