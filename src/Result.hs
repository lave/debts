module Result
where

import Side


data Result =
      Balances [(Side, Side)]
    | Logs [(Side, Log)]
    | File String
    deriving Eq

data Log = Log String
    deriving Eq
