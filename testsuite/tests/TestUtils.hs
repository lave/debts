module TestUtils
where

import Transaction
import Utils

instance (Eq group) => Eq (Transaction_ group) where
    t1 == t2 =
        same (payers t1) (payers t2) &&
        same (beneficators t1) (beneficators t2) &&
        Transaction.sum t1 == Transaction.sum t2 &&
        date t1 == date t2 &&
        contragent t1 == contragent t2 &&
        category t1 == category t2 &&
        same (tags t1) (tags t2) &&
        comment t1 == comment t2

