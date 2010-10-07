module Syntaxer ( parseString, parse )
where

import Lexer
import Option
import Money
import Fx
import Transaction

-- parser produced by Happy Version 1.18.2

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (([Option], [Group], [Fx], [RawTransaction]))
	| HappyAbsSyn5 ([Option])
	| HappyAbsSyn6 (Option)
	| HappyAbsSyn7 ([Group])
	| HappyAbsSyn8 (Group)
	| HappyAbsSyn9 ([RawSide])
	| HappyAbsSyn10 (RawSide)
	| HappyAbsSyn11 ([Fx])
	| HappyAbsSyn12 (Fx)
	| HappyAbsSyn13 ([RawTransaction])
	| HappyAbsSyn14 (RawTransaction)
	| HappyAbsSyn15 (String)
	| HappyAbsSyn16 (Maybe Moneys)
	| HappyAbsSyn17 (Moneys)
	| HappyAbsSyn18 ([Money])
	| HappyAbsSyn19 (Money)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (35) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 _ = happyReduce_6

action_3 (37) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (35) = happyShift action_10
action_5 (8) = happyGoto action_8
action_5 (11) = happyGoto action_9
action_5 _ = happyReduce_15

action_6 (29) = happyShift action_7
action_6 _ = happyFail

action_7 (35) = happyShift action_16
action_7 (36) = happyShift action_17
action_7 _ = happyFail

action_8 _ = happyReduce_7

action_9 (36) = happyShift action_15
action_9 (12) = happyGoto action_12
action_9 (13) = happyGoto action_13
action_9 (21) = happyGoto action_14
action_9 _ = happyReduce_18

action_10 (33) = happyShift action_11
action_10 _ = happyFail

action_11 (33) = happyShift action_35
action_11 (34) = happyShift action_28
action_11 (35) = happyShift action_36
action_11 (9) = happyGoto action_30
action_11 (10) = happyGoto action_31
action_11 (24) = happyGoto action_32
action_11 (25) = happyGoto action_33
action_11 (27) = happyGoto action_34
action_11 _ = happyFail

action_12 _ = happyReduce_16

action_13 (33) = happyShift action_27
action_13 (34) = happyShift action_28
action_13 (35) = happyShift action_29
action_13 (14) = happyGoto action_20
action_13 (22) = happyGoto action_21
action_13 (23) = happyGoto action_22
action_13 (24) = happyGoto action_23
action_13 (25) = happyGoto action_24
action_13 (26) = happyGoto action_25
action_13 (27) = happyGoto action_26
action_13 _ = happyReduce_1

action_14 (33) = happyShift action_19
action_14 _ = happyFail

action_15 (35) = happyShift action_18
action_15 _ = happyFail

action_16 _ = happyReduce_4

action_17 _ = happyReduce_5

action_18 _ = happyReduce_30

action_19 (36) = happyShift action_15
action_19 (21) = happyGoto action_50
action_19 _ = happyFail

action_20 _ = happyReduce_19

action_21 (28) = happyShift action_48
action_21 (31) = happyShift action_49
action_21 _ = happyFail

action_22 _ = happyReduce_31

action_23 _ = happyReduce_33

action_24 _ = happyReduce_34

action_25 _ = happyReduce_35

action_26 _ = happyReduce_36

action_27 (33) = happyShift action_27
action_27 (34) = happyShift action_28
action_27 (35) = happyShift action_29
action_27 (23) = happyGoto action_47
action_27 (24) = happyGoto action_23
action_27 (25) = happyGoto action_24
action_27 (26) = happyGoto action_25
action_27 (27) = happyGoto action_26
action_27 _ = happyFail

action_28 (35) = happyShift action_46
action_28 _ = happyFail

action_29 (32) = happyShift action_37
action_29 (36) = happyShift action_45
action_29 (17) = happyGoto action_40
action_29 (18) = happyGoto action_41
action_29 (19) = happyGoto action_42
action_29 (20) = happyGoto action_43
action_29 (21) = happyGoto action_44
action_29 _ = happyReduce_38

action_30 (31) = happyShift action_39
action_30 _ = happyReduce_8

action_31 _ = happyReduce_9

action_32 _ = happyReduce_11

action_33 _ = happyReduce_12

action_34 _ = happyReduce_13

action_35 (33) = happyShift action_35
action_35 (34) = happyShift action_28
action_35 (35) = happyShift action_36
action_35 (10) = happyGoto action_38
action_35 (24) = happyGoto action_32
action_35 (25) = happyGoto action_33
action_35 (27) = happyGoto action_34
action_35 _ = happyFail

action_36 (32) = happyShift action_37
action_36 _ = happyReduce_38

action_37 (36) = happyShift action_57
action_37 _ = happyFail

action_38 _ = happyReduce_14

action_39 (33) = happyShift action_35
action_39 (34) = happyShift action_28
action_39 (35) = happyShift action_36
action_39 (10) = happyGoto action_56
action_39 (24) = happyGoto action_32
action_39 (25) = happyGoto action_33
action_39 (27) = happyGoto action_34
action_39 _ = happyFail

action_40 _ = happyReduce_40

action_41 (36) = happyShift action_45
action_41 (19) = happyGoto action_55
action_41 (20) = happyGoto action_43
action_41 (21) = happyGoto action_44
action_41 _ = happyReduce_24

action_42 _ = happyReduce_25

action_43 _ = happyReduce_27

action_44 _ = happyReduce_28

action_45 (35) = happyShift action_18
action_45 _ = happyReduce_29

action_46 _ = happyReduce_41

action_47 _ = happyReduce_37

action_48 (30) = happyShift action_54
action_48 (36) = happyShift action_45
action_48 (16) = happyGoto action_52
action_48 (17) = happyGoto action_53
action_48 (18) = happyGoto action_41
action_48 (19) = happyGoto action_42
action_48 (20) = happyGoto action_43
action_48 (21) = happyGoto action_44
action_48 _ = happyFail

action_49 (33) = happyShift action_27
action_49 (34) = happyShift action_28
action_49 (35) = happyShift action_29
action_49 (23) = happyGoto action_51
action_49 (24) = happyGoto action_23
action_49 (25) = happyGoto action_24
action_49 (26) = happyGoto action_25
action_49 (27) = happyGoto action_26
action_49 _ = happyFail

action_50 _ = happyReduce_17

action_51 _ = happyReduce_32

action_52 (28) = happyShift action_58
action_52 _ = happyFail

action_53 _ = happyReduce_23

action_54 _ = happyReduce_22

action_55 _ = happyReduce_26

action_56 _ = happyReduce_10

action_57 _ = happyReduce_39

action_58 (33) = happyShift action_27
action_58 (34) = happyShift action_28
action_58 (35) = happyShift action_29
action_58 (22) = happyGoto action_59
action_58 (23) = happyGoto action_22
action_58 (24) = happyGoto action_23
action_58 (25) = happyGoto action_24
action_58 (26) = happyGoto action_25
action_58 (27) = happyGoto action_26
action_58 _ = happyFail

action_59 (29) = happyShift action_60
action_59 (31) = happyShift action_49
action_59 _ = happyFail

action_60 (35) = happyShift action_62
action_60 (15) = happyGoto action_61
action_60 _ = happyFail

action_61 _ = happyReduce_20

action_62 _ = happyReduce_21

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_1, happy_var_2, happy_var_3, happy_var_4)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyTerminal (String happy_var_3))
	_
	(HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn6
		 (StringOption happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyTerminal (Number happy_var_3))
	_
	(HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn6
		 (NumberOption happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn8
		 (Group happy_var_1 (reverse happy_var_3)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (RawSideOverride happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  11 happyReduction_15
happyReduction_15  =  HappyAbsSyn11
		 ([]
	)

happyReduce_16 = happySpecReduce_2  11 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 (Fx happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  13 happyReduction_18
happyReduction_18  =  HappyAbsSyn13
		 ([]
	)

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 7 14 happyReduction_20
happyReduction_20 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (RawTransaction (reverse happy_var_1) (reverse happy_var_5) happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn16
		 (Nothing
	)

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (Just happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (Moneys happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  18 happyReduction_26
happyReduction_26 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 (HappyTerminal (Number happy_var_1))
	 =  HappyAbsSyn19
		 (Sum happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  21 happyReduction_30
happyReduction_30 (HappyTerminal (String happy_var_2))
	(HappyTerminal (Number happy_var_1))
	 =  HappyAbsSyn19
		 (Money happy_var_1 happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  22 happyReduction_31
happyReduction_31 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  22 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  23 happyReduction_34
happyReduction_34 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  23 happyReduction_35
happyReduction_35 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  23 happyReduction_36
happyReduction_36 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  23 happyReduction_37
happyReduction_37 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (RawSideOverride happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  24 happyReduction_38
happyReduction_38 (HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn10
		 (RawSide happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  25 happyReduction_39
happyReduction_39 (HappyTerminal (Number happy_var_3))
	_
	(HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn10
		 (RawSideWithFactor happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  26 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (String happy_var_1))
	 =  HappyAbsSyn10
		 (RawSideWithMoney happy_var_1 happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  27 happyReduction_41
happyReduction_41 (HappyTerminal (String happy_var_2))
	_
	 =  HappyAbsSyn10
		 (RawSideRemove happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Arrow -> cont 28;
	Column -> cont 29;
	Underscore -> cont 30;
	Comma -> cont 31;
	Asterisk -> cont 32;
	Equal -> cont 33;
	Hyphen -> cont 34;
	String happy_dollar_dollar -> cont 35;
	Number happy_dollar_dollar -> cont 36;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError token = error ("Parse error: " ++ show token)

parseString :: String -> ([Option], [Group], [Fx], [RawTransaction])
parseString = parse . alexScanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
