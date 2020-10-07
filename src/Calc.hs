module Calc (State(),
             populate, display) where

import           Data.Default (Default (def))

data Operation = Add | Sub | Mul | Div deriving (Show, Eq)

data Command = Digit Char
             | Dot
             | Operation Operation
             | Flush | Clear | ClearError
             deriving (Show, Eq)

type Raw = (Float, Bool)

data State = EnteringA     Raw                     -- raw A
           | EnteredAandOp Float  Operation        -- A, Op
           | EnteringB     Float  Operation Raw    -- A, Op, raw B
           | Calculated    Float  Operation Float  -- A, Op, B
           | Error         Float  String           -- A, Message
           deriving Show


instance Default State where
  def = EnteringA (0, False)


display :: State -> String
display s =
  case s of
    EnteringA     a     -> show (fromRaw a)
    EnteredAandOp a _   -> show a
    EnteringB     _ _ b -> show (fromRaw b)
    Calculated    a _ _ -> show a
    Error         _ msg -> msg


fromRaw :: Raw -> Float
fromRaw = fst

asRaw :: Float -> Raw
asRaw x = (x, x /= (fromInteger . truncate) x)


parseInput :: String -> Maybe Command
parseInput (x:_) | x `elem` "0123456789" = Just $ Digit x
parseInput x =
  case x of
    "."  -> Just Dot
    "+"  -> Just (Operation Add)
    "-"  -> Just (Operation Sub)
    "*"  -> Just (Operation Mul)
    "/"  -> Just (Operation Div)
    "="  -> Just Flush
    "C"  -> Just Clear
    "CE" -> Just ClearError
    _    -> Nothing


populate :: String -> State -> State
populate i =
  case parseInput i of
    Just (Digit x)      -> addDigit x
    Just Dot            -> addDot
    Just (Operation op) -> applyOp op
    Just cmd            -> applyCmd cmd
    Nothing             -> id


addDigit :: Char -> State -> State
addDigit x s =
  case s of
    (EnteringA a)        -> EnteringA (update a)
    (EnteringB a op b)   -> EnteringB a op (update b)
    (EnteredAandOp a op) -> EnteringB a op (asRaw x')
    (Calculated _ _ _)   -> EnteringA (asRaw x')
    _                    -> s
  where
    update (a, False) = (a * 10 + x', False)
    update (a, True)  =
      let (a', b) = properFraction a
      in (fromInteger a' + (x' + b / 10) / 10, True)
      --  ^ FIXME: add digits to the fractional part properly!
    x' = read (x:[]) :: Float


addDot :: State -> State
addDot s =
  case s of
    (EnteringA a)      -> EnteringA (dotted a)
    (EnteringB a op b) -> EnteringB a op (dotted b)
    _                  -> s
  where
    dotted (a, _) = (a, True)


tryToCalc :: Float -> Operation -> Float -- A op B
          -> (String -> a)               -- error handler
          -> (Float -> a)                -- result handler
          -> a
tryToCalc _ Div b mkError _  | b == 0 = mkError "Dision by Zero!"
tryToCalc a op  b _ mkResult =
  let f = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> (/)
  in mkResult $ f a b


applyOp :: Operation -> State -> State
applyOp op s =
  case s of
    (EnteringA a) -> EnteredAandOp (fromRaw a) op
    (EnteringB a op' b) -> tryToCalc a op' (fromRaw b)
                                     (Error a)
                                     (\a' -> EnteredAandOp a' op)
    (EnteredAandOp a _) -> Error a "Can't do this!"
    (Calculated a _ _)  -> EnteredAandOp a op
    _ -> s


applyCmd :: Command -> State -> State
applyCmd cmd s =
  case (cmd, s) of
    (ClearError, Error a _)         -> EnteringA (asRaw a)
    (Clear,      _)                 -> def
    (_,          Error _ _)         -> s
    (Flush,      EnteringA _)       -> s
    (Flush,      EnteredAandOp a _) -> Error a "Can't do this!"
    (Flush,      EnteringB  a op b) -> calc a op (fromRaw b)
    (Flush,      Calculated a op b) -> calc a op b
    _ -> s
  where
    calc a op b = tryToCalc a op b
                  (Error a)
                  (\a' -> Calculated a' op b)
