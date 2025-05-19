module NagyBead where

import Data.Either
import Data.Maybe
import Data.List
import Text.Read

basicInstances = 0 -- Mágikus tesztelőnek kell ez, NE TÖRÖLD!

data Dir = InfixL | InfixR deriving (Show, Eq, Ord)

data Tok a = BrckOpen | BrckClose | TokLit a | TokBinOp (a -> a -> a) Char Int Dir | TokFun (a -> a) String

instance (Show a) => Show (Tok a) where
  show BrckOpen = "BrckOpen"
  show BrckClose = "BrckClose"
  show (TokLit a) = "TokLit " ++ show a
  show (TokBinOp _ a aa aaa) = "TokBinOp"++ " " ++ show a ++ " " ++ show aa ++" "++ show aaa
  show (TokFun _ param) = "TokFun"++ " " ++ param

instance (Eq a) => Eq (Tok a) where
  BrckOpen == BrckOpen = True
  BrckClose == BrckClose = True
  TokLit x == TokLit y = x == y
  TokBinOp _ a aa aaa == TokBinOp _ b bb bbb = a == b && aa == bb && aaa == bbb
  TokFun _ fv1 == TokFun _ fv2 = fv1 == fv2
  _ == _ = False
  
type FunctionTable a = [(String, a -> a )]
type OperatorTable a = [(Char, (a -> a -> a, Int, Dir))]

tAdd, tMinus, tMul, tDiv, tPow :: (Floating a) => Tok a
tAdd = TokBinOp (+) '+' 6 InfixL
tMinus = TokBinOp (-) '-' 6 InfixL
tMul = TokBinOp (*) '*' 7 InfixL
tDiv = TokBinOp (/) '/' 7 InfixL
tPow = TokBinOp (**) '^' 8 InfixR

operatorTable :: (Floating a) => OperatorTable a
operatorTable =
    [ ('+', ((+), 6, InfixL))
    , ('-', ((-), 6, InfixL))
    , ('*', ((*), 7, InfixL))
    , ('/', ((/), 7, InfixL))
    , ('^', ((**), 8, InfixR))
    ]


data ShuntingYardError
    = OperatorOrClosingParenExpected
    | LiteralOrOpeningParenExpected
    | NoClosingParen 
    | NoOpeningParen
    | ParseError
    deriving (Eq, Show)

type ShuntingYardResult a = Either ShuntingYardError a

data Assoc = LeftAssoc | RightAssoc
  deriving (Show, Eq)

operatorFromChar :: OperatorTable a -> Char -> Maybe (Tok a)
operatorFromChar [] _ = Nothing
operatorFromChar ((x,(y,z,u)):xs) a
  | x==a = Just (TokBinOp y x z u)
  | otherwise = operatorFromChar xs a

getOp :: (Floating a) => Char -> Maybe (Tok a)
getOp = operatorFromChar operatorTable

parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable

toke :: (Read a) => OperatorTable a -> String -> Maybe [Tok a]
toke opTable string
  | Just (TokLit val) <- szamtoken string = Just [TokLit val] --Literálba próbáljuk parseolni
  | length string == 1 = case head string of -- ha 1 hosszú akkor megnézzük, hogy zárójel-e, vagy operátor, ha nem illeszkedik Nothingot adunk
      '(' -> Just [BrckOpen] 
      ')' -> Just [BrckClose]
      op  -> case operatorFromChar opTable op of
              Just tokOp -> Just [tokOp]
              Nothing -> Nothing
  | otherwise = tokzarojel string -- ha nem 1 hosszú megpróbáljuk zárójelekből álló stringre illeszteni

tokzarojel :: String -> Maybe [Tok a] -- zárójelre illesztés
tokzarojel str
  | all isParenthesis str = Just (map toBracketToken str) -- magasabb szintű fv.
  | otherwise = Nothing
  where
    isParenthesis :: Char -> Bool
    isParenthesis c = c == '(' || c == ')'

    toBracketToken :: Char -> Tok a --zárójelből token
    toBracketToken '(' = BrckOpen
    toBracketToken ')' = BrckClose

szamtoken :: (Read a) => String -> Maybe (Tok a)
szamtoken str
  | null str = Nothing
  | otherwise = case reads str of -- reads - adott típusú beolvasás, csak a "sikeres" részét veszzük a beolvasásnak
      [(val, "")]
        | str /= "()" -> Just (TokLit val) --simán TokLit "()" -nél bekerül TokLit () -be és így fog majd a "()" belemenni a tokzarojel stringbe
      _ -> Nothing

areFirstTwoCharactersSame :: OperatorTable a -> String -> Bool -- a str ÉS az opTable végtelenségének ellenőrzése
areFirstTwoCharactersSame opTable str = 
    case take 2 str of
        [op1, op2] -> 
            -- Két karakter összehasonlítása, hogy mindkettő szerepel az operátor táblában
            case (operatorFromChar opTable op1, operatorFromChar opTable op2) of
                (Just _, Just _) -> 
                    if op1 == op2 then 
                        True  -- Ha mindkettő operátor ÉS egyenlőek, akkor ez True-t fog visszaadni
                    else 
                        -- Ha nem egyenlőek, akkor új operátortábla készítése a második kereséshez
                        let newOpTable = removeDuplicateOperator opTable op2
                        in case operatorFromChar newOpTable op2 of
                            Just _  -> False  -- Ha második op2 is megtalálható a módosított táblában, akkor False
                            Nothing -> True  -- Ha a második op2 már nincs a táblában, akkor True
                _ -> False  -- Ha egyik sem operátor akkor hamis
        _ -> False  -- Ha rövidebb esetleg a string mint 2 karakter

-- Segédfüggvény, amely eltávolítja a duplikált operátort az operátor táblából.
removeDuplicateOperator :: OperatorTable a -> Char -> OperatorTable a
removeDuplicateOperator opTable op = filter (\(c, _) -> c /= op) opTable


parseTokens :: (Read a) => OperatorTable a -> String -> Maybe [Tok a]
parseTokens opTable str = parseWords opTable (words str)
  -- | isOverSoManyChars str = Nothing  -- végtelen beadás szűrése
  -- | otherwise = parseWords opTable (words str)
  where
    parseWords :: (Read a) => OperatorTable a -> [String] -> Maybe [Tok a] -- tényleges feldolgozás
    parseWords _ [] = Just []
    parseWords opTable (w:ws) = do
      currentTokens <- toke opTable w
      remainingTokens <- parseWords opTable ws
      return (currentTokens ++ remainingTokens)


shuntingYardBasic :: [Tok a] -> ([a], [Tok a])
shuntingYardBasic = alfv [] []
  where
    alfv litlista oplista [] = (litlista, oplista)
    alfv litlista oplista (x:xs) = case x of
      TokLit lit -> alfv (lit:litlista) oplista xs  -- A számokat először adjuk hozzá a listához
      TokBinOp op _ _ _ -> alfv litlista (x:oplista) xs  -- Az operátorokat fordított sorrendben adjuk hozzá
      BrckOpen -> alfv litlista (BrckOpen:oplista) xs
      BrckClose -> let (ujLitList, ujOpList) = zarojelKezelo litlista oplista
                   in alfv ujLitList ujOpList xs

    zarojelKezelo litlista (BrckOpen:os) = (litlista, os)
    zarojelKezelo litlista (op:os) = zarojelKezelo (muvelet litlista op) os

    muvelet (x:y:ys) (TokBinOp op _ _ _) = (op y x) : ys  -- Műveletek kiértékelése az operátorokkal

parseAndEval :: (String -> Maybe [Tok a]) -> ([Tok a] -> ([a], [Tok a])) -> String -> Maybe ([a], [Tok a])
parseAndEval parse eval input = maybe Nothing (Just . eval) (parse input)

syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))


syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
syEvalPrecedence = parseAndEval parse (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])
shuntingYardPrecedence = alfv [] []
  where
    alfv :: [a] -> [Tok a] -> [Tok a] -> ([a], [Tok a]) --rekurzívan feldolgozzuk a literálokat meg az operátorokat
    -- Ha a lista üres, visszatérünk a felhalmozott operandusokkal és operátorokkal.
    alfv litlista oplista [] = (litlista, oplista)

    alfv litlista oplista (TokLit val : xs) = --ha literál -> litlista
      alfv (val : litlista) oplista xs

    alfv litlista oplista (TokBinOp op c prec assoc : xs) = --ha operátor -> oplista
      let (newLitList, newOpList) = handleOperator litlista oplista (TokBinOp op c prec assoc)
      in alfv newLitList newOpList xs

    alfv litlista oplista (BrckOpen : xs) = --ha nyitózárójel -> egyenlőre oplista mert nem garantált a záró zárójel
      alfv litlista (BrckOpen : oplista) xs

    -- Ha záró zárójel található, akkor a nyitó zárójellel együtt feldolgozzuk a zárójelet.
    alfv litlista oplista (BrckClose : xs) = --ha záró zárójelt találtunk akkor csinálunk egy segédlistát
      let (newLitList, newOpList) = handleClose litlista oplista
      in alfv newLitList newOpList xs

    -- operátorkezelő függvény, nézi a kötés erősségét és irányát
    handleOperator :: [a] -> [Tok a] -> Tok a -> ([a], [Tok a])
    handleOperator litlista oplista op@(TokBinOp _ _ prec assoc) =
      case oplista of
        -- Ha az oplista üres akkor csak hozzáadjuk az operátort
        [] -> (litlista, [op])
        
        -- Ha az operátor lista nem üres, akkor kiértékeljük.
        (TokBinOp _ _ prec' assoc' : os)
          | (prec' > prec) || (prec' == prec && assoc == InfixL) -> 
              -- Ha az operátorok kötési értékei a feladatban leírtakkal megegyeznek, kiértékeljük az operátort
              let (newLitList, _) = applyOp litlista (head oplista)
              in handleOperator newLitList (tail oplista) op
          | otherwise -> (litlista, op : oplista)  -- Ha nem, akkor csak az oplistához hozzáadjukúj operátort
        _ -> (litlista, op : oplista)

    handleClose :: [a] -> [Tok a] -> ([a], [Tok a])
    handleClose litlista (BrckOpen : os) = (litlista, os)  -- Nyitó zárójel eltávolítása
    handleClose litlista (op : os) =
      let (newLitList, _) = applyOp litlista op  -- Operátor alkalmazása
      in handleClose newLitList os
    handleClose litlista [] = (litlista, [])  -- Üres lista esetén nincs további feldolgozás

 
    applyOp :: [a] -> Tok a -> ([a], [Tok a]) --operátor alkamazó függvény
    applyOp (x : y : ys) (TokBinOp op _ _ _) = (op y x : ys, []) 
    applyOp ys _ = (ys, [])  -- ha nincsenek számok csak visszaadjuk az operátort





-- eqError-t vedd ki a kommentből, ha megcsináltad az 1 pontos "Hibatípus definiálása" feladatot
eqError = 0 -- Mágikus tesztelőnek szüksége van rá, NE TÖRÖLD!


-- Ezt akkor vedd ki a kommentblokkból, ha a 3 pontos "A parser és az algoritmus újradefiniálása" feladatot megcsináltad.
{-
parseAndEvalSafe ::
    (String -> ShuntingYardResult [Tok a]) ->
    ([Tok a] -> ShuntingYardResult ([a], [Tok a])) ->
    String -> ShuntingYardResult ([a], [Tok a])
parseAndEvalSafe parse eval input = either Left eval (parse input)

sySafe :: String -> ShuntingYardResult ([Double], [Tok Double])
sySafe = parseAndEvalSafe
  (parseSafe operatorTable)
  (\ts -> shuntingYardSafe (BrckOpen : ts ++ [BrckClose]))


bindE :: Either e a -> (a -> Either e b) -> Either e b
bindE (Left err) _ = Left err
bindE (Right x) f  = f x
-}
--4 óra munka, esetleges maradék szabadidőben ismét próba
{-
parseSafe :: Read a => OperatorTable a -> String -> ShuntingYardResult [Tok a]
parseSafe opTable str = case parseTokens opTable str of
    Just tokens -> Right tokens
    Nothing     -> Left ParseError

shuntingYardSafe :: [Tok a] -> ShuntingYardResult ([a], [Tok a])
shuntingYardSafe = alfv [] []
  where
    alfv litlista oplista [] = 
        case oplista of
            [] -> 
                if hasUnmatchedOpen oplista
                then Left NoClosingParen
                else Right (litlista, [])
            _  -> case checkBalance oplista of
                    Left err -> Left err
                    Right () -> Right (litlista, [])

    alfv litlista oplista (TokLit val : xs) = alfv (val : litlista) oplista xs

    alfv litlista oplista (TokBinOp op _ prec assoc : xs) = 
        -- Ha az első vagy az utolsó elem operátor, akkor hibát dobunk
        if (null litlista || null xs)
            then Left OperatorOrClosingParenExpected
            else
                case oplista of
                    [] -> alfv litlista [TokBinOp op undefined prec assoc] xs
                    (TokBinOp _ _ prec' assoc' : os)
                        | (prec' > prec) || (prec' == prec && assoc == InfixL) -> 
                            let (newLitList, _) = applyOp litlista (head oplista)
                            in alfv newLitList (tail oplista) (TokBinOp op undefined prec assoc : xs)
                        | otherwise -> alfv litlista (TokBinOp op undefined prec assoc : oplista) xs
                    _ -> alfv litlista (TokBinOp op undefined prec assoc : oplista) xs

    alfv litlista oplista (BrckOpen : xs) = 
        alfv litlista (BrckOpen : oplista) xs

    alfv litlista oplista (BrckClose : xs) = 
        case handleClose litlista oplista of
            Left err -> Left err
            Right (newLitList, newOpList) -> alfv newLitList newOpList xs

    -- Ellenőrizzük, hogy az operátor a kifejezés első vagy utolsó eleme
    isOperator :: Tok a -> Bool
    isOperator (TokBinOp _ _ _ _) = True
    isOperator _ = False

    -- Külön ellenőrizzük, hogy operátor előtt van-e operandus
    checkBalance :: [Tok a] -> Either ShuntingYardError ()
    checkBalance tokens =
        let (openCount, closeCount) = foldl countBrackets (0, 0) tokens
        in if openCount == closeCount then Right () else Left NoClosingParen

    countBrackets :: (Int, Int) -> Tok a -> (Int, Int)
    countBrackets (open, close) BrckOpen = (open + 1, close)
    countBrackets (open, close) BrckClose = (open, close + 1)
    countBrackets counts _ = counts

    hasUnmatchedOpen :: [Tok a] -> Bool
    hasUnmatchedOpen tokens =
        let (openCount, closeCount) = foldl countBrackets (0, 0) tokens
        in openCount > closeCount

    handleClose :: [a] -> [Tok a] -> ShuntingYardResult ([a], [Tok a])
    handleClose litlista (BrckOpen : os) = Right (litlista, os)
    handleClose litlista (op : os) =
        case applyOp litlista op of
            (newLitList, _) -> handleClose newLitList os
    handleClose _ [] = Left NoOpeningParen

    applyOp :: [a] -> Tok a -> ([a], [Tok a])
    applyOp (x : y : ys) (TokBinOp op _ _ _) = (op y x : ys, [])
    applyOp ys _ = (ys, [])
-}











-- Ezt akkor vedd ki a kommentblokkból, ha az 1 pontos "Függvénytábla és a típus kiegészítése" feladatot megcsináltad.
tSin, tCos, tLog, tExp, tSqrt :: Floating a => Tok a
tSin = TokFun sin "sin"
tCos = TokFun cos "cos"
tLog = TokFun log "log"
tExp = TokFun exp "exp"
tSqrt = TokFun sqrt "sqrt"

functionTable :: (RealFrac a, Floating a) => FunctionTable a
functionTable =
    [ ("sin", sin)
    , ("cos", cos)
    , ("log", log)
    , ("exp", exp)
    , ("sqrt", sqrt)
    , ("round", (\x -> fromIntegral (round x :: Integer)))
    ]



-- Ezt akkor vedd ki a kommentblokkból, ha a 2 pontos "Függvények parse-olása és kiértékelése" feladatot megcsináltad.
{-
syFun :: String -> Maybe ([Double], [Tok Double])
syFun = parseAndEval
  (parseWithFunctions operatorTable functionTable)
  (\t -> shuntingYardWithFunctions $ BrckOpen : (t ++ [BrckClose]))
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha minden más feladatot megcsináltál ez előtt.
syComplete :: String -> ShuntingYardResult ([Double], [Tok Double])
syComplete = parseAndEvalSafe
  (parseComplete operatorTable functionTable)
  (\ts -> shuntingYardComplete (BrckOpen : ts ++ [BrckClose]))
-}
