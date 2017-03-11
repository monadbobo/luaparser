{-# LANGUAGE NoImplicitPrelude #-}

--Lua EBNF http://wiki.luajit.org/Extended-BNF
--(no left-recursion BNF) http://wiki.luajit.org/Extended-BNF

module Lib
    ( run
    ) where

import Protolude
import Data.String
import Data.List
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Text as PText
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec.Expr as PExpr

import qualified Debug.Trace as DTrace

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Options.Applicative as OA
import Data.Semigroup ((<>))

data OptField = OptField {file :: String}

data Chunk = Chunk [Stat] (Maybe LastStat) deriving(Show)

data LastStat = RetStat [Exp] | Break | Goto T.Text deriving(Show)

data UnOp = NegateUnOp | NotUnOp | HashUnOp  deriving (Show)

data BinOp = AddBinOp | SubBinOp | MultBinOp | DivBinOp | ExpBinOp | ModBinOp |
             ConcatBinOp | LTBinOp | LTEBinOp | GTBinOp | GTEBinOp |
             EqBinOp | NEqBinOp | AndBinOp | OrBinOp
             deriving (Show)

data Exp = Nil
  | Boolean Bool
  | LuaInteger Integer
  | LuaString Text
  | Vararg
  | PEP PrefixExp
  | AFC AnonFunction
  | PET TableConstructor
  | BinOpExp BinOp Exp Exp
  | UnOpExp UnOp Exp
  deriving(Show)

data Index = TableIndex Exp | NameIndex T.Text deriving(Show)

data Suffix = SFC Call | SFI Index deriving(Show)

data TableConstructor = TableConstructor [Field]  deriving(Show)

data Field = FieldAssignExp Exp Exp | FieldAssignName T.Text Exp | FieldExp Exp deriving(Show)

data Args = ArgsExp [Exp]
  | ArgsText T.Text
  | ArgsTable TableConstructor deriving(Show)

data Call = CallArgs Args | CallNameArgs T.Text Args deriving(Show)

data FunctionCall = FunctionCall PrefixExp [Suffix] Call deriving(Show)

data PrefixExp = PEN T.Text | PEE Exp deriving(Show)

data Var = Name T.Text | VComplex PrefixExp [Suffix] Index deriving(Show)

data Label = Label T.Text deriving(Show)

data DoBlock = DoBlock Chunk deriving(Show)

data WhileLoop = WhileLoop Exp DoBlock deriving(Show)

data RepeatLoop = RepeatLoop Chunk Exp deriving(Show)

data If = If Exp Chunk [(Exp, Chunk)] (Maybe Chunk) deriving(Show)

data FunctionBody = FunctionBody [T.Text] Chunk deriving(Show)

data AnonFunction = AnonFunction FunctionBody deriving(Show)

data For = ForRange T.Text [Exp] DoBlock | ForEach [T.Text] [Exp] DoBlock deriving(Show)

data Stat = AssignExpr [Var] [Exp] | FCCallStat FunctionCall
  | LBStat Label |DBStat DoBlock | WLStat WhileLoop
  | RPStat RepeatLoop | IFStat If | FCStat Bool T.Text FunctionBody
  | ForStat For | AssignName Bool [T.Text] (Maybe [Exp]) | EmptyStat deriving(Show)

lexer :: Tok.GenTokenParser T.Text() Identity
lexer = Tok.makeTokenParser ldf

ldf :: Tok.GenLanguageDef T.Text() Identity
ldf = Lang.emptyDef
  { Tok.commentStart = "--[["
  , Tok.commentEnd = "]]-"
  , Tok.commentLine = "--"
  , Tok.nestedComments = False
  , Tok.identStart = Parsec.letter
  , Tok.identLetter = Parsec.alphaNum
  , Tok.opStart = Tok.opLetter ldf <|> Parsec.oneOf "-#"
  , Tok.opLetter = Parsec.oneOf "+-*/^%<"
  , Tok.reservedNames = ["and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
    "//", "::", "<eof>", "<number>", "<integer>", "<name>", "<string>"]
  , Tok.reservedOpNames = ["..", "...", "==", ">=", "<=", "~=", "<<",
                           ">>", "+","-", "*", "/",
                           "^", "%", "#", "-"]
  , Tok.caseSensitive = True
  }

ident :: PText.Parser T.Text
ident =  T.pack <$> Tok.identifier lexer

reservedOp :: T.Text -> PText.Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)

reserverNames :: T.Text -> PText.Parser ()
reserverNames name = Tok.reserved lexer (T.unpack name)

semi :: PText.Parser T.Text
semi = T.pack <$> Tok.semi lexer

comma :: PText.Parser T.Text
comma = T.pack <$> Tok.comma lexer

dot :: PText.Parser T.Text
dot = T.pack <$> Tok.dot lexer

parens :: PText.Parser a -> PText.Parser a
parens = Tok.parens lexer

brackets :: PText.Parser a -> PText.Parser a
brackets = Tok.brackets lexer

colon ::  PText.Parser T.Text
colon = T.pack <$> Tok.colon lexer

braces :: PText.Parser a -> PText.Parser a
braces = Tok.braces lexer

whiteSpace :: PText.Parser ()
whiteSpace = Tok.whiteSpace lexer

suffixCall :: PText.Parser Suffix
suffixCall = do
  sc <- call
  return $ SFC sc

suffixIndex :: PText.Parser Suffix
suffixIndex = do
  inx <- index
  return $ SFI inx

suffix :: PText.Parser Suffix
suffix = Parsec.try suffixCall <|> Parsec.try suffixIndex

indexTable :: PText.Parser Index
indexTable = do
  exp <- brackets expr
  return $ TableIndex exp

indexDot :: PText.Parser Index
indexDot = do
  dot
  ide <- ident
  return $ NameIndex ide

index :: PText.Parser Index
index = Parsec.try indexTable <|> indexDot

varName :: PText.Parser Var
varName = do
  vr <- ident
  return $ Name vr

varComplex :: PText.Parser Var
varComplex = do
  pl <- prefixExp
  suf <- Parsec.many1 suffix
  let lastSuf = if null suf then Nothing else Just (last suf)
  case lastSuf of
    Just (SFI i) -> return $ VComplex pl (init suf) i
    _ -> do
      ix <- index
      return $ VComplex pl suf ix

var :: PText.Parser Var
var = Parsec.try varComplex <|> varName

numericExp :: PText.Parser Exp
numericExp = do
  el <- Tok.integer lexer
  return $ LuaInteger el

stringExp :: PText.Parser Exp
stringExp = do
  el <- Tok.stringLiteral lexer
  return $ LuaString $ T.pack el

nilExp :: PText.Parser Exp
nilExp = do
  reserverNames $ T.pack "nil"
  return Nil

trueExp :: PText.Parser Exp
trueExp = do
  reserverNames $ T.pack "true"
  return $ Boolean True

falseExp :: PText.Parser Exp
falseExp = do
  reserverNames $ T.pack "false"
  return $ Boolean False

varargExp :: PText.Parser Exp
varargExp = do
  Parsec.string "..."
  return $ Vararg

reservedExp :: PText.Parser Exp
reservedExp = Parsec.try nilExp <|> Parsec.try trueExp
  <|> Parsec.try falseExp <|> varargExp

unop :: PText.Parser UnOp
unop = Parsec.try negateUnOp <|> Parsec.try notUnOp <|> hashUnOp where
  negateUnOp = do
    reservedOp $ T.pack "-"
    return NegateUnOp
  notUnOp = do
    reserverNames $ T.pack "not"
    return NotUnOp
  hashUnOp = do
    reservedOp $ T.pack "#"
    return HashUnOp

binOpExpr :: PText.Parser Exp
binOpExpr = Parsec.chainl1 commonExpr (Parsec.try addOp <|> Parsec.try subOp
                                       <|> Parsec.try mulOp <|> Parsec.try divOp
                                      <|> Parsec.try expOp <|> Parsec.try modOp <|>  Parsec.try concatOp
                                      <|> Parsec.try ltOp <|> Parsec.try lteOp
                                      <|> Parsec.try gtOp <|> Parsec.try gteOp
                                      <|> Parsec.try eqOp <|> Parsec.try neqOp <|> Parsec.try andOp
                                      <|>  orOp ) where
  addOp = do
    reservedOp $ T.pack "+"
    return $ BinOpExp AddBinOp
  subOp = do
    reservedOp $ T.pack "-"
    return $ BinOpExp SubBinOp
  mulOp = do
    reservedOp $ T.pack "*"
    return $ BinOpExp MultBinOp
  divOp = do
    reservedOp $ T.pack "/"
    return $ BinOpExp DivBinOp
  expOp = do
    reservedOp $ T.pack "^"
    return $ BinOpExp ExpBinOp
  modOp = do
    reservedOp $ T.pack "%"
    return $ BinOpExp ModBinOp
  concatOp = do
    reservedOp $ T.pack ".."
    return $ BinOpExp ConcatBinOp
  ltOp = do
    reservedOp $ T.pack "<"
    return $ BinOpExp LTBinOp
  lteOp = do
    reservedOp $ T.pack "<="
    return $ BinOpExp LTEBinOp
  gtOp = do
    reservedOp $ T.pack ">"
    return $ BinOpExp GTBinOp
  gteOp = do
    reservedOp $ T.pack ">="
    return $ BinOpExp GTEBinOp
  eqOp = do
    reservedOp $ T.pack "=="
    return $ BinOpExp EqBinOp
  neqOp = do
    reservedOp $ T.pack "~="
    return $ BinOpExp NEqBinOp
  andOp = do
    reserverNames $ T.pack "and"
    return $ BinOpExp AndBinOp
  orOp = do
    reserverNames $ T.pack "or"
    return $ BinOpExp GTEBinOp



commonExpr :: PText.Parser Exp
commonExpr = Parsec.try numericExp <|> Parsec.try stringExp <|> Parsec.try reservedExp
  <|> Parsec.try functionExp <|> Parsec.try tableConsExp <|> Parsec.try unopExp
  <|> pExp where
  functionExp = do
    fc <- function
    return $ AFC fc
  tableConsExp = do
    tc <- tableCons
    return $ PET tc
  unopExp = do
    up <- unop
    ex <- expr
    return $ UnOpExp up ex
  pExp = do
    p <- prefixExp
    return $ PEP p

expr :: PText.Parser Exp
expr = Parsec.try binOpExpr <|> commonExpr

explist :: PText.Parser [Exp]
explist = Parsec.sepBy1 expr comma

args :: PText.Parser Args
args = do
  Parsec.optional Parsec.space
  el <- parens explist
  return $ ArgsExp el

functionCall :: PText.Parser FunctionCall
functionCall = do
  pfix <- prefixExp
  sfix <- Parsec.many1 suffix
  let lastSfix = if null sfix then Nothing else Just(last sfix)
  case lastSfix of
    Just (SFC i) -> return $ FunctionCall pfix (init sfix) i
    _ -> do
      ca <- call
      return $ FunctionCall pfix sfix ca

call :: PText.Parser Call
call = Parsec.try fcArgs <|> fcName where
  fcArgs = do
    ag <- args
    return $ CallArgs ag
  fcName = do
    colon
    ne <- ident
    ag <- args
    return $ CallNameArgs ne ag

varPrefixExp :: PText.Parser PrefixExp
varPrefixExp = do
  vl <- ident
  return $ PEN vl

parentsPExp :: PText.Parser PrefixExp
parentsPExp = do
  vl <- parens expr
  return $ PEE vl

prefixExp :: PText.Parser PrefixExp
prefixExp = Parsec.try parentsPExp <|> varPrefixExp

varlist :: PText.Parser [Var]
varlist = Parsec.sepBy1 var comma

assignExpr :: PText.Parser Stat
assignExpr = do
  vl <- varlist
  Parsec.char '='
  el <- explist
  return $ AssignExpr vl el

returnstat :: PText.Parser LastStat
returnstat = do
  reserverNames $ T.pack("return")
  exps <- explist
  return $ RetStat exps

breakstat :: PText.Parser LastStat
breakstat = do
  reserverNames $ T.pack("break")
  return $ Break

laststat :: PText.Parser LastStat
laststat = Parsec.try returnstat <|> Parsec.try breakstat

label :: PText.Parser Label
label = do
  Parsec.string "::"
  name <- ident
  Parsec.string "::"
  return $ Label name

doblock :: PText.Parser DoBlock
doblock = do
  Parsec.optional $ Parsec.space
  reserverNames $ T.pack("do")
  bl <- block
  reserverNames $ T.pack("end")
  return $ DoBlock bl

whileloop :: PText.Parser WhileLoop
whileloop = do
  reserverNames $ T.pack("while")
  ep <- expr
  dl <- doblock
  return $ WhileLoop ep dl

repeatLoop :: PText.Parser RepeatLoop
repeatLoop = do
  reserverNames $ T.pack("repeat")
  bl <- block
  reserverNames $ T.pack("until")
  ex <- expr
  return $ RepeatLoop bl ex

block :: PText.Parser Chunk
block = do
  stats <- Parsec.endBy stat (Parsec.optional semi)
  retStats <- Parsec.optionMaybe laststat
  return $ Chunk stats retStats

functionBody :: PText.Parser FunctionBody
functionBody = do
  parlist <- parens $ Parsec.sepBy1 ident comma
  Parsec.optional $ Parsec.space
  bl <- block
  Parsec.optional $ Parsec.space
  reserverNames $ T.pack("end")
  return $ FunctionBody parlist bl

function :: PText.Parser AnonFunction
function = do
  reserverNames $ T.pack("function")
  Parsec.optional $ Parsec.space
  body <- functionBody
  return $ AnonFunction body

field :: PText.Parser Field
field = Parsec.try fieldAssignExp <|> Parsec.try fieldAssignName <|> fieldExp where
  fieldAssignExp = do
    ex <- brackets expr
    Parsec.optional $ Parsec.space
    Parsec.char '='
    Parsec.optional $ Parsec.space
    e <- expr
    return $ FieldAssignExp ex e
  fieldAssignName = do
    name <- ident
    Parsec.optional $ Parsec.space
    Parsec.char '='
    Parsec.optional $ Parsec.space
    e <- expr
    return $ FieldAssignName name e
  fieldExp = do
    Parsec.optional $ Parsec.space
    e <- expr
    return $ FieldExp e

fieldList :: PText.Parser [Field]
fieldList = do
  f <- Parsec.sepBy1 field (Parsec.oneOf ",;")
  return $ f

tableCons :: PText.Parser TableConstructor
tableCons = do
  ts <- braces fieldList
  return $ TableConstructor ts

ifstat :: PText.Parser If
ifstat = do
  reserverNames $ T.pack("if")
  ex <- expr
  Parsec.optional Parsec.space
  reserverNames $ T.pack("then")
  block1 <- block
  el <- Parsec.many elifstat
  els <- Parsec.optionMaybe elsestat
  reserverNames $ T.pack("end")
  return $ If ex block1 el els where
    elifstat = do
      reserverNames $ T.pack("elseif")
      ex <- expr
      reserverNames $ T.pack("then")
      block3 <- block
      return $ (ex, block3)
    elsestat = do
      reserverNames $ T.pack("else")
      block2 <- block
      return $ block2

emptyStat :: PText.Parser Stat
emptyStat = do
  Parsec.string "\n"
  return EmptyStat

forRangeStat :: PText.Parser For
forRangeStat = do
  reserverNames $ T.pack("for")
  name <- ident
  Parsec.optional $ Parsec.space
  Parsec.char '='
  Parsec.optional $ Parsec.space
  ex <- Parsec.sepBy1 expr comma
  bl <- doblock
  return $ ForRange name ex bl

forEachStat :: PText.Parser For
forEachStat = do
  reserverNames $ T.pack("for")
  name <- Parsec.sepBy1 ident comma
  Parsec.optional $ Parsec.space
  reserverNames $ T.pack("in")
  exl <- explist
  Parsec.optional $ Parsec.space
  bl <- doblock
  return $ ForEach name exl bl

eqExpList = do
  Parsec.optional Parsec.space
  Parsec.string "="
  Parsec.optional Parsec.space
  ex <- explist
  return ex

stat :: PText.Parser Stat
stat = Parsec.try emptyStat <|> Parsec.try assignExpr <|> Parsec.try fcStat
  <|> Parsec.try labelStat <|> Parsec.try doblockStat <|> Parsec.try whileStat
  <|> Parsec.try rpStat <|> Parsec.try ifStat
  <|> Parsec.try functionStat <|> Parsec.try forStat <|> nameStat where
  fcStat = do
    fc <- functionCall
    return $ FCCallStat fc
  labelStat = do
    la <- label
    return $ LBStat la
  doblockStat = do
    bl <- doblock
    return $ DBStat bl
  whileStat = do
    wl <- whileloop
    return $ WLStat wl
  rpStat = do
    rs <- repeatLoop
    return $ RPStat rs
  ifStat = do
    is <- ifstat
    return $ IFStat is
  functionStat = do
    l <- Parsec.optionMaybe $ reserverNames $ T.pack("local")
    reserverNames $ T.pack("function")
    Parsec.optional $ Parsec.space
    name <- ident
    body <- functionBody
    case l of
      Nothing -> return $ FCStat False name body
      _ -> return $ FCStat True name body
  forStat = do
    fs <- Parsec.try forEachStat <|> forRangeStat
    return $ ForStat fs
  nameStat = do
    l <- Parsec.optionMaybe $ reserverNames $ T.pack("local")
    name <- Parsec.sepBy1 ident comma
    ex <- Parsec.optionMaybe $ eqExpList
    case l of
      Nothing -> return $ AssignName False name ex
      _ -> return $ AssignName True name ex

chunk :: PText.Parser Chunk
chunk = do
  stats <- Parsec.endBy stat (Parsec.optional semi)
  retStats <- Parsec.optionMaybe laststat
  Parsec.eof
  return $ Chunk stats retStats

llex :: T.Text -> Either Parsec.ParseError Chunk
llex = Parsec.parse (whiteSpace >> chunk) "<stdin>"

doFile :: OptField -> IO ()
doFile (OptField path) = do
  contents <- TIO.readFile path
  case llex contents of
    Left a -> do
      print a
    Right b -> do
      print b
  return ()

options :: OA.Parser OptField
options = OptField
  <$> OA.strOption
  ( OA.long "file"
  <> OA.short 'f'
  <> OA.metavar "FILE"
  <> OA.help "loaded load script")

run :: IO ()
run = do
  doFile =<< OA.execParser opts
  where
    opts = OA.info (options <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc ""
     <> OA.header "" )
