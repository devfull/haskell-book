-- ------------------------------------------------------------------------- --
-- Chapter  24    — Parser Combinators
-- ------------------------------------------------------------------------- --

import           Control.Exception          ( ArithException(..), evaluate )
import           Control.Applicative        ( Alternative(..), (<|>), liftA2 )

import           Data.Char                  ( digitToInt )
import           Data.List                  ( foldl' )
import           Data.Ratio                 ( (%) )
import           Data.Maybe                 ( fromJust, fromMaybe )

import           Text.Trifecta

import           Test.Hspec
import           Test.Hspec.QuickCheck      ( prop )
import           Test.QuickCheck     hiding ( Success, Failure )

-- ------------------------------------------------------------------------- --
-- Chapter  24.03 — Understanding the parsing process
-- Exercise 1     — Parsing Practice

-- There's a combinator that'll let us mark that we expect an input stream
-- to be finished at a particular point in our parser. In the parsers
-- library this is simply called eof (end-of-file) and is in the
-- Text.Parser.Combinators module. See if you can make the one and oneTwo
-- parsers fail because they didn't exhaust the input stream!

maybeParse :: Parser a -> String -> Maybe a
maybeParse p xs = case parseString p mempty xs of
    Failure _ -> Nothing
    Success a -> Just a

one :: CharParsing m => m Char
one = char '1'

two :: CharParsing m => m Char
two = char '2'

oneTwo :: CharParsing m => m Char
oneTwo = one *> two

oneEof :: CharParsing m => m ()
oneEof = one *> eof

oneEof':: CharParsing m => m Char
oneEof'= one <* eof

oneTwoEof:: CharParsing m => m ()
oneTwoEof= one *> two *> eof

testEof :: IO ()
testEof = hspec $ do
    describe "Experimenting with eof" $ do
        prop "Testing one" $
                 (maybeParse one "1"         `shouldBe` Just '1')
            .&&. (maybeParse one "12"        `shouldBe` Just '1')
            .&&. (maybeParse one "2"         `shouldBe` Nothing )
        prop "Testing two" $
                 (maybeParse two "2"         `shouldBe` Just '2')
            .&&. (maybeParse two "21"        `shouldBe` Just '2')
            .&&. (maybeParse two "1"         `shouldBe` Nothing )
        prop "Testing oneTwo" $
                 (maybeParse oneTwo "12"     `shouldBe` Just '2')
            .&&. (maybeParse oneTwo "123"    `shouldBe` Just '2')
            .&&. (maybeParse oneTwo "1"      `shouldBe` Nothing )
            .&&. (maybeParse oneTwo "2"      `shouldBe` Nothing )
        prop "Testing oneEof" $
                 (maybeParse oneEof "1"      `shouldBe` Just () )
            .&&. (maybeParse oneEof "12"     `shouldBe` Nothing )
            .&&. (maybeParse oneEof "2"      `shouldBe` Nothing )
        prop "Testing oneEof'" $
                 (maybeParse oneEof' "1"     `shouldBe` Just '1')
            .&&. (maybeParse oneEof' "12"    `shouldBe` Nothing )
            .&&. (maybeParse oneEof' "2"     `shouldBe` Nothing )
        prop "Testing oneTwoEof" $
                 (maybeParse oneTwoEof "1"   `shouldBe` Nothing )
            .&&. (maybeParse oneTwoEof "12"  `shouldBe` Just () )
            .&&. (maybeParse oneTwoEof "123" `shouldBe` Nothing )

-- ------------------------------------------------------------------------- --
-- Chapter  24.03 — Understanding the parsing process
-- Exercise 2     — Parsing Practice
--
--  Use string to make a Parser that parses "1", "12", and "123" out of the
--  example input respectively. Try combining it with stop too. That is, a
--  single parser should be able to parse all three of those strings.

string1 :: CharParsing m => m String
string1 = string "1"

string12 :: CharParsing m => m String
string12 = string "12"

string123 :: CharParsing m => m String
string123 = string "123"

testString :: IO ()
testString = hspec $ do
    describe "Experimenting with string" $ do
        prop "Testing string1" $
                 (maybeParse string1 "1"     `shouldBe` Just "1"  )
            .&&. (maybeParse string1 "12"    `shouldBe` Just "1"  )
            .&&. (maybeParse string1 "2"     `shouldBe` Nothing   )
        prop "Testing string12" $
                 (maybeParse string12 "1"    `shouldBe` Nothing   )
            .&&. (maybeParse string12 "12"   `shouldBe` Just "12" )
            .&&. (maybeParse string12 "123"  `shouldBe` Just "12" )
        prop "Testing string123" $
                 (maybeParse string123 "1"   `shouldBe` Nothing   )
            .&&. (maybeParse string123 "12"  `shouldBe` Nothing   )
            .&&. (maybeParse string123 "123" `shouldBe` Just "123")

choiceString :: CharParsing m => m String
choiceString = choice [string "123", string "12", string "1"] <* eof

alternativeString :: CharParsing m => m String
alternativeString =
        string "123"
    <|> string "12"
    <|> string "1"

testChoice :: IO ()
testChoice = hspec $ do
    describe "Experimenting with choice" $ do
        prop "Testing choiceString" $
                 (maybeParse choiceString "0"         `shouldBe` Nothing   )
            .&&. (maybeParse choiceString "1"         `shouldBe` Just "1"  )
            .&&. (maybeParse choiceString "12"        `shouldBe` Just "12" )
            .&&. (maybeParse choiceString "123"       `shouldBe` Just "123")
            .&&. (maybeParse choiceString "1234"      `shouldBe` Nothing   )
        prop "Testing alternativeString" $
                 (maybeParse alternativeString "0"    `shouldBe` Nothing   )
            .&&. (maybeParse alternativeString "1"    `shouldBe` Just "1"  )
            .&&. (maybeParse alternativeString "12"   `shouldBe` Just "12" )
            .&&. (maybeParse alternativeString "123"  `shouldBe` Just "123")
            .&&. (maybeParse alternativeString "1234" `shouldBe` Just "123")

-- ------------------------------------------------------------------------- --
-- Chapter  24.03 — Understanding the parsing process
-- Exercise 3     — Parsing Practice
--
-- Try writing a Parser that does what string does, but using char.

oneTwoA :: CharParsing m => m String
oneTwoA = (\c c' -> c:c':"") <$> one <*> two

oneTwoM :: (CharParsing m, Monad m) => m String
oneTwoM = do
    c  <- one
    c' <- two
    return [c, c']

stringRecursive :: CharParsing m => String -> m String
stringRecursive []     = pure []
stringRecursive (x:xs) = char x *> stringRecursive xs *> pure (x:xs)

stringFoldr :: CharParsing m => String -> m String
stringFoldr = foldr (\x ps -> (:) <$> char x <*> ps) (pure [])

stringSequenceA :: CharParsing m => String -> m String
stringSequenceA xs = sequenceA $ fmap char xs

testChar :: IO ()
testChar = hspec $ do
    describe "Experimenting with char" $ do
        prop "Testing oneTwo (applicative implementation)" $
                 (maybeParse oneTwoA "12"  `shouldBe` Just "12")
            .&&. (maybeParse oneTwoA "123" `shouldBe` Just "12")
            .&&. (maybeParse oneTwoA "1"   `shouldBe` Nothing )
            .&&. (maybeParse oneTwoA "2"   `shouldBe` Nothing )
        prop "Testing oneTwo (monadic implementation)" $
                 (maybeParse oneTwoM "12"  `shouldBe` Just "12")
            .&&. (maybeParse oneTwoM "123" `shouldBe` Just "12")
            .&&. (maybeParse oneTwoM "1"   `shouldBe` Nothing )
            .&&. (maybeParse oneTwoM "2"   `shouldBe` Nothing )
        prop "Testing string (recursive implementation)" $
                 (maybeParse (stringRecursive "123") "1"    `shouldBe` Nothing   )
            .&&. (maybeParse (stringRecursive "123") "12"   `shouldBe` Nothing   )
            .&&. (maybeParse (stringRecursive "123") "123"  `shouldBe` Just "123")
            .&&. (maybeParse (stringRecursive "123") "1234" `shouldBe` Just "123")
        prop "Testing string (foldr implementation)" $
                 (maybeParse (stringFoldr     "123") "1"    `shouldBe` Nothing   )
            .&&. (maybeParse (stringFoldr     "123") "12"   `shouldBe` Nothing   )
            .&&. (maybeParse (stringFoldr     "123") "123"  `shouldBe` Just "123")
            .&&. (maybeParse (stringFoldr     "123") "1234" `shouldBe` Just "123")
        prop "Testing string (sequenceA implementation)" $
                 (maybeParse (stringSequenceA "123") "1"    `shouldBe` Nothing   )
            .&&. (maybeParse (stringSequenceA "123") "12"   `shouldBe` Nothing   )
            .&&. (maybeParse (stringSequenceA "123") "123"  `shouldBe` Just "123")
            .&&. (maybeParse (stringSequenceA "123") "1234" `shouldBe` Just "123")

-- ------------------------------------------------------------------------- --
-- Chapter  24.04 — Parsing Fractions
-- Exercise       — Unit of Success

integerEofA :: TokenParsing m => m Integer
integerEofA = integer <* eof

integerEofM :: (TokenParsing m, Monad m) => m Integer
integerEofM = integer >>= \x -> eof >>= \_ -> return x

integerEofM' :: (TokenParsing m, Monad m) => m Integer
integerEofM' = do
    n <- integer
    eof
    return n

testInteger :: IO ()
testInteger = hspec $ do
    describe "Experimenting with integer" $ do
        prop "Testing integerEof (applicative implementation)" $
                 (maybeParse integerEofA ""       `shouldBe` Nothing )
            .&&. (maybeParse integerEofA "123"    `shouldBe` Just 123)
            .&&. (maybeParse integerEofA "123ac"  `shouldBe` Nothing )
        prop "Testing integerEof (monadic implementation)" $
                 (maybeParse integerEofM ""       `shouldBe` Nothing )
            .&&. (maybeParse integerEofM "123"    `shouldBe` Just 123)
            .&&. (maybeParse integerEofM "123ac"  `shouldBe` Nothing )
        prop "Testing integerEof (monadic with do notation implementation)" $
                 (maybeParse integerEofM' ""      `shouldBe` Nothing )
            .&&. (maybeParse integerEofM' "123"   `shouldBe` Just 123)
            .&&. (maybeParse integerEofM' "123ac" `shouldBe` Nothing )

-- ------------------------------------------------------------------------- --
-- Chapter  24.06 — Alternative
-- Exercise       — Try Try

parseFractionM :: (TokenParsing m, MonadFail m) => m Rational
parseFractionM = do
    numerator   <- try $ decimal <* char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return $ numerator % denominator

parseFractionA :: TokenParsing m => m Rational
parseFractionA = (%) <$> try (decimal <* char '/') <*> decimal

parseNum :: (TokenParsing m, MonadFail m) => m (Either Rational Integer)
parseNum =
        Left  <$> parseFractionM
    <|> Right <$> decimal

testDecimal :: IO ()
testDecimal = hspec $ do
    describe "Experimenting with decimal" $ do
        prop "Testing parseFraction (applicative implementation)" $
                 (maybeParse parseFractionA "1"     `shouldBe` Nothing     )
            .&&. (maybeParse parseFractionA "1/"    `shouldBe` Nothing     )
            .&&. (maybeParse parseFractionA "1/3"   `shouldBe` Just (1 % 3))
            .&&. ((evaluate . fromJust $
                    maybeParse parseFractionA "1/0")
                        `shouldThrow` (== RatioZeroDenominator))
        prop "Testing parseFraction (monadic implementation)" $
                 (maybeParse parseFractionM "1"     `shouldBe` Nothing     )
            .&&. (maybeParse parseFractionM "1/"    `shouldBe` Nothing     )
            .&&. (maybeParse parseFractionM "1/3"   `shouldBe` Just (1 % 3))
            .&&. (maybeParse parseFractionM "1/0"   `shouldBe` Nothing     )
        prop "Testing parseNum (monadic implementation)" $
                 (maybeParse parseNum "1"    `shouldBe` Just (Right 1     ))
            .&&. (maybeParse parseNum "1/"   `shouldBe` Nothing            )
            .&&. (maybeParse parseNum "1/3"  `shouldBe` Just (Left (1 % 3)))
            .&&. (maybeParse parseNum "1/0"  `shouldBe` Nothing            )

-- ------------------------------------------------------------------------- --
-- Chapter  24.11 — Chapter Exercices
-- Exercise 1     — SemVer Parser
--
--  Write a parser for semantic versions as defined by http: //semver.org/.
--  After making a working parser, write an Ord instance for the SemVer type
--  that obeys the specification outlined on the SemVer website.

singleton :: Functor f => f a -> f [a]
singleton = fmap (:[])

cons :: Applicative m => m a -> m [a] -> m [a]
cons = liftA2 (:)

append :: Applicative m => m [a] -> m [a] -> m [a]
append = liftA2 (++)

manyTill' :: Alternative m => m a -> m a -> m [a]
manyTill' p end = go where go = singleton end <|> p `cons` go

data    SemVer
    =   SemVer VersionCore PreRelease Build
    deriving (Eq, Show)

data    VersionCore
    =   VersionCore Major Minor Patch
    deriving (Eq, Show)

newtype Major
    =   Major Integer
    deriving (Eq, Ord, Show)

newtype Minor
    =   Minor Integer
    deriving (Eq, Ord, Show)

newtype Patch
    =   Patch Integer
    deriving (Eq, Ord, Show)

newtype PreRelease
    =   PreRelease [PreReleaseIdentifier]
    deriving (Eq, Show)

newtype Build
    =   Build [BuildIdentifier]
    deriving (Eq, Show)

data PreReleaseIdentifier
    =   PreReleaseNumericIdentifier Integer
    |   PreReleaseAlphaNumericIdentifier String
    deriving (Eq, Show)

newtype BuildIdentifier
    =   BuildIdentifier String
    deriving (Eq, Show)

semVer :: CharParsing m => m SemVer
semVer = SemVer
    <$> versionCore
    <*> option (PreRelease mempty) (char '-' *> preRelease)
    <*> option (Build      mempty) (char '+' *> build)

versionCore :: CharParsing m => m VersionCore
versionCore = VersionCore
    <$> major <* string "."
    <*> minor <* string "."
    <*> patch

preRelease :: CharParsing m => m PreRelease
preRelease = PreRelease <$> dotSeparatedPreReleaseIdentifiers

build :: CharParsing m => m Build
build = Build <$> dotSeparatedBuildIdentifiers

dotSeparatedPreReleaseIdentifiers :: CharParsing m => m [PreReleaseIdentifier]
dotSeparatedPreReleaseIdentifiers = preReleaseIdentifier `sepBy1` char '.'

dotSeparatedBuildIdentifiers :: CharParsing m => m [BuildIdentifier]
dotSeparatedBuildIdentifiers = buildIdentifier `sepBy1` char '.'

preReleaseIdentifier :: CharParsing m => m PreReleaseIdentifier
preReleaseIdentifier =
        (PreReleaseAlphaNumericIdentifier <$> try alphanumericIdentifier)
    <|> (PreReleaseNumericIdentifier . read <$> numericIdentifier)

buildIdentifier :: CharParsing m => m BuildIdentifier
buildIdentifier = BuildIdentifier
    <$> (try alphanumericIdentifier <|> digits)

major :: CharParsing m => m Major
major = Major . read <$> numericIdentifier

minor :: CharParsing m => m Minor
minor = Minor . read <$> numericIdentifier

patch :: CharParsing m => m Patch
patch = Patch . read <$> numericIdentifier

positiveDigit :: CharParsing m => m Char
positiveDigit = oneOf ['1'..'9']

digits :: CharParsing m => m String
digits = some digit

nonDigit :: CharParsing m => m Char
nonDigit = letter <|> char '-'

identifierCharacter :: CharParsing m => m Char
identifierCharacter = digit <|> nonDigit

identifierCharacters :: CharParsing m => m String
identifierCharacters = some identifierCharacter

alphanumericIdentifier :: CharParsing m => m String
alphanumericIdentifier =
        manyTill' identifierCharacter nonDigit `append` many identifierCharacter

numericIdentifier :: CharParsing m => m String
numericIdentifier =
        string "0"
    <|> positiveDigit `cons` many digit

instance Ord SemVer where
    (SemVer core (PreRelease []) _) `compare` (SemVer core' (PreRelease []) _) =
            core `compare` core'
    (SemVer core _               _) `compare` (SemVer core' (PreRelease []) _) =
            core `compare` core' <> LT
    (SemVer core (PreRelease []) _) `compare` (SemVer core' _               _) =
            core `compare` core' <> GT
    (SemVer core pre             _) `compare` (SemVer core' pre'            _) =
            core `compare` core'
        <>  pre  `compare` pre'

instance Ord VersionCore where
    (VersionCore a b c) `compare` (VersionCore a' b' c') =
            a `compare` a'
        <>  b `compare` b'
        <>  c `compare` c'

instance Ord PreRelease where
    PreRelease []     `compare` PreRelease []     = EQ
    PreRelease []     `compare` _                 = LT
    _                 `compare` PreRelease []     = GT
    PreRelease (x:xs) `compare` PreRelease (y:ys) =
            x `compare` y
        <>  PreRelease xs `compare` PreRelease ys

instance Ord PreReleaseIdentifier where
    PreReleaseNumericIdentifier      n `compare` PreReleaseNumericIdentifier      k = n `compare` k
    PreReleaseNumericIdentifier      _ `compare` PreReleaseAlphaNumericIdentifier _ = LT
    PreReleaseAlphaNumericIdentifier _ `compare` PreReleaseNumericIdentifier      _ = GT
    PreReleaseAlphaNumericIdentifier a `compare` PreReleaseAlphaNumericIdentifier b = a `compare` b

testSemVer :: IO ()
testSemVer = hspec $ do
    describe "Testing SemVer Parser (Unit Tests)" $ do
        it "can parse simple VersionCore" $ property $
                 (maybeParse semVer "1"     `shouldBe` Nothing)
            .&&. (maybeParse semVer "1.0"   `shouldBe` Nothing)
            .&&. (maybeParse semVer "1.0."  `shouldBe` Nothing)
            .&&. (maybeParse semVer"v1.0.1" `shouldBe` Nothing)
            .&&. (maybeParse semVer"01.0.1" `shouldBe` Nothing)
            .&&. (maybeParse semVer "1.0.2" `shouldBe`
                    Just (SemVer (VersionCore (Major 1) (Minor 0) (Patch 2))
                                 (PreRelease [])
                                 (Build      [])))
        it "can parse PreRelease" $ property $
                 (maybeParse semVer "1.0.0-alpha" `shouldBe`
                    Just (SemVer (VersionCore (Major 1) (Minor 0) (Patch 0))
                                 (PreRelease [ PreReleaseAlphaNumericIdentifier "alpha" ])
                                 (Build      [])))
            .&&. (maybeParse semVer "1.0.0-alpha.1" `shouldBe`
                    Just (SemVer (VersionCore (Major 1) (Minor 0) (Patch 0))
                                 (PreRelease [ PreReleaseAlphaNumericIdentifier "alpha"
                                             , PreReleaseNumericIdentifier 1
                                             ])
                                 (Build      [])))
            .&&. (maybeParse semVer "1.0.0-alpha.beta" `shouldBe`
                    Just (SemVer (VersionCore (Major 1) (Minor 0) (Patch 0))
                                 (PreRelease [ PreReleaseAlphaNumericIdentifier "alpha"
                                             , PreReleaseAlphaNumericIdentifier "beta"
                                             ])
                                 (Build      [])))
        it "can parse full SemVer" $ property $
                 (maybeParse semVer "1.0.0-alpha+exp" `shouldBe`
                    Just (SemVer (VersionCore (Major 1) (Minor 0) (Patch 0))
                                 (PreRelease [ PreReleaseAlphaNumericIdentifier "alpha" ])
                                 (Build      [ BuildIdentifier "exp" ])))
            .&&. (maybeParse semVer "1.0.0+exp-more" `shouldBe`
                    Just (SemVer (VersionCore (Major 1) (Minor 0) (Patch 0))
                                 (PreRelease [])
                                 (Build      [ BuildIdentifier "exp-more" ])))
    describe "Testing SemVer Ordering (Unit Tests)" $ do
        it "can order VersionCore" $ property $
                 (compare (maybeParse semVer "1.0.0"           )
                          (maybeParse semVer "1.0.0"           ) `shouldBe` EQ)
            .&&. (compare (maybeParse semVer "1.0.0"           )
                          (maybeParse semVer "1.0.1"           ) `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0"           )
                          (maybeParse semVer "2.0.0"           ) `shouldBe` LT)
        it "can order PreRelease" $ property $
                 (compare (maybeParse semVer "1.0.0-alpha"     )
                          (maybeParse semVer "1.0.0-alpha"     ) `shouldBe` EQ)
            .&&. (compare (maybeParse semVer "1.0.0-alpha"     )
                          (maybeParse semVer "1.0.0-alpha.1"   ) `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0-alpha.1"   )
                          (maybeParse semVer "1.0.0-alpha.beta") `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0-alpha.beta")
                          (maybeParse semVer "1.0.0-beta"      ) `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0-beta"      )
                          (maybeParse semVer "1.0.0-beta.2"    ) `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0-beta.2"    )
                          (maybeParse semVer "1.0.0-beta.11"   ) `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0-beta.11"   )
                          (maybeParse semVer "1.0.0-rc.1"      ) `shouldBe` LT)
            .&&. (compare (maybeParse semVer "1.0.0-rc.1"      )
                          (maybeParse semVer "1.0.0"           ) `shouldBe` LT)
        it "can order while ignoring build metadata" $ property $
                 (compare (maybeParse semVer "1.0.0+exp"       )
                          (maybeParse semVer "1.0.0+rel"       ) `shouldBe` EQ)

-- ------------------------------------------------------------------------- --
-- Chapter  24.11 — Chapter Exercices
-- Exercise 2
--
-- Write a parser for positive integer values. Don't reuse the pre-existing
-- digit or integer functions, but you can use the rest of the libraries
-- we've shown you so far.

parseDigit :: CharParsing m => m Char
parseDigit = choice $ char <$> ['0'..'9']

testParseDigit :: IO ()
testParseDigit = hspec $ do
    describe "parseDigit" $ do
        it "can parse single digits" $ property $
             (maybeParse parseDigit "123" `shouldBe` Just '1')
        it "fails on letters" $ property $
             (maybeParse parseDigit "abc" `shouldBe` Nothing )

base10Integer :: CharParsing m => m Integer
base10Integer  =
        foldl' (\n d -> 10 * n + toInteger(digitToInt d)) 0
    <$> some digit

testBase10Integer :: IO ()
testBase10Integer = hspec $ do
    describe "base10Integer" $ do
        it "can parse integers" $ property $
             (maybeParse base10Integer "123abc" `shouldBe` Just 123)
        it "fails on letters" $ property $
             (maybeParse base10Integer "abc"    `shouldBe` Nothing )

-- ------------------------------------------------------------------------- --
-- Chapter  24.11 — Chapter Exercices
-- Exercise 3
--
-- Extend the parser you wrote to handle negative and positive integers. Try
-- writing a new parser in terms of the one you already have to do this.

sign :: CharParsing m => m (Integer -> Integer)
sign =  negate <$ char '-'
    <|> pure id

base10Integer' :: CharParsing m => m Integer
base10Integer' = sign <*> base10Integer

testBase10Integer' :: IO ()
testBase10Integer' = hspec $ do
    describe "base10Integer'" $ do
        it "can parse positive integers" $ property $
             (maybeParse base10Integer' "123abc"  `shouldBe` Just 123)
        it "can parse negative integers" $ property $
             (maybeParse base10Integer' "-123abc" `shouldBe` Just (-123))
        it "fails on letters" $ property $
             (maybeParse base10Integer' "abc"     `shouldBe` Nothing )
