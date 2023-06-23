{-# LANGUAGE TemplateHaskell #-}
module Trout.Game
    ( Pieces (..)
    , pieceActive, pieceLimit, pieceDiag, pieceRoyal
    , pawns, knights, bishops, rooks, queens, kings
    , byPiece
    , active, inactive
    , flipPieces
    , Sides
    , sideWhite, sideBlack
    , Game (..)
    , HasGame (..)
    , startingGame
    , gameAsBoard
    , HGame (..)
    , hgGame, hgHash
    , mkHGame
    , startingHGame
    , flipTurn
    , allMoves
    , inCheck
    , makeMove
    ) where

import Data.Foldable                    (foldl')
import Data.Hashable                    (Hashable (..))
import Data.Vector.Primitive            (unsafeIndex)
import Lens.Micro                       (Lens', (%~), (&), (<&>), (?~), (^.))
import Lens.Micro.TH                    (makeLenses)
import Trout.Bitboard
    ( Bitboard
    , bit
    , clearBit
    , complement
    , countTrailingZeros
    , fileA
    , fileH
    , setBit
    , testBit
    , toSqs
    , xor
    , xyToSq
    , zeroBits
    , (!<<.)
    , (!>>.)
    , (.&.)
    , (.|.)
    )
import Trout.Game.MoveGen
    ( Move (..)
    , SpecialMove (..)
    , bishopMoves
    , concatDL
    , kingMoves
    , kingTable
    , knightMoves
    , knightTable
    , mapOnes
    , pawnsMoves
    , queenMoves
    , rookMoves
    )
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Game.Zobrists
    ( blackBishopZobrists
    , blackKingZobrists
    , blackKnightZobrists
    , blackPawnZobrists
    , blackQueenZobrists
    , blackRookZobrists
    , castleZobrists
    , enPassantZobrists
    , pieceZobrists
    , playingZobrist
    , whiteBishopZobrists
    , whiteKingZobrists
    , whiteKnightZobrists
    , whitePawnZobrists
    , whiteQueenZobrists
    , whiteRookZobrists
    )
import Trout.Piece                      (Color (..), Piece (..), other)

-- TODO maybe 1 more field for occupancy?
data Pieces = Pieces
    { _pieceActive :: !Bitboard -- pieces of the current player
    , _pieceLimit  :: !Bitboard -- pieces that don't have infinite range
    , _pieceDiag   :: !Bitboard -- can piece move diagonally? (no knights!)
    , _pieceRoyal  :: !Bitboard -- rooks, queens, kings
    } deriving (Eq, Show)
makeLenses ''Pieces

-- helper to generate a piece lens
-- TODO slow because of bb difference calculation
-- no easy solution though...
genPiece :: Bool -> Bool -> Bool -> Lens' Pieces Bitboard
genPiece slide diag royal afb pieces = afb pieceBB
    <&> \newBB -> pieces
        & pieceLimit %~ sfSet pieceBB newBB
        & pieceDiag  %~ dfSet pieceBB newBB
        & pieceRoyal %~ rfSet pieceBB newBB
  where
    {-
    old new cur res
     0   0   0   0
     0   0   1   1
     0   1   0   1
     0   1   1   1
     1   0   1   0
     1   1   1   1
     1   0   0   ? IMPOSSIBLE
     1   1   0   ? IMPOSSIBLE
    -}
    tf old new x = x `xor` old .|. new
    {-
    old new cur res
     0   0   0   0
     0   0   1   1
     0   1   0   0
     0   1   1   0
     1   0   0   0
     1   1   0   0
     1   0   1   ? IMPOSSIBLE
     1   1   1   ? IMPOSSIBLE
    -}
    ff old new x = x .&. complement (old .|. new)
    (sfGet, sfSet) = if slide then (id, tf) else (complement, ff)
    (dfGet, dfSet) = if diag  then (id, tf) else (complement, ff)
    (rfGet, rfSet) = if royal then (id, tf) else (complement, ff)
    pieceBB = sfGet (pieces ^. pieceLimit)
        .&. dfGet (pieces ^. pieceDiag)
        .&. rfGet (pieces ^. pieceRoyal)
{-# INLINE genPiece #-}

-- remember: no piece can be all Falses! then it looks empty

-- yes limit, yes diagonal, no royal
pawns :: Lens' Pieces Bitboard
pawns = genPiece True True False
{-# INLINE pawns #-}

-- yes limit, no diagonal, no royal
knights :: Lens' Pieces Bitboard
knights = genPiece True False False
{-# INLINE knights #-}

-- no limit, yes diagonal, no royal
bishops :: Lens' Pieces Bitboard
bishops = genPiece False True False
{-# INLINE bishops #-}

-- no limit, no diagonal, yes royal
rooks :: Lens' Pieces Bitboard
rooks = genPiece False False True
{-# INLINE rooks #-}

-- no limit, yes diagonal, yes royal
queens :: Lens' Pieces Bitboard
queens = genPiece False True True
{-# INLINE queens #-}

-- yes limit, yes diagonal, yes royal
kings :: Lens' Pieces Bitboard
kings = genPiece True True True
{-# INLINE kings #-}

-- getter for current active pieces
active :: Lens' Pieces Pieces
active afb (Pieces a l d r) = afb masked
    -- new active not used, should not be changed from within current func
    <&> \(Pieces _ nl nd nr) ->
        let na = nl .|. nd .|. nr
            na' = complement na
        in Pieces
            -- masking with active isnt necessary, already premasked
            na
            (nl .|. inl .&. na')
            (nd .|. ind .&. na')
            (nr .|. inr .&. na')
  where
    masked@(Pieces _ ml md mr) = Pieces a (l .&. a) (d .&. a) (r .&. a)
    -- xor instead of & complement is safe; ml is strictly a subset of l
    inl = ml `xor` l
    ind = md `xor` d
    inr = mr `xor` r
{-# INLINE active #-}

-- getter for current inactive pieces
inactive :: Lens' Pieces Pieces
inactive afb (Pieces a l d r) = afb masked
    <&> \(Pieces _ nl nd nr) ->
        let na = nl .|. nd .|. nr
            na' = complement na
        in Pieces
            (na' .&. a)
            (nl .|. al .&. na')
            (nd .|. ad .&. na')
            (nr .|. ar .&. na')
  where
    a' = complement a
    masked@(Pieces _ ml md mr) = Pieces a (l .&. a') (d .&. a') (r .&. a')
    al = ml `xor` l
    ad = md `xor` d
    ar = mr `xor` r
{-# INLINE inactive #-}

-- flip active side
flipPieces :: Pieces -> Pieces
flipPieces (Pieces a l d r) = Pieces (complement a .&. (l .|. d .|. r)) l d r
{-# INLINE flipPieces #-}

-- get the respective lens for the piece
byPiece :: Piece -> Lens' Pieces Bitboard
byPiece Pawn   = pawns
byPiece Knight = knights
byPiece Bishop = bishops
byPiece Rook   = rooks
byPiece Queen  = queens
byPiece King   = kings
{-# INLINE byPiece #-}

-- all occupied squares
piecesAll :: Pieces -> Bitboard
piecesAll (Pieces _ l d r) = l .|. d .|. r
{-# INLINE piecesAll #-}

-- TODO kill this

type Sides a = (a, a)

sideWhite :: Lens' (Sides a) a
sideWhite afb (a, b) = (, b) <$> afb a

sideBlack :: Lens' (Sides a) a
sideBlack afb (a, b) = (a, ) <$> afb b
{-# INLINE sideBlack #-}

-- base game
-- current rules are this should be all the data needed for move generation
-- so no history, move counter, hash, etc
-- is this good idea?
data Game = Game
    { _gamePieces    :: Pieces
    , _gameCastling  :: !Int
    , _gameEnPassant :: !(Maybe Int)
    , _gameTurn      :: !Color
    } deriving (Eq, Show)

-- literally just makeClassy but change the name of game
-- i don't want to rename all my variables
class HasGame c where
    intoGame :: Lens' c Game

    gamePieces :: Lens' c Pieces
    gamePieces = intoGame
        . \afb g -> afb (_gamePieces g) <&> \x -> g { _gamePieces = x }

    gameCastling :: Lens' c Int
    gameCastling = intoGame
        . \afb g -> afb (_gameCastling g) <&> \x -> g { _gameCastling = x }

    gameEnPassant :: Lens' c (Maybe Int)
    gameEnPassant = intoGame
        . \afb g -> afb (_gameEnPassant g) <&> \x -> g { _gameEnPassant = x }

    gameTurn :: Lens' c Color
    gameTurn = intoGame
        . \afb g -> afb (_gameTurn g) <&> \x -> g { _gameTurn = x }

instance HasGame Game where intoGame = id

instance Hashable Game where
    hash (Game pieces castle enP turn) = turnHash
        `xor` castleHash
        `xor` enPassantHash
        `xor` hashBitboard (white ^. pawns) whitePawnZobrists
        `xor` hashBitboard (black ^. pawns) blackPawnZobrists
        `xor` hashBitboard (white ^. knights) whiteKnightZobrists
        `xor` hashBitboard (black ^. knights) blackKnightZobrists
        `xor` hashBitboard (white ^. bishops) whiteBishopZobrists
        `xor` hashBitboard (black ^. bishops) blackBishopZobrists
        `xor` hashBitboard (white ^. rooks) whiteRookZobrists
        `xor` hashBitboard (black ^. rooks) blackRookZobrists
        `xor` hashBitboard (white ^. queens) whiteQueenZobrists
        `xor` hashBitboard (black ^. queens) blackQueenZobrists
        `xor` hashBitboard (white ^. kings) whiteKingZobrists
        `xor` hashBitboard (black ^. kings) blackKingZobrists
      where
        hashBitboard bb table = foldl'
            (\b a -> table `unsafeIndex` a `xor` b)
            0
            (toSqs bb)
        (white, black, turnHash) = case turn of
            White -> (pieces ^. active, pieces ^. inactive, playingZobrist)
            Black -> (pieces ^. inactive, pieces ^. active, 0)
        -- only first 4 bits of castle should be used
        castleHash = castleZobrists `unsafeIndex` castle
        enPassantHash = case enP of
            Just sq -> enPassantZobrists `unsafeIndex` (sq `rem` 8)
            Nothing -> 0

    hashWithSalt salt game = hash game `xor` salt

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame = Game
    (Pieces
        0xFFFF
        0x52FF00000000FF52
        0x3CFF00000000FF3C
        0x9900000000000099)
    15
    Nothing
    White

-- pretty human-readable board
gameAsBoard :: Game -> String
gameAsBoard game = unlines [[posChar x y | x <- [0..7]] | y <- [7, 6..0]]
  where
    posChar x y = fst $ head $ filter (\(_, b) -> b `testBit` sq)
        [ ('P', white ^. pawns)
        , ('N', white ^. knights)
        , ('B', white ^. bishops)
        , ('R', white ^. rooks)
        , ('Q', white ^. queens)
        , ('K', white ^. kings)
        , ('p', black ^. pawns)
        , ('n', black ^. knights)
        , ('b', black ^. bishops)
        , ('r', black ^. rooks)
        , ('q', black ^. queens)
        , ('k', black ^. kings)
        , ('.', complement zeroBits)
        ]
      where
        (white, black) = case game ^. gameTurn of
            White -> (pieces ^. active, pieces ^. inactive)
            Black -> (pieces ^. inactive, pieces ^. active)
        pieces = game ^. gamePieces
        sq = xyToSq x y

-- incrementally hashed game
data HGame = HGame
    { _hgGame :: {-# UNPACK #-} !Game
    , _hgHash :: !Int
    } deriving (Eq, Show)
makeLenses ''HGame

instance HasGame HGame where intoGame = hgGame

instance Hashable HGame where
    hash (HGame _ h) = h
    hashWithSalt salt game = hash game `xor` salt

-- create a HGame (hashed game) from a Game
mkHGame :: Game -> HGame
mkHGame game = HGame game (hash game)
{-# INLINE mkHGame #-}

startingHGame :: HGame
startingHGame = mkHGame startingGame

-- change current player
-- also swaps playing and waiting
flipTurn :: HGame -> HGame
flipTurn (HGame (Game pcs c enP t) h) = HGame
    (Game
        (flipPieces pcs)
        c enP (other t))
    (h `xor` playingZobrist)
{-# INLINE flipTurn #-}

-- helpers for moving whole pieces around
-- TODO CONSIDER -> MAYBE HGAME; NOTHING IF UNCHANGED
setSqActive :: Piece -> Int -> HGame -> HGame
setSqActive piece sq game = game
    & gamePieces . active . byPiece piece %~ (`setBit` sq)
    & hgHash %~ xor
        (pieceZobrists (game ^. gameTurn) piece `unsafeIndex` sq)
{-# INLINE setSqActive #-}

-- works on both active an inactive
clearSq :: Piece -> Int -> HGame -> HGame
clearSq piece sq game = game
    & gamePieces . pieceActive %~ (`clearBit` sq)
    & gamePieces . byPiece piece %~ (`clearBit` sq)
    & hgHash %~ xor
        (pieceZobrists (game ^. gameTurn) piece `unsafeIndex` sq)
{-# INLINE clearSq #-}

moveSqActive :: Piece -> Int -> Int -> HGame -> HGame
moveSqActive piece from to game = game
    & gamePieces . active . byPiece piece %~ (`setBit` to) . (`clearBit` from)
    & hgHash %~ xor
        (pieceZobrists color piece `unsafeIndex` from
            `xor` pieceZobrists color piece `unsafeIndex` to)
  where
    color = game ^. gameTurn
{-# INLINE moveSqActive #-}

-- returns possible moves at a position
allMoves :: HasGame a => a -> [Move]
allMoves game =
    pawnsMoves
        (game ^. gameEnPassant)
        (game ^. gameTurn)
        block
        myBlock
        (actives ^. pawns)
    $ concatDL
        (moveSqs knightMoves (actives ^. knights)
            $ moveSqs bishopMoves (actives ^. bishops)
            $ moveSqs rookMoves (actives ^. rooks)
            $ moveSqs queenMoves (actives ^. queens)
            $ moveSqs (kingMoves kingside queenside) (actives ^. kings) [])
        []
  where
    kingside = 0 /= 10 .&. thisCastling
    queenside = 0 /= 5 .&. thisCastling
    -- only having 1 switch on turn is important. apparently.
    thisCastling = game ^. gameCastling
        .&. case game ^. gameTurn of
            White -> 3
            Black -> 12
    -- gets and concats the move for a set of squares (for a piece)
    moveSqs mover = mapOnes (mover block myBlock)
    block = piecesAll (game ^. gamePieces)
    myBlock = piecesAll actives
    actives = game ^. gamePieces . active
{-# SPECIALIZE allMoves :: HGame -> [Move] #-}
{-# SPECIALIZE allMoves :: Game -> [Move] #-}

-- checks if a square is attacked by the opponent (waiting)
squareAttacked :: HasGame a => Bitboard -> Int -> a -> Bool
squareAttacked block sq game =
    knightTable `unsafeIndex` sq .&. inactives ^. knights
    .|. bishoped .&. inactives ^. bishops
    .|. rooked .&. inactives ^. rooks
    .|. bishoped .&. inactives ^. queens
    .|. rooked .&. inactives ^. queens
    .|. kingTable `unsafeIndex` sq .&. inactives ^. kings
    /= 0
    || pawnMask `testBit` sq
  where
    bishoped = bishopMovesMagic block sq
    rooked = rookMovesMagic block sq
    pawnMask = case game ^. gameTurn of
        White -> (p .&. complement fileA) !>>. 9
            .|. (p .&. complement fileH) !>>. 7
        Black -> (p .&. complement fileA) !<<. 7
            .|. (p .&. complement fileH) !<<. 9
    p = inactives ^. pawns
    inactives = game ^. gamePieces . inactive

-- simple wrapper around squareAttacked for the king's square
inCheck :: HasGame a => a -> Bool
inCheck game
    | kingSq == 64 = True -- king gone!
    | otherwise =
        squareAttacked (piecesAll (game ^. gamePieces)) kingSq game
  where
    kingSq = countTrailingZeros (game ^.  gamePieces . active . kings)

-- maybe rename game/g's to hgame/hg?
makeMove :: HGame -> Move -> Maybe HGame
makeMove game (Move piece special from to) = do
    let movedAndCleared = game
            & \(HGame (Game pieces c enP t) h) ->
                let (nPieces, pz) = doMove pieces
                    wz = captureAll pieces
                    (nCastles, cz) = clearCastles c
                    ez = maybe 0
                            (unsafeIndex enPassantZobrists . (`rem` 8))
                            enP
                in HGame
                    (Game
                        nPieces
                        nCastles
                        Nothing
                        t)
                    (h `xor` pz `xor` wz `xor` cz `xor` ez)
    afterSpecials <- specials movedAndCleared
    checkChecked <- nothingIfCheck afterSpecials
    pure (flipTurn checkChecked)
  where
    playZs = pieceZobrists (game ^. gameTurn)
    waitZs = pieceZobrists (other (game ^. gameTurn))
    moverZs = playZs piece
    -- basic moving
    doMove !pcs =
        ( pcs & active . byPiece piece %~ (`clearBit` from) . (`setBit` to)
        , (moverZs `unsafeIndex` from) `xor` (moverZs `unsafeIndex` to)
        )
    -- capture opponent's piece if needed
    captureAll pcs
        | inactives ^. pawns   .&. toBit /= 0 = mkMask Pawn
        | inactives ^. knights .&. toBit /= 0 = mkMask Knight
        | inactives ^. bishops .&. toBit /= 0 = mkMask Bishop
        | inactives ^. rooks   .&. toBit /= 0 = mkMask Rook
        | inactives ^. queens  .&. toBit /= 0 = mkMask Queen
        | inactives ^. kings   .&. toBit /= 0 = mkMask King
        | otherwise = 0
      where
        toBit = bit to
        mkMask pc = waitZs pc `unsafeIndex` to
        inactives = pcs ^. inactive
    -- clears any castling rights if needed
    clearCastles c =
        ( newC
        , castleZobrists `unsafeIndex` c
            `xor` castleZobrists `unsafeIndex` newC
        )
      where
        newC = c .&. complement
            (castleMask from
                .|. castleMask to
                .|. case piece of
                    King -> case game ^. gameTurn of
                        White -> 3
                        Black -> 12
                    _ -> 0)
        castleMask sq = case sq of
            0  -> 1
            7  -> 2
            56 -> 4
            63 -> 8
            _  -> 0
    -- checks for check after moving
    nothingIfCheck hg = if inCheck hg then Nothing else Just hg
    -- handle special cases
    specials g = case special of
        PawnDouble -> Just
            $ g
            & gameEnPassant ?~ to
            & hgHash %~ xor (enPassantZobrists `unsafeIndex` (to `rem` 8))
        EnPassant enPSq -> Just (clearSq Pawn enPSq g)
        Promotion promote -> Just
            $ setSqActive promote to
            $ clearSq Pawn to g
        CastleKing -> moveSqActive Rook (kingOrigin + 3) (kingOrigin + 1)
            <$> throughCheckKing g
        CastleQueen -> moveSqActive Rook (kingOrigin - 4) (kingOrigin - 1)
            <$> throughCheckQueen g
        Normal -> Just g
    -- checks for castling through check
    -- assumes king is moved, rook is not
    -- originally it cleared the original king square from blockers
    -- doesnt matter because if kingOrigin +- 1 is prevented from check by blocker then kingOrigin has to be in check too
    throughCheckKing hg@(HGame g _)
        | squareAttacked block kingOrigin g = Nothing
        | squareAttacked block (kingOrigin + 1) g = Nothing
        | otherwise = Just hg
      where block = piecesAll (g ^. gamePieces)
    throughCheckQueen hg@(HGame g _)
        | squareAttacked block kingOrigin g = Nothing
        | squareAttacked block (kingOrigin - 1) g = Nothing
        | otherwise = Just hg
      where block = piecesAll (g ^. gamePieces)
    -- where the king starts on the board
    -- for castling things
    kingOrigin = case game ^. gameTurn of
        White -> 4
        Black -> 60
