{-# LANGUAGE TemplateHaskell #-}
module Trout.Game
    ( Pieces (..)
    , pawns, knights, bishops, rooks, queens, kings, byPiece
    , Sides
    , sideWhite, sideBlack
    , Game (..)
    , gamePlaying, gameWaiting, gameCastling, gameEnPassant, gameTurn
    , HGame (..)
    , hgGame, hgHash
    , gamePieces
    , gameAsBoard
    , startingGame
    , allMoves
    , inCheck
    , makeMove, mkHGame
    ) where

import Data.Foldable                    (foldl')
import Data.Hashable                    (Hashable (..))
import Data.Vector.Primitive            (unsafeIndex)
import Lens.Micro
    ( Lens
    , Lens'
    , (%~)
    , (&)
    , (<&>)
    , (?~)
    , (^.)
    )
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

data Pieces = Pieces
    { _pawns   :: !Bitboard
    , _knights :: !Bitboard
    , _bishops :: !Bitboard
    , _rooks   :: !Bitboard
    , _queens  :: !Bitboard
    , _kings   :: !Bitboard
    } deriving (Eq, Show)
makeLenses ''Pieces

byPiece :: Piece -> Lens' Pieces Bitboard
byPiece Pawn   = pawns
byPiece Knight = knights
byPiece Bishop = bishops
byPiece Rook   = rooks
byPiece Queen  = queens
byPiece King   = kings
{-# INLINE byPiece #-}

piecesAll :: Pieces -> Bitboard
piecesAll (Pieces p n b r q k) = p .|. n .|. b .|. r .|. q .|. k
{-# INLINE piecesAll #-}

type Sides a = (a, a)

sideWhite :: Lens' (Sides a) a
sideWhite afb (a, b) = (, b) <$> afb a
{-# INLINE sideWhite #-}

sideBlack :: Lens' (Sides a) a
sideBlack afb (a, b) = (a, ) <$> afb b
{-# INLINE sideBlack #-}

data Game = Game
    { _gamePlaying   :: Pieces
    , _gameWaiting   :: Pieces
    , _gameCastling  :: !Int
    , _gameEnPassant :: !(Maybe Int)
    , _gameTurn      :: !Color
    } deriving (Eq, Show)
makeLenses ''Game

instance Hashable Game where
    hash (Game play wait castle enP turn) = turnHash
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
            White -> (play, wait, playingZobrist)
            Black -> (wait, play, 0)
        -- only first 4 bits of castle should be used
        castleHash = castleZobrists `unsafeIndex` castle
        enPassantHash = case enP of
            Just sq -> enPassantZobrists `unsafeIndex` (sq `rem` 8)
            Nothing -> 0

    hashWithSalt salt game = hash game `xor` salt

gamePieces :: Lens Game Game (Sides Pieces) (Sides Pieces)
gamePieces afb game@(Game p w _ _ t) = case t of
    White -> afb (p, w)
        <&> \(p', w') -> game {_gamePlaying = p', _gameWaiting = w'}
    Black -> afb (w, p)
        <&> \(w', p') -> game {_gamePlaying = w', _gameWaiting = p'}
{-# INLINE gamePieces #-}

-- incrementally hashed game
data HGame = HGame
    { _hgGame :: {-# UNPACK #-} !Game
    , _hgHash :: !Int
    } deriving (Eq, Show)
makeLenses ''HGame

instance Hashable HGame where
    hash (HGame _ h) = h
    hashWithSalt salt game = hash game `xor` salt

mkHGame :: Game -> HGame
mkHGame game = HGame game (hash game)
{-# INLINE mkHGame #-}

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame = Game
    (Pieces
        65280
        66
        36
        129
        8
        16)
    (Pieces
        71776119061217280
        4755801206503243776
        2594073385365405696
        9295429630892703744
        576460752303423488
        1152921504606846976)
    15
    Nothing
    White

gameAsBoard :: Game -> String
gameAsBoard game = unlines [[posChar x y | x <- [0..7]] | y <- [7, 6..0]]
  where
    posChar x y = fst $ head $ filter (\(_, b) -> b `testBit` sq)
        [ ('P', whitePieces ^. pawns)
        , ('N', whitePieces ^. knights)
        , ('B', whitePieces ^. bishops)
        , ('R', whitePieces ^. rooks)
        , ('Q', whitePieces ^. queens)
        , ('K', whitePieces ^. kings)
        , ('p', blackPieces ^. pawns)
        , ('n', blackPieces ^. knights)
        , ('b', blackPieces ^. bishops)
        , ('r', blackPieces ^. rooks)
        , ('q', blackPieces ^. queens)
        , ('k', blackPieces ^. kings)
        , ('.', complement zeroBits)
        ]
      where
        whitePieces = game ^. gamePieces . sideWhite
        blackPieces = game ^. gamePieces . sideBlack
        sq = xyToSq x y

flipTurn :: HGame -> HGame
flipTurn (HGame (Game p w c enP t) h) = HGame
    (Game w p c enP (other t))
    (h `xor` playingZobrist)
{-# INLINE flipTurn #-}

gameBlockers :: Game -> Bitboard
gameBlockers game =
    piecesAll (game ^. gamePlaying)
    .|. piecesAll (game ^. gameWaiting)
{-# INLINE gameBlockers #-}

-- TODO CONSIDER -> MAYBE HGAME; NOTHING IF UNCHANGED
setSqPlaying :: Piece -> Int -> HGame -> HGame
setSqPlaying piece sq game = game
    & hgGame . gamePlaying . byPiece piece %~ (`setBit` sq)
    & hgHash %~ xor
        (pieceZobrists (game ^. hgGame . gameTurn) piece `unsafeIndex` sq)
{-# INLINE setSqPlaying #-}

clearSqPlaying :: Piece -> Int -> HGame -> HGame
clearSqPlaying piece sq game = game
    & hgGame . gamePlaying . byPiece piece %~ (`clearBit` sq)
    & hgHash %~ xor
        (pieceZobrists (game ^. hgGame . gameTurn) piece `unsafeIndex` sq)
{-# INLINE clearSqPlaying #-}

clearSqWaiting :: Piece -> Int -> HGame -> HGame
clearSqWaiting piece sq game = game
    & hgGame . gamePlaying . byPiece piece %~ (`clearBit` sq)
    & hgHash %~ xor
        (pieceZobrists
            (other (game ^. hgGame . gameTurn))
            piece
            `unsafeIndex` sq)
{-# INLINE clearSqWaiting #-}

moveSqPlaying :: Piece -> Int -> Int -> HGame -> HGame
moveSqPlaying piece from to game = game
    & hgGame . gamePlaying . byPiece piece %~ (`setBit` to) . (`clearBit` from)
    & hgHash %~ xor
        (pieceZobrists color piece `unsafeIndex` from
            `xor` pieceZobrists color piece `unsafeIndex` to)
  where
    color = game ^. hgGame . gameTurn
{-# INLINE moveSqPlaying #-}

allMoves :: Game -> [Move]
allMoves game =
    pawnsMoves
        (game ^. gameEnPassant)
        (game ^. gameTurn)
        block
        myBlock
        p
    $ concatDL
        (moveSqs knightMoves n
            $ moveSqs bishopMoves b
            $ moveSqs rookMoves r
            $ moveSqs queenMoves q
            $ moveSqs (kingMoves kingside queenside) k [])
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
    block = myBlock .|. piecesAll (game ^. gameWaiting)
    myBlock = piecesAll turnPieces
    turnPieces@(Pieces p n b r q k) = game ^. gamePlaying

squareAttacked :: Bitboard -> Int -> Game -> Bool
squareAttacked block sq game = knightTable `unsafeIndex` sq .&. n
    .|. bishoped .&. b
    .|. rooked .&. r
    .|. bishoped .&. q
    .|. rooked .&. q
    .|. kingTable `unsafeIndex` sq .&. k
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
    (Pieces p n b r q k) = game ^. gameWaiting

inCheck :: Game -> Bool
inCheck game
    | kingSq == 64 = True -- king gone!
    | otherwise    = squareAttacked (gameBlockers game) kingSq game
  where
    kingSq = countTrailingZeros kingMask
    kingMask = game ^. gamePlaying . kings

-- maybe rename game/g's to hgame/hg?
makeMove :: HGame -> Move -> Maybe HGame
makeMove game (Move piece special from to) = do
    let movedAndCleared = game
            & \(HGame (Game playing waiting c enP t) h) ->
                let (nPlaying, pz) = doMove playing
                    (nWaiting, wz) = captureAll waiting
                    (nCastles, cz) = clearCastles c
                    ez = maybe
                            0
                            (unsafeIndex enPassantZobrists . (`rem` 8))
                            enP
                in HGame
                    (Game
                        nPlaying
                        nWaiting
                        nCastles
                        Nothing
                        t)
                    (h `xor` pz `xor` wz `xor` cz `xor` ez)
    afterSpecials <- specials movedAndCleared
    checkChecked <- nothingIfCheck afterSpecials
    pure (flipTurn checkChecked)
  where
    playZs = pieceZobrists (game ^. hgGame . gameTurn)
    waitZs = pieceZobrists (other (game ^. hgGame . gameTurn))
    moverZs = playZs piece
    -- basic moving
    doMove !pcs =
        ( pcs & byPiece piece %~ (`clearBit` from) . (`setBit` to)
        , (moverZs `unsafeIndex` from) `xor` (moverZs `unsafeIndex` to)
        )
    captureAll (Pieces p n b r q k)
        | p .&. toBit /= 0 =
            (Pieces (p .&. clearMask) n b r q k, mkMask Pawn)
        | n .&. toBit /= 0 =
            (Pieces p (n .&. clearMask) b r q k, mkMask Knight)
        | b .&. toBit /= 0 =
            (Pieces p n (b .&. clearMask) r q k, mkMask Bishop)
        | r .&. toBit /= 0 =
            (Pieces p n b (r .&. clearMask) q k, mkMask Rook)
        | q .&. toBit /= 0 =
            (Pieces p n b r (q .&. clearMask) k, mkMask Queen)
        | k .&. toBit /= 0 =
            (Pieces p n r b q (k .&. clearMask), mkMask King)
        | otherwise = (Pieces p n b r q k, 0)
      where
        toBit = bit to
        clearMask = complement toBit
        mkMask pc = waitZs pc `unsafeIndex` to
    clearCastles c = (newC, castleZobrists `unsafeIndex` newC)
      where
        newC = c .&. complement
            (castleMask from
                .|. castleMask to
                .|. case piece of
                    King -> case game ^. hgGame . gameTurn of
                        White -> 3
                        Black -> 12
                    _ -> 0)
        castleMask sq = case sq of
            0  -> 1
            7  -> 2
            56 -> 4
            63 -> 8
            _  -> 0
    nothingIfCheck hg@(HGame g _) = if inCheck g then Nothing else Just hg
    specials g = case special of
        PawnDouble -> Just (g & hgGame . gameEnPassant ?~ to)
        EnPassant enPSq -> Just (clearSqWaiting Pawn enPSq g)
        Promotion promote -> Just
            $ setSqPlaying promote to
            $ clearSqPlaying Pawn to g
        CastleKing -> moveSqPlaying Rook (kingOrigin + 3) (kingOrigin + 1)
            <$> throughCheckKing g
        CastleQueen -> moveSqPlaying Rook (kingOrigin - 4) (kingOrigin - 1)
            <$> throughCheckQueen g
        Normal -> Just g
    -- assumes king is moved, rook is not
    -- originally it cleared the original king square from blockers
    -- doesnt matter because if kingOrigin +- 1 is prevented from check by blocker then kingOrigin has to be in check too
    throughCheckKing hg@(HGame g _)
        | squareAttacked block kingOrigin g = Nothing
        | squareAttacked block (kingOrigin + 1) g = Nothing
        | otherwise = Just hg
      where block = gameBlockers g
    throughCheckQueen hg@(HGame g _)
        | squareAttacked block kingOrigin g = Nothing
        | squareAttacked block (kingOrigin - 1) g = Nothing
        | otherwise = Just hg
      where block = gameBlockers g
    -- for castling things
    kingOrigin = case game ^. hgGame . gameTurn of
        White -> 4
        Black -> 60
