-- Chess.hs
-- Se definen las funciones principales, tipos y datos que se utilizaran a la hora de 
-- ejecutar el juego por consola. Además, se incluyen funciones auxiliares
-- Jesús Celada Garcia <jescelgar@alum.us.es>
-----------------------------------------------------------------------

module Chess (
    CurrentGameState(..),
    initialState,         -- CurrentGameState
    drawByStaleMateState, -- CurrentGameState
    checkMateState,       -- CurrentGameState
    pawnPromotionState,   -- CurrentGameState
    boardRender,          -- CurrentGameState -> String
    renderPossibleMoves,  -- CurrentGameState -> Square -> [Square]
    makeMove,             -- CurrentGameState -> Square -> Square -> CurrentGameState
    undoLastMove,         -- CurrentGameState -> CurrentGameState
    isValidMovement,      -- CurrentGameState -> Square -> Square -> Bool
    playerInCheckMate,    -- CurrentGameState -> Bool
    drawByStaleMate,      -- CurrentGameState -> Bool
    isPawnPromoting,      -- CurrentGameState -> Bool
    promotePawn,          -- CurrentGameState -> Int -> CurrentGameState
    
    Square,
) where

import PilaTA
import qualified Data.Map as Map

import Data.Char(toUpper)
import Data.Maybe(fromMaybe)

-- >DECLARACIONES DE TIPO<
type Square = (Int,Int) -- Representa las casillas en el tablero
type Board = Map.Map Square Piece -- Almacena la relacion entre las piezas y la casilla donde se encuentran

-- >NUEVOS TIPOS<
data Colour = White | Black deriving(Eq,Show) -- Representa el color de las piezas
data Piece = Blank | Pawn Colour | Knight Colour | Bishop Colour | Rook Colour | Queen Colour | King Colour deriving(Eq) -- Representa lo que puede haber en una casilla siendo esto vacío o una pieza con un color asociado
data CurrentGameState = CurrentGameState Board Colour (Pila CurrentGameState) -- Representa la partida actual con el tablero, el color que toca mover y todos los movimientos anteriormente realizados

-- >FUNCIONES PRINCIPALES (EXPORTADAS)<
-- Devuelve un CurrentGameState con el estado inicial de la partida
initialState :: CurrentGameState
initialState = CurrentGameState initialBoard White vacia

-- Devuelve un CurrentGameState con una partida para probar el rey ahogado
drawByStaleMateState :: CurrentGameState
drawByStaleMateState = CurrentGameState drawByStaleMateBoard Black vacia

-- Devuelve un CurrentGameState con una partida para probar el jaque mate
checkMateState :: CurrentGameState
checkMateState = CurrentGameState checkMateBoard Black vacia

-- Devuelve un CurrentGameState con una partida para probar la coronación de un peón
pawnPromotionState :: CurrentGameState
pawnPromotionState = CurrentGameState pawnPromotionBoard White vacia

-- Transforma una partida en una cadena que representa el tablero interpretable visualmente
boardRender :: CurrentGameState -> String
boardRender (CurrentGameState b c _) = squares ++ sepRow ++ lettersFile
    where
        letters       = if (c == White) then ['A'..'H'] else ['H','G'..'A']
        squares       = if (c == White) then squareRender b White (1,8)  else squareRender b Black (8,1)
        sepRow       = "\n--+" ++ replicate 24 '-' 
        lettersFile   = "\n  |" ++ foldr (\l ac -> "  " ++ [l] ++ ac) "" letters

-- Devuelve la lista de casillas a las que se puede mover en ese turno la pieza que se indique como parámetro
renderPossibleMoves :: CurrentGameState -> Square -> [Square]
renderPossibleMoves (CurrentGameState b c _) s
    | isPieceEnemy b c s || isBlankSquare b s = []
    | otherwise = pieceLegalMoves b c s

-- Ejecuta un movimiento en una partida
makeMove :: CurrentGameState -> Square -> Square -> CurrentGameState          
makeMove gs@(CurrentGameState b c stack) originSq targetSq =  CurrentGameState nextBoard (getEnemyColour c) (apila gs stack)
    where
        piece = getPieceFromSquare b originSq
        nextBoard = Map.insert targetSq piece $ Map.delete originSq b

-- Deshace el ultimo movimiento guardado en la partida
undoLastMove :: CurrentGameState -> CurrentGameState
undoLastMove (CurrentGameState _ _ stack) = cima stack

-- Devuelve TRUE si el movimiento que se intenta hacer es posible
isValidMovement :: CurrentGameState -> Square -> Square -> Bool
isValidMovement gs@(CurrentGameState b c _) originSq targetSq = elem targetSq (pieceLegalMoves b c originSq)

-- Devuelve TRUE si al jugador que le toca mover está en jaque mate
playerInCheckMate :: CurrentGameState -> Bool
playerInCheckMate (CurrentGameState b c _) = playerNoLegalMoves b c && playerInCheck b c

-- Devuelve TRUE si la partida está en tablas por rey ahogado
drawByStaleMate :: CurrentGameState -> Bool
drawByStaleMate (CurrentGameState b c _)   = playerNoLegalMoves b c && not (playerInCheck b c)

-- Devuelve TRUE si el jugador que movió en la ocasión anterior ha coronado algún peón 
isPawnPromoting :: CurrentGameState -> Bool
isPawnPromoting (CurrentGameState b c _)   = (not. null) $ getPawnsPromoted b (getEnemyColour c)

-- Cambia el peón coronado por la pieza que se desee siguiendo la siguiente correlación [1=Queen,2=Rook,3=Bishop,4=Knight]
promotePawn :: CurrentGameState -> Int -> CurrentGameState
promotePawn (CurrentGameState b c stack) n = (CurrentGameState newBoard c stack) -- n : {1,2,3,4}
    where
        colorPromotedPawn = getEnemyColour c
        targetSq:_ = getPawnsPromoted b colorPromotedPawn -- length getPawnsPromoted == 1 en este caso
        newPiece:_ = drop (n-1) [Queen colorPromotedPawn, Rook colorPromotedPawn, Bishop colorPromotedPawn, Knight colorPromotedPawn]
        newBoard = Map.insert targetSq newPiece b

-- >FUNCIONES SECUNDARIAS<
-- >>REALTIVAS A initialState<<
-- Devuelve el tablero inicial
initialBoard :: Board
initialBoard = Map.fromList [(s, piecesInitialBoard s) | x <- [1..8], y <- [1,2,7,8], let s = (x,y)]

-- Devuelve las piezas correspondinetes al tablero inicial segun la casilla
piecesInitialBoard :: Square ->  Piece
piecesInitialBoard (x,y)
    | y == 2 || y == 7 = Pawn c
    | x == 2 || x == 7 = Knight c
    | x == 3 || x == 6 = Bishop c
    | x == 1 || x == 8 = Rook c
    | x == 4           = Queen c
    | x == 5           = King c
        where
            c = if (y <= 2) then White else Black

-- >>REALTIVAS A drawByStaleMateState<<
-- Devuelve el tablero para poder probar el rey ahogado ejecutando el movimiento g1g7
drawByStaleMateBoard :: Board
drawByStaleMateBoard = Map.fromList [((1,8),(King White)), ((4,6),(Bishop Black)), ((7,1),(Rook Black)), ((8,1),(King Black))]

-- >>REALTIVAS A checkMateState<<
-- Devuelve el tablero para poder probar el jaque mate ejecutando el movimiento g1a1
checkMateBoard :: Board
checkMateBoard = Map.fromList [((1,8),(King White)), ((4,6),(Bishop Black)), ((7,1),(Rook Black)), ((8,1),(King Black)), ((2,2),(Rook Black))]

-- >>REALTIVAS A pawnPromotionState<<
-- Devuelve el tablero para poder probar la coronacion del peon ejecutando el movimiento a7a8
pawnPromotionBoard :: Board
pawnPromotionBoard = Map.fromList [((1,7),(Pawn White)), ((3,1),(King White)), ((8,8),(King Black))]

-- >>RELATIVAS A boardRender<<
-- Devuelve puramente el tablero convertido a cadena
squareRender :: Board -> Colour -> Square -> String
squareRender b c s@(x,y)
    | y == 0 || y == 9 = ""
    | otherwise = numberRender c ++ (pieceRender p) ++ (squareRender b c nextS)
        where
            numberRender White = if (x == 1) then "\n" ++ show y ++ " | " else ""
            numberRender Black = if (x == 8) then "\n" ++ show y ++ " | " else ""

            nextX White = (mod x 8) + 1
            nextX Black = if (x == 1) then 8 else (x-1)

            nextY White = if (nextX c == 1) then (y - 1) else y
            nextY Black = if (nextX c == 8) then (y + 1) else y

            p = getPieceFromSquare b s
            nextS = (nextX c, nextY c)

-- Traduce la pieza en cadena
pieceRender :: Piece -> String
pieceRender p = if (getColour p == Just White) then map toUpper (rP p) else rP p
    where
        rP  Blank     = " · " --" \x00B7 "
        rP (Pawn _)   = " p "
        rP (Knight _) = " n "
        rP (Bishop _) = " b "
        rP (Rook _)   = " r "
        rP (Queen _)  = " q "
        rP (King _)   = " k "

-- >>RELATIVAS A isValidMovement, playerInCheckMate y drawByStaleMate<<
-- >>>MOVIMIENTO DE LAS PIEZAS<<<
-- Devuelve los posibles movimientos de la pieza que se encuentre en la casilla indicada
possibleMoves  :: Board -> Square -> [Square]
possibleMoves b s = possibleMoves' b s (getPieceFromSquare b s)

possibleMoves' :: Board -> Square -> Piece -> [Square]
possibleMoves' b _  Blank     = []
possibleMoves' b s (Pawn c)   = pawnMoves b c s
possibleMoves' b s (Knight c) = knightMoves b c s
possibleMoves' b s (Bishop c) = bishopMoves b c s
possibleMoves' b s (Rook c)   = rookMoves b c s
possibleMoves' b s (Queen c)  = queenMoves b c s
possibleMoves' b s (King c)   = kingMoves b c s

-- Devuelve los posibles movimientos de un peon que se encuentre en la casilla indicada teniendo en cuenta las posiciones de las otras piezas
-- Si es turno de las negras se multiplica todo los movimientos y revisiones por -1 ya que van en sentido contrario a las blancas
pawnMoves :: Board -> Colour -> Square -> [Square] -- blancas a mas y negras a menos
pawnMoves b c (x,y) = moveForward ++ moveForward2 ++ captureLeft ++ captureRight
    where
        moveForward  = if isBlankSquare b (x,y + f 1)                                                 then [(x      ,y + f 1)] else []
        moveForward2 = if isBlankSquare b (x,y + f 1) && isBlankSquare b (x,y + f 2) && y == condMvF2 then [(x      ,y + f 2)] else [] 
        captureLeft  = if isPieceEnemy  b c (x - f 1,y + f 1)                                         then [(x - f 1,y + f 1)] else []
        captureRight = if isPieceEnemy  b c (x + f 1,y + f 1)                                         then [(x + f 1,y + f 1)] else []

        f n      = if (c == White) then n else n*(-1)
        condMvF2 = if (c == White) then 2 else 7

-- Devuelve los posibles movimientos de un caballo que se encuentre en la casilla indicada teniendo en cuenta las posiciones de las otras piezas
knightMoves :: Board -> Colour -> Square -> [Square] 
knightMoves b c (x,y) = knightMoves' b c potMoves
    where
        relativeMoves = [(-1,2),(1,2),(-2,1),(2,1),(-2,-1),(2,-1),(-1,-2),(1,-2)]
        potMoves = map (\(mx,my) -> (x+mx,y+my)) relativeMoves

knightMoves' :: Board -> Colour -> [Square] -> [Square]
knightMoves' b c potMoves
    | null potMoves = []
    | isInBoardRange move && isInvasibleSquare b c move = move:knightMoves' b c (drop 1 potMoves)
    | otherwise = knightMoves' b c (drop 1 potMoves)
        where
            move = head potMoves

-- Devuelve los posibles movimientos de un alfil que se encuentre en la casilla indicada teniendo en cuenta las posiciones de las otras piezas
bishopMoves :: Board -> Colour -> Square -> [Square]
bishopMoves b c (x,y) = bAndRMoves b c potentialMoves
    where
        potentialMoves = [zip xs ys | xs <- [nPlus x, nMinus x], ys <- [nPlus y,nMinus y]]

-- Devuelve los posibles movimientos de una torre que se encuentre en la casilla indicada teniendo en cuenta las posiciones de las otras piezas
rookMoves :: Board -> Colour -> Square -> [Square]
rookMoves b c (x,y) = bAndRMoves b c potentialMoves
    where
        potentialMoves = [zip (repeat x) (nPlus y), zip (repeat x) (nMinus y), zip (nPlus x) (repeat y), zip (nMinus x) (repeat y)]

-- Funciones auxiliares de las dos anteriores
nPlus :: Int -> [Int]
nPlus n = [n+1..8]

nMinus :: Int -> [Int]
nMinus n = [n-1,n-2..1]

-- Funcion que ayuda a calcular los movimientos de los alfiles y torres. Se comparten porque siguen lógicas iguales
bAndRMoves :: Board -> Colour -> [[Square]] -> [Square]
bAndRMoves b c sQss = blankMoves ++ map head captureMoves
    where
        splitSquares = map (break (not . isBlankSquare b)) sQss
        blankMoves = concat $ map fst splitSquares
        captureMoves = filter (\xs -> (not . null) xs && isPieceEnemy b c (head xs)) (map snd splitSquares)

-- -- Devuelve los posibles movimientos de una reina que se encuentre en la casilla indicada teniendo en cuenta las posiciones de las otras piezas
queenMoves :: Board -> Colour -> Square -> [Square]
queenMoves b c s = (bishopMoves b c s) ++ (rookMoves b c s)

-- Devuelve los posibles movimientos de un rey que se encuentre en la casilla indicada teniendo en cuenta las posiciones de las otras piezas
kingMoves :: Board -> Colour -> Square -> [Square]
kingMoves b c (x,y) = [move | mx <- [-1,0,1], my <- [-1,0,1], let move = (x+mx,y+my), move /= (x,y), isInvasibleSquare b c move, isInBoardRange move]

-- >>>JAQUES<<<
-- Devuelve TRUE si el color que se le indica está en jaque
playerInCheck :: Board -> Colour -> Bool
playerInCheck b c = any (kingPosition==) enemyPossibleMoves
    where
        enemyPieces        = getPiecesOfColor b (getEnemyColour c)
        enemyPossibleMoves = concat $ map (possibleMoves b) (Map.keys enemyPieces)
        kingPosition:_     = Map.keys $ Map.filter (King c ==) b -- El rey siempre existe

-- Devuelve TRUE si el color que se indica no tiene ningún movimiento legal disponible, es decir, no puede realizar ningún movimiento
playerNoLegalMoves :: Board -> Colour -> Bool
playerNoLegalMoves b c = all (null . pieceLegalMoves b c) (Map.keys $ getPiecesOfColor b c)

-- Devuelve los movimientos legales que tiene la pieza que se encuentre en la casilla indicada
-- Aquí se le añade una dimensión mas a los posibles movimientos de las piezas que podríamos haber obtenido con anterioridad
-- teniendo en cuenta si el movimiento que pudieses hacer te dejase en jaque, algo que no está permitido
pieceLegalMoves :: Board -> Colour -> Square -> [Square]
pieceLegalMoves b c s = filter (\targetSq -> 
                                let (CurrentGameState nextBoard _ _) = makeMove (CurrentGameState b c vacia) s targetSq
                                in not $ playerInCheck nextBoard c) piecePossibleMoves
    where
        piecePossibleMoves = possibleMoves b s

-- >>RELATIVAS A isPawnPromoting y promotePawn<<
-- Devuelve los peones que estan coronando segun su color
-- Esta funcion nunca va a devolver mas de un peon coronando ya que en cuanto corona uno se cambia directamente por la pieza elegida en nuestro juego
getPawnsPromoted :: Board -> Colour -> [Square]
getPawnsPromoted b White = filter ((8==).snd) (Map.keys $ Map.filter (Pawn White ==) b)
getPawnsPromoted b Black = filter ((1==).snd) (Map.keys $ Map.filter (Pawn Black ==) b)
    
-- >FUNCIONES AUXILIARES<
-- Devuelve TRUE si la casilla indicada está vacía
isBlankSquare :: Board -> Square -> Bool
isBlankSquare b s = Blank == getPieceFromSquare b s

-- Devuelve TRUE si la casilla indicada tiene una pieza que es enemiga o contraria a la que se indica
isPieceEnemy :: Board -> Colour -> Square -> Bool
isPieceEnemy b c s = (Just c) /= (getColour piece) && piece /= Blank
    where
        piece = getPieceFromSquare b s

-- Devuelve TRUE si la casilla indicada pertenece a una del tablero
isInBoardRange :: Square -> Bool
isInBoardRange (x,y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

-- Devuelve TRUE si la casilla indicada es invadible
isInvasibleSquare :: Board -> Colour -> Square -> Bool
isInvasibleSquare b c s = isBlankSquare b s || isPieceEnemy b c s

-- Devuelve la pieza que se encuentre en la casilla
getPieceFromSquare :: Board -> Square -> Piece
getPieceFromSquare b s = fromMaybe Blank (Map.lookup s b)

-- Devuelve el color empaquetado en un tipo Maybe de la pieza indicada
getColour :: Piece -> Maybe Colour
getColour  Blank     = Nothing
getColour (Pawn c)   = Just c
getColour (Knight c) = Just c
getColour (Bishop c) = Just c
getColour (Rook c)   = Just c
getColour (Queen c)  = Just c
getColour (King c)   = Just c

-- Devuelve en un tablero todas las piezas del color que se indica
getPiecesOfColor :: Board -> Colour -> Board
getPiecesOfColor b c = Map.filter (\p -> Just c == getColour p) b

-- Devuelve el color del oponente
getEnemyColour :: Colour -> Colour
getEnemyColour c = case c of White -> Black
                             c     -> White



---------------------------------------------------------------------------------

{-
knightMoves'' :: Board -> Colour -> Square -> [Square]
knightMoves'' b c (x,y) = [move | (mx,my) <- relativeMoves, let move = (x+mx,y+my), isInBoardRange move, isInvasibleSquare b c move]
    where
        relativeMoves = [(-1,2),(1,2),(-2,1),(2,1),(-2,-1),(2,-1),(-1,-2),(1,-2)]
-}