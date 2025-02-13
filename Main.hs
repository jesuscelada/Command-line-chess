-- Main.hs
-- Se define la lógica del juego usando las funciones exportadas de Chess.hs
-- Jesús Celada Garcia <jescelgar@alum.us.es>
-----------------------------------------------------------------------

import Chess

import System.Info (os)
import System.Process (callCommand)
import Data.Char(toUpper,isDigit,digitToInt)

main  = playChess initialState
main2 = playChess drawByStaleMateState 
main3 = playChess checkMateState
main4 = playChess pawnPromotionState

-- Bucle del juego
playChess :: CurrentGameState -> IO()
playChess gs@(CurrentGameState _ playerColour _) = do
    putStrLn $ boardRender gs
    putStrLn "\nSi desea saber los posibles movimientos de alguna pieza introduzca la casilla la cual se encuentra la pieza [ln]"
    putStrLn "Si desea hacer un movimiento indica la casilla de partida y la casilla final que desea modificar [lnln]"
    putStrLn "Si desea deshacer el ultimo movimiento pulse ENTER"
    putStrLn "Si desea salir teclee [e]"
    putStr   ("Turno de " ++ show playerColour ++ ": ")
    input1 <- getLine
    if (checkFormat input1)
    then do
        if (length input1 == 2)
        then do
            clearScreen
            let possibleMovesString = (renderPossibleMoves gs (fst $ convertFromChessNotation input1))
            putStrLn $ "\nMovimientos posibles de " ++ input1 ++ ": " ++ unwords (map convertIntoChessNotation possibleMovesString)
            playChess gs
        else do
            let (originSq, targetSq) = convertFromChessNotation input1
            if (isValidMovement gs originSq targetSq)
            then do
                let nxtGS = makeMove gs originSq targetSq
                if (playerInCheckMate nxtGS || drawByStaleMate nxtGS)
                then do
                    putStrLn "\nEND OF THE GAME!"
                else do
                    if (isPawnPromoting nxtGS)
                    then do
                        putStrLn "\nElija la pieza por la cual desea cambiar el peón: Queen [1], Rook [2], Bishop [3], Knight [4]"
                        inputPP <- getLine
                        if (checkFormatPromotion inputPP)
                        then do
                            clearScreen
                            playChess $  promotePawn nxtGS (convertIntoDigit inputPP)
                        else do
                            clearScreen
                            putStrLn "\nNO VALID FORMAT!"
                            playChess gs
                    else do
                        clearScreen
                        playChess nxtGS
            else do
                clearScreen
                putStrLn "\nNO VALID MOVE!"
                playChess gs
    else do
        if (input1 == "")
        then do
            clearScreen
            playChess (undoLastMove gs)
        else do
            if ((toUpper . head) input1 == 'E')
            then do
                putStrLn "\nEXIT"
            else do
                clearScreen
                putStrLn "\nNO VALID FORMAT!"
                playChess gs

-- Comprueba si el formato es correcto. Como hay dos formatos posibles comprueba los dos casos
-- Estrictamente tiene que introducirse [ln] o [lnln], sin ningun caracter mas y con posibilidad de minuscula y mayuscula
checkFormat :: String -> Bool
checkFormat (l1:n1:l2:n2:[]) = checkFormat (l1:n1:[]) && checkFormat (l2:n2:[])
checkFormat (l:n:[]) = elem (toUpper l) ['A'..'H'] && isDigit n && n /= '0' && n /= '9'
checkFormat _        = False

-- Comprueba el formato para cuando se tiene que elgir que pieza intercambiar por el peon coronado
checkFormatPromotion :: String -> Bool
checkFormatPromotion (n:[]) = elem n ['1'..'4']

-- Transforma el input del usuario en una casilla del tablero
convertFromChessNotation :: String -> (Square, Square)
convertFromChessNotation [o1,o2]       = ((lett2Numb o1, digitToInt o2),(0,0))
    where
        lett2Numb l = fromEnum l - 96
convertFromChessNotation [o1,o2,t1,t2] = ((lett2Numb o1, digitToInt o2),(lett2Numb t1, digitToInt t2))
    where
        lett2Numb l = fromEnum l - 96 -- 96 == fromEnum 'a' + 1

-- Transforma una casilla del tablero en un output que el usuario pueda interpretar
convertIntoChessNotation :: Square -> String
convertIntoChessNotation (o1,o2) = (toEnum (o1+96) :: Char):show o2

-- Convierte a digito la cadena del usuario 
convertIntoDigit :: String -> Int
convertIntoDigit (n:[]) = digitToInt n

-- Limpia la pantalla para que no se acumulen muchas partidas
-- Es una funcion que funcion en distintos so. (Función copiada de internet)
clearScreen :: IO ()
clearScreen = callCommand (if os == "mingw32" then "cls" else "clear")