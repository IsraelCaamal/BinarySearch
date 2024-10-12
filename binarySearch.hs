-- binarySearch.hs

import Data.Maybe (fromMaybe)

-- Función de búsqueda binaria
binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch list x = binarySearch' list x 0 (length list - 1)

-- Implementación de búsqueda binaria
binarySearch' :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
binarySearch' list x low high
  | low > high = Nothing  -- No se encontró el elemento
  | otherwise  = 
      let mid = (low + high) `div` 2
      in if list !! mid == x
            then Just mid  -- Se encontró el elemento
            else if list !! mid > x
                    then binarySearch' list x low (mid - 1)
                    else binarySearch' list x (mid + 1) high

-- Función para insertar un elemento en la lista manteniéndola ordenada
insertInOrder :: Ord a => [a] -> a -> [a]
insertInOrder [] x = [x]
insertInOrder (y:ys) x
  | x <= y    = x : y : ys
  | otherwise = y : insertInOrder ys x

-- Función principal para interactuar con el usuario
main :: IO ()
main = do
    -- Inicializamos una lista vacía
    let emptyList = []
    
    -- Llamamos a la función de inserción de datos por el usuario
    putStrLn "Ingrese los números para la lista (si le da espacio, quedara vacio y terminar con las lista):"
    list <- insertNumbers emptyList
    
    -- Mostramos la lista final ordenada
    putStrLn "Lista ordenada:"
    print list

    -- Solicitar número para buscar
    searchNumberLoop list

-- Esta función es para solicitar un número y realizar la búsqueda
searchNumberLoop :: [Int] -> IO ()
searchNumberLoop list = do
    putStrLn "Digite el número que desea buscar:"
    searchNumber <- getLine
    if null searchNumber then do
        putStrLn "Digite un número." 
        searchNumberLoop list  -- Repetir si no se ingresó nada
    else do
        let num = read searchNumber :: Int
        -- Realizamos la búsqueda binaria
        let result = binarySearch list num
        
        -- Mostramos el resultado de la búsqueda
        case result of
            Just index -> putStrLn $ "Resultado de la búsqueda: " ++ show index
            Nothing    -> do
                putStrLn "No se encontro el numero, por favor intentelo nuevavemente."
                searchNumberLoop list  -- hay que volver a pedir el número

-- Esta función es para insertar números ingresados por el usuario y mantener la lista ordenada
insertNumbers :: [Int] -> IO [Int]
insertNumbers list = do
    number <- getLine
    if number == "" 
    then return list  -- Si el usuario no ingresa más números, devolvemos la lista final
    else do
        let num = read number :: Int
        let newList = insertInOrder list num  -- Insertamos el número manteniendo la lista ordenada
        insertNumbers newList  -- Repetimos hasta que el usuario termine
