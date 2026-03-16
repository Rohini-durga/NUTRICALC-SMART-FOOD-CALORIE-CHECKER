import Data.Char (toLower)
import Text.Read (readMaybe)

type FoodItem = (String, Double, String)

foodDatabase :: [FoodItem]
foodDatabase =
    [ ("idli", 58, "Breakfast")
    , ("dosa", 168, "Breakfast")
    , ("plain dosa", 168, "Breakfast")
    , ("masala dosa", 250, "Breakfast")
    , ("pongal", 180, "Breakfast")
    , ("upma", 190, "Breakfast")
    , ("poori", 110, "Breakfast")
    , ("chapati", 120, "Main Meal")
    , ("parotta", 260, "Main Meal")
    , ("rice", 130, "Main Meal")
    , ("curd rice", 160, "Main Meal")
    , ("sambar rice", 180, "Main Meal")
    , ("lemon rice", 190, "Main Meal")
    , ("tomato rice", 185, "Main Meal")
    , ("biryani", 290, "Main Meal")
    , ("fried rice", 210, "Main Meal")
    , ("noodles", 220, "Main Meal")
    , ("paneer butter masala", 260, "Side Dish")
    , ("dal", 140, "Side Dish")
    , ("sambar", 80, "Side Dish")
    , ("rasam", 45, "Side Dish")
    , ("curd", 98, "Side Dish")
    , ("milk", 60, "Drink")
    , ("tea", 40, "Drink")
    , ("coffee", 45, "Drink")
    , ("banana", 89, "Fruit")
    , ("apple", 52, "Fruit")
    , ("orange", 47, "Fruit")
    , ("grapes", 69, "Fruit")
    , ("mango", 60, "Fruit")
    , ("watermelon", 30, "Fruit")
    , ("pineapple", 50, "Fruit")
    , ("egg", 78, "Protein")
    , ("boiled egg", 78, "Protein")
    , ("omelette", 154, "Protein")
    , ("chicken curry", 240, "Protein")
    , ("grilled chicken", 165, "Protein")
    , ("fish fry", 220, "Protein")
    , ("paneer", 265, "Protein")
    , ("burger", 295, "Fast Food")
    , ("pizza", 266, "Fast Food")
    , ("sandwich", 180, "Fast Food")
    , ("french fries", 312, "Fast Food")
    , ("samosa", 140, "Snacks")
    , ("puffs", 210, "Snacks")
    , ("vada", 97, "Snacks")
    , ("bajji", 150, "Snacks")
    , ("chips", 152, "Snacks")
    , ("biscuits", 50, "Snacks")
    , ("cake", 257, "Dessert")
    , ("ice cream", 207, "Dessert")
    , ("chocolate", 208, "Dessert")
    , ("laddu", 180, "Dessert")
    , ("jalebi", 150, "Dessert")
    ]

normalize :: String -> String
normalize = map toLower

containsIgnoreCase :: String -> String -> Bool
containsIgnoreCase small big =
    normalize small `isSubstrOf` normalize big

isSubstrOf :: String -> String -> Bool
isSubstrOf [] _ = True
isSubstrOf _ [] = False
isSubstrOf xs ys
    | prefix xs ys = True
    | otherwise = isSubstrOf xs (tail ys)
  where
    prefix [] _ = True
    prefix _ [] = False
    prefix (a:as) (b:bs) = a == b && prefix as bs

findExactFood :: String -> [FoodItem] -> Maybe FoodItem
findExactFood name db =
    case filter (\(n, _, _) -> normalize n == normalize name) db of
        (x:_) -> Just x
        []    -> Nothing

findSimilarFoods :: String -> [FoodItem] -> [FoodItem]
findSimilarFoods name db =
    filter (\(n, _, _) -> containsIgnoreCase name n) db

getCaloriesMessage :: Double -> String
getCaloriesMessage total
    | total < 100 = "Very low calorie food."
    | total < 250 = "Low to moderate calorie food."
    | total < 500 = "Moderate calorie food."
    | total < 800 = "High calorie food."
    | otherwise   = "Very high calorie intake."

showFoodItem :: FoodItem -> IO ()
showFoodItem (name, cal, category) = do
    putStrLn ("Food Name         : " ++ name)
    putStrLn ("Calories per unit : " ++ show cal ++ " kcal")
    putStrLn ("Category          : " ++ category)

checkFoodCalories :: [FoodItem] -> IO ()
checkFoodCalories db = do
    putStrLn "\nEnter food name:"
    foodName <- getLine

    case findExactFood foodName db of
        Just item@(_, cal, _) -> do
            putStrLn "\nFood found!"
            showFoodItem item
            putStrLn "\nEnter quantity:"
            qtyInput <- getLine
            case readMaybe qtyInput :: Maybe Double of
                Nothing -> putStrLn "Invalid quantity. Please enter a number like 1 or 2."
                Just qty ->
                    if qty <= 0
                    then putStrLn "Quantity must be greater than 0."
                    else do
                        let total = cal * qty
                        putStrLn ("Total Calories    : " ++ show total ++ " kcal")
                        putStrLn ("Health Suggestion : " ++ getCaloriesMessage total)

        Nothing -> do
            let suggestions = findSimilarFoods foodName db
            if null suggestions
            then putStrLn "\nFood not found in database."
            else do
                putStrLn "\nFood not found exactly. Similar foods available:"
                mapM_ (\(n, c, cat) ->
                    putStrLn ("- " ++ n ++ " | " ++ show c ++ " kcal | " ++ cat)) suggestions

displayFoods :: [FoodItem] -> IO ()
displayFoods db = do
    putStrLn "\nAvailable Food Items:"
    mapM_ (\(name, cal, category) ->
        putStrLn (name ++ " - " ++ show cal ++ " kcal - " ++ category)) db

addFoodItem :: [FoodItem] -> IO [FoodItem]
addFoodItem db = do
    putStrLn "\nEnter new food name:"
    name <- getLine

    putStrLn "Enter calories per unit:"
    calInput <- getLine

    putStrLn "Enter category:"
    category <- getLine

    case readMaybe calInput :: Maybe Double of
        Nothing -> do
            putStrLn "Invalid calorie value. Food not added."
            return db
        Just cal ->
            if cal <= 0
            then do
                putStrLn "Calories must be greater than 0. Food not added."
                return db
            else do
                let newItem = (name, cal, category)
                putStrLn "Food item added successfully."
                return (db ++ [newItem])

menuLoop :: [FoodItem] -> IO ()
menuLoop db = do
    putStrLn "\n====== NutriCalc Menu ======"
    putStrLn "1. Check food calories"
    putStrLn "2. View all available foods"
    putStrLn "3. Add new food item"
    putStrLn "4. Exit"
    putStrLn "Enter your choice:"
    choice <- getLine

    case choice of
        "1" -> do
            checkFoodCalories db
            menuLoop db
        "2" -> do
            displayFoods db
            menuLoop db
        "3" -> do
            newDb <- addFoodItem db
            menuLoop newDb
        "4" -> putStrLn "Exiting NutriCalc..."
        _ -> do
            putStrLn "Invalid choice. Enter 1, 2, 3, or 4."
            menuLoop db

main :: IO ()
main = do
    putStrLn "Welcome to NutriCalc - Food Calorie Checker"
    menuLoop foodDatabase