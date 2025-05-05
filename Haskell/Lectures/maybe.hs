-- Define a type alias for clarity
type PhoneBook = [(String, String)]

-- A sample phone book
phoneBook :: PhoneBook
phoneBook = [("Alice", "1234"), ("Bob", "5678")]

-- Lookup a number using Maybe
findNumber :: String -> PhoneBook -> Maybe String
findNumber name book = lookup name book

-- Use Maybe to provide a fallback
getNumberOrDefault :: String -> PhoneBook -> String
getNumberOrDefault name book =
    case findNumber name book of
        Just number -> number
        Nothing     -> "Not found"

-- Main function to test it
main :: IO ()
main = do
    print (getNumberOrDefault "Alice" phoneBook)  -- prints "1234"
    print (getNumberOrDefault "Charlie" phoneBook)  -- prints "Not found"
