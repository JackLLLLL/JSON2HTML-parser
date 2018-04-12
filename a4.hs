import System.Environment

-- Main function for Json parser, take name of Json file and name of putput file as input, otherwise use built-in file names
main = do 
    arglist <- getArgs
    let input = ((get_file_name arglist)!!0)
    let output = ((get_file_name arglist)!!1)
    content <- readFile input
    let tokens = parser content
    let html = "<span style=\"font-family:monospace; white-space:pre\">\n" ++ colorize tokens ++ "</span>"
    writeFile output html


-- Get file names from argument list depending on length
get_file_name :: [String] -> [String]
get_file_name arglist | (length arglist) == 0 = ["input.json", "output.html"]
                      | (length arglist) == 1 = [(arglist!!0), "output.html"]
                      | (length arglist) == 2 = [(arglist!!0), (arglist!!1)]
                      | otherwise = error "incorrect input format, arguments should be less than 3"

 
-- Parser for Json and return a html format result
parser :: String -> [String]
parser content = get_tokens content


-- Split the content and get a list of tokens
get_tokens :: String -> [String]
get_tokens [] = []
get_tokens ('"': xs) = ["&quot;" ++ (quot_convert (take (next_quot xs) xs))] ++ (get_tokens (drop (next_quot xs) xs))
get_tokens ('t': xs) = ["true"] ++ (get_tokens (drop 3 xs))
get_tokens ('f': xs) = ["false"] ++ (get_tokens (drop 4 xs))
get_tokens ('n': xs) = ["null"] ++ (get_tokens (drop 3 xs))
get_tokens (x: xs) | isNum x = [x:get_num xs] ++ (get_tokens (drop (num_len xs) xs))
                   | otherwise = [[x]] ++ (get_tokens xs)


-- Find next quot and return position of next quot
next_quot :: String -> Int
next_quot (x:xs) | x == '\\' && head xs == '"' = 2 + next_quot(tail xs)
                 | x == '"' = 1
                 | otherwise = 1 + next_quot(xs)


-- Conversion between quots
quot_convert :: String -> String
quot_convert ['"'] = "&quot;"
quot_convert (x:xs) | x == '\\' = after_escape xs
                    | x == '>' = "&gt;" ++ (quot_convert xs)
                    | x == '<' = "&lt;" ++ (quot_convert xs)
                    | x == '&' = "&amp;" ++ (quot_convert xs)
                    | x == '>' = "&apos;" ++ (quot_convert xs)
                    | otherwise = x:quot_convert xs


-- Conversion after escape
after_escape :: String -> String
after_escape (x:xs) | x == '"' = "<span style=\"color:Gray\">&quot;</span>" ++ (quot_convert xs)
                    | otherwise = "<span style=\"color:Gray\">\\" ++ [x] ++ (until_space xs)


-- Escape characters until space
until_space :: String -> String
until_space ['"'] = "</span><span style=\"color:MediumSeaGreen\">&quot;"
until_space (x:xs) | x == ' ' = " </span>" ++ (quot_convert xs)
                   | otherwise = x:(until_space xs)


-- Check whether is the head of number
isNum :: Char -> Bool
isNum x | x == '-' = True
        | x == '.' = True
        | x == 'e' = True
        | x == 'E' = True
        | x == '0' = True
        | x == '1' = True
        | x == '2' = True
        | x == '3' = True
        | x == '4' = True
        | x == '5' = True
        | x == '6' = True
        | x == '7' = True
        | x == '8' = True
        | x == '9' = True
        | otherwise = False


-- Get a numeric token
get_num :: String -> String
get_num (x:xs) | isNum x && isNum (head xs) = x:get_num xs
               | otherwise = [x]


-- When numeric token reaches terminals
num_len :: String -> Int
num_len (x:xs) | isNum x = 1 + num_len xs
               | otherwise = 0


-- Colorize tokens in html format
colorize :: [String] -> String
colorize [s] = choose_color s
colorize tokens = choose_color (head tokens) ++ (colorize (drop 1 tokens))


-- Choose color for every token
choose_color :: String -> String
choose_color s | head s == '{' = "<span style=\"color:red\">\n" ++ s ++ "\n</span>"
               | head s == '}' = "<span style=\"color:red\">\n" ++ s ++ "</span>"
               | head s == '[' = "<span style=\"color:DodgerBlue\">" ++ s ++ "</span>"
               | head s == ']' = "<span style=\"color:DodgerBlue\">" ++ s ++ "</span>"
               | head s == ':' = "<span style=\"color:Tomato\">" ++ s ++ "</span>"
               | head s == ',' = "<span style=\"color:Violet\">" ++ s ++ "</span>"
               | head s == '&' = "<span style=\"color:MediumSeaGreen\">\t" ++ s ++ "</span>"
               | head s == 't' = "<span style=\"color:SlateBlue\">" ++ s ++ "</span>"
               | head s == 'f' = "<span style=\"color:SlateBlue\">" ++ s ++ "</span>"
               | head s == 'n' = "<span style=\"color:SlateBlue\">" ++ s ++ "</span>"
               | otherwise = "<span style=\"color:Orange\">" ++ s ++ "</span>"