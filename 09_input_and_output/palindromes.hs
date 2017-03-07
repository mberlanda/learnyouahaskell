-- file: 09_input_and_output/palindromes.hs

main = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs