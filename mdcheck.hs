import Text.Regex.PCRE
import Debug.HTrace

red = "\x1b[1;31m"
black = "\x1b[0m"

warnings :: [(String -> Bool, String)]
warnings = [ (\md -> (md =~ "^\\S\\S\\s*#{2,4}" :: Bool) && (take 2 md /= "\\n"), "Heading missing preceding newline character")
         , (\md -> (md =~ "^\\S\\S\\s*\\[#" :: Bool) && (take 2 md /= "\\n"), "Embed missing preceding newline character")
         ]

main = do
  markdown <- getContents
  putStrLn "Beginning Checks:"
  putStrLn "-----------------\n"
  mapM putStrLn $ checkMarkdown markdown 0

checkMarkdown :: String -> Int -> [String]
checkMarkdown [] i = ["Done!"]
checkMarkdown md i =
  let errs = getErrs warnings md in
    if null errs
      then checkMarkdown (tail md) (i + 1)
      else ([red ++ take 30 md ++ black] ++ [errs] ++ ["At position " ++ show i ++ "\n"]) ++ checkMarkdown (tail md) (i + 1)

getErrs :: [(String -> Bool, String)] -> String -> String
getErrs warnings md =
  foldl (\acc (f,s) -> if f md then acc ++ s else acc) [] warnings

headingWithoutPrecedingNewline :: String -> Bool
headingWithoutPrecedingNewline md = (md =~ "^\\S\\S\\s*#{2,4}" :: Bool) && (take 2 md /= "\\n")

embedWithoutPrecedingNewline :: String -> Bool
embedWithoutPrecedingNewline md = (md =~ "^\\S\\S\\s*\\[#" :: Bool) && (take 2 md /= "\\n")
