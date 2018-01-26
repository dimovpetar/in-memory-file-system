import Data.List (break, intercalate)
import Data.List.Split (splitOn)


data File       = File Name Content | Directory Name [File]  deriving (Show, Eq)
data RestFiles  = RestFiles Name [File] [File] deriving (Show)
type Node       = (File, [RestFiles])
type Name       = String
type Content    = String

fileSystem :: File
fileSystem = Directory "/" 
    [
        File "file1" "Hello world",
        File "file2" "some content",
        Directory "images" 
            [
                File "pic1.jpg" "dark",
                File "pic2.jpg" "blue",
                Directory "summer" 
                [
                    File "beach" "sea"
                ]
            ]

    ]


traverseUp :: Node -> Node
traverseUp (file, []) = (file, [])
traverseUp (file, (RestFiles parentName lsiblings rsiblings):t) 
         = (Directory parentName (lsiblings ++ [file] ++ rsiblings), t)


traverseDown :: Name -> Node -> Node
traverseDown _    (File fileName content, restFiles)      = ((File fileName content), restFiles)
traverseDown name (Directory parentName files, restFiles) = (file, (RestFiles parentName lsiblings rsiblings):restFiles)
    where (lsiblings, file:rsiblings) = break (checkName name) files
         
checkName :: Name -> File -> Bool
checkName name (Directory dirname _) = name == dirname
checkName name (File filename _)     = name == filename


pwd :: Node -> Name
pwd (Directory dirName _, [])               = dirName
pwd (Directory dirName _, [_])              =   "/" ++  dirName
pwd (File     fileName _, [_])              =   "/" ++ fileName
pwd (Directory dirName content, restFiles ) = pwd (traverseUp (Directory dirName content, restFiles)) ++ "/" ++ dirName
pwd (File fileName content,     restFiles ) = pwd (traverseUp (File fileName content, restFiles)) ++ "/" ++ fileName


cd :: Name -> Node -> Node
cd path dir = moveTo path dir

moveTo :: Name -> Node -> Node 
moveTo path dir = 
    if isAbsolute path
        then helper (splitPath (tail path)) (fileSystem, [])
        else helper (splitPath path) dir
    where helper p currDir
            | length p == 0    = currDir
            | head p     == ".."  = helper (tail p) (traverseUp currDir)
            | otherwise         = helper (tail p) (traverseDown (head p) currDir)

splitPath :: Name -> [Name]
splitPath path = splitOn "/" path

isAbsolute :: Name -> Bool
isAbsolute (prefix:path) = prefix == '/'

getFile :: Name -> Node -> File
getFile name dir = file
        where (file,restFiles) = moveTo name dir

getContent :: Name -> Node -> String
getContent fileName dir = content
            where (File name content) = getFile fileName dir

ls :: Name -> Node -> [String]
ls []   dir    = listFiles dir
ls path dir    = listFiles (cd path dir)

listFiles (Directory dirName files, _) = map fileName files

fileName :: File -> Name
fileName (Directory dirName _) = dirName
fileName (File     fileName _) = fileName

rm :: [Name] -> Node -> Node
rm files (Directory dirName oldFiles, restFiles) = (Directory dirName newFiles, restFiles)
        where newFiles = (filter (\x -> not ((fileName x) `elem` files)) oldFiles)

parseInput :: String -> (String, [String])
parseInput input = (command,args)
    where (command:args) = splitOn " " input

parseArgs :: [String] -> String
parseArgs []    = []
parseArgs l     = head l

replaceFile :: File -> Node -> Node
replaceFile (File name content) dir
    = addFile (File name content) (rm [name] dir)

fileExists :: File -> [File] -> Bool
fileExists file [] = False
fileExists (File name content) ((Directory _ _):files) = fileExists (File name content) files
fileExists (File name content) ((File fileName _):files)
            = if name == fileName
                then True 
                else fileExists (File name content) files


addFile :: File -> Node -> Node
addFile file (Directory parentName files, restFiles) 
        | not (fileExists file files)   = (Directory parentName newFiles,restFiles)
        | otherwise                     = replaceFile file (Directory parentName newFiles,restFiles)
        where newFiles = files ++ [file]

-- cat file1 file2 .. fileN > file
cat :: [String] -> Node -> Node
cat files  dir = addFile newFile dir
    where 
        newFile = (File outputFile catContent)
        catContent = foldl (++) "" (map (\x -> getContent x dir) items)
        (items, s:last) = break (\x -> x == ">") files
        outputFile = head last
                
                
cli currDir = 
    do 
    input <- getLine 
    helper (parseInput input)
    where helper (command, args)
            | command == "pwd"                   = callback $ pwd currDir
            | command == "ls"                    = callback $ intercalate " " (ls (parseArgs args) currDir)
            | command == "cd"                    = cli (cd (parseArgs args) currDir)
            | command == "rm"                    = cli (rm args currDir)
            | command == "cat" && noOutputFile   = callback (foldl (++) "" (map (\x -> getContent x currDir) args))
            | command == "cat" && noInputFile    = fillFile []
            | otherwise                          = cli (cat args currDir)
            where 
            noOutputFile = ">" `notElem` args
            noInputFile  = ">" == head args
            outputFile   = head $ tail args
            callback message =  do 
                                putStrLn message
                                cli  currDir      
            fillFile buffer  =  do 
                                input <- getLine
                                if input == "."
                                    then cli (addFile (File outputFile buffer) currDir)
                                    else fillFile (buffer ++ input ++ "\n")
               
main = cli (fileSystem, [])
