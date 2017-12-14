import           Data.List     (find)
import qualified Data.Map.Lazy as Map'
import           Data.Maybe    (isNothing)


insertIntoTree :: Map'.Map String (String, Maybe String) -> [String] -> Map'.Map String (String, Maybe String)
insertIntoTree tree [name, weight] =
    if Map'.member name tree
    then
        Map'.adjust (\(_, parent) -> (weight, parent)) name tree
    else
        Map'.insert name (weight, Nothing) tree
insertIntoTree tree (name : weight : _ : children') =
    let
        nextTree = insertIntoTree tree [name, weight]
        children = map (takeWhile (',' /=)) children'

        f m child =
            if Map'.member child m
            then
                Map'.adjust (\(w, _) -> (w, Just name)) child m
            else
                Map'.insert child ("" :: String, Just name) m
    in
        foldl f nextTree children
insertIntoTree map _ = map


constructTree :: FilePath -> IO (Map'.Map String (String, Maybe String))
constructTree path =
    (foldl insertIntoTree Map'.empty . map words . lines) <$> readFile path


solve :: Map'.Map String (String, Maybe String) -> Maybe String
solve tree =
    fmap fst $ find (isNothing . snd . snd) $ Map'.assocs tree
