module ListUtils where
import Graphics.Vty.Widgets.All
import Control.Applicative ((<$>))
import Control.Monad (liftM, void)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (find)

-- | List helper
-- Replaces an item in a Vty Widget List at a given position.  If the index is
-- `Nothing`, the item will be added at the end of the list.  Otherwise it's
-- placed at the index.
replaceAtIndex :: Show b => Widget (List a b) -- ^ the list
                  -> a         -- ^ the backing item in the list
                  -> Widget b  -- ^ the widget representing the item in the list.
                  -> Maybe Int -- ^ the index or nothing, if adding to the end.
                  -> IO ()
replaceAtIndex l i w Nothing = addToList l i w
replaceAtIndex l i w (Just idx) = do
  -- get the last selected index
  lastIdx <- getSelected l
  -- insert the new item
  insertIntoList l i w idx
  -- remove the "old" item (now one positon below)
  removeFromList l (succ idx)
  -- reselect the last selected item if a selection was given.
  case lastIdx of Just (p, _) -> setSelected l p
  return ()

-- Given a List Widget, find the index of a given Item.
indexOfItem :: (a -> Bool) -- ^ compare function for the backing items.
               -> Widget (List a b) -- ^ the list
               -> IO (Maybe Int) -- ^ returns the index or nothing if not found.
indexOfItem cmp lst = do
  -- get the listsize ...
  len <- getListSize lst
  -- ... to get all items.
  itms <- mapM (getListItem lst) [0..len-1]
  -- index all "just" items, and find the first item that matches with the `cmp`.
  -- this is technically not correct, as we are potentially skipping "Nothing"
  -- and therfore compute the wrong index.
  let mtch = liftM snd $ find (cmp . fst . fst) $ zip (catMaybes itms) [0..]
  return mtch

-- | Removes an item (idx, item) from a list
dropItem :: Widget (List a b) -- ^ the list
            -> Maybe (Int, b1) -- ^ the (idx, item) pair.
            -> IO ()
dropItem l e = fromMaybe (return ()) $ (void . removeFromList l . fst) <$> e

-- | Removes the selected item from a list
dropSelected :: Widget (List a b) -- ^ the list
                -> IO ()
dropSelected l = getSelected l >>= dropItem l
