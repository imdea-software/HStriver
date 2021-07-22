module Engine.Pointer (pull, unhookPointer) where
import Control.Lens
import Data.Sequence (Seq(..))

import Engine.Table
import Data.Maybe

pull :: PointerIndex -> Stateful Event
pull pindex@(streamid, ix) = do
  pointerdeleted <- use $ dyn.pointers.(at streamid).(to (fromMaybe $ error "No pointer")).(at ix).(to isNothing)
  if pointerdeleted then
    return $ posOutside -- quickreturn
  else do
    reentrant <- ensureHead pindex
    if reentrant then
      return NegOutside
    else do
      (hd :<| rest) <- use $ dyn.pointers.(at streamid)._Just.(at ix)._Just
      if isPosOutside hd then
        unhookPointer pindex -- quickreturn next time
      else
        dyn.pointers.(at streamid)._Just.(at ix) .= Just rest -- consumption
      return hd

-- Private
ensureHead :: PointerIndex -> Stateful Bool
ensureHead (streamid, ix) = do
  hasHead <- use $ dyn.pointers.(at streamid).(to (fromMaybe $ error "No pointer")).(at ix).(to (fromMaybe $ error "No pointer")).(to (not.null))
  if hasHead then
    return False -- it already has something
  else
    getNext streamid -- something will be added to it, or the result of reentry will be true

unhookPointer :: PointerIndex -> Stateful ()
unhookPointer (streamid,ix) = dyn.pointers.(at streamid)._Just.(at ix) .= Nothing
