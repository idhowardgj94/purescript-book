module Test.MySolutions where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (head, filter, null, find, nubBy)
import Data.AddressBook (AddressBook, Entry, emptyBook, findEntry)
-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet addr = head <<< filter filterEntry
    where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == addr

-- Data.List.null :: forall a. List a -> Boolean
isInBook :: String -> String ->  AddressBook -> Boolean
isInBook fn ln b = (find findEntry b) /= Nothing
    where 
    findEntry :: Entry -> Boolean
    findEntry it = it.lastName == ln && it.firstName == fn

-- Data.List.nubBy forall a. (a -> a -> Boolean) -> List a -> List a
removeDuplicates ::  AddressBook -> AddressBook
removeDuplicates = nubBy removePredicate 
    where
    removePredicate p c = p.firstName == c.firstName && p.lastName == c.lastName  
