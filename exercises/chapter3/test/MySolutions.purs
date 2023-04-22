module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.Maybe (Maybe, Maybe(Nothing))
import Data.List (filter, head, nubByEq, null)

-- Note to reader: Add your solutions to this file
findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = filter byStreet >>> head
  where
    byStreet :: Entry -> Boolean
    byStreet entry = entry.address.street == street

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter (_.address.street >>> (==) street) >>> head

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fst last = not <<< null <<< filter byName
  where
    byName e = e.firstName == fst && e.lastName == last

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq myEq
  where
    myEq a b = a.firstName == b.firstName && a.lastName == b.lastName