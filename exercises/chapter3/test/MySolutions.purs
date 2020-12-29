module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, nubBy, null)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet s = head <<< filter filterStreet
  where
    filterStreet :: Entry -> Boolean
    filterStreet e = e.address.street == s

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fName lName = not <<< null <<< filter filterName
  where
    filterName :: Entry -> Boolean
    filterName e = e.firstName == fName && e.lastName == lName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy eqName
  where
    eqName :: Entry -> Entry -> Boolean
    eqName e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
