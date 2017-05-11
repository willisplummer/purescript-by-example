module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                    addr.city <> ", " <>
                    addr.state

emptyBook :: AddressBook
emptyBook = empty

address :: Address
address = { street: "123 Fake St.", city: "Faketown", state: "CA" }

entry :: Entry
entry = { firstName: "John", lastName: "Smith", address: address }

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findEntry street city state = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street
                      && entry.address.state == state
                      && entry.address.city == city

entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName = not null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy equal where
  equal :: Entry -> Entry -> Boolean
  equal a b = a.firstName == b.firstName 
            && a.lastName == b.lastName
