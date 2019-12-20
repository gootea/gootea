{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Messages where
import qualified Capnp.Message as Message
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Basics as Basics
import qualified Capnp.GenHelpers as GenHelpers
import qualified Capnp.Classes as Classes
import qualified GHC.Generics as Generics
import qualified Capnp.Bits as Std_
import qualified Data.Maybe as Std_
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
newtype Message msg
    = Message'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Message) where
    tMsg f (Message'newtype_ s) = (Message'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Message msg)) where
    fromStruct struct = (Std_.pure (Message'newtype_ struct))
instance (Classes.ToStruct msg (Message msg)) where
    toStruct (Message'newtype_ struct) = struct
instance (Untyped.HasMessage (Message msg)) where
    type InMessage (Message msg) = msg
    message (Message'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Message msg)) where
    messageDefault msg = (Message'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Message msg)) where
    fromPtr msg ptr = (Message'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Message (Message.MutMsg s))) where
    toPtr msg (Message'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Message (Message.MutMsg s))) where
    new msg = (Message'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem msg (Message msg)) where
    newtype List msg (Message msg)
        = Message'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Message'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Message'List_ l) = (Untyped.ListStruct l)
    length (Message'List_ l) = (Untyped.length l)
    index i (Message'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Message (Message.MutMsg s))) where
    setIndex (Message'newtype_ elt) i (Message'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Message'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Message'message :: ((Untyped.ReadCtx m msg)) => (Message msg) -> (m (Message'message msg))
get_Message'message (Message'newtype_ struct) = (Classes.fromStruct struct)
newtype Message'message msg
    = Message'message'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Message'message) where
    tMsg f (Message'message'newtype_ s) = (Message'message'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Message'message msg)) where
    fromStruct struct = (Std_.pure (Message'message'newtype_ struct))
instance (Classes.ToStruct msg (Message'message msg)) where
    toStruct (Message'message'newtype_ struct) = struct
instance (Untyped.HasMessage (Message'message msg)) where
    type InMessage (Message'message msg) = msg
    message (Message'message'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Message'message msg)) where
    messageDefault msg = (Message'message'newtype_ (Untyped.messageDefault msg))
data Message'message' msg
    = Message'message'fingerTableQuery 
    | Message'message'fingerTableResponse (FingerTableResponse msg)
    | Message'message'getValuesQuery (GetValuesQuery msg)
    | Message'message'getValuesResponse (GetValuesResponse msg)
    | Message'message'addValueQuery (AddValueQuery msg)
    | Message'message'unknown' Std_.Word16
instance (Classes.FromStruct msg (Message'message' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure Message'message'fingerTableQuery)
            1 ->
                (Message'message'fingerTableResponse <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            2 ->
                (Message'message'getValuesQuery <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            3 ->
                (Message'message'getValuesResponse <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            4 ->
                (Message'message'addValueQuery <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Message'message'unknown' (Std_.fromIntegral tag)))
        )
get_Message'message' :: ((Untyped.ReadCtx m msg)) => (Message'message msg) -> (m (Message'message' msg))
get_Message'message' (Message'message'newtype_ struct) = (Classes.fromStruct struct)
set_Message'message'fingerTableQuery :: ((Untyped.RWCtx m s)) => (Message'message (Message.MutMsg s)) -> (m ())
set_Message'message'fingerTableQuery (Message'message'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Message'message'fingerTableResponse :: ((Untyped.RWCtx m s)) => (Message'message (Message.MutMsg s)) -> (FingerTableResponse (Message.MutMsg s)) -> (m ())
set_Message'message'fingerTableResponse (Message'message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'message'getValuesQuery :: ((Untyped.RWCtx m s)) => (Message'message (Message.MutMsg s)) -> (GetValuesQuery (Message.MutMsg s)) -> (m ())
set_Message'message'getValuesQuery (Message'message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'message'getValuesResponse :: ((Untyped.RWCtx m s)) => (Message'message (Message.MutMsg s)) -> (GetValuesResponse (Message.MutMsg s)) -> (m ())
set_Message'message'getValuesResponse (Message'message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'message'addValueQuery :: ((Untyped.RWCtx m s)) => (Message'message (Message.MutMsg s)) -> (AddValueQuery (Message.MutMsg s)) -> (m ())
set_Message'message'addValueQuery (Message'message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'message'unknown' :: ((Untyped.RWCtx m s)) => (Message'message (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Message'message'unknown' (Message'message'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype FingerTableResponse msg
    = FingerTableResponse'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg FingerTableResponse) where
    tMsg f (FingerTableResponse'newtype_ s) = (FingerTableResponse'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (FingerTableResponse msg)) where
    fromStruct struct = (Std_.pure (FingerTableResponse'newtype_ struct))
instance (Classes.ToStruct msg (FingerTableResponse msg)) where
    toStruct (FingerTableResponse'newtype_ struct) = struct
instance (Untyped.HasMessage (FingerTableResponse msg)) where
    type InMessage (FingerTableResponse msg) = msg
    message (FingerTableResponse'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (FingerTableResponse msg)) where
    messageDefault msg = (FingerTableResponse'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (FingerTableResponse msg)) where
    fromPtr msg ptr = (FingerTableResponse'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (FingerTableResponse (Message.MutMsg s))) where
    toPtr msg (FingerTableResponse'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (FingerTableResponse (Message.MutMsg s))) where
    new msg = (FingerTableResponse'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (FingerTableResponse msg)) where
    newtype List msg (FingerTableResponse msg)
        = FingerTableResponse'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (FingerTableResponse'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (FingerTableResponse'List_ l) = (Untyped.ListStruct l)
    length (FingerTableResponse'List_ l) = (Untyped.length l)
    index i (FingerTableResponse'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (FingerTableResponse (Message.MutMsg s))) where
    setIndex (FingerTableResponse'newtype_ elt) i (FingerTableResponse'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (FingerTableResponse'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_FingerTableResponse'nodes :: ((Untyped.ReadCtx m msg)) => (FingerTableResponse msg) -> (m (Basics.List msg (Node msg)))
get_FingerTableResponse'nodes (FingerTableResponse'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_FingerTableResponse'nodes :: ((Untyped.RWCtx m s)) => (FingerTableResponse (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Node (Message.MutMsg s))) -> (m ())
set_FingerTableResponse'nodes (FingerTableResponse'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_FingerTableResponse'nodes :: ((Untyped.ReadCtx m msg)) => (FingerTableResponse msg) -> (m Std_.Bool)
has_FingerTableResponse'nodes (FingerTableResponse'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_FingerTableResponse'nodes :: ((Untyped.RWCtx m s)) => Std_.Int -> (FingerTableResponse (Message.MutMsg s)) -> (m (Basics.List (Message.MutMsg s) (Node (Message.MutMsg s))))
new_FingerTableResponse'nodes len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_FingerTableResponse'nodes struct result)
    (Std_.pure result)
    )
newtype GetValuesQuery msg
    = GetValuesQuery'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg GetValuesQuery) where
    tMsg f (GetValuesQuery'newtype_ s) = (GetValuesQuery'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (GetValuesQuery msg)) where
    fromStruct struct = (Std_.pure (GetValuesQuery'newtype_ struct))
instance (Classes.ToStruct msg (GetValuesQuery msg)) where
    toStruct (GetValuesQuery'newtype_ struct) = struct
instance (Untyped.HasMessage (GetValuesQuery msg)) where
    type InMessage (GetValuesQuery msg) = msg
    message (GetValuesQuery'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (GetValuesQuery msg)) where
    messageDefault msg = (GetValuesQuery'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (GetValuesQuery msg)) where
    fromPtr msg ptr = (GetValuesQuery'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (GetValuesQuery (Message.MutMsg s))) where
    toPtr msg (GetValuesQuery'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (GetValuesQuery (Message.MutMsg s))) where
    new msg = (GetValuesQuery'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (GetValuesQuery msg)) where
    newtype List msg (GetValuesQuery msg)
        = GetValuesQuery'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (GetValuesQuery'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (GetValuesQuery'List_ l) = (Untyped.ListStruct l)
    length (GetValuesQuery'List_ l) = (Untyped.length l)
    index i (GetValuesQuery'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (GetValuesQuery (Message.MutMsg s))) where
    setIndex (GetValuesQuery'newtype_ elt) i (GetValuesQuery'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (GetValuesQuery'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_GetValuesQuery'key :: ((Untyped.ReadCtx m msg)) => (GetValuesQuery msg) -> (m (ID msg))
get_GetValuesQuery'key (GetValuesQuery'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_GetValuesQuery'key :: ((Untyped.RWCtx m s)) => (GetValuesQuery (Message.MutMsg s)) -> (ID (Message.MutMsg s)) -> (m ())
set_GetValuesQuery'key (GetValuesQuery'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_GetValuesQuery'key :: ((Untyped.ReadCtx m msg)) => (GetValuesQuery msg) -> (m Std_.Bool)
has_GetValuesQuery'key (GetValuesQuery'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_GetValuesQuery'key :: ((Untyped.RWCtx m s)) => (GetValuesQuery (Message.MutMsg s)) -> (m (ID (Message.MutMsg s)))
new_GetValuesQuery'key struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_GetValuesQuery'key struct result)
    (Std_.pure result)
    )
newtype GetValuesResponse msg
    = GetValuesResponse'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg GetValuesResponse) where
    tMsg f (GetValuesResponse'newtype_ s) = (GetValuesResponse'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (GetValuesResponse msg)) where
    fromStruct struct = (Std_.pure (GetValuesResponse'newtype_ struct))
instance (Classes.ToStruct msg (GetValuesResponse msg)) where
    toStruct (GetValuesResponse'newtype_ struct) = struct
instance (Untyped.HasMessage (GetValuesResponse msg)) where
    type InMessage (GetValuesResponse msg) = msg
    message (GetValuesResponse'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (GetValuesResponse msg)) where
    messageDefault msg = (GetValuesResponse'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (GetValuesResponse msg)) where
    fromPtr msg ptr = (GetValuesResponse'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (GetValuesResponse (Message.MutMsg s))) where
    toPtr msg (GetValuesResponse'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (GetValuesResponse (Message.MutMsg s))) where
    new msg = (GetValuesResponse'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem msg (GetValuesResponse msg)) where
    newtype List msg (GetValuesResponse msg)
        = GetValuesResponse'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (GetValuesResponse'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (GetValuesResponse'List_ l) = (Untyped.ListStruct l)
    length (GetValuesResponse'List_ l) = (Untyped.length l)
    index i (GetValuesResponse'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (GetValuesResponse (Message.MutMsg s))) where
    setIndex (GetValuesResponse'newtype_ elt) i (GetValuesResponse'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (GetValuesResponse'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_GetValuesResponse'key :: ((Untyped.ReadCtx m msg)) => (GetValuesResponse msg) -> (m (ID msg))
get_GetValuesResponse'key (GetValuesResponse'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_GetValuesResponse'key :: ((Untyped.RWCtx m s)) => (GetValuesResponse (Message.MutMsg s)) -> (ID (Message.MutMsg s)) -> (m ())
set_GetValuesResponse'key (GetValuesResponse'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_GetValuesResponse'key :: ((Untyped.ReadCtx m msg)) => (GetValuesResponse msg) -> (m Std_.Bool)
has_GetValuesResponse'key (GetValuesResponse'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_GetValuesResponse'key :: ((Untyped.RWCtx m s)) => (GetValuesResponse (Message.MutMsg s)) -> (m (ID (Message.MutMsg s)))
new_GetValuesResponse'key struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_GetValuesResponse'key struct result)
    (Std_.pure result)
    )
get_GetValuesResponse'values :: ((Untyped.ReadCtx m msg)) => (GetValuesResponse msg) -> (m (Basics.List msg (Basics.Data msg)))
get_GetValuesResponse'values (GetValuesResponse'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_GetValuesResponse'values :: ((Untyped.RWCtx m s)) => (GetValuesResponse (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Basics.Data (Message.MutMsg s))) -> (m ())
set_GetValuesResponse'values (GetValuesResponse'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_GetValuesResponse'values :: ((Untyped.ReadCtx m msg)) => (GetValuesResponse msg) -> (m Std_.Bool)
has_GetValuesResponse'values (GetValuesResponse'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_GetValuesResponse'values :: ((Untyped.RWCtx m s)) => Std_.Int -> (GetValuesResponse (Message.MutMsg s)) -> (m (Basics.List (Message.MutMsg s) (Basics.Data (Message.MutMsg s))))
new_GetValuesResponse'values len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_GetValuesResponse'values struct result)
    (Std_.pure result)
    )
newtype AddValueQuery msg
    = AddValueQuery'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg AddValueQuery) where
    tMsg f (AddValueQuery'newtype_ s) = (AddValueQuery'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (AddValueQuery msg)) where
    fromStruct struct = (Std_.pure (AddValueQuery'newtype_ struct))
instance (Classes.ToStruct msg (AddValueQuery msg)) where
    toStruct (AddValueQuery'newtype_ struct) = struct
instance (Untyped.HasMessage (AddValueQuery msg)) where
    type InMessage (AddValueQuery msg) = msg
    message (AddValueQuery'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (AddValueQuery msg)) where
    messageDefault msg = (AddValueQuery'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (AddValueQuery msg)) where
    fromPtr msg ptr = (AddValueQuery'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (AddValueQuery (Message.MutMsg s))) where
    toPtr msg (AddValueQuery'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (AddValueQuery (Message.MutMsg s))) where
    new msg = (AddValueQuery'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem msg (AddValueQuery msg)) where
    newtype List msg (AddValueQuery msg)
        = AddValueQuery'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (AddValueQuery'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (AddValueQuery'List_ l) = (Untyped.ListStruct l)
    length (AddValueQuery'List_ l) = (Untyped.length l)
    index i (AddValueQuery'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (AddValueQuery (Message.MutMsg s))) where
    setIndex (AddValueQuery'newtype_ elt) i (AddValueQuery'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (AddValueQuery'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_AddValueQuery'key :: ((Untyped.ReadCtx m msg)) => (AddValueQuery msg) -> (m (ID msg))
get_AddValueQuery'key (AddValueQuery'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_AddValueQuery'key :: ((Untyped.RWCtx m s)) => (AddValueQuery (Message.MutMsg s)) -> (ID (Message.MutMsg s)) -> (m ())
set_AddValueQuery'key (AddValueQuery'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_AddValueQuery'key :: ((Untyped.ReadCtx m msg)) => (AddValueQuery msg) -> (m Std_.Bool)
has_AddValueQuery'key (AddValueQuery'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_AddValueQuery'key :: ((Untyped.RWCtx m s)) => (AddValueQuery (Message.MutMsg s)) -> (m (ID (Message.MutMsg s)))
new_AddValueQuery'key struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_AddValueQuery'key struct result)
    (Std_.pure result)
    )
get_AddValueQuery'value :: ((Untyped.ReadCtx m msg)) => (AddValueQuery msg) -> (m (Basics.Data msg))
get_AddValueQuery'value (AddValueQuery'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_AddValueQuery'value :: ((Untyped.RWCtx m s)) => (AddValueQuery (Message.MutMsg s)) -> (Basics.Data (Message.MutMsg s)) -> (m ())
set_AddValueQuery'value (AddValueQuery'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_AddValueQuery'value :: ((Untyped.ReadCtx m msg)) => (AddValueQuery msg) -> (m Std_.Bool)
has_AddValueQuery'value (AddValueQuery'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_AddValueQuery'value :: ((Untyped.RWCtx m s)) => Std_.Int -> (AddValueQuery (Message.MutMsg s)) -> (m (Basics.Data (Message.MutMsg s)))
new_AddValueQuery'value len struct = (do
    result <- (Basics.newData (Untyped.message struct) len)
    (set_AddValueQuery'value struct result)
    (Std_.pure result)
    )
newtype Node msg
    = Node'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node) where
    tMsg f (Node'newtype_ s) = (Node'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node msg)) where
    fromStruct struct = (Std_.pure (Node'newtype_ struct))
instance (Classes.ToStruct msg (Node msg)) where
    toStruct (Node'newtype_ struct) = struct
instance (Untyped.HasMessage (Node msg)) where
    type InMessage (Node msg) = msg
    message (Node'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node msg)) where
    messageDefault msg = (Node'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node msg)) where
    fromPtr msg ptr = (Node'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node (Message.MutMsg s))) where
    toPtr msg (Node'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node (Message.MutMsg s))) where
    new msg = (Node'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem msg (Node msg)) where
    newtype List msg (Node msg)
        = Node'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Node'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'List_ l) = (Untyped.ListStruct l)
    length (Node'List_ l) = (Untyped.length l)
    index i (Node'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node (Message.MutMsg s))) where
    setIndex (Node'newtype_ elt) i (Node'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Node'id :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (ID msg))
get_Node'id (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'id :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (ID (Message.MutMsg s)) -> (m ())
set_Node'id (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'id :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'id (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'id :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (m (ID (Message.MutMsg s)))
new_Node'id struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'id struct result)
    (Std_.pure result)
    )
get_Node'address :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (Node'address msg))
get_Node'address (Node'newtype_ struct) = (Classes.fromStruct struct)
get_Node'port :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Int16)
get_Node'port (Node'newtype_ struct) = (GenHelpers.getWordField struct 0 16 0)
set_Node'port :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Int16 -> (m ())
set_Node'port (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
newtype Node'address msg
    = Node'address'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'address) where
    tMsg f (Node'address'newtype_ s) = (Node'address'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'address msg)) where
    fromStruct struct = (Std_.pure (Node'address'newtype_ struct))
instance (Classes.ToStruct msg (Node'address msg)) where
    toStruct (Node'address'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'address msg)) where
    type InMessage (Node'address msg) = msg
    message (Node'address'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'address msg)) where
    messageDefault msg = (Node'address'newtype_ (Untyped.messageDefault msg))
data Node'address' msg
    = Node'address'ipv6Address (IPV6Address msg)
    | Node'address'ipv4Address (IPV4Address msg)
    | Node'address'unknown' Std_.Word16
instance (Classes.FromStruct msg (Node'address' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Node'address'ipv6Address <$> (do
                    ptr <- (Untyped.getPtr 1 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            1 ->
                (Node'address'ipv4Address <$> (do
                    ptr <- (Untyped.getPtr 1 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Node'address'unknown' (Std_.fromIntegral tag)))
        )
get_Node'address' :: ((Untyped.ReadCtx m msg)) => (Node'address msg) -> (m (Node'address' msg))
get_Node'address' (Node'address'newtype_ struct) = (Classes.fromStruct struct)
set_Node'address'ipv6Address :: ((Untyped.RWCtx m s)) => (Node'address (Message.MutMsg s)) -> (IPV6Address (Message.MutMsg s)) -> (m ())
set_Node'address'ipv6Address (Node'address'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 1 struct)
        )
    )
set_Node'address'ipv4Address :: ((Untyped.RWCtx m s)) => (Node'address (Message.MutMsg s)) -> (IPV4Address (Message.MutMsg s)) -> (m ())
set_Node'address'ipv4Address (Node'address'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 1 struct)
        )
    )
set_Node'address'unknown' :: ((Untyped.RWCtx m s)) => (Node'address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Node'address'unknown' (Node'address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype IPV4Address msg
    = IPV4Address'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg IPV4Address) where
    tMsg f (IPV4Address'newtype_ s) = (IPV4Address'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (IPV4Address msg)) where
    fromStruct struct = (Std_.pure (IPV4Address'newtype_ struct))
instance (Classes.ToStruct msg (IPV4Address msg)) where
    toStruct (IPV4Address'newtype_ struct) = struct
instance (Untyped.HasMessage (IPV4Address msg)) where
    type InMessage (IPV4Address msg) = msg
    message (IPV4Address'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (IPV4Address msg)) where
    messageDefault msg = (IPV4Address'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (IPV4Address msg)) where
    fromPtr msg ptr = (IPV4Address'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (IPV4Address (Message.MutMsg s))) where
    toPtr msg (IPV4Address'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (IPV4Address (Message.MutMsg s))) where
    new msg = (IPV4Address'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem msg (IPV4Address msg)) where
    newtype List msg (IPV4Address msg)
        = IPV4Address'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (IPV4Address'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (IPV4Address'List_ l) = (Untyped.ListStruct l)
    length (IPV4Address'List_ l) = (Untyped.length l)
    index i (IPV4Address'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (IPV4Address (Message.MutMsg s))) where
    setIndex (IPV4Address'newtype_ elt) i (IPV4Address'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (IPV4Address'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_IPV4Address'b1 :: ((Untyped.ReadCtx m msg)) => (IPV4Address msg) -> (m Std_.Word8)
get_IPV4Address'b1 (IPV4Address'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_IPV4Address'b1 :: ((Untyped.RWCtx m s)) => (IPV4Address (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_IPV4Address'b1 (IPV4Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 0 0)
get_IPV4Address'b2 :: ((Untyped.ReadCtx m msg)) => (IPV4Address msg) -> (m Std_.Word8)
get_IPV4Address'b2 (IPV4Address'newtype_ struct) = (GenHelpers.getWordField struct 0 8 0)
set_IPV4Address'b2 :: ((Untyped.RWCtx m s)) => (IPV4Address (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_IPV4Address'b2 (IPV4Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 8 0)
get_IPV4Address'b3 :: ((Untyped.ReadCtx m msg)) => (IPV4Address msg) -> (m Std_.Word8)
get_IPV4Address'b3 (IPV4Address'newtype_ struct) = (GenHelpers.getWordField struct 0 16 0)
set_IPV4Address'b3 :: ((Untyped.RWCtx m s)) => (IPV4Address (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_IPV4Address'b3 (IPV4Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
get_IPV4Address'b4 :: ((Untyped.ReadCtx m msg)) => (IPV4Address msg) -> (m Std_.Word8)
get_IPV4Address'b4 (IPV4Address'newtype_ struct) = (GenHelpers.getWordField struct 0 24 0)
set_IPV4Address'b4 :: ((Untyped.RWCtx m s)) => (IPV4Address (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_IPV4Address'b4 (IPV4Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 24 0)
newtype IPV6Address msg
    = IPV6Address'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg IPV6Address) where
    tMsg f (IPV6Address'newtype_ s) = (IPV6Address'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (IPV6Address msg)) where
    fromStruct struct = (Std_.pure (IPV6Address'newtype_ struct))
instance (Classes.ToStruct msg (IPV6Address msg)) where
    toStruct (IPV6Address'newtype_ struct) = struct
instance (Untyped.HasMessage (IPV6Address msg)) where
    type InMessage (IPV6Address msg) = msg
    message (IPV6Address'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (IPV6Address msg)) where
    messageDefault msg = (IPV6Address'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (IPV6Address msg)) where
    fromPtr msg ptr = (IPV6Address'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (IPV6Address (Message.MutMsg s))) where
    toPtr msg (IPV6Address'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (IPV6Address (Message.MutMsg s))) where
    new msg = (IPV6Address'newtype_ <$> (Untyped.allocStruct msg 2 0))
instance (Basics.ListElem msg (IPV6Address msg)) where
    newtype List msg (IPV6Address msg)
        = IPV6Address'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (IPV6Address'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (IPV6Address'List_ l) = (Untyped.ListStruct l)
    length (IPV6Address'List_ l) = (Untyped.length l)
    index i (IPV6Address'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (IPV6Address (Message.MutMsg s))) where
    setIndex (IPV6Address'newtype_ elt) i (IPV6Address'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (IPV6Address'List_ <$> (Untyped.allocCompositeList msg 2 0 len))
get_IPV6Address'w1 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w1 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_IPV6Address'w1 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w1 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_IPV6Address'w2 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w2 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 0 16 0)
set_IPV6Address'w2 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w2 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
get_IPV6Address'w3 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w3 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_IPV6Address'w3 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w3 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
get_IPV6Address'w4 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w4 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 0 48 0)
set_IPV6Address'w4 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w4 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 48 0)
get_IPV6Address'w5 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w5 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_IPV6Address'w5 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w5 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
get_IPV6Address'w6 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w6 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 1 16 0)
set_IPV6Address'w6 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w6 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
get_IPV6Address'w7 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w7 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 1 32 0)
set_IPV6Address'w7 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w7 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 32 0)
get_IPV6Address'w8 :: ((Untyped.ReadCtx m msg)) => (IPV6Address msg) -> (m Std_.Word16)
get_IPV6Address'w8 (IPV6Address'newtype_ struct) = (GenHelpers.getWordField struct 1 48 0)
set_IPV6Address'w8 :: ((Untyped.RWCtx m s)) => (IPV6Address (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_IPV6Address'w8 (IPV6Address'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 48 0)
newtype ID msg
    = ID'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg ID) where
    tMsg f (ID'newtype_ s) = (ID'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (ID msg)) where
    fromStruct struct = (Std_.pure (ID'newtype_ struct))
instance (Classes.ToStruct msg (ID msg)) where
    toStruct (ID'newtype_ struct) = struct
instance (Untyped.HasMessage (ID msg)) where
    type InMessage (ID msg) = msg
    message (ID'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (ID msg)) where
    messageDefault msg = (ID'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (ID msg)) where
    fromPtr msg ptr = (ID'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (ID (Message.MutMsg s))) where
    toPtr msg (ID'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (ID (Message.MutMsg s))) where
    new msg = (ID'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (ID msg)) where
    newtype List msg (ID msg)
        = ID'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (ID'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ID'List_ l) = (Untyped.ListStruct l)
    length (ID'List_ l) = (Untyped.length l)
    index i (ID'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (ID (Message.MutMsg s))) where
    setIndex (ID'newtype_ elt) i (ID'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (ID'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_ID'value :: ((Untyped.ReadCtx m msg)) => (ID msg) -> (m (Basics.Data msg))
get_ID'value (ID'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_ID'value :: ((Untyped.RWCtx m s)) => (ID (Message.MutMsg s)) -> (Basics.Data (Message.MutMsg s)) -> (m ())
set_ID'value (ID'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_ID'value :: ((Untyped.ReadCtx m msg)) => (ID msg) -> (m Std_.Bool)
has_ID'value (ID'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_ID'value :: ((Untyped.RWCtx m s)) => Std_.Int -> (ID (Message.MutMsg s)) -> (m (Basics.Data (Message.MutMsg s)))
new_ID'value len struct = (do
    result <- (Basics.newData (Untyped.message struct) len)
    (set_ID'value struct result)
    (Std_.pure result)
    )