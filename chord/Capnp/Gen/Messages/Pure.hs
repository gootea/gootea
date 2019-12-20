{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Messages.Pure(Message(..)
                              ,Message'message(..)
                              ,FingerTableResponse(..)
                              ,GetValuesQuery(..)
                              ,GetValuesResponse(..)
                              ,AddValueQuery(..)
                              ,Node(..)
                              ,Node'address(..)
                              ,IPV4Address(..)
                              ,IPV6Address(..)
                              ,ID(..)) where
import qualified Capnp.GenHelpers.ReExports.Data.Vector as V
import qualified Capnp.GenHelpers.ReExports.Data.Text as T
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Capnp.GenHelpers.ReExports.Data.Default as Default
import qualified GHC.Generics as Generics
import qualified Control.Monad.IO.Class as MonadIO
import qualified Capnp.Untyped.Pure as UntypedPure
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Message as Message
import qualified Capnp.Classes as Classes
import qualified Capnp.Basics.Pure as BasicsPure
import qualified Capnp.GenHelpers.Pure as GenHelpersPure
import qualified Capnp.Gen.ById.Xfa197b59aed5fcfc
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
    = Message 
        {message :: Message'message}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Message) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Message) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Message) where
    type Cerial msg Message = (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message msg)
    decerialize raw = (Message <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Message'message raw) >>= Classes.decerialize))
instance (Classes.Marshal Message) where
    marshalInto raw_ value_ = case value_ of
        Message{..} ->
            (do
                (do
                    raw_ <- (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Message'message raw_)
                    (Classes.marshalInto raw_ message)
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Message)
instance (Classes.Cerialize (V.Vector Message)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Message))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Message)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Message))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Message'message 
    = Message'message'fingerTableQuery 
    | Message'message'fingerTableResponse FingerTableResponse
    | Message'message'getValuesQuery GetValuesQuery
    | Message'message'getValuesResponse GetValuesResponse
    | Message'message'addValueQuery AddValueQuery
    | Message'message'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Message'message) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Message'message) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Message'message) where
    type Cerial msg Message'message = (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Message'message' raw)
        case raw of
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message'fingerTableQuery) ->
                (Std_.pure Message'message'fingerTableQuery)
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message'fingerTableResponse raw) ->
                (Message'message'fingerTableResponse <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message'getValuesQuery raw) ->
                (Message'message'getValuesQuery <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message'getValuesResponse raw) ->
                (Message'message'getValuesResponse <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message'addValueQuery raw) ->
                (Message'message'addValueQuery <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Message'message'unknown' tag) ->
                (Std_.pure (Message'message'unknown' tag))
        )
instance (Classes.Marshal Message'message) where
    marshalInto raw_ value_ = case value_ of
        (Message'message'fingerTableQuery) ->
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Message'message'fingerTableQuery raw_)
        (Message'message'fingerTableResponse arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Message'message'fingerTableResponse raw_))
        (Message'message'getValuesQuery arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Message'message'getValuesQuery raw_))
        (Message'message'getValuesResponse arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Message'message'getValuesResponse raw_))
        (Message'message'addValueQuery arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Message'message'addValueQuery raw_))
        (Message'message'unknown' tag) ->
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Message'message'unknown' raw_ tag)
data FingerTableResponse 
    = FingerTableResponse 
        {nodes :: (V.Vector Node)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default FingerTableResponse) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg FingerTableResponse) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize FingerTableResponse) where
    type Cerial msg FingerTableResponse = (Capnp.Gen.ById.Xfa197b59aed5fcfc.FingerTableResponse msg)
    decerialize raw = (FingerTableResponse <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_FingerTableResponse'nodes raw) >>= Classes.decerialize))
instance (Classes.Marshal FingerTableResponse) where
    marshalInto raw_ value_ = case value_ of
        FingerTableResponse{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) nodes) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_FingerTableResponse'nodes raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize FingerTableResponse)
instance (Classes.Cerialize (V.Vector FingerTableResponse)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector FingerTableResponse))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector FingerTableResponse)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector FingerTableResponse))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector FingerTableResponse)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector FingerTableResponse))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector FingerTableResponse)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data GetValuesQuery 
    = GetValuesQuery 
        {key :: ID}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default GetValuesQuery) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg GetValuesQuery) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize GetValuesQuery) where
    type Cerial msg GetValuesQuery = (Capnp.Gen.ById.Xfa197b59aed5fcfc.GetValuesQuery msg)
    decerialize raw = (GetValuesQuery <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_GetValuesQuery'key raw) >>= Classes.decerialize))
instance (Classes.Marshal GetValuesQuery) where
    marshalInto raw_ value_ = case value_ of
        GetValuesQuery{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) key) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_GetValuesQuery'key raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize GetValuesQuery)
instance (Classes.Cerialize (V.Vector GetValuesQuery)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector GetValuesQuery))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector GetValuesQuery)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector GetValuesQuery))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector GetValuesQuery)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector GetValuesQuery))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector GetValuesQuery)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data GetValuesResponse 
    = GetValuesResponse 
        {key :: ID
        ,values :: (V.Vector BS.ByteString)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default GetValuesResponse) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg GetValuesResponse) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize GetValuesResponse) where
    type Cerial msg GetValuesResponse = (Capnp.Gen.ById.Xfa197b59aed5fcfc.GetValuesResponse msg)
    decerialize raw = (GetValuesResponse <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_GetValuesResponse'key raw) >>= Classes.decerialize)
                                         <*> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_GetValuesResponse'values raw) >>= Classes.decerialize))
instance (Classes.Marshal GetValuesResponse) where
    marshalInto raw_ value_ = case value_ of
        GetValuesResponse{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) key) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_GetValuesResponse'key raw_))
                ((Classes.cerialize (Untyped.message raw_) values) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_GetValuesResponse'values raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize GetValuesResponse)
instance (Classes.Cerialize (V.Vector GetValuesResponse)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector GetValuesResponse))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector GetValuesResponse)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector GetValuesResponse))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector GetValuesResponse)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector GetValuesResponse))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector GetValuesResponse)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data AddValueQuery 
    = AddValueQuery 
        {key :: ID
        ,value :: BS.ByteString}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default AddValueQuery) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg AddValueQuery) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize AddValueQuery) where
    type Cerial msg AddValueQuery = (Capnp.Gen.ById.Xfa197b59aed5fcfc.AddValueQuery msg)
    decerialize raw = (AddValueQuery <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_AddValueQuery'key raw) >>= Classes.decerialize)
                                     <*> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_AddValueQuery'value raw) >>= Classes.decerialize))
instance (Classes.Marshal AddValueQuery) where
    marshalInto raw_ value_ = case value_ of
        AddValueQuery{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) key) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_AddValueQuery'key raw_))
                ((Classes.cerialize (Untyped.message raw_) value) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_AddValueQuery'value raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize AddValueQuery)
instance (Classes.Cerialize (V.Vector AddValueQuery)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector AddValueQuery))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector AddValueQuery)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector AddValueQuery))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector AddValueQuery)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector AddValueQuery))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector AddValueQuery)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Node 
    = Node 
        {id :: ID
        ,address :: Node'address
        ,port :: Std_.Int16}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node) where
    type Cerial msg Node = (Capnp.Gen.ById.Xfa197b59aed5fcfc.Node msg)
    decerialize raw = (Node <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Node'id raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Node'address raw) >>= Classes.decerialize)
                            <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Node'port raw))
instance (Classes.Marshal Node) where
    marshalInto raw_ value_ = case value_ of
        Node{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) id) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Node'id raw_))
                (do
                    raw_ <- (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Node'address raw_)
                    (Classes.marshalInto raw_ address)
                    )
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Node'port raw_ port)
                (Std_.pure ())
                )
instance (Classes.Cerialize Node)
instance (Classes.Cerialize (V.Vector Node)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Node))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Node)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Node))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Node'address 
    = Node'address'ipv6Address IPV6Address
    | Node'address'ipv4Address IPV4Address
    | Node'address'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node'address) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node'address) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node'address) where
    type Cerial msg Node'address = (Capnp.Gen.ById.Xfa197b59aed5fcfc.Node'address msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_Node'address' raw)
        case raw of
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Node'address'ipv6Address raw) ->
                (Node'address'ipv6Address <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Node'address'ipv4Address raw) ->
                (Node'address'ipv4Address <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.Node'address'unknown' tag) ->
                (Std_.pure (Node'address'unknown' tag))
        )
instance (Classes.Marshal Node'address) where
    marshalInto raw_ value_ = case value_ of
        (Node'address'ipv6Address arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Node'address'ipv6Address raw_))
        (Node'address'ipv4Address arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Node'address'ipv4Address raw_))
        (Node'address'unknown' tag) ->
            (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_Node'address'unknown' raw_ tag)
data IPV4Address 
    = IPV4Address 
        {b1 :: Std_.Word8
        ,b2 :: Std_.Word8
        ,b3 :: Std_.Word8
        ,b4 :: Std_.Word8}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default IPV4Address) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg IPV4Address) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize IPV4Address) where
    type Cerial msg IPV4Address = (Capnp.Gen.ById.Xfa197b59aed5fcfc.IPV4Address msg)
    decerialize raw = (IPV4Address <$> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV4Address'b1 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV4Address'b2 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV4Address'b3 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV4Address'b4 raw))
instance (Classes.Marshal IPV4Address) where
    marshalInto raw_ value_ = case value_ of
        IPV4Address{..} ->
            (do
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV4Address'b1 raw_ b1)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV4Address'b2 raw_ b2)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV4Address'b3 raw_ b3)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV4Address'b4 raw_ b4)
                (Std_.pure ())
                )
instance (Classes.Cerialize IPV4Address)
instance (Classes.Cerialize (V.Vector IPV4Address)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector IPV4Address))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector IPV4Address)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector IPV4Address))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector IPV4Address)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector IPV4Address))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector IPV4Address)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data IPV6Address 
    = IPV6Address 
        {w1 :: Std_.Word16
        ,w2 :: Std_.Word16
        ,w3 :: Std_.Word16
        ,w4 :: Std_.Word16
        ,w5 :: Std_.Word16
        ,w6 :: Std_.Word16
        ,w7 :: Std_.Word16
        ,w8 :: Std_.Word16}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default IPV6Address) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg IPV6Address) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize IPV6Address) where
    type Cerial msg IPV6Address = (Capnp.Gen.ById.Xfa197b59aed5fcfc.IPV6Address msg)
    decerialize raw = (IPV6Address <$> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w1 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w2 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w3 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w4 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w5 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w6 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w7 raw)
                                   <*> (Capnp.Gen.ById.Xfa197b59aed5fcfc.get_IPV6Address'w8 raw))
instance (Classes.Marshal IPV6Address) where
    marshalInto raw_ value_ = case value_ of
        IPV6Address{..} ->
            (do
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w1 raw_ w1)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w2 raw_ w2)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w3 raw_ w3)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w4 raw_ w4)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w5 raw_ w5)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w6 raw_ w6)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w7 raw_ w7)
                (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_IPV6Address'w8 raw_ w8)
                (Std_.pure ())
                )
instance (Classes.Cerialize IPV6Address)
instance (Classes.Cerialize (V.Vector IPV6Address)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector IPV6Address))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector IPV6Address)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector IPV6Address))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector IPV6Address)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector IPV6Address))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector IPV6Address)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data ID 
    = ID 
        {value :: BS.ByteString}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default ID) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg ID) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize ID) where
    type Cerial msg ID = (Capnp.Gen.ById.Xfa197b59aed5fcfc.ID msg)
    decerialize raw = (ID <$> ((Capnp.Gen.ById.Xfa197b59aed5fcfc.get_ID'value raw) >>= Classes.decerialize))
instance (Classes.Marshal ID) where
    marshalInto raw_ value_ = case value_ of
        ID{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) value) >>= (Capnp.Gen.ById.Xfa197b59aed5fcfc.set_ID'value raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize ID)
instance (Classes.Cerialize (V.Vector ID)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector ID))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector ID)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector ID))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ID)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ID))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ID)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec