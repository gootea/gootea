@0xfa197b59aed5fcfc;

struct Message {
  message :union {
    fingerTableQuery @0 :Void;
    fingerTableResponse @1 :FingerTableResponse;
    getValuesQuery @2 :GetValuesQuery;
    getValuesResponse @3 :GetValuesResponse;
    addValueQuery @4 :AddValueQuery;
  }
}

struct FingerTableResponse {
  nodes @0 :List(Node);
}

struct GetValuesQuery {
  key @0 :ID;
}

struct GetValuesResponse {
  key @0 :ID;
  # Each value is a InfoHash (20 bytes of data each)
  values @1 :List(Data);
}

struct AddValueQuery {
  key @0 :ID;
  # Value represents a InfoHash (20 bytes)
  value @1 :Data;
}

struct Node {
  id @0 :ID;
  address :union {
    ipv6Address @1 :IPV6Address;
    ipv4Address @2 :IPV4Address;
  }
  port @3 :Int16;
}

struct IPV4Address {
  b1 @0 :UInt8;
  b2 @1 :UInt8;
  b3 @2 :UInt8;
  b4 @3 :UInt8;
}


struct IPV6Address {
   w1 @0 :UInt16;
   w2 @1 :UInt16;
   w3 @2 :UInt16;
   w4 @3 :UInt16;
   w5 @4 :UInt16;
   w6 @5 :UInt16;
   w7 @6 :UInt16;
   w8 @7 :UInt16;
}

struct ID {
  # The ID value is a 20 bytes number
  value @0 :Data;
}
