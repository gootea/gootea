@0x8c4f3e9cf5d85d79;

#
# Messages to index torrents
#

struct IndexRequest {
  key @0 :Data;
  infohash @1 :Data;
  torrent @2 :Data;
}

struct IndexResponse {
  key @0 :Data;
  infohash @1 :Data;
  keyIsDiscriminative @2 :Bool;
}


#
# Messages to search torrents
#

struct SearchRequest {
  key @0 :Data;
}

struct SearchResponse {
  key @0 :Data;
  keyIsDiscriminative @1 :Bool;
  results @2 :List(Data);
}

