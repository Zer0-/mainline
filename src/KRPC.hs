module KRPC where

type NodeID = Int --Word160

type InfoHash = Int --Word160

type IP = Int

type Port = Int

type CompactInfo = (IP, Port)

type NodeInfo = (NodeID, CompactInfo)

type Token = [Int] -- [Word8]

data Message
    = Query Message
    | Response Message
    | Ping NodeID
    | FindNode { queryingNodeId :: NodeID, targetNodeId :: NodeID }
    | NodeFound NodeID (Either NodeInfo [NodeInfo])
    | AskPeers NodeID InfoHash
    | PeersFound NodeID Token Message
    | Values [CompactInfo]
    | Nodes [NodeInfo]
    | AnnouncePeer NodeID Port Token Bool -- last arg is implied_port
    | Error
