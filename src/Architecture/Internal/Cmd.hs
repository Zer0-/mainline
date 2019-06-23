module Architecture.Internal.Cmd
    ( TCmd (..)
    , Cmd (..)
    , execCmd
    ) where

import System.Random (randomIO)
import Control.Monad (foldM)
import Data.Hashable (hash)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)
import Network.Socket.ByteString (sendTo, sendAll)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.IO (hFlush, stdout)

import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Architecture.Internal.Types
    ( SubscriptionData (..)
    , InternalState (..)
    )
import Architecture.Internal.Sub
    ( TSub (..)
    , openUDPPort
    , connectTCP
    , ciToAddr
    )

data TCmd msg
    = CmdLog String
    | CmdGetRandom (Float -> msg)
    | CmdGetTime (POSIXTime -> msg)
    | CmdRandomBytes Int (BS.ByteString -> msg)
    | CmdSendUDP Port CompactInfo BS.ByteString
    | CmdSendTCP CompactInfo BS.ByteString
    | CmdReadFile String (BS.ByteString -> msg)
    | CmdWriteFile String BS.ByteString


newtype Cmd msg = Cmd [ TCmd msg ]


execTCmd :: InternalState msg -> TCmd msg -> IO (InternalState msg, Maybe msg)
execTCmd state (CmdGetRandom f) =
    randomIO >>= \i -> return (state, Just $ f i)

execTCmd state (CmdGetTime f) =
    getPOSIXTime >>= \t -> return (state, Just $ f t)

execTCmd state (CmdLog msg) =
    putStr msg >> hFlush stdout >> return (state, Nothing)

execTCmd state (CmdRandomBytes n f) =
    do
        g <- newGenIO :: IO CtrDRBG

        case genBytes n g of
            Left err -> error $ show err
            Right (result, _) -> return (state, Just (f result))


execTCmd state (CmdReadFile filename f) =
    do
        bytes <- BS.readFile filename
        return (state, Just $ f bytes)


execTCmd state (CmdWriteFile filename bs) =
    do
        BS.writeFile filename bs
        return (state, Nothing)


execTCmd state (CmdSendUDP _ (CompactInfo ip 0) _) =
    execTCmd state $ CmdLog msg

    where
        msg = "Error: Cannot send message to " ++ show (CompactInfo ip 0)
            ++ ". Invalid port 0!"

execTCmd state (CmdSendUDP srcPort dest bs) =
    do
        (newSubStates, sock) <- getSock

        _ <- sendTo sock bs (ciToAddr dest)

        return (state { subState = newSubStates }, Nothing)

        where
            getSock =
                maybe
                    ((openUDPPort srcPort) >>=
                        \sock ->
                            return
                                ( Map.insert
                                    key
                                    (UDPDat srcPort sock undefined)
                                    substates
                                , sock
                                )
                    )
                    (return . ((,) substates) . boundSocket)
                    (Map.lookup key substates)

            substates = subState state

            key = hash (UDP srcPort undefined)

execTCmd state (CmdSendTCP ci bs) = do
    (newSubStates, sock) <- getSock

    sendAll sock bs

    return (state { subState = newSubStates }, Nothing)

    where
        getSock =
            maybe
                ((connectTCP ci) >>=
                    \sock ->
                        return
                            ( Map.insert
                                key
                                (TCPClientDat ci sock undefined undefined)
                                substates
                            , sock
                            )
                )
                (return . ((,) substates) . boundSocket)
                (Map.lookup key substates)

        substates = subState state

        key = hash (TCPClient ci undefined undefined)


execCmd :: InternalState msg -> Cmd msg -> IO (InternalState msg , [ msg ])
execCmd state (Cmd l) = foldM ff (state, []) l

    where
        ff :: (InternalState msg, [ msg ])
           -> TCmd msg
           -> IO (InternalState msg, [ msg ])
        ff (state2, msgs) tcmd =
            (execTCmd state2 tcmd)
                >>=
                    \ (state3, mmsg) ->
                        return (state3, maybe msgs (: msgs) mmsg)

-- execCmd (Cmd l) = (mapM execTCmd l) >>= (return . catMaybes)
