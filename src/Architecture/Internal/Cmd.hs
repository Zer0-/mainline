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
import Network.Socket (SockAddr (..))
import Network.Socket.ByteString (sendTo)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Architecture.Internal.Sub
    ( SubStates
    , SubscriptionData (..)
    , TSub (..)
    , openUDPPort
    )

data TCmd msg
    = CmdLog String
    | CmdGetRandom (Float -> msg)
    | CmdGetTime (POSIXTime -> msg)
    | CmdRandomBytes Int (BS.ByteString -> msg)
    | CmdSendUDP Port CompactInfo BS.ByteString


newtype Cmd msg = Cmd [ TCmd msg ]


execTCmd :: SubStates msg -> TCmd msg -> IO (SubStates msg, Maybe msg)
execTCmd states (CmdGetRandom f) =
    randomIO >>= \i -> return (states, Just $ f i)

execTCmd states (CmdGetTime f) =
    getPOSIXTime >>= \t -> return (states, Just $ f t)

execTCmd states (CmdLog msg) = putStr msg >> return (states, Nothing)

execTCmd states (CmdRandomBytes n f) =
    do
        g <- newGenIO :: IO CtrDRBG

        case genBytes n g of
            Left err -> error $ show err
            Right (result, _) -> return (states, Just (f result))


execTCmd states (CmdSendUDP srcPort dest bs) =
    do
        (states2, sock) <- getSock

        nsent <- sendTo sock bs sockaddr --returns bytes sent.
        putStrLn $ "bytes sent: " ++ show nsent ++ " to: " ++ show sockaddr
        --TODO: something intelligent with this

        return (states2, Nothing)

        where
            sockaddr = ciToAddr dest
            ciToAddr :: CompactInfo -> SockAddr
            ciToAddr (CompactInfo ip p) =
                SockAddrInet (fromIntegral p) ip


            getSock =
                maybe
                    ( openUDPPort srcPort >>=
                        \sock ->
                            return
                                ( Map.insert
                                    key
                                    (UDPDat srcPort sock undefined)
                                    states
                                , sock
                                )
                    )
                    (return . ((,) states) . boundSocket)
                    (Map.lookup key states)

            key = hash (UDP srcPort undefined)


execCmd :: SubStates msg -> Cmd msg -> IO (SubStates msg , [ msg ])
execCmd states (Cmd l) = foldM ff (states, []) l

    where
        ff :: (SubStates msg, [ msg ]) -> TCmd msg -> IO (SubStates msg, [ msg ])
        ff (states2, msgs) tcmd =
            (execTCmd states2 tcmd)
                >>=
                    \ (states3, mmsg) ->
                        return (states3, maybe msgs (: msgs) mmsg)

-- execCmd (Cmd l) = (mapM execTCmd l) >>= (return . catMaybes)
