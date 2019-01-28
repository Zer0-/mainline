module Network.KRPC.InternalConstants where

import Data.ByteString (ByteString)

import Network.KRPC.Helpers (stringpack)

bs_y :: ByteString
bs_y = stringpack "y"

bs_e :: ByteString
bs_e = stringpack "e"

bs_q :: ByteString
bs_q = stringpack "q"

bs_a :: ByteString
bs_a = stringpack "a"

bs_r :: ByteString
bs_r = stringpack "r"

bs_t :: ByteString
bs_t = stringpack "t"

bs_id :: ByteString
bs_id = stringpack "id"

bs_v :: ByteString
bs_v = stringpack "v"

bs_ip :: ByteString
bs_ip = stringpack "ip"
