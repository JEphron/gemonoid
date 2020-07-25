{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Status where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data Status = Status {statusCode :: Int, statusMessage :: B.ByteString}

instance Eq Status where
  Status {statusCode = a} == Status {statusCode = b} = a == b

instance Ord Status where
  compare Status {statusCode = a} Status {statusCode = b} = a `compare` b

instance Show Status where
  show Status {statusCode, statusMessage} =
    "<Status (" <> show statusCode <> ")"
      <> ( if BC.length statusMessage > 0
             then " " <> BC.unpack statusMessage
             else ""
         )
      <> ">"

instance Enum Status where
  fromEnum = statusCode
  toEnum 10 = input10
  toEnum 11 = sensitiveInput11
  toEnum 20 = success20
  toEnum 30 = redirectTemporary30
  toEnum 31 = redirectPermanent31
  toEnum 40 = temporaryFailure40
  toEnum 41 = serverUnavailable41
  toEnum 42 = cgiError42
  toEnum 43 = proxyError43
  toEnum 44 = slowDown44
  toEnum 50 = permanentFailure50
  toEnum 51 = notFound50
  toEnum 52 = gone52
  toEnum 53 = proxyRequestRefused53
  toEnum 59 = badRequest59
  toEnum 60 = clientCertificateRequired60
  toEnum 61 = certificateNotAuthorized61
  toEnum 62 = certificateNotValid62
  toEnum n = Status n BC.empty

instance Bounded Status where
  minBound = input10
  maxBound = certificateNotValid62

input10 = Status 10 "Input"

sensitiveInput11 = Status 11 "Sensitive Input"

success20 = Status 20 "Success"

redirectTemporary30 = Status 30 "Temporary Redirect"

redirectPermanent31 = Status 31 "Permanent Redirect"

temporaryFailure40 = Status 40 "Temporary Failure"

serverUnavailable41 = Status 41 "Server Unavailable"

cgiError42 = Status 42 "CGI Error"

proxyError43 = Status 43 "Proxy Error"

slowDown44 = Status 44 "Slow Down"

permanentFailure50 = Status 50 "Permanent Failure"

notFound50 = Status 51 "Not Found"

gone52 = Status 52 "Gone"

proxyRequestRefused53 = Status 53 "Proxy Request Refused"

badRequest59 = Status 59 "Bad Request"

clientCertificateRequired60 = Status 60 "Client Certificate Required"

certificateNotAuthorized61 = Status 61 "Certificate Not Authorized"

certificateNotValid62 = Status 62 "Certificate Not Valid"
