{-# LANGUAGE OverloadedStrings #-}

-- Module  : Data.FIX.Spec.FIX42
-- License : LGPL-2.1

module Data.FIX.Spec.FIX42 ( fix42 ) where

import Data.Functor ( (<$>) )
import Data.IntMap (IntMap)
import Test.QuickCheck

import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.LookupTable as LT ( new, insert )

import Data.FIX.Arbitrary
import Data.FIX.Message
import Data.FIX.Parser

tAccount :: FIXTag
tAccount = FIXTag
   { tName = "Account"
   , tnum = 1
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAdvId :: FIXTag
tAdvId = FIXTag
   { tName = "AdvId"
   , tnum = 2
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAdvRefID :: FIXTag
tAdvRefID = FIXTag
   { tName = "AdvRefID"
   , tnum = 3
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAdvSide :: FIXTag
tAdvSide = FIXTag
   { tName = "AdvSide"
   , tnum = 4
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tAdvTransType :: FIXTag
tAdvTransType = FIXTag
   { tName = "AdvTransType"
   , tnum = 5
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAvgPx :: FIXTag
tAvgPx = FIXTag
   { tName = "AvgPx"
   , tnum = 6
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tBeginSeqNo :: FIXTag
tBeginSeqNo = FIXTag
   { tName = "BeginSeqNo"
   , tnum = 7
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tClOrdID :: FIXTag
tClOrdID = FIXTag
   { tName = "ClOrdID"
   , tnum = 11
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCommission :: FIXTag
tCommission = FIXTag
   { tName = "Commission"
   , tnum = 12
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tCommType :: FIXTag
tCommType = FIXTag
   { tName = "CommType"
   , tnum = 13
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tCumQty :: FIXTag
tCumQty = FIXTag
   { tName = "CumQty"
   , tnum = 14
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tCurrency :: FIXTag
tCurrency = FIXTag
   { tName = "Currency"
   , tnum = 15
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tEndSeqNo :: FIXTag
tEndSeqNo = FIXTag
   { tName = "EndSeqNo"
   , tnum = 16
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tExecID :: FIXTag
tExecID = FIXTag
   { tName = "ExecID"
   , tnum = 17
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tExecInst :: FIXTag
tExecInst = FIXTag
   { tName = "ExecInst"
   , tnum = 18
   , tparser = toFIXMultipleValueString
   , genValue = FIXMultipleValueString <$> genByteString }

tExecRefID :: FIXTag
tExecRefID = FIXTag
   { tName = "ExecRefID"
   , tnum = 19
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tExecTransType :: FIXTag
tExecTransType = FIXTag
   { tName = "ExecTransType"
   , tnum = 20
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tHandlInst :: FIXTag
tHandlInst = FIXTag
   { tName = "HandlInst"
   , tnum = 21
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tIDSource :: FIXTag
tIDSource = FIXTag
   { tName = "IDSource"
   , tnum = 22
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tIOIid :: FIXTag
tIOIid = FIXTag
   { tName = "IOIid"
   , tnum = 23
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tIOIQltyInd :: FIXTag
tIOIQltyInd = FIXTag
   { tName = "IOIQltyInd"
   , tnum = 25
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tIOIRefID :: FIXTag
tIOIRefID = FIXTag
   { tName = "IOIRefID"
   , tnum = 26
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tIOIShares :: FIXTag
tIOIShares = FIXTag
   { tName = "IOIShares"
   , tnum = 27
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tIOITransType :: FIXTag
tIOITransType = FIXTag
   { tName = "IOITransType"
   , tnum = 28
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tLastCapacity :: FIXTag
tLastCapacity = FIXTag
   { tName = "LastCapacity"
   , tnum = 29
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tLastMkt :: FIXTag
tLastMkt = FIXTag
   { tName = "LastMkt"
   , tnum = 30
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tLastPx :: FIXTag
tLastPx = FIXTag
   { tName = "LastPx"
   , tnum = 31
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLastShares :: FIXTag
tLastShares = FIXTag
   { tName = "LastShares"
   , tnum = 32
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLinesOfText :: FIXTag
tLinesOfText = FIXTag
   { tName = "LinesOfText"
   , tnum = 33
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMsgSeqNum :: FIXTag
tMsgSeqNum = FIXTag
   { tName = "MsgSeqNum"
   , tnum = 34
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNewSeqNo :: FIXTag
tNewSeqNo = FIXTag
   { tName = "NewSeqNo"
   , tnum = 36
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tOrderID :: FIXTag
tOrderID = FIXTag
   { tName = "OrderID"
   , tnum = 37
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tOrderQty :: FIXTag
tOrderQty = FIXTag
   { tName = "OrderQty"
   , tnum = 38
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOrdStatus :: FIXTag
tOrdStatus = FIXTag
   { tName = "OrdStatus"
   , tnum = 39
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tOrdType :: FIXTag
tOrdType = FIXTag
   { tName = "OrdType"
   , tnum = 40
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tOrigClOrdID :: FIXTag
tOrigClOrdID = FIXTag
   { tName = "OrigClOrdID"
   , tnum = 41
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tOrigTime :: FIXTag
tOrigTime = FIXTag
   { tName = "OrigTime"
   , tnum = 42
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tPossDupFlag :: FIXTag
tPossDupFlag = FIXTag
   { tName = "PossDupFlag"
   , tnum = 43
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tPrice :: FIXTag
tPrice = FIXTag
   { tName = "Price"
   , tnum = 44
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tRefSeqNum :: FIXTag
tRefSeqNum = FIXTag
   { tName = "RefSeqNum"
   , tnum = 45
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tRelatdSym :: FIXTag
tRelatdSym = FIXTag
   { tName = "RelatdSym"
   , tnum = 46
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tRule80A :: FIXTag
tRule80A = FIXTag
   { tName = "Rule80A"
   , tnum = 47
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tSecurityID :: FIXTag
tSecurityID = FIXTag
   { tName = "SecurityID"
   , tnum = 48
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSenderCompID :: FIXTag
tSenderCompID = FIXTag
   { tName = "SenderCompID"
   , tnum = 49
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSenderSubID :: FIXTag
tSenderSubID = FIXTag
   { tName = "SenderSubID"
   , tnum = 50
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSendingTime :: FIXTag
tSendingTime = FIXTag
   { tName = "SendingTime"
   , tnum = 52
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tShares :: FIXTag
tShares = FIXTag
   { tName = "Shares"
   , tnum = 53
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSide :: FIXTag
tSide = FIXTag
   { tName = "Side"
   , tnum = 54
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tSymbol :: FIXTag
tSymbol = FIXTag
   { tName = "Symbol"
   , tnum = 55
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTargetCompID :: FIXTag
tTargetCompID = FIXTag
   { tName = "TargetCompID"
   , tnum = 56
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTargetSubID :: FIXTag
tTargetSubID = FIXTag
   { tName = "TargetSubID"
   , tnum = 57
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tText :: FIXTag
tText = FIXTag
   { tName = "Text"
   , tnum = 58
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTimeInForce :: FIXTag
tTimeInForce = FIXTag
   { tName = "TimeInForce"
   , tnum = 59
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tTransactTime :: FIXTag
tTransactTime = FIXTag
   { tName = "TransactTime"
   , tnum = 60
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tUrgency :: FIXTag
tUrgency = FIXTag
   { tName = "Urgency"
   , tnum = 61
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tValidUntilTime :: FIXTag
tValidUntilTime = FIXTag
   { tName = "ValidUntilTime"
   , tnum = 62
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tSettlmntTyp :: FIXTag
tSettlmntTyp = FIXTag
   { tName = "SettlmntTyp"
   , tnum = 63
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tFutSettDate :: FIXTag
tFutSettDate = FIXTag
   { tName = "FutSettDate"
   , tnum = 64
   , tparser = toFIXDateOnly
   , genValue = FIXDateOnly <$> genDay }

tSymbolSfx :: FIXTag
tSymbolSfx = FIXTag
   { tName = "SymbolSfx"
   , tnum = 65
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tListID :: FIXTag
tListID = FIXTag
   { tName = "ListID"
   , tnum = 66
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tListSeqNo :: FIXTag
tListSeqNo = FIXTag
   { tName = "ListSeqNo"
   , tnum = 67
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTotNoOrders :: FIXTag
tTotNoOrders = FIXTag
   { tName = "TotNoOrders"
   , tnum = 68
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tListExecInst :: FIXTag
tListExecInst = FIXTag
   { tName = "ListExecInst"
   , tnum = 69
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAllocID :: FIXTag
tAllocID = FIXTag
   { tName = "AllocID"
   , tnum = 70
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAllocTransType :: FIXTag
tAllocTransType = FIXTag
   { tName = "AllocTransType"
   , tnum = 71
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tRefAllocID :: FIXTag
tRefAllocID = FIXTag
   { tName = "RefAllocID"
   , tnum = 72
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tNoOrders :: FIXTag
tNoOrders = FIXTag
   { tName = "NoOrders"
   , tnum = 73
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tAvgPrxPrecision :: FIXTag
tAvgPrxPrecision = FIXTag
   { tName = "AvgPrxPrecision"
   , tnum = 74
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTradeDate :: FIXTag
tTradeDate = FIXTag
   { tName = "TradeDate"
   , tnum = 75
   , tparser = toFIXDateOnly
   , genValue = FIXDateOnly <$> genDay }

tExecBroker :: FIXTag
tExecBroker = FIXTag
   { tName = "ExecBroker"
   , tnum = 76
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tOpenClose :: FIXTag
tOpenClose = FIXTag
   { tName = "OpenClose"
   , tnum = 77
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tNoAllocs :: FIXTag
tNoAllocs = FIXTag
   { tName = "NoAllocs"
   , tnum = 78
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tAllocAccount :: FIXTag
tAllocAccount = FIXTag
   { tName = "AllocAccount"
   , tnum = 79
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAllocShares :: FIXTag
tAllocShares = FIXTag
   { tName = "AllocShares"
   , tnum = 80
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tProcessCode :: FIXTag
tProcessCode = FIXTag
   { tName = "ProcessCode"
   , tnum = 81
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tNoRpts :: FIXTag
tNoRpts = FIXTag
   { tName = "NoRpts"
   , tnum = 82
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tRptSeq :: FIXTag
tRptSeq = FIXTag
   { tName = "RptSeq"
   , tnum = 83
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tCxlQty :: FIXTag
tCxlQty = FIXTag
   { tName = "CxlQty"
   , tnum = 84
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tAllocStatus :: FIXTag
tAllocStatus = FIXTag
   { tName = "AllocStatus"
   , tnum = 87
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tAllocRejCode :: FIXTag
tAllocRejCode = FIXTag
   { tName = "AllocRejCode"
   , tnum = 88
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSignature :: FIXTag
tSignature = FIXTag
   { tName = "Signature"
   , tnum = 89
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tSecureDataLen :: FIXTag
tSecureDataLen = FIXTag
   { tName = "SecureDataLen"
   , tnum = 90
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSecureData :: FIXTag
tSecureData = FIXTag
   { tName = "SecureData"
   , tnum = 91
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tBrokerOfCredit :: FIXTag
tBrokerOfCredit = FIXTag
   { tName = "BrokerOfCredit"
   , tnum = 92
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSignatureLength :: FIXTag
tSignatureLength = FIXTag
   { tName = "SignatureLength"
   , tnum = 93
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEmailType :: FIXTag
tEmailType = FIXTag
   { tName = "EmailType"
   , tnum = 94
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tRawDataLength :: FIXTag
tRawDataLength = FIXTag
   { tName = "RawDataLength"
   , tnum = 95
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tRawData :: FIXTag
tRawData = FIXTag
   { tName = "RawData"
   , tnum = 96
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tPossResend :: FIXTag
tPossResend = FIXTag
   { tName = "PossResend"
   , tnum = 97
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tEncryptMethod :: FIXTag
tEncryptMethod = FIXTag
   { tName = "EncryptMethod"
   , tnum = 98
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tStopPx :: FIXTag
tStopPx = FIXTag
   { tName = "StopPx"
   , tnum = 99
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tExDestination :: FIXTag
tExDestination = FIXTag
   { tName = "ExDestination"
   , tnum = 100
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCxlRejReason :: FIXTag
tCxlRejReason = FIXTag
   { tName = "CxlRejReason"
   , tnum = 102
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tOrdRejReason :: FIXTag
tOrdRejReason = FIXTag
   { tName = "OrdRejReason"
   , tnum = 103
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tIOIQualifier :: FIXTag
tIOIQualifier = FIXTag
   { tName = "IOIQualifier"
   , tnum = 104
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tWaveNo :: FIXTag
tWaveNo = FIXTag
   { tName = "WaveNo"
   , tnum = 105
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tIssuer :: FIXTag
tIssuer = FIXTag
   { tName = "Issuer"
   , tnum = 106
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecurityDesc :: FIXTag
tSecurityDesc = FIXTag
   { tName = "SecurityDesc"
   , tnum = 107
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tHeartBtInt :: FIXTag
tHeartBtInt = FIXTag
   { tName = "HeartBtInt"
   , tnum = 108
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tClientID :: FIXTag
tClientID = FIXTag
   { tName = "ClientID"
   , tnum = 109
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tMinQty :: FIXTag
tMinQty = FIXTag
   { tName = "MinQty"
   , tnum = 110
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tMaxFloor :: FIXTag
tMaxFloor = FIXTag
   { tName = "MaxFloor"
   , tnum = 111
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tTestReqID :: FIXTag
tTestReqID = FIXTag
   { tName = "TestReqID"
   , tnum = 112
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tReportToExch :: FIXTag
tReportToExch = FIXTag
   { tName = "ReportToExch"
   , tnum = 113
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tLocateReqd :: FIXTag
tLocateReqd = FIXTag
   { tName = "LocateReqd"
   , tnum = 114
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tOnBehalfOfCompID :: FIXTag
tOnBehalfOfCompID = FIXTag
   { tName = "OnBehalfOfCompID"
   , tnum = 115
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tOnBehalfOfSubID :: FIXTag
tOnBehalfOfSubID = FIXTag
   { tName = "OnBehalfOfSubID"
   , tnum = 116
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tQuoteID :: FIXTag
tQuoteID = FIXTag
   { tName = "QuoteID"
   , tnum = 117
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tNetMoney :: FIXTag
tNetMoney = FIXTag
   { tName = "NetMoney"
   , tnum = 118
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSettlCurrAmt :: FIXTag
tSettlCurrAmt = FIXTag
   { tName = "SettlCurrAmt"
   , tnum = 119
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSettlCurrency :: FIXTag
tSettlCurrency = FIXTag
   { tName = "SettlCurrency"
   , tnum = 120
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tForexReq :: FIXTag
tForexReq = FIXTag
   { tName = "ForexReq"
   , tnum = 121
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tOrigSendingTime :: FIXTag
tOrigSendingTime = FIXTag
   { tName = "OrigSendingTime"
   , tnum = 122
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tGapFillFlag :: FIXTag
tGapFillFlag = FIXTag
   { tName = "GapFillFlag"
   , tnum = 123
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tNoExecs :: FIXTag
tNoExecs = FIXTag
   { tName = "NoExecs"
   , tnum = 124
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tExpireTime :: FIXTag
tExpireTime = FIXTag
   { tName = "ExpireTime"
   , tnum = 126
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tDKReason :: FIXTag
tDKReason = FIXTag
   { tName = "DKReason"
   , tnum = 127
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tDeliverToCompID :: FIXTag
tDeliverToCompID = FIXTag
   { tName = "DeliverToCompID"
   , tnum = 128
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tDeliverToSubID :: FIXTag
tDeliverToSubID = FIXTag
   { tName = "DeliverToSubID"
   , tnum = 129
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tIOINaturalFlag :: FIXTag
tIOINaturalFlag = FIXTag
   { tName = "IOINaturalFlag"
   , tnum = 130
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tQuoteReqID :: FIXTag
tQuoteReqID = FIXTag
   { tName = "QuoteReqID"
   , tnum = 131
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tBidPx :: FIXTag
tBidPx = FIXTag
   { tName = "BidPx"
   , tnum = 132
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOfferPx :: FIXTag
tOfferPx = FIXTag
   { tName = "OfferPx"
   , tnum = 133
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tBidSize :: FIXTag
tBidSize = FIXTag
   { tName = "BidSize"
   , tnum = 134
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOfferSize :: FIXTag
tOfferSize = FIXTag
   { tName = "OfferSize"
   , tnum = 135
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tNoMiscFees :: FIXTag
tNoMiscFees = FIXTag
   { tName = "NoMiscFees"
   , tnum = 136
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMiscFeeAmt :: FIXTag
tMiscFeeAmt = FIXTag
   { tName = "MiscFeeAmt"
   , tnum = 137
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tMiscFeeCurr :: FIXTag
tMiscFeeCurr = FIXTag
   { tName = "MiscFeeCurr"
   , tnum = 138
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tMiscFeeType :: FIXTag
tMiscFeeType = FIXTag
   { tName = "MiscFeeType"
   , tnum = 139
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tPrevClosePx :: FIXTag
tPrevClosePx = FIXTag
   { tName = "PrevClosePx"
   , tnum = 140
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tResetSeqNumFlag :: FIXTag
tResetSeqNumFlag = FIXTag
   { tName = "ResetSeqNumFlag"
   , tnum = 141
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tSenderLocationID :: FIXTag
tSenderLocationID = FIXTag
   { tName = "SenderLocationID"
   , tnum = 142
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTargetLocationID :: FIXTag
tTargetLocationID = FIXTag
   { tName = "TargetLocationID"
   , tnum = 143
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tOnBehalfOfLocationID :: FIXTag
tOnBehalfOfLocationID = FIXTag
   { tName = "OnBehalfOfLocationID"
   , tnum = 144
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tDeliverToLocationID :: FIXTag
tDeliverToLocationID = FIXTag
   { tName = "DeliverToLocationID"
   , tnum = 145
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tNoRelatedSym :: FIXTag
tNoRelatedSym = FIXTag
   { tName = "NoRelatedSym"
   , tnum = 146
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSubject :: FIXTag
tSubject = FIXTag
   { tName = "Subject"
   , tnum = 147
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tHeadline :: FIXTag
tHeadline = FIXTag
   { tName = "Headline"
   , tnum = 148
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tURLLink :: FIXTag
tURLLink = FIXTag
   { tName = "URLLink"
   , tnum = 149
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tExecType :: FIXTag
tExecType = FIXTag
   { tName = "ExecType"
   , tnum = 150
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tLeavesQty :: FIXTag
tLeavesQty = FIXTag
   { tName = "LeavesQty"
   , tnum = 151
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tCashOrderQty :: FIXTag
tCashOrderQty = FIXTag
   { tName = "CashOrderQty"
   , tnum = 152
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tAllocAvgPx :: FIXTag
tAllocAvgPx = FIXTag
   { tName = "AllocAvgPx"
   , tnum = 153
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tAllocNetMoney :: FIXTag
tAllocNetMoney = FIXTag
   { tName = "AllocNetMoney"
   , tnum = 154
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSettlCurrFxRate :: FIXTag
tSettlCurrFxRate = FIXTag
   { tName = "SettlCurrFxRate"
   , tnum = 155
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSettlCurrFxRateCalc :: FIXTag
tSettlCurrFxRateCalc = FIXTag
   { tName = "SettlCurrFxRateCalc"
   , tnum = 156
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tNumDaysInterest :: FIXTag
tNumDaysInterest = FIXTag
   { tName = "NumDaysInterest"
   , tnum = 157
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tAccruedInterestRate :: FIXTag
tAccruedInterestRate = FIXTag
   { tName = "AccruedInterestRate"
   , tnum = 158
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tAccruedInterestAmt :: FIXTag
tAccruedInterestAmt = FIXTag
   { tName = "AccruedInterestAmt"
   , tnum = 159
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSettlInstMode :: FIXTag
tSettlInstMode = FIXTag
   { tName = "SettlInstMode"
   , tnum = 160
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tAllocText :: FIXTag
tAllocText = FIXTag
   { tName = "AllocText"
   , tnum = 161
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSettlInstID :: FIXTag
tSettlInstID = FIXTag
   { tName = "SettlInstID"
   , tnum = 162
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSettlInstTransType :: FIXTag
tSettlInstTransType = FIXTag
   { tName = "SettlInstTransType"
   , tnum = 163
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tEmailThreadID :: FIXTag
tEmailThreadID = FIXTag
   { tName = "EmailThreadID"
   , tnum = 164
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSettlInstSource :: FIXTag
tSettlInstSource = FIXTag
   { tName = "SettlInstSource"
   , tnum = 165
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tSettlLocation :: FIXTag
tSettlLocation = FIXTag
   { tName = "SettlLocation"
   , tnum = 166
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecurityType :: FIXTag
tSecurityType = FIXTag
   { tName = "SecurityType"
   , tnum = 167
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tEffectiveTime :: FIXTag
tEffectiveTime = FIXTag
   { tName = "EffectiveTime"
   , tnum = 168
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tStandInstDbType :: FIXTag
tStandInstDbType = FIXTag
   { tName = "StandInstDbType"
   , tnum = 169
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tStandInstDbName :: FIXTag
tStandInstDbName = FIXTag
   { tName = "StandInstDbName"
   , tnum = 170
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tStandInstDbID :: FIXTag
tStandInstDbID = FIXTag
   { tName = "StandInstDbID"
   , tnum = 171
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSettlDeliveryType :: FIXTag
tSettlDeliveryType = FIXTag
   { tName = "SettlDeliveryType"
   , tnum = 172
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSettlDepositoryCode :: FIXTag
tSettlDepositoryCode = FIXTag
   { tName = "SettlDepositoryCode"
   , tnum = 173
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSettlBrkrCode :: FIXTag
tSettlBrkrCode = FIXTag
   { tName = "SettlBrkrCode"
   , tnum = 174
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSettlInstCode :: FIXTag
tSettlInstCode = FIXTag
   { tName = "SettlInstCode"
   , tnum = 175
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecuritySettlAgentName :: FIXTag
tSecuritySettlAgentName = FIXTag
   { tName = "SecuritySettlAgentName"
   , tnum = 176
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecuritySettlAgentCode :: FIXTag
tSecuritySettlAgentCode = FIXTag
   { tName = "SecuritySettlAgentCode"
   , tnum = 177
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecuritySettlAgentAcctNum :: FIXTag
tSecuritySettlAgentAcctNum = FIXTag
   { tName = "SecuritySettlAgentAcctNum"
   , tnum = 178
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecuritySettlAgentAcctName :: FIXTag
tSecuritySettlAgentAcctName = FIXTag
   { tName = "SecuritySettlAgentAcctName"
   , tnum = 179
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecuritySettlAgentContactName :: FIXTag
tSecuritySettlAgentContactName = FIXTag
   { tName = "SecuritySettlAgentContactName"
   , tnum = 180
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecuritySettlAgentContactPhone :: FIXTag
tSecuritySettlAgentContactPhone = FIXTag
   { tName = "SecuritySettlAgentContactPhone"
   , tnum = 181
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCashSettlAgentName :: FIXTag
tCashSettlAgentName = FIXTag
   { tName = "CashSettlAgentName"
   , tnum = 182
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCashSettlAgentCode :: FIXTag
tCashSettlAgentCode = FIXTag
   { tName = "CashSettlAgentCode"
   , tnum = 183
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCashSettlAgentAcctNum :: FIXTag
tCashSettlAgentAcctNum = FIXTag
   { tName = "CashSettlAgentAcctNum"
   , tnum = 184
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCashSettlAgentAcctName :: FIXTag
tCashSettlAgentAcctName = FIXTag
   { tName = "CashSettlAgentAcctName"
   , tnum = 185
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCashSettlAgentContactName :: FIXTag
tCashSettlAgentContactName = FIXTag
   { tName = "CashSettlAgentContactName"
   , tnum = 186
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tCashSettlAgentContactPhone :: FIXTag
tCashSettlAgentContactPhone = FIXTag
   { tName = "CashSettlAgentContactPhone"
   , tnum = 187
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tBidSpotRate :: FIXTag
tBidSpotRate = FIXTag
   { tName = "BidSpotRate"
   , tnum = 188
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tBidForwardPoints :: FIXTag
tBidForwardPoints = FIXTag
   { tName = "BidForwardPoints"
   , tnum = 189
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOfferSpotRate :: FIXTag
tOfferSpotRate = FIXTag
   { tName = "OfferSpotRate"
   , tnum = 190
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOfferForwardPoints :: FIXTag
tOfferForwardPoints = FIXTag
   { tName = "OfferForwardPoints"
   , tnum = 191
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOrderQty2 :: FIXTag
tOrderQty2 = FIXTag
   { tName = "OrderQty2"
   , tnum = 192
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tFutSettDate2 :: FIXTag
tFutSettDate2 = FIXTag
   { tName = "FutSettDate2"
   , tnum = 193
   , tparser = toFIXDateOnly
   , genValue = FIXDateOnly <$> genDay }

tLastSpotRate :: FIXTag
tLastSpotRate = FIXTag
   { tName = "LastSpotRate"
   , tnum = 194
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLastForwardPoints :: FIXTag
tLastForwardPoints = FIXTag
   { tName = "LastForwardPoints"
   , tnum = 195
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tAllocLinkID :: FIXTag
tAllocLinkID = FIXTag
   { tName = "AllocLinkID"
   , tnum = 196
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tAllocLinkType :: FIXTag
tAllocLinkType = FIXTag
   { tName = "AllocLinkType"
   , tnum = 197
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSecondaryOrderID :: FIXTag
tSecondaryOrderID = FIXTag
   { tName = "SecondaryOrderID"
   , tnum = 198
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tNoIOIQualifiers :: FIXTag
tNoIOIQualifiers = FIXTag
   { tName = "NoIOIQualifiers"
   , tnum = 199
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMaturityMonthYear :: FIXTag
tMaturityMonthYear = FIXTag
   { tName = "MaturityMonthYear"
   , tnum = 200
   , tparser = toFIXMonthYear
   , genValue = FIXMonthYear <$> genMonthYear }

tPutOrCall :: FIXTag
tPutOrCall = FIXTag
   { tName = "PutOrCall"
   , tnum = 201
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tStrikePrice :: FIXTag
tStrikePrice = FIXTag
   { tName = "StrikePrice"
   , tnum = 202
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tCoveredOrUncovered :: FIXTag
tCoveredOrUncovered = FIXTag
   { tName = "CoveredOrUncovered"
   , tnum = 203
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tCustomerOrFirm :: FIXTag
tCustomerOrFirm = FIXTag
   { tName = "CustomerOrFirm"
   , tnum = 204
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMaturityDay :: FIXTag
tMaturityDay = FIXTag
   { tName = "MaturityDay"
   , tnum = 205
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tOptAttribute :: FIXTag
tOptAttribute = FIXTag
   { tName = "OptAttribute"
   , tnum = 206
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tSecurityExchange :: FIXTag
tSecurityExchange = FIXTag
   { tName = "SecurityExchange"
   , tnum = 207
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tNotifyBrokerOfCredit :: FIXTag
tNotifyBrokerOfCredit = FIXTag
   { tName = "NotifyBrokerOfCredit"
   , tnum = 208
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tAllocHandlInst :: FIXTag
tAllocHandlInst = FIXTag
   { tName = "AllocHandlInst"
   , tnum = 209
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMaxShow :: FIXTag
tMaxShow = FIXTag
   { tName = "MaxShow"
   , tnum = 210
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tPegDifference :: FIXTag
tPegDifference = FIXTag
   { tName = "PegDifference"
   , tnum = 211
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tXmlDataLen :: FIXTag
tXmlDataLen = FIXTag
   { tName = "XmlDataLen"
   , tnum = 212
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tXmlData :: FIXTag
tXmlData = FIXTag
   { tName = "XmlData"
   , tnum = 213
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tSettlInstRefID :: FIXTag
tSettlInstRefID = FIXTag
   { tName = "SettlInstRefID"
   , tnum = 214
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tNoRoutingIDs :: FIXTag
tNoRoutingIDs = FIXTag
   { tName = "NoRoutingIDs"
   , tnum = 215
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tRoutingType :: FIXTag
tRoutingType = FIXTag
   { tName = "RoutingType"
   , tnum = 216
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tRoutingID :: FIXTag
tRoutingID = FIXTag
   { tName = "RoutingID"
   , tnum = 217
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSpreadToBenchmark :: FIXTag
tSpreadToBenchmark = FIXTag
   { tName = "SpreadToBenchmark"
   , tnum = 218
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tBenchmark :: FIXTag
tBenchmark = FIXTag
   { tName = "Benchmark"
   , tnum = 219
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tCouponRate :: FIXTag
tCouponRate = FIXTag
   { tName = "CouponRate"
   , tnum = 223
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tContractMultiplier :: FIXTag
tContractMultiplier = FIXTag
   { tName = "ContractMultiplier"
   , tnum = 231
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tMDReqID :: FIXTag
tMDReqID = FIXTag
   { tName = "MDReqID"
   , tnum = 262
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSubscriptionRequestType :: FIXTag
tSubscriptionRequestType = FIXTag
   { tName = "SubscriptionRequestType"
   , tnum = 263
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tMarketDepth :: FIXTag
tMarketDepth = FIXTag
   { tName = "MarketDepth"
   , tnum = 264
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMDUpdateType :: FIXTag
tMDUpdateType = FIXTag
   { tName = "MDUpdateType"
   , tnum = 265
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tAggregatedBook :: FIXTag
tAggregatedBook = FIXTag
   { tName = "AggregatedBook"
   , tnum = 266
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tNoMDEntryTypes :: FIXTag
tNoMDEntryTypes = FIXTag
   { tName = "NoMDEntryTypes"
   , tnum = 267
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNoMDEntries :: FIXTag
tNoMDEntries = FIXTag
   { tName = "NoMDEntries"
   , tnum = 268
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMDEntryType :: FIXTag
tMDEntryType = FIXTag
   { tName = "MDEntryType"
   , tnum = 269
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tMDEntryPx :: FIXTag
tMDEntryPx = FIXTag
   { tName = "MDEntryPx"
   , tnum = 270
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tMDEntrySize :: FIXTag
tMDEntrySize = FIXTag
   { tName = "MDEntrySize"
   , tnum = 271
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tMDEntryDate :: FIXTag
tMDEntryDate = FIXTag
   { tName = "MDEntryDate"
   , tnum = 272
   , tparser = toFIXDateOnly
   , genValue = FIXDateOnly <$> genDay }

tMDEntryTime :: FIXTag
tMDEntryTime = FIXTag
   { tName = "MDEntryTime"
   , tnum = 273
   , tparser = toFIXTimeOnly
   , genValue = FIXTimeOnly <$> genDiffTime }

tTickDirection :: FIXTag
tTickDirection = FIXTag
   { tName = "TickDirection"
   , tnum = 274
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tMDMkt :: FIXTag
tMDMkt = FIXTag
   { tName = "MDMkt"
   , tnum = 275
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tQuoteCondition :: FIXTag
tQuoteCondition = FIXTag
   { tName = "QuoteCondition"
   , tnum = 276
   , tparser = toFIXMultipleValueString
   , genValue = FIXMultipleValueString <$> genByteString }

tTradeCondition :: FIXTag
tTradeCondition = FIXTag
   { tName = "TradeCondition"
   , tnum = 277
   , tparser = toFIXMultipleValueString
   , genValue = FIXMultipleValueString <$> genByteString }

tMDEntryID :: FIXTag
tMDEntryID = FIXTag
   { tName = "MDEntryID"
   , tnum = 278
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tMDUpdateAction :: FIXTag
tMDUpdateAction = FIXTag
   { tName = "MDUpdateAction"
   , tnum = 279
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tMDEntryRefID :: FIXTag
tMDEntryRefID = FIXTag
   { tName = "MDEntryRefID"
   , tnum = 280
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tMDReqRejReason :: FIXTag
tMDReqRejReason = FIXTag
   { tName = "MDReqRejReason"
   , tnum = 281
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tMDEntryOriginator :: FIXTag
tMDEntryOriginator = FIXTag
   { tName = "MDEntryOriginator"
   , tnum = 282
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tLocationID :: FIXTag
tLocationID = FIXTag
   { tName = "LocationID"
   , tnum = 283
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tDeskID :: FIXTag
tDeskID = FIXTag
   { tName = "DeskID"
   , tnum = 284
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tDeleteReason :: FIXTag
tDeleteReason = FIXTag
   { tName = "DeleteReason"
   , tnum = 285
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tOpenCloseSettleFlag :: FIXTag
tOpenCloseSettleFlag = FIXTag
   { tName = "OpenCloseSettleFlag"
   , tnum = 286
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tSellerDays :: FIXTag
tSellerDays = FIXTag
   { tName = "SellerDays"
   , tnum = 287
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMDEntryBuyer :: FIXTag
tMDEntryBuyer = FIXTag
   { tName = "MDEntryBuyer"
   , tnum = 288
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tMDEntrySeller :: FIXTag
tMDEntrySeller = FIXTag
   { tName = "MDEntrySeller"
   , tnum = 289
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tMDEntryPositionNo :: FIXTag
tMDEntryPositionNo = FIXTag
   { tName = "MDEntryPositionNo"
   , tnum = 290
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tFinancialStatus :: FIXTag
tFinancialStatus = FIXTag
   { tName = "FinancialStatus"
   , tnum = 291
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tCorporateAction :: FIXTag
tCorporateAction = FIXTag
   { tName = "CorporateAction"
   , tnum = 292
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tDefBidSize :: FIXTag
tDefBidSize = FIXTag
   { tName = "DefBidSize"
   , tnum = 293
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tDefOfferSize :: FIXTag
tDefOfferSize = FIXTag
   { tName = "DefOfferSize"
   , tnum = 294
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tNoQuoteEntries :: FIXTag
tNoQuoteEntries = FIXTag
   { tName = "NoQuoteEntries"
   , tnum = 295
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNoQuoteSets :: FIXTag
tNoQuoteSets = FIXTag
   { tName = "NoQuoteSets"
   , tnum = 296
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tQuoteAckStatus :: FIXTag
tQuoteAckStatus = FIXTag
   { tName = "QuoteAckStatus"
   , tnum = 297
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tQuoteCancelType :: FIXTag
tQuoteCancelType = FIXTag
   { tName = "QuoteCancelType"
   , tnum = 298
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tQuoteEntryID :: FIXTag
tQuoteEntryID = FIXTag
   { tName = "QuoteEntryID"
   , tnum = 299
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tQuoteRejectReason :: FIXTag
tQuoteRejectReason = FIXTag
   { tName = "QuoteRejectReason"
   , tnum = 300
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tQuoteResponseLevel :: FIXTag
tQuoteResponseLevel = FIXTag
   { tName = "QuoteResponseLevel"
   , tnum = 301
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tQuoteSetID :: FIXTag
tQuoteSetID = FIXTag
   { tName = "QuoteSetID"
   , tnum = 302
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tQuoteRequestType :: FIXTag
tQuoteRequestType = FIXTag
   { tName = "QuoteRequestType"
   , tnum = 303
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTotQuoteEntries :: FIXTag
tTotQuoteEntries = FIXTag
   { tName = "TotQuoteEntries"
   , tnum = 304
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tUnderlyingIDSource :: FIXTag
tUnderlyingIDSource = FIXTag
   { tName = "UnderlyingIDSource"
   , tnum = 305
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingIssuer :: FIXTag
tUnderlyingIssuer = FIXTag
   { tName = "UnderlyingIssuer"
   , tnum = 306
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingSecurityDesc :: FIXTag
tUnderlyingSecurityDesc = FIXTag
   { tName = "UnderlyingSecurityDesc"
   , tnum = 307
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingSecurityExchange :: FIXTag
tUnderlyingSecurityExchange = FIXTag
   { tName = "UnderlyingSecurityExchange"
   , tnum = 308
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingSecurityID :: FIXTag
tUnderlyingSecurityID = FIXTag
   { tName = "UnderlyingSecurityID"
   , tnum = 309
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingSecurityType :: FIXTag
tUnderlyingSecurityType = FIXTag
   { tName = "UnderlyingSecurityType"
   , tnum = 310
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingSymbol :: FIXTag
tUnderlyingSymbol = FIXTag
   { tName = "UnderlyingSymbol"
   , tnum = 311
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingSymbolSfx :: FIXTag
tUnderlyingSymbolSfx = FIXTag
   { tName = "UnderlyingSymbolSfx"
   , tnum = 312
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnderlyingMaturityMonthYear :: FIXTag
tUnderlyingMaturityMonthYear = FIXTag
   { tName = "UnderlyingMaturityMonthYear"
   , tnum = 313
   , tparser = toFIXMonthYear
   , genValue = FIXMonthYear <$> genMonthYear }

tUnderlyingMaturityDay :: FIXTag
tUnderlyingMaturityDay = FIXTag
   { tName = "UnderlyingMaturityDay"
   , tnum = 314
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tUnderlyingPutOrCall :: FIXTag
tUnderlyingPutOrCall = FIXTag
   { tName = "UnderlyingPutOrCall"
   , tnum = 315
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tUnderlyingStrikePrice :: FIXTag
tUnderlyingStrikePrice = FIXTag
   { tName = "UnderlyingStrikePrice"
   , tnum = 316
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tUnderlyingOptAttribute :: FIXTag
tUnderlyingOptAttribute = FIXTag
   { tName = "UnderlyingOptAttribute"
   , tnum = 317
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tUnderlyingCurrency :: FIXTag
tUnderlyingCurrency = FIXTag
   { tName = "UnderlyingCurrency"
   , tnum = 318
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tRatioQty :: FIXTag
tRatioQty = FIXTag
   { tName = "RatioQty"
   , tnum = 319
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSecurityReqID :: FIXTag
tSecurityReqID = FIXTag
   { tName = "SecurityReqID"
   , tnum = 320
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecurityRequestType :: FIXTag
tSecurityRequestType = FIXTag
   { tName = "SecurityRequestType"
   , tnum = 321
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSecurityResponseID :: FIXTag
tSecurityResponseID = FIXTag
   { tName = "SecurityResponseID"
   , tnum = 322
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSecurityResponseType :: FIXTag
tSecurityResponseType = FIXTag
   { tName = "SecurityResponseType"
   , tnum = 323
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSecurityStatusReqID :: FIXTag
tSecurityStatusReqID = FIXTag
   { tName = "SecurityStatusReqID"
   , tnum = 324
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tUnsolicitedIndicator :: FIXTag
tUnsolicitedIndicator = FIXTag
   { tName = "UnsolicitedIndicator"
   , tnum = 325
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tSecurityTradingStatus :: FIXTag
tSecurityTradingStatus = FIXTag
   { tName = "SecurityTradingStatus"
   , tnum = 326
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tHaltReasonChar :: FIXTag
tHaltReasonChar = FIXTag
   { tName = "HaltReasonChar"
   , tnum = 327
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tInViewOfCommon :: FIXTag
tInViewOfCommon = FIXTag
   { tName = "InViewOfCommon"
   , tnum = 328
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tDueToRelated :: FIXTag
tDueToRelated = FIXTag
   { tName = "DueToRelated"
   , tnum = 329
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tBuyVolume :: FIXTag
tBuyVolume = FIXTag
   { tName = "BuyVolume"
   , tnum = 330
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSellVolume :: FIXTag
tSellVolume = FIXTag
   { tName = "SellVolume"
   , tnum = 331
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tHighPx :: FIXTag
tHighPx = FIXTag
   { tName = "HighPx"
   , tnum = 332
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLowPx :: FIXTag
tLowPx = FIXTag
   { tName = "LowPx"
   , tnum = 333
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tAdjustment :: FIXTag
tAdjustment = FIXTag
   { tName = "Adjustment"
   , tnum = 334
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTradSesReqID :: FIXTag
tTradSesReqID = FIXTag
   { tName = "TradSesReqID"
   , tnum = 335
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTradingSessionID :: FIXTag
tTradingSessionID = FIXTag
   { tName = "TradingSessionID"
   , tnum = 336
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tContraTrader :: FIXTag
tContraTrader = FIXTag
   { tName = "ContraTrader"
   , tnum = 337
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTradSesMethod :: FIXTag
tTradSesMethod = FIXTag
   { tName = "TradSesMethod"
   , tnum = 338
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTradSesMode :: FIXTag
tTradSesMode = FIXTag
   { tName = "TradSesMode"
   , tnum = 339
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTradSesStatus :: FIXTag
tTradSesStatus = FIXTag
   { tName = "TradSesStatus"
   , tnum = 340
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTradSesStartTime :: FIXTag
tTradSesStartTime = FIXTag
   { tName = "TradSesStartTime"
   , tnum = 341
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tTradSesOpenTime :: FIXTag
tTradSesOpenTime = FIXTag
   { tName = "TradSesOpenTime"
   , tnum = 342
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tTradSesPreCloseTime :: FIXTag
tTradSesPreCloseTime = FIXTag
   { tName = "TradSesPreCloseTime"
   , tnum = 343
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tTradSesCloseTime :: FIXTag
tTradSesCloseTime = FIXTag
   { tName = "TradSesCloseTime"
   , tnum = 344
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tTradSesEndTime :: FIXTag
tTradSesEndTime = FIXTag
   { tName = "TradSesEndTime"
   , tnum = 345
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tNumberOfOrders :: FIXTag
tNumberOfOrders = FIXTag
   { tName = "NumberOfOrders"
   , tnum = 346
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMessageEncoding :: FIXTag
tMessageEncoding = FIXTag
   { tName = "MessageEncoding"
   , tnum = 347
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tEncodedIssuerLen :: FIXTag
tEncodedIssuerLen = FIXTag
   { tName = "EncodedIssuerLen"
   , tnum = 348
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedIssuer :: FIXTag
tEncodedIssuer = FIXTag
   { tName = "EncodedIssuer"
   , tnum = 349
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedSecurityDescLen :: FIXTag
tEncodedSecurityDescLen = FIXTag
   { tName = "EncodedSecurityDescLen"
   , tnum = 350
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedSecurityDesc :: FIXTag
tEncodedSecurityDesc = FIXTag
   { tName = "EncodedSecurityDesc"
   , tnum = 351
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedListExecInstLen :: FIXTag
tEncodedListExecInstLen = FIXTag
   { tName = "EncodedListExecInstLen"
   , tnum = 352
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedListExecInst :: FIXTag
tEncodedListExecInst = FIXTag
   { tName = "EncodedListExecInst"
   , tnum = 353
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedTextLen :: FIXTag
tEncodedTextLen = FIXTag
   { tName = "EncodedTextLen"
   , tnum = 354
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedText :: FIXTag
tEncodedText = FIXTag
   { tName = "EncodedText"
   , tnum = 355
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedSubjectLen :: FIXTag
tEncodedSubjectLen = FIXTag
   { tName = "EncodedSubjectLen"
   , tnum = 356
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedSubject :: FIXTag
tEncodedSubject = FIXTag
   { tName = "EncodedSubject"
   , tnum = 357
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedHeadlineLen :: FIXTag
tEncodedHeadlineLen = FIXTag
   { tName = "EncodedHeadlineLen"
   , tnum = 358
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedHeadline :: FIXTag
tEncodedHeadline = FIXTag
   { tName = "EncodedHeadline"
   , tnum = 359
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedAllocTextLen :: FIXTag
tEncodedAllocTextLen = FIXTag
   { tName = "EncodedAllocTextLen"
   , tnum = 360
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedAllocText :: FIXTag
tEncodedAllocText = FIXTag
   { tName = "EncodedAllocText"
   , tnum = 361
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedUnderlyingIssuerLen :: FIXTag
tEncodedUnderlyingIssuerLen = FIXTag
   { tName = "EncodedUnderlyingIssuerLen"
   , tnum = 362
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedUnderlyingIssuer :: FIXTag
tEncodedUnderlyingIssuer = FIXTag
   { tName = "EncodedUnderlyingIssuer"
   , tnum = 363
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tEncodedUnderlyingSecurityDescLen :: FIXTag
tEncodedUnderlyingSecurityDescLen = FIXTag
   { tName = "EncodedUnderlyingSecurityDescLen"
   , tnum = 364
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedUnderlyingSecurityDesc :: FIXTag
tEncodedUnderlyingSecurityDesc = FIXTag
   { tName = "EncodedUnderlyingSecurityDesc"
   , tnum = 365
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

tAllocPrice :: FIXTag
tAllocPrice = FIXTag
   { tName = "AllocPrice"
   , tnum = 366
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tQuoteSetValidUntilTime :: FIXTag
tQuoteSetValidUntilTime = FIXTag
   { tName = "QuoteSetValidUntilTime"
   , tnum = 367
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tQuoteEntryRejectReason :: FIXTag
tQuoteEntryRejectReason = FIXTag
   { tName = "QuoteEntryRejectReason"
   , tnum = 368
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tLastMsgSeqNumProcessed :: FIXTag
tLastMsgSeqNumProcessed = FIXTag
   { tName = "LastMsgSeqNumProcessed"
   , tnum = 369
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tOnBehalfOfSendingTime :: FIXTag
tOnBehalfOfSendingTime = FIXTag
   { tName = "OnBehalfOfSendingTime"
   , tnum = 370
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tRefTagID :: FIXTag
tRefTagID = FIXTag
   { tName = "RefTagID"
   , tnum = 371
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tRefMsgType :: FIXTag
tRefMsgType = FIXTag
   { tName = "RefMsgType"
   , tnum = 372
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSessionRejectReason :: FIXTag
tSessionRejectReason = FIXTag
   { tName = "SessionRejectReason"
   , tnum = 373
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tBidRequestTransType :: FIXTag
tBidRequestTransType = FIXTag
   { tName = "BidRequestTransType"
   , tnum = 374
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tContraBroker :: FIXTag
tContraBroker = FIXTag
   { tName = "ContraBroker"
   , tnum = 375
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tComplianceID :: FIXTag
tComplianceID = FIXTag
   { tName = "ComplianceID"
   , tnum = 376
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSolicitedFlag :: FIXTag
tSolicitedFlag = FIXTag
   { tName = "SolicitedFlag"
   , tnum = 377
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tExecRestatementReason :: FIXTag
tExecRestatementReason = FIXTag
   { tName = "ExecRestatementReason"
   , tnum = 378
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tBusinessRejectRefID :: FIXTag
tBusinessRejectRefID = FIXTag
   { tName = "BusinessRejectRefID"
   , tnum = 379
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tBusinessRejectReason :: FIXTag
tBusinessRejectReason = FIXTag
   { tName = "BusinessRejectReason"
   , tnum = 380
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tGrossTradeAmt :: FIXTag
tGrossTradeAmt = FIXTag
   { tName = "GrossTradeAmt"
   , tnum = 381
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tNoContraBrokers :: FIXTag
tNoContraBrokers = FIXTag
   { tName = "NoContraBrokers"
   , tnum = 382
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMaxMessageSize :: FIXTag
tMaxMessageSize = FIXTag
   { tName = "MaxMessageSize"
   , tnum = 383
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNoMsgTypes :: FIXTag
tNoMsgTypes = FIXTag
   { tName = "NoMsgTypes"
   , tnum = 384
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMsgDirection :: FIXTag
tMsgDirection = FIXTag
   { tName = "MsgDirection"
   , tnum = 385
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tNoTradingSessions :: FIXTag
tNoTradingSessions = FIXTag
   { tName = "NoTradingSessions"
   , tnum = 386
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTotalVolumeTraded :: FIXTag
tTotalVolumeTraded = FIXTag
   { tName = "TotalVolumeTraded"
   , tnum = 387
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tDiscretionInst :: FIXTag
tDiscretionInst = FIXTag
   { tName = "DiscretionInst"
   , tnum = 388
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tDiscretionOffset :: FIXTag
tDiscretionOffset = FIXTag
   { tName = "DiscretionOffset"
   , tnum = 389
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tBidID :: FIXTag
tBidID = FIXTag
   { tName = "BidID"
   , tnum = 390
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tClientBidID :: FIXTag
tClientBidID = FIXTag
   { tName = "ClientBidID"
   , tnum = 391
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tListName :: FIXTag
tListName = FIXTag
   { tName = "ListName"
   , tnum = 392
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTotalNumSecurities :: FIXTag
tTotalNumSecurities = FIXTag
   { tName = "TotalNumSecurities"
   , tnum = 393
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tBidType :: FIXTag
tBidType = FIXTag
   { tName = "BidType"
   , tnum = 394
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNumTickets :: FIXTag
tNumTickets = FIXTag
   { tName = "NumTickets"
   , tnum = 395
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tSideValue1 :: FIXTag
tSideValue1 = FIXTag
   { tName = "SideValue1"
   , tnum = 396
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tSideValue2 :: FIXTag
tSideValue2 = FIXTag
   { tName = "SideValue2"
   , tnum = 397
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tNoBidDescriptors :: FIXTag
tNoBidDescriptors = FIXTag
   { tName = "NoBidDescriptors"
   , tnum = 398
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tBidDescriptorType :: FIXTag
tBidDescriptorType = FIXTag
   { tName = "BidDescriptorType"
   , tnum = 399
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tBidDescriptor :: FIXTag
tBidDescriptor = FIXTag
   { tName = "BidDescriptor"
   , tnum = 400
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tSideValueInd :: FIXTag
tSideValueInd = FIXTag
   { tName = "SideValueInd"
   , tnum = 401
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tLiquidityPctLow :: FIXTag
tLiquidityPctLow = FIXTag
   { tName = "LiquidityPctLow"
   , tnum = 402
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLiquidityPctHigh :: FIXTag
tLiquidityPctHigh = FIXTag
   { tName = "LiquidityPctHigh"
   , tnum = 403
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLiquidityValue :: FIXTag
tLiquidityValue = FIXTag
   { tName = "LiquidityValue"
   , tnum = 404
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tEFPTrackingError :: FIXTag
tEFPTrackingError = FIXTag
   { tName = "EFPTrackingError"
   , tnum = 405
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tFairValue :: FIXTag
tFairValue = FIXTag
   { tName = "FairValue"
   , tnum = 406
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tOutsideIndexPct :: FIXTag
tOutsideIndexPct = FIXTag
   { tName = "OutsideIndexPct"
   , tnum = 407
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tValueOfFutures :: FIXTag
tValueOfFutures = FIXTag
   { tName = "ValueOfFutures"
   , tnum = 408
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tLiquidityIndType :: FIXTag
tLiquidityIndType = FIXTag
   { tName = "LiquidityIndType"
   , tnum = 409
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tWtAverageLiquidity :: FIXTag
tWtAverageLiquidity = FIXTag
   { tName = "WtAverageLiquidity"
   , tnum = 410
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tExchangeForPhysical :: FIXTag
tExchangeForPhysical = FIXTag
   { tName = "ExchangeForPhysical"
   , tnum = 411
   , tparser = toFIXBool
   , genValue = FIXBool <$> arbitrary }

tOutMainCntryUIndex :: FIXTag
tOutMainCntryUIndex = FIXTag
   { tName = "OutMainCntryUIndex"
   , tnum = 412
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tCrossPercent :: FIXTag
tCrossPercent = FIXTag
   { tName = "CrossPercent"
   , tnum = 413
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tProgRptReqs :: FIXTag
tProgRptReqs = FIXTag
   { tName = "ProgRptReqs"
   , tnum = 414
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tProgPeriodInterval :: FIXTag
tProgPeriodInterval = FIXTag
   { tName = "ProgPeriodInterval"
   , tnum = 415
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tIncTaxInd :: FIXTag
tIncTaxInd = FIXTag
   { tName = "IncTaxInd"
   , tnum = 416
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNumBidders :: FIXTag
tNumBidders = FIXTag
   { tName = "NumBidders"
   , tnum = 417
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tTradeType :: FIXTag
tTradeType = FIXTag
   { tName = "TradeType"
   , tnum = 418
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tBasisPxType :: FIXTag
tBasisPxType = FIXTag
   { tName = "BasisPxType"
   , tnum = 419
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tNoBidComponents :: FIXTag
tNoBidComponents = FIXTag
   { tName = "NoBidComponents"
   , tnum = 420
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tCountry :: FIXTag
tCountry = FIXTag
   { tName = "Country"
   , tnum = 421
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tTotNoStrikes :: FIXTag
tTotNoStrikes = FIXTag
   { tName = "TotNoStrikes"
   , tnum = 422
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tPriceType :: FIXTag
tPriceType = FIXTag
   { tName = "PriceType"
   , tnum = 423
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tDayOrderQty :: FIXTag
tDayOrderQty = FIXTag
   { tName = "DayOrderQty"
   , tnum = 424
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tDayCumQty :: FIXTag
tDayCumQty = FIXTag
   { tName = "DayCumQty"
   , tnum = 425
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tDayAvgPx :: FIXTag
tDayAvgPx = FIXTag
   { tName = "DayAvgPx"
   , tnum = 426
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tGTBookingInst :: FIXTag
tGTBookingInst = FIXTag
   { tName = "GTBookingInst"
   , tnum = 427
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNoStrikes :: FIXTag
tNoStrikes = FIXTag
   { tName = "NoStrikes"
   , tnum = 428
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tListStatusType :: FIXTag
tListStatusType = FIXTag
   { tName = "ListStatusType"
   , tnum = 429
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tNetGrossInd :: FIXTag
tNetGrossInd = FIXTag
   { tName = "NetGrossInd"
   , tnum = 430
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tListOrderStatus :: FIXTag
tListOrderStatus = FIXTag
   { tName = "ListOrderStatus"
   , tnum = 431
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tExpireDate :: FIXTag
tExpireDate = FIXTag
   { tName = "ExpireDate"
   , tnum = 432
   , tparser = toFIXDateOnly
   , genValue = FIXDateOnly <$> genDay }

tListExecInstType :: FIXTag
tListExecInstType = FIXTag
   { tName = "ListExecInstType"
   , tnum = 433
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tCxlRejResponseTo :: FIXTag
tCxlRejResponseTo = FIXTag
   { tName = "CxlRejResponseTo"
   , tnum = 434
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tUnderlyingCouponRate :: FIXTag
tUnderlyingCouponRate = FIXTag
   { tName = "UnderlyingCouponRate"
   , tnum = 435
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tUnderlyingContractMultiplier :: FIXTag
tUnderlyingContractMultiplier = FIXTag
   { tName = "UnderlyingContractMultiplier"
   , tnum = 436
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tContraTradeQty :: FIXTag
tContraTradeQty = FIXTag
   { tName = "ContraTradeQty"
   , tnum = 437
   , tparser = toFIXDouble
   , genValue = FIXDouble <$> (return (-2.112 :: Double)) }

tContraTradeTime :: FIXTag
tContraTradeTime = FIXTag
   { tName = "ContraTradeTime"
   , tnum = 438
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tClearingFirm :: FIXTag
tClearingFirm = FIXTag
   { tName = "ClearingFirm"
   , tnum = 439
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tClearingAccount :: FIXTag
tClearingAccount = FIXTag
   { tName = "ClearingAccount"
   , tnum = 440
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tLiquidityNumSecurities :: FIXTag
tLiquidityNumSecurities = FIXTag
   { tName = "LiquidityNumSecurities"
   , tnum = 441
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tMultiLegReportingType :: FIXTag
tMultiLegReportingType = FIXTag
   { tName = "MultiLegReportingType"
   , tnum = 442
   , tparser = toFIXChar
   , genValue = FIXChar <$> (return 'A') }

tStrikeTime :: FIXTag
tStrikeTime = FIXTag
   { tName = "StrikeTime"
   , tnum = 443
   , tparser = toFIXTimestamp
   , genValue = FIXTimestamp <$> genUTCTime }

tListStatusText :: FIXTag
tListStatusText = FIXTag
   { tName = "ListStatusText"
   , tnum = 444
   , tparser = toFIXString
   , genValue = FIXString <$> genByteString }

tEncodedListStatusTextLen :: FIXTag
tEncodedListStatusTextLen = FIXTag
   { tName = "EncodedListStatusTextLen"
   , tnum = 445
   , tparser = toFIXInt
   , genValue = FIXInt <$> arbitrary }

tEncodedListStatusText :: FIXTag
tEncodedListStatusText = FIXTag
   { tName = "EncodedListStatusText"
   , tnum = 446
   , tparser = toFIXData
   , genValue = FIXData <$> genByteString }

headerFIX42 :: IntMap FIXTag
headerFIX42 =
   LT.insert (tnum tSenderCompID) tSenderCompID $
   LT.insert (tnum tTargetCompID) tTargetCompID $
   LT.insert (tnum tOnBehalfOfCompID) tOnBehalfOfCompID $
   LT.insert (tnum tDeliverToCompID) tDeliverToCompID $
   LT.insert (tnum tSecureDataLen) tSecureDataLen $
   LT.insert (tnum tSecureData) tSecureData $
   LT.insert (tnum tMsgSeqNum) tMsgSeqNum $
   LT.insert (tnum tSenderSubID) tSenderSubID $
   LT.insert (tnum tSenderLocationID) tSenderLocationID $
   LT.insert (tnum tTargetSubID) tTargetSubID $
   LT.insert (tnum tTargetLocationID) tTargetLocationID $
   LT.insert (tnum tOnBehalfOfSubID) tOnBehalfOfSubID $
   LT.insert (tnum tOnBehalfOfLocationID) tOnBehalfOfLocationID $
   LT.insert (tnum tDeliverToSubID) tDeliverToSubID $
   LT.insert (tnum tDeliverToLocationID) tDeliverToLocationID $
   LT.insert (tnum tPossDupFlag) tPossDupFlag $
   LT.insert (tnum tPossResend) tPossResend $
   LT.insert (tnum tSendingTime) tSendingTime $
   LT.insert (tnum tOrigSendingTime) tOrigSendingTime $
   LT.insert (tnum tXmlDataLen) tXmlDataLen $
   LT.insert (tnum tXmlData) tXmlData $
   LT.insert (tnum tMessageEncoding) tMessageEncoding $
   LT.insert (tnum tLastMsgSeqNumProcessed) tLastMsgSeqNumProcessed $
   LT.insert (tnum tOnBehalfOfSendingTime) tOnBehalfOfSendingTime    LT.new


trailerFIX42 :: IntMap FIXTag
trailerFIX42 =
   LT.insert (tnum tSignatureLength) tSignatureLength $
   LT.insert (tnum tSignature) tSignature    LT.new


mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec
   { msName = "Heartbeat"
   , msType = "0"
   , msHeader = headerFIX42
   , msBody = mHeartbeatBody
   , msTrailer = trailerFIX42 }
   where
   mHeartbeatBody =
      LT.insert (tnum tTestReqID) tTestReqID       LT.new


mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec
   { msName = "TestRequest"
   , msType = "1"
   , msHeader = headerFIX42
   , msBody = mTestRequestBody
   , msTrailer = trailerFIX42 }
   where
   mTestRequestBody =
      LT.insert (tnum tTestReqID) tTestReqID       LT.new


mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec
   { msName = "ResendRequest"
   , msType = "2"
   , msHeader = headerFIX42
   , msBody = mResendRequestBody
   , msTrailer = trailerFIX42 }
   where
   mResendRequestBody =
      LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
      LT.insert (tnum tEndSeqNo) tEndSeqNo       LT.new


mReject :: FIXMessageSpec
mReject = FMSpec
   { msName = "Reject"
   , msType = "3"
   , msHeader = headerFIX42
   , msBody = mRejectBody
   , msTrailer = trailerFIX42 }
   where
   mRejectBody =
      LT.insert (tnum tRefSeqNum) tRefSeqNum $
      LT.insert (tnum tRefTagID) tRefTagID $
      LT.insert (tnum tRefMsgType) tRefMsgType $
      LT.insert (tnum tSessionRejectReason) tSessionRejectReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mSequenceReset :: FIXMessageSpec
mSequenceReset = FMSpec
   { msName = "SequenceReset"
   , msType = "4"
   , msHeader = headerFIX42
   , msBody = mSequenceResetBody
   , msTrailer = trailerFIX42 }
   where
   mSequenceResetBody =
      LT.insert (tnum tGapFillFlag) tGapFillFlag $
      LT.insert (tnum tNewSeqNo) tNewSeqNo       LT.new


mLogout :: FIXMessageSpec
mLogout = FMSpec
   { msName = "Logout"
   , msType = "5"
   , msHeader = headerFIX42
   , msBody = mLogoutBody
   , msTrailer = trailerFIX42 }
   where
   mLogoutBody =
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mIndicationofInterest :: FIXMessageSpec
mIndicationofInterest = FMSpec
   { msName = "IndicationofInterest"
   , msType = "6"
   , msHeader = headerFIX42
   , msBody = mIndicationofInterestBody
   , msTrailer = trailerFIX42 }
   where
   mIndicationofInterestBody =
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tIOITransType) tIOITransType $
      LT.insert (tnum tIOIRefID) tIOIRefID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tIOIShares) tIOIShares $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tValidUntilTime) tValidUntilTime $
      LT.insert (tnum tIOIQltyInd) tIOIQltyInd $
      LT.insert (tnum tIOINaturalFlag) tIOINaturalFlag $
      LT.insert (tnum tNoIOIQualifiers) gNoIOIQualifiers''' $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tNoRoutingIDs) gNoRoutingIDs''' $
      LT.insert (tnum tSpreadToBenchmark) tSpreadToBenchmark $
      LT.insert (tnum tBenchmark) tBenchmark       LT.new
      where
         gNoIOIQualifiers''' = FIXTag
            { tName = "NoIOIQualifiers"
            , tnum = tnum tNoIOIQualifiers
            , tparser = gNoIOIQualifiersP'''
            , genValue = genFIXGroup gNoIOIQualifiersSpec''' }

         gNoIOIQualifiersP''' = groupP gNoIOIQualifiersSpec'''
         gNoIOIQualifiersSpec''' = FGSpec
            { gsLength = tNoIOIQualifiers
            , gsSeparator = tIOIQualifier
            , gsBody = gNoIOIQualifiersBody''' }
            where
            gNoIOIQualifiersBody''' =
               LT.new

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP'''
            , genValue = genFIXGroup gNoRoutingIDsSpec''' }

         gNoRoutingIDsP''' = groupP gNoRoutingIDsSpec'''
         gNoRoutingIDsSpec''' = FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeparator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' =
               LT.insert (tnum tRoutingID) tRoutingID                LT.new



mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec
   { msName = "Advertisement"
   , msType = "7"
   , msHeader = headerFIX42
   , msBody = mAdvertisementBody
   , msTrailer = trailerFIX42 }
   where
   mAdvertisementBody =
      LT.insert (tnum tAdvId) tAdvId $
      LT.insert (tnum tAdvTransType) tAdvTransType $
      LT.insert (tnum tAdvRefID) tAdvRefID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tAdvSide) tAdvSide $
      LT.insert (tnum tShares) tShares $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID       LT.new


mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec
   { msName = "ExecutionReport"
   , msType = "8"
   , msHeader = headerFIX42
   , msBody = mExecutionReportBody
   , msTrailer = trailerFIX42 }
   where
   mExecutionReportBody =
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tNoContraBrokers) gNoContraBrokers''' $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tExecTransType) tExecTransType $
      LT.insert (tnum tExecRefID) tExecRefID $
      LT.insert (tnum tExecType) tExecType $
      LT.insert (tnum tOrdStatus) tOrdStatus $
      LT.insert (tnum tOrdRejReason) tOrdRejReason $
      LT.insert (tnum tExecRestatementReason) tExecRestatementReason $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tLastShares) tLastShares $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tLastSpotRate) tLastSpotRate $
      LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tLastCapacity) tLastCapacity $
      LT.insert (tnum tLeavesQty) tLeavesQty $
      LT.insert (tnum tCumQty) tCumQty $
      LT.insert (tnum tAvgPx) tAvgPx $
      LT.insert (tnum tDayOrderQty) tDayOrderQty $
      LT.insert (tnum tDayCumQty) tDayCumQty $
      LT.insert (tnum tDayAvgPx) tDayAvgPx $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tReportToExch) tReportToExch $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
      LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
      LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tClearingFirm) tClearingFirm $
      LT.insert (tnum tClearingAccount) tClearingAccount $
      LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType       LT.new
      where
         gNoContraBrokers''' = FIXTag
            { tName = "NoContraBrokers"
            , tnum = tnum tNoContraBrokers
            , tparser = gNoContraBrokersP'''
            , genValue = genFIXGroup gNoContraBrokersSpec''' }

         gNoContraBrokersP''' = groupP gNoContraBrokersSpec'''
         gNoContraBrokersSpec''' = FGSpec
            { gsLength = tNoContraBrokers
            , gsSeparator = tContraBroker
            , gsBody = gNoContraBrokersBody''' }
            where
            gNoContraBrokersBody''' =
               LT.insert (tnum tContraTrader) tContraTrader $
               LT.insert (tnum tContraTradeQty) tContraTradeQty $
               LT.insert (tnum tContraTradeTime) tContraTradeTime                LT.new



mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec
   { msName = "OrderCancelReject"
   , msType = "9"
   , msHeader = headerFIX42
   , msBody = mOrderCancelRejectBody
   , msTrailer = trailerFIX42 }
   where
   mOrderCancelRejectBody =
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tOrdStatus) tOrdStatus $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tCxlRejResponseTo) tCxlRejResponseTo $
      LT.insert (tnum tCxlRejReason) tCxlRejReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mLogon :: FIXMessageSpec
mLogon = FMSpec
   { msName = "Logon"
   , msType = "A"
   , msHeader = headerFIX42
   , msBody = mLogonBody
   , msTrailer = trailerFIX42 }
   where
   mLogonBody =
      LT.insert (tnum tEncryptMethod) tEncryptMethod $
      LT.insert (tnum tHeartBtInt) tHeartBtInt $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData $
      LT.insert (tnum tResetSeqNumFlag) tResetSeqNumFlag $
      LT.insert (tnum tMaxMessageSize) tMaxMessageSize $
      LT.insert (tnum tNoMsgTypes) gNoMsgTypes'''       LT.new
      where
         gNoMsgTypes''' = FIXTag
            { tName = "NoMsgTypes"
            , tnum = tnum tNoMsgTypes
            , tparser = gNoMsgTypesP'''
            , genValue = genFIXGroup gNoMsgTypesSpec''' }

         gNoMsgTypesP''' = groupP gNoMsgTypesSpec'''
         gNoMsgTypesSpec''' = FGSpec
            { gsLength = tNoMsgTypes
            , gsSeparator = tRefMsgType
            , gsBody = gNoMsgTypesBody''' }
            where
            gNoMsgTypesBody''' =
               LT.insert (tnum tMsgDirection) tMsgDirection                LT.new



mNews :: FIXMessageSpec
mNews = FMSpec
   { msName = "News"
   , msType = "B"
   , msHeader = headerFIX42
   , msBody = mNewsBody
   , msTrailer = trailerFIX42 }
   where
   mNewsBody =
      LT.insert (tnum tOrigTime) tOrigTime $
      LT.insert (tnum tUrgency) tUrgency $
      LT.insert (tnum tHeadline) tHeadline $
      LT.insert (tnum tEncodedHeadlineLen) tEncodedHeadlineLen $
      LT.insert (tnum tEncodedHeadline) tEncodedHeadline $
      LT.insert (tnum tNoRoutingIDs) gNoRoutingIDs''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tLinesOfText) gLinesOfText''' $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData       LT.new
      where
         gLinesOfText''' = FIXTag
            { tName = "LinesOfText"
            , tnum = tnum tLinesOfText
            , tparser = gLinesOfTextP'''
            , genValue = genFIXGroup gLinesOfTextSpec''' }

         gLinesOfTextP''' = groupP gLinesOfTextSpec'''
         gLinesOfTextSpec''' = FGSpec
            { gsLength = tLinesOfText
            , gsSeparator = tText
            , gsBody = gLinesOfTextBody''' }
            where
            gLinesOfTextBody''' =
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , genValue = genFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeparator = tRelatdSym
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' =
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc                LT.new

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP'''
            , genValue = genFIXGroup gNoRoutingIDsSpec''' }

         gNoRoutingIDsP''' = groupP gNoRoutingIDsSpec'''
         gNoRoutingIDsSpec''' = FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeparator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' =
               LT.insert (tnum tRoutingID) tRoutingID                LT.new



mEmail :: FIXMessageSpec
mEmail = FMSpec
   { msName = "Email"
   , msType = "C"
   , msHeader = headerFIX42
   , msBody = mEmailBody
   , msTrailer = trailerFIX42 }
   where
   mEmailBody =
      LT.insert (tnum tEmailThreadID) tEmailThreadID $
      LT.insert (tnum tEmailType) tEmailType $
      LT.insert (tnum tOrigTime) tOrigTime $
      LT.insert (tnum tSubject) tSubject $
      LT.insert (tnum tEncodedSubjectLen) tEncodedSubjectLen $
      LT.insert (tnum tEncodedSubject) tEncodedSubject $
      LT.insert (tnum tNoRoutingIDs) gNoRoutingIDs''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tLinesOfText) gLinesOfText''' $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData       LT.new
      where
         gLinesOfText''' = FIXTag
            { tName = "LinesOfText"
            , tnum = tnum tLinesOfText
            , tparser = gLinesOfTextP'''
            , genValue = genFIXGroup gLinesOfTextSpec''' }

         gLinesOfTextP''' = groupP gLinesOfTextSpec'''
         gLinesOfTextSpec''' = FGSpec
            { gsLength = tLinesOfText
            , gsSeparator = tText
            , gsBody = gLinesOfTextBody''' }
            where
            gLinesOfTextBody''' =
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , genValue = genFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeparator = tRelatdSym
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' =
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc                LT.new

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP'''
            , genValue = genFIXGroup gNoRoutingIDsSpec''' }

         gNoRoutingIDsP''' = groupP gNoRoutingIDsSpec'''
         gNoRoutingIDsSpec''' = FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeparator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' =
               LT.insert (tnum tRoutingID) tRoutingID                LT.new



mNewOrderSingle :: FIXMessageSpec
mNewOrderSingle = FMSpec
   { msName = "NewOrderSingle"
   , msType = "D"
   , msHeader = headerFIX42
   , msBody = mNewOrderSingleBody
   , msTrailer = trailerFIX42 }
   where
   mNewOrderSingleBody =
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tProcessCode) tProcessCode $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tPrevClosePx) tPrevClosePx $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tClearingFirm) tClearingFirm $
      LT.insert (tnum tClearingAccount) tClearingAccount       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , genValue = genFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeparator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' =
               LT.insert (tnum tAllocShares) tAllocShares                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , genValue = genFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeparator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' =
               LT.new



mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec
   { msName = "NewOrderList"
   , msType = "E"
   , msHeader = headerFIX42
   , msBody = mNewOrderListBody
   , msTrailer = trailerFIX42 }
   where
   mNewOrderListBody =
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tProgRptReqs) tProgRptReqs $
      LT.insert (tnum tBidType) tBidType $
      LT.insert (tnum tProgPeriodInterval) tProgPeriodInterval $
      LT.insert (tnum tListExecInstType) tListExecInstType $
      LT.insert (tnum tListExecInst) tListExecInst $
      LT.insert (tnum tEncodedListExecInstLen) tEncodedListExecInstLen $
      LT.insert (tnum tEncodedListExecInst) tEncodedListExecInst $
      LT.insert (tnum tTotNoOrders) tTotNoOrders $
      LT.insert (tnum tNoOrders) gNoOrders'''       LT.new
      where
         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP'''
            , genValue = genFIXGroup gNoOrdersSpec''' }

         gNoOrdersP''' = groupP gNoOrdersSpec'''
         gNoOrdersSpec''' = FGSpec
            { gsLength = tNoOrders
            , gsSeparator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' =
               LT.insert (tnum tListSeqNo) tListSeqNo $
               LT.insert (tnum tSettlInstMode) tSettlInstMode $
               LT.insert (tnum tClientID) tClientID $
               LT.insert (tnum tExecBroker) tExecBroker $
               LT.insert (tnum tAccount) tAccount $
               LT.insert (tnum tNoAllocs) gNoAllocs'''''' $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tHandlInst) tHandlInst $
               LT.insert (tnum tExecInst) tExecInst $
               LT.insert (tnum tMinQty) tMinQty $
               LT.insert (tnum tMaxFloor) tMaxFloor $
               LT.insert (tnum tExDestination) tExDestination $
               LT.insert (tnum tNoTradingSessions) gNoTradingSessions'''''' $
               LT.insert (tnum tProcessCode) tProcessCode $
               LT.insert (tnum tSymbol) tSymbol $
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tSideValueInd) tSideValueInd $
               LT.insert (tnum tLocateReqd) tLocateReqd $
               LT.insert (tnum tTransactTime) tTransactTime $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tOrdType) tOrdType $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tStopPx) tStopPx $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tComplianceID) tComplianceID $
               LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
               LT.insert (tnum tIOIid) tIOIid $
               LT.insert (tnum tQuoteID) tQuoteID $
               LT.insert (tnum tTimeInForce) tTimeInForce $
               LT.insert (tnum tEffectiveTime) tEffectiveTime $
               LT.insert (tnum tExpireDate) tExpireDate $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tGTBookingInst) tGTBookingInst $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tRule80A) tRule80A $
               LT.insert (tnum tForexReq) tForexReq $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tOpenClose) tOpenClose $
               LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
               LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
               LT.insert (tnum tMaxShow) tMaxShow $
               LT.insert (tnum tPegDifference) tPegDifference $
               LT.insert (tnum tDiscretionInst) tDiscretionInst $
               LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
               LT.insert (tnum tClearingFirm) tClearingFirm $
               LT.insert (tnum tClearingAccount) tClearingAccount                LT.new
               where
                  gNoAllocs'''''' = FIXTag
                     { tName = "NoAllocs"
                     , tnum = tnum tNoAllocs
                     , tparser = gNoAllocsP''''''
                     , genValue = genFIXGroup gNoAllocsSpec'''''' }

                  gNoAllocsP'''''' = groupP gNoAllocsSpec''''''
                  gNoAllocsSpec'''''' = FGSpec
                     { gsLength = tNoAllocs
                     , gsSeparator = tAllocAccount
                     , gsBody = gNoAllocsBody'''''' }
                     where
                     gNoAllocsBody'''''' =
                        LT.insert (tnum tAllocShares) tAllocShares                         LT.new

                  gNoTradingSessions'''''' = FIXTag
                     { tName = "NoTradingSessions"
                     , tnum = tnum tNoTradingSessions
                     , tparser = gNoTradingSessionsP''''''
                     , genValue = genFIXGroup gNoTradingSessionsSpec'''''' }

                  gNoTradingSessionsP'''''' = groupP gNoTradingSessionsSpec''''''
                  gNoTradingSessionsSpec'''''' = FGSpec
                     { gsLength = tNoTradingSessions
                     , gsSeparator = tTradingSessionID
                     , gsBody = gNoTradingSessionsBody'''''' }
                     where
                     gNoTradingSessionsBody'''''' =
                        LT.new




mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec
   { msName = "OrderCancelRequest"
   , msType = "F"
   , msHeader = headerFIX42
   , msBody = mOrderCancelRequestBody
   , msTrailer = trailerFIX42 }
   where
   mOrderCancelRequestBody =
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec
   { msName = "OrderCancelReplaceRequest"
   , msType = "G"
   , msHeader = headerFIX42
   , msBody = mOrderCancelReplaceRequestBody
   , msTrailer = trailerFIX42 }
   where
   mOrderCancelReplaceRequestBody =
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tClearingFirm) tClearingFirm $
      LT.insert (tnum tClearingAccount) tClearingAccount       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , genValue = genFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeparator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' =
               LT.insert (tnum tAllocShares) tAllocShares                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , genValue = genFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeparator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' =
               LT.new



mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec
   { msName = "OrderStatusRequest"
   , msType = "H"
   , msHeader = headerFIX42
   , msBody = mOrderStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mOrderStatusRequestBody =
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide       LT.new


mAllocation :: FIXMessageSpec
mAllocation = FMSpec
   { msName = "Allocation"
   , msType = "J"
   , msHeader = headerFIX42
   , msBody = mAllocationBody
   , msTrailer = trailerFIX42 }
   where
   mAllocationBody =
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tAllocTransType) tAllocTransType $
      LT.insert (tnum tRefAllocID) tRefAllocID $
      LT.insert (tnum tAllocLinkID) tAllocLinkID $
      LT.insert (tnum tAllocLinkType) tAllocLinkType $
      LT.insert (tnum tNoOrders) gNoOrders''' $
      LT.insert (tnum tNoExecs) gNoExecs''' $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tShares) tShares $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tAvgPx) tAvgPx $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tAvgPrxPrecision) tAvgPrxPrecision $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
      LT.insert (tnum tNetMoney) tNetMoney $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tNoAllocs) gNoAllocs'''       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , genValue = genFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeparator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' =
               LT.insert (tnum tAllocPrice) tAllocPrice $
               LT.insert (tnum tAllocShares) tAllocShares $
               LT.insert (tnum tProcessCode) tProcessCode $
               LT.insert (tnum tBrokerOfCredit) tBrokerOfCredit $
               LT.insert (tnum tNotifyBrokerOfCredit) tNotifyBrokerOfCredit $
               LT.insert (tnum tAllocHandlInst) tAllocHandlInst $
               LT.insert (tnum tAllocText) tAllocText $
               LT.insert (tnum tEncodedAllocTextLen) tEncodedAllocTextLen $
               LT.insert (tnum tEncodedAllocText) tEncodedAllocText $
               LT.insert (tnum tExecBroker) tExecBroker $
               LT.insert (tnum tClientID) tClientID $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tAllocAvgPx) tAllocAvgPx $
               LT.insert (tnum tAllocNetMoney) tAllocNetMoney $
               LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
               LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
               LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
               LT.insert (tnum tSettlInstMode) tSettlInstMode $
               LT.insert (tnum tNoMiscFees) gNoMiscFees''''''                LT.new
               where
                  gNoMiscFees'''''' = FIXTag
                     { tName = "NoMiscFees"
                     , tnum = tnum tNoMiscFees
                     , tparser = gNoMiscFeesP''''''
                     , genValue = genFIXGroup gNoMiscFeesSpec'''''' }

                  gNoMiscFeesP'''''' = groupP gNoMiscFeesSpec''''''
                  gNoMiscFeesSpec'''''' = FGSpec
                     { gsLength = tNoMiscFees
                     , gsSeparator = tMiscFeeAmt
                     , gsBody = gNoMiscFeesBody'''''' }
                     where
                     gNoMiscFeesBody'''''' =
                        LT.insert (tnum tMiscFeeCurr) tMiscFeeCurr $
                        LT.insert (tnum tMiscFeeType) tMiscFeeType                         LT.new


         gNoExecs''' = FIXTag
            { tName = "NoExecs"
            , tnum = tnum tNoExecs
            , tparser = gNoExecsP'''
            , genValue = genFIXGroup gNoExecsSpec''' }

         gNoExecsP''' = groupP gNoExecsSpec'''
         gNoExecsSpec''' = FGSpec
            { gsLength = tNoExecs
            , gsSeparator = tLastShares
            , gsBody = gNoExecsBody''' }
            where
            gNoExecsBody''' =
               LT.insert (tnum tExecID) tExecID $
               LT.insert (tnum tLastPx) tLastPx $
               LT.insert (tnum tLastCapacity) tLastCapacity                LT.new

         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP'''
            , genValue = genFIXGroup gNoOrdersSpec''' }

         gNoOrdersP''' = groupP gNoOrdersSpec'''
         gNoOrdersSpec''' = FGSpec
            { gsLength = tNoOrders
            , gsSeparator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' =
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
               LT.insert (tnum tListID) tListID $
               LT.insert (tnum tWaveNo) tWaveNo                LT.new



mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec
   { msName = "ListCancelRequest"
   , msType = "K"
   , msHeader = headerFIX42
   , msBody = mListCancelRequestBody
   , msTrailer = trailerFIX42 }
   where
   mListCancelRequestBody =
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListExecute :: FIXMessageSpec
mListExecute = FMSpec
   { msName = "ListExecute"
   , msType = "L"
   , msHeader = headerFIX42
   , msBody = mListExecuteBody
   , msTrailer = trailerFIX42 }
   where
   mListExecuteBody =
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListStatusRequest :: FIXMessageSpec
mListStatusRequest = FMSpec
   { msName = "ListStatusRequest"
   , msType = "M"
   , msHeader = headerFIX42
   , msBody = mListStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mListStatusRequestBody =
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListStatus :: FIXMessageSpec
mListStatus = FMSpec
   { msName = "ListStatus"
   , msType = "N"
   , msHeader = headerFIX42
   , msBody = mListStatusBody
   , msTrailer = trailerFIX42 }
   where
   mListStatusBody =
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tListStatusType) tListStatusType $
      LT.insert (tnum tNoRpts) tNoRpts $
      LT.insert (tnum tListOrderStatus) tListOrderStatus $
      LT.insert (tnum tRptSeq) tRptSeq $
      LT.insert (tnum tListStatusText) tListStatusText $
      LT.insert (tnum tEncodedListStatusTextLen) tEncodedListStatusTextLen $
      LT.insert (tnum tEncodedListStatusText) tEncodedListStatusText $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tTotNoOrders) tTotNoOrders $
      LT.insert (tnum tNoOrders) gNoOrders'''       LT.new
      where
         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP'''
            , genValue = genFIXGroup gNoOrdersSpec''' }

         gNoOrdersP''' = groupP gNoOrdersSpec'''
         gNoOrdersSpec''' = FGSpec
            { gsLength = tNoOrders
            , gsSeparator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' =
               LT.insert (tnum tCumQty) tCumQty $
               LT.insert (tnum tOrdStatus) tOrdStatus $
               LT.insert (tnum tLeavesQty) tLeavesQty $
               LT.insert (tnum tCxlQty) tCxlQty $
               LT.insert (tnum tAvgPx) tAvgPx $
               LT.insert (tnum tOrdRejReason) tOrdRejReason $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mAllocationACK :: FIXMessageSpec
mAllocationACK = FMSpec
   { msName = "AllocationACK"
   , msType = "P"
   , msHeader = headerFIX42
   , msBody = mAllocationACKBody
   , msTrailer = trailerFIX42 }
   where
   mAllocationACKBody =
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tAllocStatus) tAllocStatus $
      LT.insert (tnum tAllocRejCode) tAllocRejCode $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec
   { msName = "DontKnowTrade"
   , msType = "Q"
   , msHeader = headerFIX42
   , msBody = mDontKnowTradeBody
   , msTrailer = trailerFIX42 }
   where
   mDontKnowTradeBody =
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tDKReason) tDKReason $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tLastShares) tLastShares $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec
   { msName = "QuoteRequest"
   , msType = "R"
   , msHeader = headerFIX42
   , msBody = mQuoteRequestBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteRequestBody =
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , genValue = genFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeparator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' =
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tQuoteRequestType) tQuoteRequestType $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tOrdType) tOrdType $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tTransactTime) tTransactTime $
               LT.insert (tnum tCurrency) tCurrency                LT.new



mQuote :: FIXMessageSpec
mQuote = FMSpec
   { msName = "Quote"
   , msType = "S"
   , msHeader = headerFIX42
   , msBody = mQuoteBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteBody =
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tBidPx) tBidPx $
      LT.insert (tnum tOfferPx) tOfferPx $
      LT.insert (tnum tBidSize) tBidSize $
      LT.insert (tnum tOfferSize) tOfferSize $
      LT.insert (tnum tValidUntilTime) tValidUntilTime $
      LT.insert (tnum tBidSpotRate) tBidSpotRate $
      LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
      LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
      LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tCurrency) tCurrency       LT.new


mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec
   { msName = "SettlementInstructions"
   , msType = "T"
   , msHeader = headerFIX42
   , msBody = mSettlementInstructionsBody
   , msTrailer = trailerFIX42 }
   where
   mSettlementInstructionsBody =
      LT.insert (tnum tSettlInstID) tSettlInstID $
      LT.insert (tnum tSettlInstTransType) tSettlInstTransType $
      LT.insert (tnum tSettlInstRefID) tSettlInstRefID $
      LT.insert (tnum tSettlInstMode) tSettlInstMode $
      LT.insert (tnum tSettlInstSource) tSettlInstSource $
      LT.insert (tnum tAllocAccount) tAllocAccount $
      LT.insert (tnum tSettlLocation) tSettlLocation $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tStandInstDbType) tStandInstDbType $
      LT.insert (tnum tStandInstDbName) tStandInstDbName $
      LT.insert (tnum tStandInstDbID) tStandInstDbID $
      LT.insert (tnum tSettlDeliveryType) tSettlDeliveryType $
      LT.insert (tnum tSettlDepositoryCode) tSettlDepositoryCode $
      LT.insert (tnum tSettlBrkrCode) tSettlBrkrCode $
      LT.insert (tnum tSettlInstCode) tSettlInstCode $
      LT.insert (tnum tSecuritySettlAgentName) tSecuritySettlAgentName $
      LT.insert (tnum tSecuritySettlAgentCode) tSecuritySettlAgentCode $
      LT.insert (tnum tSecuritySettlAgentAcctNum) tSecuritySettlAgentAcctNum $
      LT.insert (tnum tSecuritySettlAgentAcctName) tSecuritySettlAgentAcctName $
      LT.insert (tnum tSecuritySettlAgentContactName) tSecuritySettlAgentContactName $
      LT.insert (tnum tSecuritySettlAgentContactPhone) tSecuritySettlAgentContactPhone $
      LT.insert (tnum tCashSettlAgentName) tCashSettlAgentName $
      LT.insert (tnum tCashSettlAgentCode) tCashSettlAgentCode $
      LT.insert (tnum tCashSettlAgentAcctNum) tCashSettlAgentAcctNum $
      LT.insert (tnum tCashSettlAgentAcctName) tCashSettlAgentAcctName $
      LT.insert (tnum tCashSettlAgentContactName) tCashSettlAgentContactName $
      LT.insert (tnum tCashSettlAgentContactPhone) tCashSettlAgentContactPhone       LT.new


mMarketDataRequest :: FIXMessageSpec
mMarketDataRequest = FMSpec
   { msName = "MarketDataRequest"
   , msType = "V"
   , msHeader = headerFIX42
   , msBody = mMarketDataRequestBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataRequestBody =
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
      LT.insert (tnum tMarketDepth) tMarketDepth $
      LT.insert (tnum tMDUpdateType) tMDUpdateType $
      LT.insert (tnum tAggregatedBook) tAggregatedBook $
      LT.insert (tnum tNoMDEntryTypes) gNoMDEntryTypes''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoMDEntryTypes''' = FIXTag
            { tName = "NoMDEntryTypes"
            , tnum = tnum tNoMDEntryTypes
            , tparser = gNoMDEntryTypesP'''
            , genValue = genFIXGroup gNoMDEntryTypesSpec''' }

         gNoMDEntryTypesP''' = groupP gNoMDEntryTypesSpec'''
         gNoMDEntryTypesSpec''' = FGSpec
            { gsLength = tNoMDEntryTypes
            , gsSeparator = tMDEntryType
            , gsBody = gNoMDEntryTypesBody''' }
            where
            gNoMDEntryTypesBody''' =
               LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , genValue = genFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeparator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' =
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tTradingSessionID) tTradingSessionID                LT.new



mMarketDataSnapshotFullRefresh :: FIXMessageSpec
mMarketDataSnapshotFullRefresh = FMSpec
   { msName = "MarketDataSnapshotFullRefresh"
   , msType = "W"
   , msHeader = headerFIX42
   , msBody = mMarketDataSnapshotFullRefreshBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataSnapshotFullRefreshBody =
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tFinancialStatus) tFinancialStatus $
      LT.insert (tnum tCorporateAction) tCorporateAction $
      LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
      LT.insert (tnum tNoMDEntries) gNoMDEntries'''       LT.new
      where
         gNoMDEntries''' = FIXTag
            { tName = "NoMDEntries"
            , tnum = tnum tNoMDEntries
            , tparser = gNoMDEntriesP'''
            , genValue = genFIXGroup gNoMDEntriesSpec''' }

         gNoMDEntriesP''' = groupP gNoMDEntriesSpec'''
         gNoMDEntriesSpec''' = FGSpec
            { gsLength = tNoMDEntries
            , gsSeparator = tMDEntryType
            , gsBody = gNoMDEntriesBody''' }
            where
            gNoMDEntriesBody''' =
               LT.insert (tnum tMDEntryPx) tMDEntryPx $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tMDEntrySize) tMDEntrySize $
               LT.insert (tnum tMDEntryDate) tMDEntryDate $
               LT.insert (tnum tMDEntryTime) tMDEntryTime $
               LT.insert (tnum tTickDirection) tTickDirection $
               LT.insert (tnum tMDMkt) tMDMkt $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tQuoteCondition) tQuoteCondition $
               LT.insert (tnum tTradeCondition) tTradeCondition $
               LT.insert (tnum tMDEntryOriginator) tMDEntryOriginator $
               LT.insert (tnum tLocationID) tLocationID $
               LT.insert (tnum tDeskID) tDeskID $
               LT.insert (tnum tOpenCloseSettleFlag) tOpenCloseSettleFlag $
               LT.insert (tnum tTimeInForce) tTimeInForce $
               LT.insert (tnum tExpireDate) tExpireDate $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tMinQty) tMinQty $
               LT.insert (tnum tExecInst) tExecInst $
               LT.insert (tnum tSellerDays) tSellerDays $
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tQuoteEntryID) tQuoteEntryID $
               LT.insert (tnum tMDEntryBuyer) tMDEntryBuyer $
               LT.insert (tnum tMDEntrySeller) tMDEntrySeller $
               LT.insert (tnum tNumberOfOrders) tNumberOfOrders $
               LT.insert (tnum tMDEntryPositionNo) tMDEntryPositionNo $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mMarketDataIncrementalRefresh :: FIXMessageSpec
mMarketDataIncrementalRefresh = FMSpec
   { msName = "MarketDataIncrementalRefresh"
   , msType = "X"
   , msHeader = headerFIX42
   , msBody = mMarketDataIncrementalRefreshBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataIncrementalRefreshBody =
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tNoMDEntries) gNoMDEntries'''       LT.new
      where
         gNoMDEntries''' = FIXTag
            { tName = "NoMDEntries"
            , tnum = tnum tNoMDEntries
            , tparser = gNoMDEntriesP'''
            , genValue = genFIXGroup gNoMDEntriesSpec''' }

         gNoMDEntriesP''' = groupP gNoMDEntriesSpec'''
         gNoMDEntriesSpec''' = FGSpec
            { gsLength = tNoMDEntries
            , gsSeparator = tMDUpdateAction
            , gsBody = gNoMDEntriesBody''' }
            where
            gNoMDEntriesBody''' =
               LT.insert (tnum tDeleteReason) tDeleteReason $
               LT.insert (tnum tMDEntryType) tMDEntryType $
               LT.insert (tnum tMDEntryID) tMDEntryID $
               LT.insert (tnum tMDEntryRefID) tMDEntryRefID $
               LT.insert (tnum tSymbol) tSymbol $
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tFinancialStatus) tFinancialStatus $
               LT.insert (tnum tCorporateAction) tCorporateAction $
               LT.insert (tnum tMDEntryPx) tMDEntryPx $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tMDEntrySize) tMDEntrySize $
               LT.insert (tnum tMDEntryDate) tMDEntryDate $
               LT.insert (tnum tMDEntryTime) tMDEntryTime $
               LT.insert (tnum tTickDirection) tTickDirection $
               LT.insert (tnum tMDMkt) tMDMkt $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tQuoteCondition) tQuoteCondition $
               LT.insert (tnum tTradeCondition) tTradeCondition $
               LT.insert (tnum tMDEntryOriginator) tMDEntryOriginator $
               LT.insert (tnum tLocationID) tLocationID $
               LT.insert (tnum tDeskID) tDeskID $
               LT.insert (tnum tOpenCloseSettleFlag) tOpenCloseSettleFlag $
               LT.insert (tnum tTimeInForce) tTimeInForce $
               LT.insert (tnum tExpireDate) tExpireDate $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tMinQty) tMinQty $
               LT.insert (tnum tExecInst) tExecInst $
               LT.insert (tnum tSellerDays) tSellerDays $
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tQuoteEntryID) tQuoteEntryID $
               LT.insert (tnum tMDEntryBuyer) tMDEntryBuyer $
               LT.insert (tnum tMDEntrySeller) tMDEntrySeller $
               LT.insert (tnum tNumberOfOrders) tNumberOfOrders $
               LT.insert (tnum tMDEntryPositionNo) tMDEntryPositionNo $
               LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mMarketDataRequestReject :: FIXMessageSpec
mMarketDataRequestReject = FMSpec
   { msName = "MarketDataRequestReject"
   , msType = "Y"
   , msHeader = headerFIX42
   , msBody = mMarketDataRequestRejectBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataRequestRejectBody =
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tMDReqRejReason) tMDReqRejReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mQuoteCancel :: FIXMessageSpec
mQuoteCancel = FMSpec
   { msName = "QuoteCancel"
   , msType = "Z"
   , msHeader = headerFIX42
   , msBody = mQuoteCancelBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteCancelBody =
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries'''       LT.new
      where
         gNoQuoteEntries''' = FIXTag
            { tName = "NoQuoteEntries"
            , tnum = tnum tNoQuoteEntries
            , tparser = gNoQuoteEntriesP'''
            , genValue = genFIXGroup gNoQuoteEntriesSpec''' }

         gNoQuoteEntriesP''' = groupP gNoQuoteEntriesSpec'''
         gNoQuoteEntriesSpec''' = FGSpec
            { gsLength = tNoQuoteEntries
            , gsSeparator = tSymbol
            , gsBody = gNoQuoteEntriesBody''' }
            where
            gNoQuoteEntriesBody''' =
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol                LT.new



mQuoteStatusRequest :: FIXMessageSpec
mQuoteStatusRequest = FMSpec
   { msName = "QuoteStatusRequest"
   , msType = "a"
   , msHeader = headerFIX42
   , msBody = mQuoteStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteStatusRequestBody =
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTradingSessionID) tTradingSessionID       LT.new


mQuoteAcknowledgement :: FIXMessageSpec
mQuoteAcknowledgement = FMSpec
   { msName = "QuoteAcknowledgement"
   , msType = "b"
   , msHeader = headerFIX42
   , msBody = mQuoteAcknowledgementBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteAcknowledgementBody =
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteAckStatus) tQuoteAckStatus $
      LT.insert (tnum tQuoteRejectReason) tQuoteRejectReason $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tNoQuoteSets) gNoQuoteSets'''       LT.new
      where
         gNoQuoteSets''' = FIXTag
            { tName = "NoQuoteSets"
            , tnum = tnum tNoQuoteSets
            , tparser = gNoQuoteSetsP'''
            , genValue = genFIXGroup gNoQuoteSetsSpec''' }

         gNoQuoteSetsP''' = groupP gNoQuoteSetsSpec'''
         gNoQuoteSetsSpec''' = FGSpec
            { gsLength = tNoQuoteSets
            , gsSeparator = tQuoteSetID
            , gsBody = gNoQuoteSetsBody''' }
            where
            gNoQuoteSetsBody''' =
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
               LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
               LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
               LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
               LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
               LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
               LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
               LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
               LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
               LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
               LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
               LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
               LT.insert (tnum tTotQuoteEntries) tTotQuoteEntries $
               LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries''''''                LT.new
               where
                  gNoQuoteEntries'''''' = FIXTag
                     { tName = "NoQuoteEntries"
                     , tnum = tnum tNoQuoteEntries
                     , tparser = gNoQuoteEntriesP''''''
                     , genValue = genFIXGroup gNoQuoteEntriesSpec'''''' }

                  gNoQuoteEntriesP'''''' = groupP gNoQuoteEntriesSpec''''''
                  gNoQuoteEntriesSpec'''''' = FGSpec
                     { gsLength = tNoQuoteEntries
                     , gsSeparator = tQuoteEntryID
                     , gsBody = gNoQuoteEntriesBody'''''' }
                     where
                     gNoQuoteEntriesBody'''''' =
                        LT.insert (tnum tSymbol) tSymbol $
                        LT.insert (tnum tSymbolSfx) tSymbolSfx $
                        LT.insert (tnum tSecurityID) tSecurityID $
                        LT.insert (tnum tIDSource) tIDSource $
                        LT.insert (tnum tSecurityType) tSecurityType $
                        LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
                        LT.insert (tnum tMaturityDay) tMaturityDay $
                        LT.insert (tnum tPutOrCall) tPutOrCall $
                        LT.insert (tnum tStrikePrice) tStrikePrice $
                        LT.insert (tnum tOptAttribute) tOptAttribute $
                        LT.insert (tnum tContractMultiplier) tContractMultiplier $
                        LT.insert (tnum tCouponRate) tCouponRate $
                        LT.insert (tnum tSecurityExchange) tSecurityExchange $
                        LT.insert (tnum tIssuer) tIssuer $
                        LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
                        LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
                        LT.insert (tnum tSecurityDesc) tSecurityDesc $
                        LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
                        LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
                        LT.insert (tnum tQuoteEntryRejectReason) tQuoteEntryRejectReason                         LT.new




mSecurityDefinitionRequest :: FIXMessageSpec
mSecurityDefinitionRequest = FMSpec
   { msName = "SecurityDefinitionRequest"
   , msType = "c"
   , msHeader = headerFIX42
   , msBody = mSecurityDefinitionRequestBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityDefinitionRequestBody =
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityRequestType) tSecurityRequestType $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , genValue = genFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeparator = tUnderlyingSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' =
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
               LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
               LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
               LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
               LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
               LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
               LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
               LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
               LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
               LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
               LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
               LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
               LT.insert (tnum tRatioQty) tRatioQty $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tUnderlyingCurrency) tUnderlyingCurrency                LT.new



mSecurityDefinition :: FIXMessageSpec
mSecurityDefinition = FMSpec
   { msName = "SecurityDefinition"
   , msType = "d"
   , msHeader = headerFIX42
   , msBody = mSecurityDefinitionBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityDefinitionBody =
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
      LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
      LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , genValue = genFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeparator = tUnderlyingSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' =
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
               LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
               LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
               LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
               LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
               LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
               LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
               LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
               LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
               LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
               LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
               LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
               LT.insert (tnum tRatioQty) tRatioQty $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tUnderlyingCurrency) tUnderlyingCurrency                LT.new



mSecurityStatusRequest :: FIXMessageSpec
mSecurityStatusRequest = FMSpec
   { msName = "SecurityStatusRequest"
   , msType = "e"
   , msHeader = headerFIX42
   , msBody = mSecurityStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityStatusRequestBody =
      LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID       LT.new


mSecurityStatus :: FIXMessageSpec
mSecurityStatus = FMSpec
   { msName = "SecurityStatus"
   , msType = "f"
   , msHeader = headerFIX42
   , msBody = mSecurityStatusBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityStatusBody =
      LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
      LT.insert (tnum tSecurityTradingStatus) tSecurityTradingStatus $
      LT.insert (tnum tFinancialStatus) tFinancialStatus $
      LT.insert (tnum tCorporateAction) tCorporateAction $
      LT.insert (tnum tHaltReasonChar) tHaltReasonChar $
      LT.insert (tnum tInViewOfCommon) tInViewOfCommon $
      LT.insert (tnum tDueToRelated) tDueToRelated $
      LT.insert (tnum tBuyVolume) tBuyVolume $
      LT.insert (tnum tSellVolume) tSellVolume $
      LT.insert (tnum tHighPx) tHighPx $
      LT.insert (tnum tLowPx) tLowPx $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tAdjustment) tAdjustment       LT.new


mTradingSessionStatusRequest :: FIXMessageSpec
mTradingSessionStatusRequest = FMSpec
   { msName = "TradingSessionStatusRequest"
   , msType = "g"
   , msHeader = headerFIX42
   , msBody = mTradingSessionStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mTradingSessionStatusRequestBody =
      LT.insert (tnum tTradSesReqID) tTradSesReqID $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradSesMethod) tTradSesMethod $
      LT.insert (tnum tTradSesMode) tTradSesMode $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new


mTradingSessionStatus :: FIXMessageSpec
mTradingSessionStatus = FMSpec
   { msName = "TradingSessionStatus"
   , msType = "h"
   , msHeader = headerFIX42
   , msBody = mTradingSessionStatusBody
   , msTrailer = trailerFIX42 }
   where
   mTradingSessionStatusBody =
      LT.insert (tnum tTradSesReqID) tTradSesReqID $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradSesMethod) tTradSesMethod $
      LT.insert (tnum tTradSesMode) tTradSesMode $
      LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
      LT.insert (tnum tTradSesStatus) tTradSesStatus $
      LT.insert (tnum tTradSesStartTime) tTradSesStartTime $
      LT.insert (tnum tTradSesOpenTime) tTradSesOpenTime $
      LT.insert (tnum tTradSesPreCloseTime) tTradSesPreCloseTime $
      LT.insert (tnum tTradSesCloseTime) tTradSesCloseTime $
      LT.insert (tnum tTradSesEndTime) tTradSesEndTime $
      LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mMassQuote :: FIXMessageSpec
mMassQuote = FMSpec
   { msName = "MassQuote"
   , msType = "i"
   , msHeader = headerFIX42
   , msBody = mMassQuoteBody
   , msTrailer = trailerFIX42 }
   where
   mMassQuoteBody =
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tDefBidSize) tDefBidSize $
      LT.insert (tnum tDefOfferSize) tDefOfferSize $
      LT.insert (tnum tNoQuoteSets) gNoQuoteSets'''       LT.new
      where
         gNoQuoteSets''' = FIXTag
            { tName = "NoQuoteSets"
            , tnum = tnum tNoQuoteSets
            , tparser = gNoQuoteSetsP'''
            , genValue = genFIXGroup gNoQuoteSetsSpec''' }

         gNoQuoteSetsP''' = groupP gNoQuoteSetsSpec'''
         gNoQuoteSetsSpec''' = FGSpec
            { gsLength = tNoQuoteSets
            , gsSeparator = tQuoteSetID
            , gsBody = gNoQuoteSetsBody''' }
            where
            gNoQuoteSetsBody''' =
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
               LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
               LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
               LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
               LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
               LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
               LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
               LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
               LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
               LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
               LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
               LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
               LT.insert (tnum tQuoteSetValidUntilTime) tQuoteSetValidUntilTime $
               LT.insert (tnum tTotQuoteEntries) tTotQuoteEntries $
               LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries''''''                LT.new
               where
                  gNoQuoteEntries'''''' = FIXTag
                     { tName = "NoQuoteEntries"
                     , tnum = tnum tNoQuoteEntries
                     , tparser = gNoQuoteEntriesP''''''
                     , genValue = genFIXGroup gNoQuoteEntriesSpec'''''' }

                  gNoQuoteEntriesP'''''' = groupP gNoQuoteEntriesSpec''''''
                  gNoQuoteEntriesSpec'''''' = FGSpec
                     { gsLength = tNoQuoteEntries
                     , gsSeparator = tQuoteEntryID
                     , gsBody = gNoQuoteEntriesBody'''''' }
                     where
                     gNoQuoteEntriesBody'''''' =
                        LT.insert (tnum tSymbol) tSymbol $
                        LT.insert (tnum tSymbolSfx) tSymbolSfx $
                        LT.insert (tnum tSecurityID) tSecurityID $
                        LT.insert (tnum tIDSource) tIDSource $
                        LT.insert (tnum tSecurityType) tSecurityType $
                        LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
                        LT.insert (tnum tMaturityDay) tMaturityDay $
                        LT.insert (tnum tPutOrCall) tPutOrCall $
                        LT.insert (tnum tStrikePrice) tStrikePrice $
                        LT.insert (tnum tOptAttribute) tOptAttribute $
                        LT.insert (tnum tContractMultiplier) tContractMultiplier $
                        LT.insert (tnum tCouponRate) tCouponRate $
                        LT.insert (tnum tSecurityExchange) tSecurityExchange $
                        LT.insert (tnum tIssuer) tIssuer $
                        LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
                        LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
                        LT.insert (tnum tSecurityDesc) tSecurityDesc $
                        LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
                        LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
                        LT.insert (tnum tBidPx) tBidPx $
                        LT.insert (tnum tOfferPx) tOfferPx $
                        LT.insert (tnum tBidSize) tBidSize $
                        LT.insert (tnum tOfferSize) tOfferSize $
                        LT.insert (tnum tValidUntilTime) tValidUntilTime $
                        LT.insert (tnum tBidSpotRate) tBidSpotRate $
                        LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
                        LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
                        LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
                        LT.insert (tnum tTransactTime) tTransactTime $
                        LT.insert (tnum tTradingSessionID) tTradingSessionID $
                        LT.insert (tnum tFutSettDate) tFutSettDate $
                        LT.insert (tnum tOrdType) tOrdType $
                        LT.insert (tnum tFutSettDate2) tFutSettDate2 $
                        LT.insert (tnum tOrderQty2) tOrderQty2 $
                        LT.insert (tnum tCurrency) tCurrency                         LT.new




mBusinessMessageReject :: FIXMessageSpec
mBusinessMessageReject = FMSpec
   { msName = "BusinessMessageReject"
   , msType = "j"
   , msHeader = headerFIX42
   , msBody = mBusinessMessageRejectBody
   , msTrailer = trailerFIX42 }
   where
   mBusinessMessageRejectBody =
      LT.insert (tnum tRefSeqNum) tRefSeqNum $
      LT.insert (tnum tRefMsgType) tRefMsgType $
      LT.insert (tnum tBusinessRejectRefID) tBusinessRejectRefID $
      LT.insert (tnum tBusinessRejectReason) tBusinessRejectReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mBidRequest :: FIXMessageSpec
mBidRequest = FMSpec
   { msName = "BidRequest"
   , msType = "k"
   , msHeader = headerFIX42
   , msBody = mBidRequestBody
   , msTrailer = trailerFIX42 }
   where
   mBidRequestBody =
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tBidRequestTransType) tBidRequestTransType $
      LT.insert (tnum tListName) tListName $
      LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
      LT.insert (tnum tBidType) tBidType $
      LT.insert (tnum tNumTickets) tNumTickets $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tSideValue1) tSideValue1 $
      LT.insert (tnum tSideValue2) tSideValue2 $
      LT.insert (tnum tNoBidDescriptors) gNoBidDescriptors''' $
      LT.insert (tnum tNoBidComponents) gNoBidComponents''' $
      LT.insert (tnum tLiquidityIndType) tLiquidityIndType $
      LT.insert (tnum tWtAverageLiquidity) tWtAverageLiquidity $
      LT.insert (tnum tExchangeForPhysical) tExchangeForPhysical $
      LT.insert (tnum tOutMainCntryUIndex) tOutMainCntryUIndex $
      LT.insert (tnum tCrossPercent) tCrossPercent $
      LT.insert (tnum tProgRptReqs) tProgRptReqs $
      LT.insert (tnum tProgPeriodInterval) tProgPeriodInterval $
      LT.insert (tnum tIncTaxInd) tIncTaxInd $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tNumBidders) tNumBidders $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTradeType) tTradeType $
      LT.insert (tnum tBasisPxType) tBasisPxType $
      LT.insert (tnum tStrikeTime) tStrikeTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoBidComponents''' = FIXTag
            { tName = "NoBidComponents"
            , tnum = tnum tNoBidComponents
            , tparser = gNoBidComponentsP'''
            , genValue = genFIXGroup gNoBidComponentsSpec''' }

         gNoBidComponentsP''' = groupP gNoBidComponentsSpec'''
         gNoBidComponentsSpec''' = FGSpec
            { gsLength = tNoBidComponents
            , gsSeparator = tListID
            , gsBody = gNoBidComponentsBody''' }
            where
            gNoBidComponentsBody''' =
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tNetGrossInd) tNetGrossInd $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tAccount) tAccount                LT.new

         gNoBidDescriptors''' = FIXTag
            { tName = "NoBidDescriptors"
            , tnum = tnum tNoBidDescriptors
            , tparser = gNoBidDescriptorsP'''
            , genValue = genFIXGroup gNoBidDescriptorsSpec''' }

         gNoBidDescriptorsP''' = groupP gNoBidDescriptorsSpec'''
         gNoBidDescriptorsSpec''' = FGSpec
            { gsLength = tNoBidDescriptors
            , gsSeparator = tBidDescriptorType
            , gsBody = gNoBidDescriptorsBody''' }
            where
            gNoBidDescriptorsBody''' =
               LT.insert (tnum tBidDescriptor) tBidDescriptor $
               LT.insert (tnum tSideValueInd) tSideValueInd $
               LT.insert (tnum tLiquidityValue) tLiquidityValue $
               LT.insert (tnum tLiquidityNumSecurities) tLiquidityNumSecurities $
               LT.insert (tnum tLiquidityPctLow) tLiquidityPctLow $
               LT.insert (tnum tLiquidityPctHigh) tLiquidityPctHigh $
               LT.insert (tnum tEFPTrackingError) tEFPTrackingError $
               LT.insert (tnum tFairValue) tFairValue $
               LT.insert (tnum tOutsideIndexPct) tOutsideIndexPct $
               LT.insert (tnum tValueOfFutures) tValueOfFutures                LT.new



mBidResponse :: FIXMessageSpec
mBidResponse = FMSpec
   { msName = "BidResponse"
   , msType = "l"
   , msHeader = headerFIX42
   , msBody = mBidResponseBody
   , msTrailer = trailerFIX42 }
   where
   mBidResponseBody =
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tNoBidComponents) gNoBidComponents'''       LT.new
      where
         gNoBidComponents''' = FIXTag
            { tName = "NoBidComponents"
            , tnum = tnum tNoBidComponents
            , tparser = gNoBidComponentsP'''
            , genValue = genFIXGroup gNoBidComponentsSpec''' }

         gNoBidComponentsP''' = groupP gNoBidComponentsSpec'''
         gNoBidComponentsSpec''' = FGSpec
            { gsLength = tNoBidComponents
            , gsSeparator = tCommission
            , gsBody = gNoBidComponentsBody''' }
            where
            gNoBidComponentsBody''' =
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tListID) tListID $
               LT.insert (tnum tCountry) tCountry $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tPriceType) tPriceType $
               LT.insert (tnum tFairValue) tFairValue $
               LT.insert (tnum tNetGrossInd) tNetGrossInd $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mListStrikePrice :: FIXMessageSpec
mListStrikePrice = FMSpec
   { msName = "ListStrikePrice"
   , msType = "m"
   , msHeader = headerFIX42
   , msBody = mListStrikePriceBody
   , msTrailer = trailerFIX42 }
   where
   mListStrikePriceBody =
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tTotNoStrikes) tTotNoStrikes $
      LT.insert (tnum tNoStrikes) gNoStrikes'''       LT.new
      where
         gNoStrikes''' = FIXTag
            { tName = "NoStrikes"
            , tnum = tnum tNoStrikes
            , tparser = gNoStrikesP'''
            , genValue = genFIXGroup gNoStrikesSpec''' }

         gNoStrikesP''' = groupP gNoStrikesSpec'''
         gNoStrikesSpec''' = FGSpec
            { gsLength = tNoStrikes
            , gsSeparator = tSymbol
            , gsBody = gNoStrikesBody''' }
            where
            gNoStrikesBody''' =
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tClOrdID) tClOrdID $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



fix42 :: FIXSpec
fix42 = FSpec
   { fsVersion = "FIX.4.2"
   , fsHeader = headerFIX42
   , fsTrailer = trailerFIX42
   , fsMessages = fix42Messages
   , fsTags = fix42Tags }
   where
      fix42Messages =
          LT.insert (msType mHeartbeat) mHeartbeat $
          LT.insert (msType mTestRequest) mTestRequest $
          LT.insert (msType mResendRequest) mResendRequest $
          LT.insert (msType mReject) mReject $
          LT.insert (msType mSequenceReset) mSequenceReset $
          LT.insert (msType mLogout) mLogout $
          LT.insert (msType mIndicationofInterest) mIndicationofInterest $
          LT.insert (msType mAdvertisement) mAdvertisement $
          LT.insert (msType mExecutionReport) mExecutionReport $
          LT.insert (msType mOrderCancelReject) mOrderCancelReject $
          LT.insert (msType mLogon) mLogon $
          LT.insert (msType mNews) mNews $
          LT.insert (msType mEmail) mEmail $
          LT.insert (msType mNewOrderSingle) mNewOrderSingle $
          LT.insert (msType mNewOrderList) mNewOrderList $
          LT.insert (msType mOrderCancelRequest) mOrderCancelRequest $
          LT.insert (msType mOrderCancelReplaceRequest) mOrderCancelReplaceRequest $
          LT.insert (msType mOrderStatusRequest) mOrderStatusRequest $
          LT.insert (msType mAllocation) mAllocation $
          LT.insert (msType mListCancelRequest) mListCancelRequest $
          LT.insert (msType mListExecute) mListExecute $
          LT.insert (msType mListStatusRequest) mListStatusRequest $
          LT.insert (msType mListStatus) mListStatus $
          LT.insert (msType mAllocationACK) mAllocationACK $
          LT.insert (msType mDontKnowTrade) mDontKnowTrade $
          LT.insert (msType mQuoteRequest) mQuoteRequest $
          LT.insert (msType mQuote) mQuote $
          LT.insert (msType mSettlementInstructions) mSettlementInstructions $
          LT.insert (msType mMarketDataRequest) mMarketDataRequest $
          LT.insert (msType mMarketDataSnapshotFullRefresh) mMarketDataSnapshotFullRefresh $
          LT.insert (msType mMarketDataIncrementalRefresh) mMarketDataIncrementalRefresh $
          LT.insert (msType mMarketDataRequestReject) mMarketDataRequestReject $
          LT.insert (msType mQuoteCancel) mQuoteCancel $
          LT.insert (msType mQuoteStatusRequest) mQuoteStatusRequest $
          LT.insert (msType mQuoteAcknowledgement) mQuoteAcknowledgement $
          LT.insert (msType mSecurityDefinitionRequest) mSecurityDefinitionRequest $
          LT.insert (msType mSecurityDefinition) mSecurityDefinition $
          LT.insert (msType mSecurityStatusRequest) mSecurityStatusRequest $
          LT.insert (msType mSecurityStatus) mSecurityStatus $
          LT.insert (msType mTradingSessionStatusRequest) mTradingSessionStatusRequest $
          LT.insert (msType mTradingSessionStatus) mTradingSessionStatus $
          LT.insert (msType mMassQuote) mMassQuote $
          LT.insert (msType mBusinessMessageReject) mBusinessMessageReject $
          LT.insert (msType mBidRequest) mBidRequest $
          LT.insert (msType mBidResponse) mBidResponse $
          LT.insert (msType mListStrikePrice) mListStrikePrice           LT.new
      fix42Tags =
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAdvId) tAdvId $
          LT.insert (tnum tAdvRefID) tAdvRefID $
          LT.insert (tnum tAdvSide) tAdvSide $
          LT.insert (tnum tAdvTransType) tAdvTransType $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
          LT.insert (tnum tBeginString) tBeginString $
          LT.insert (tnum tBodyLength) tBodyLength $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tCumQty) tCumQty $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tEndSeqNo) tEndSeqNo $
          LT.insert (tnum tExecID) tExecID $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tExecRefID) tExecRefID $
          LT.insert (tnum tExecTransType) tExecTransType $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tIDSource) tIDSource $
          LT.insert (tnum tIOIid) tIOIid $
          LT.insert (tnum tIOIQltyInd) tIOIQltyInd $
          LT.insert (tnum tIOIRefID) tIOIRefID $
          LT.insert (tnum tIOIShares) tIOIShares $
          LT.insert (tnum tIOITransType) tIOITransType $
          LT.insert (tnum tLastCapacity) tLastCapacity $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tLastPx) tLastPx $
          LT.insert (tnum tLastShares) tLastShares $
          LT.insert (tnum tLinesOfText) tLinesOfText $
          LT.insert (tnum tMsgSeqNum) tMsgSeqNum $
          LT.insert (tnum tMsgType) tMsgType $
          LT.insert (tnum tNewSeqNo) tNewSeqNo $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tOrdStatus) tOrdStatus $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tOrigTime) tOrigTime $
          LT.insert (tnum tPossDupFlag) tPossDupFlag $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tRefSeqNum) tRefSeqNum $
          LT.insert (tnum tRelatdSym) tRelatdSym $
          LT.insert (tnum tRule80A) tRule80A $
          LT.insert (tnum tSecurityID) tSecurityID $
          LT.insert (tnum tSenderCompID) tSenderCompID $
          LT.insert (tnum tSenderSubID) tSenderSubID $
          LT.insert (tnum tSendingTime) tSendingTime $
          LT.insert (tnum tShares) tShares $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tSymbol) tSymbol $
          LT.insert (tnum tTargetCompID) tTargetCompID $
          LT.insert (tnum tTargetSubID) tTargetSubID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tUrgency) tUrgency $
          LT.insert (tnum tValidUntilTime) tValidUntilTime $
          LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
          LT.insert (tnum tFutSettDate) tFutSettDate $
          LT.insert (tnum tSymbolSfx) tSymbolSfx $
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tListSeqNo) tListSeqNo $
          LT.insert (tnum tTotNoOrders) tTotNoOrders $
          LT.insert (tnum tListExecInst) tListExecInst $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tAllocTransType) tAllocTransType $
          LT.insert (tnum tRefAllocID) tRefAllocID $
          LT.insert (tnum tNoOrders) tNoOrders $
          LT.insert (tnum tAvgPrxPrecision) tAvgPrxPrecision $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tExecBroker) tExecBroker $
          LT.insert (tnum tOpenClose) tOpenClose $
          LT.insert (tnum tNoAllocs) tNoAllocs $
          LT.insert (tnum tAllocAccount) tAllocAccount $
          LT.insert (tnum tAllocShares) tAllocShares $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tNoRpts) tNoRpts $
          LT.insert (tnum tRptSeq) tRptSeq $
          LT.insert (tnum tCxlQty) tCxlQty $
          LT.insert (tnum tAllocStatus) tAllocStatus $
          LT.insert (tnum tAllocRejCode) tAllocRejCode $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tSecureDataLen) tSecureDataLen $
          LT.insert (tnum tSecureData) tSecureData $
          LT.insert (tnum tBrokerOfCredit) tBrokerOfCredit $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tEmailType) tEmailType $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.insert (tnum tPossResend) tPossResend $
          LT.insert (tnum tEncryptMethod) tEncryptMethod $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tCxlRejReason) tCxlRejReason $
          LT.insert (tnum tOrdRejReason) tOrdRejReason $
          LT.insert (tnum tIOIQualifier) tIOIQualifier $
          LT.insert (tnum tWaveNo) tWaveNo $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tHeartBtInt) tHeartBtInt $
          LT.insert (tnum tClientID) tClientID $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tTestReqID) tTestReqID $
          LT.insert (tnum tReportToExch) tReportToExch $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tOnBehalfOfCompID) tOnBehalfOfCompID $
          LT.insert (tnum tOnBehalfOfSubID) tOnBehalfOfSubID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tNetMoney) tNetMoney $
          LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tOrigSendingTime) tOrigSendingTime $
          LT.insert (tnum tGapFillFlag) tGapFillFlag $
          LT.insert (tnum tNoExecs) tNoExecs $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tDKReason) tDKReason $
          LT.insert (tnum tDeliverToCompID) tDeliverToCompID $
          LT.insert (tnum tDeliverToSubID) tDeliverToSubID $
          LT.insert (tnum tIOINaturalFlag) tIOINaturalFlag $
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tBidPx) tBidPx $
          LT.insert (tnum tOfferPx) tOfferPx $
          LT.insert (tnum tBidSize) tBidSize $
          LT.insert (tnum tOfferSize) tOfferSize $
          LT.insert (tnum tNoMiscFees) tNoMiscFees $
          LT.insert (tnum tMiscFeeAmt) tMiscFeeAmt $
          LT.insert (tnum tMiscFeeCurr) tMiscFeeCurr $
          LT.insert (tnum tMiscFeeType) tMiscFeeType $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tResetSeqNumFlag) tResetSeqNumFlag $
          LT.insert (tnum tSenderLocationID) tSenderLocationID $
          LT.insert (tnum tTargetLocationID) tTargetLocationID $
          LT.insert (tnum tOnBehalfOfLocationID) tOnBehalfOfLocationID $
          LT.insert (tnum tDeliverToLocationID) tDeliverToLocationID $
          LT.insert (tnum tNoRelatedSym) tNoRelatedSym $
          LT.insert (tnum tSubject) tSubject $
          LT.insert (tnum tHeadline) tHeadline $
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tExecType) tExecType $
          LT.insert (tnum tLeavesQty) tLeavesQty $
          LT.insert (tnum tCashOrderQty) tCashOrderQty $
          LT.insert (tnum tAllocAvgPx) tAllocAvgPx $
          LT.insert (tnum tAllocNetMoney) tAllocNetMoney $
          LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
          LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
          LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tSettlInstMode) tSettlInstMode $
          LT.insert (tnum tAllocText) tAllocText $
          LT.insert (tnum tSettlInstID) tSettlInstID $
          LT.insert (tnum tSettlInstTransType) tSettlInstTransType $
          LT.insert (tnum tEmailThreadID) tEmailThreadID $
          LT.insert (tnum tSettlInstSource) tSettlInstSource $
          LT.insert (tnum tSettlLocation) tSettlLocation $
          LT.insert (tnum tSecurityType) tSecurityType $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tStandInstDbType) tStandInstDbType $
          LT.insert (tnum tStandInstDbName) tStandInstDbName $
          LT.insert (tnum tStandInstDbID) tStandInstDbID $
          LT.insert (tnum tSettlDeliveryType) tSettlDeliveryType $
          LT.insert (tnum tSettlDepositoryCode) tSettlDepositoryCode $
          LT.insert (tnum tSettlBrkrCode) tSettlBrkrCode $
          LT.insert (tnum tSettlInstCode) tSettlInstCode $
          LT.insert (tnum tSecuritySettlAgentName) tSecuritySettlAgentName $
          LT.insert (tnum tSecuritySettlAgentCode) tSecuritySettlAgentCode $
          LT.insert (tnum tSecuritySettlAgentAcctNum) tSecuritySettlAgentAcctNum $
          LT.insert (tnum tSecuritySettlAgentAcctName) tSecuritySettlAgentAcctName $
          LT.insert (tnum tSecuritySettlAgentContactName) tSecuritySettlAgentContactName $
          LT.insert (tnum tSecuritySettlAgentContactPhone) tSecuritySettlAgentContactPhone $
          LT.insert (tnum tCashSettlAgentName) tCashSettlAgentName $
          LT.insert (tnum tCashSettlAgentCode) tCashSettlAgentCode $
          LT.insert (tnum tCashSettlAgentAcctNum) tCashSettlAgentAcctNum $
          LT.insert (tnum tCashSettlAgentAcctName) tCashSettlAgentAcctName $
          LT.insert (tnum tCashSettlAgentContactName) tCashSettlAgentContactName $
          LT.insert (tnum tCashSettlAgentContactPhone) tCashSettlAgentContactPhone $
          LT.insert (tnum tBidSpotRate) tBidSpotRate $
          LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
          LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
          LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tFutSettDate2) tFutSettDate2 $
          LT.insert (tnum tLastSpotRate) tLastSpotRate $
          LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
          LT.insert (tnum tAllocLinkID) tAllocLinkID $
          LT.insert (tnum tAllocLinkType) tAllocLinkType $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tNoIOIQualifiers) tNoIOIQualifiers $
          LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
          LT.insert (tnum tPutOrCall) tPutOrCall $
          LT.insert (tnum tStrikePrice) tStrikePrice $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
          LT.insert (tnum tMaturityDay) tMaturityDay $
          LT.insert (tnum tOptAttribute) tOptAttribute $
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tNotifyBrokerOfCredit) tNotifyBrokerOfCredit $
          LT.insert (tnum tAllocHandlInst) tAllocHandlInst $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tPegDifference) tPegDifference $
          LT.insert (tnum tXmlDataLen) tXmlDataLen $
          LT.insert (tnum tXmlData) tXmlData $
          LT.insert (tnum tSettlInstRefID) tSettlInstRefID $
          LT.insert (tnum tNoRoutingIDs) tNoRoutingIDs $
          LT.insert (tnum tRoutingType) tRoutingType $
          LT.insert (tnum tRoutingID) tRoutingID $
          LT.insert (tnum tSpreadToBenchmark) tSpreadToBenchmark $
          LT.insert (tnum tBenchmark) tBenchmark $
          LT.insert (tnum tCouponRate) tCouponRate $
          LT.insert (tnum tContractMultiplier) tContractMultiplier $
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tMarketDepth) tMarketDepth $
          LT.insert (tnum tMDUpdateType) tMDUpdateType $
          LT.insert (tnum tAggregatedBook) tAggregatedBook $
          LT.insert (tnum tNoMDEntryTypes) tNoMDEntryTypes $
          LT.insert (tnum tNoMDEntries) tNoMDEntries $
          LT.insert (tnum tMDEntryType) tMDEntryType $
          LT.insert (tnum tMDEntryPx) tMDEntryPx $
          LT.insert (tnum tMDEntrySize) tMDEntrySize $
          LT.insert (tnum tMDEntryDate) tMDEntryDate $
          LT.insert (tnum tMDEntryTime) tMDEntryTime $
          LT.insert (tnum tTickDirection) tTickDirection $
          LT.insert (tnum tMDMkt) tMDMkt $
          LT.insert (tnum tQuoteCondition) tQuoteCondition $
          LT.insert (tnum tTradeCondition) tTradeCondition $
          LT.insert (tnum tMDEntryID) tMDEntryID $
          LT.insert (tnum tMDUpdateAction) tMDUpdateAction $
          LT.insert (tnum tMDEntryRefID) tMDEntryRefID $
          LT.insert (tnum tMDReqRejReason) tMDReqRejReason $
          LT.insert (tnum tMDEntryOriginator) tMDEntryOriginator $
          LT.insert (tnum tLocationID) tLocationID $
          LT.insert (tnum tDeskID) tDeskID $
          LT.insert (tnum tDeleteReason) tDeleteReason $
          LT.insert (tnum tOpenCloseSettleFlag) tOpenCloseSettleFlag $
          LT.insert (tnum tSellerDays) tSellerDays $
          LT.insert (tnum tMDEntryBuyer) tMDEntryBuyer $
          LT.insert (tnum tMDEntrySeller) tMDEntrySeller $
          LT.insert (tnum tMDEntryPositionNo) tMDEntryPositionNo $
          LT.insert (tnum tFinancialStatus) tFinancialStatus $
          LT.insert (tnum tCorporateAction) tCorporateAction $
          LT.insert (tnum tDefBidSize) tDefBidSize $
          LT.insert (tnum tDefOfferSize) tDefOfferSize $
          LT.insert (tnum tNoQuoteEntries) tNoQuoteEntries $
          LT.insert (tnum tNoQuoteSets) tNoQuoteSets $
          LT.insert (tnum tQuoteAckStatus) tQuoteAckStatus $
          LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
          LT.insert (tnum tQuoteEntryID) tQuoteEntryID $
          LT.insert (tnum tQuoteRejectReason) tQuoteRejectReason $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tQuoteSetID) tQuoteSetID $
          LT.insert (tnum tQuoteRequestType) tQuoteRequestType $
          LT.insert (tnum tTotQuoteEntries) tTotQuoteEntries $
          LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
          LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
          LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
          LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
          LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
          LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
          LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
          LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
          LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
          LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
          LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
          LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
          LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
          LT.insert (tnum tUnderlyingCurrency) tUnderlyingCurrency $
          LT.insert (tnum tRatioQty) tRatioQty $
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityRequestType) tSecurityRequestType $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
          LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
          LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
          LT.insert (tnum tSecurityTradingStatus) tSecurityTradingStatus $
          LT.insert (tnum tHaltReasonChar) tHaltReasonChar $
          LT.insert (tnum tInViewOfCommon) tInViewOfCommon $
          LT.insert (tnum tDueToRelated) tDueToRelated $
          LT.insert (tnum tBuyVolume) tBuyVolume $
          LT.insert (tnum tSellVolume) tSellVolume $
          LT.insert (tnum tHighPx) tHighPx $
          LT.insert (tnum tLowPx) tLowPx $
          LT.insert (tnum tAdjustment) tAdjustment $
          LT.insert (tnum tTradSesReqID) tTradSesReqID $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tContraTrader) tContraTrader $
          LT.insert (tnum tTradSesMethod) tTradSesMethod $
          LT.insert (tnum tTradSesMode) tTradSesMode $
          LT.insert (tnum tTradSesStatus) tTradSesStatus $
          LT.insert (tnum tTradSesStartTime) tTradSesStartTime $
          LT.insert (tnum tTradSesOpenTime) tTradSesOpenTime $
          LT.insert (tnum tTradSesPreCloseTime) tTradSesPreCloseTime $
          LT.insert (tnum tTradSesCloseTime) tTradSesCloseTime $
          LT.insert (tnum tTradSesEndTime) tTradSesEndTime $
          LT.insert (tnum tNumberOfOrders) tNumberOfOrders $
          LT.insert (tnum tMessageEncoding) tMessageEncoding $
          LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
          LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
          LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
          LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
          LT.insert (tnum tEncodedListExecInstLen) tEncodedListExecInstLen $
          LT.insert (tnum tEncodedListExecInst) tEncodedListExecInst $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tEncodedSubjectLen) tEncodedSubjectLen $
          LT.insert (tnum tEncodedSubject) tEncodedSubject $
          LT.insert (tnum tEncodedHeadlineLen) tEncodedHeadlineLen $
          LT.insert (tnum tEncodedHeadline) tEncodedHeadline $
          LT.insert (tnum tEncodedAllocTextLen) tEncodedAllocTextLen $
          LT.insert (tnum tEncodedAllocText) tEncodedAllocText $
          LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
          LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
          LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
          LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
          LT.insert (tnum tAllocPrice) tAllocPrice $
          LT.insert (tnum tQuoteSetValidUntilTime) tQuoteSetValidUntilTime $
          LT.insert (tnum tQuoteEntryRejectReason) tQuoteEntryRejectReason $
          LT.insert (tnum tLastMsgSeqNumProcessed) tLastMsgSeqNumProcessed $
          LT.insert (tnum tOnBehalfOfSendingTime) tOnBehalfOfSendingTime $
          LT.insert (tnum tRefTagID) tRefTagID $
          LT.insert (tnum tRefMsgType) tRefMsgType $
          LT.insert (tnum tSessionRejectReason) tSessionRejectReason $
          LT.insert (tnum tBidRequestTransType) tBidRequestTransType $
          LT.insert (tnum tContraBroker) tContraBroker $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
          LT.insert (tnum tExecRestatementReason) tExecRestatementReason $
          LT.insert (tnum tBusinessRejectRefID) tBusinessRejectRefID $
          LT.insert (tnum tBusinessRejectReason) tBusinessRejectReason $
          LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
          LT.insert (tnum tNoContraBrokers) tNoContraBrokers $
          LT.insert (tnum tMaxMessageSize) tMaxMessageSize $
          LT.insert (tnum tNoMsgTypes) tNoMsgTypes $
          LT.insert (tnum tMsgDirection) tMsgDirection $
          LT.insert (tnum tNoTradingSessions) tNoTradingSessions $
          LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
          LT.insert (tnum tDiscretionInst) tDiscretionInst $
          LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tClientBidID) tClientBidID $
          LT.insert (tnum tListName) tListName $
          LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
          LT.insert (tnum tBidType) tBidType $
          LT.insert (tnum tNumTickets) tNumTickets $
          LT.insert (tnum tSideValue1) tSideValue1 $
          LT.insert (tnum tSideValue2) tSideValue2 $
          LT.insert (tnum tNoBidDescriptors) tNoBidDescriptors $
          LT.insert (tnum tBidDescriptorType) tBidDescriptorType $
          LT.insert (tnum tBidDescriptor) tBidDescriptor $
          LT.insert (tnum tSideValueInd) tSideValueInd $
          LT.insert (tnum tLiquidityPctLow) tLiquidityPctLow $
          LT.insert (tnum tLiquidityPctHigh) tLiquidityPctHigh $
          LT.insert (tnum tLiquidityValue) tLiquidityValue $
          LT.insert (tnum tEFPTrackingError) tEFPTrackingError $
          LT.insert (tnum tFairValue) tFairValue $
          LT.insert (tnum tOutsideIndexPct) tOutsideIndexPct $
          LT.insert (tnum tValueOfFutures) tValueOfFutures $
          LT.insert (tnum tLiquidityIndType) tLiquidityIndType $
          LT.insert (tnum tWtAverageLiquidity) tWtAverageLiquidity $
          LT.insert (tnum tExchangeForPhysical) tExchangeForPhysical $
          LT.insert (tnum tOutMainCntryUIndex) tOutMainCntryUIndex $
          LT.insert (tnum tCrossPercent) tCrossPercent $
          LT.insert (tnum tProgRptReqs) tProgRptReqs $
          LT.insert (tnum tProgPeriodInterval) tProgPeriodInterval $
          LT.insert (tnum tIncTaxInd) tIncTaxInd $
          LT.insert (tnum tNumBidders) tNumBidders $
          LT.insert (tnum tTradeType) tTradeType $
          LT.insert (tnum tBasisPxType) tBasisPxType $
          LT.insert (tnum tNoBidComponents) tNoBidComponents $
          LT.insert (tnum tCountry) tCountry $
          LT.insert (tnum tTotNoStrikes) tTotNoStrikes $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tDayOrderQty) tDayOrderQty $
          LT.insert (tnum tDayCumQty) tDayCumQty $
          LT.insert (tnum tDayAvgPx) tDayAvgPx $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tNoStrikes) tNoStrikes $
          LT.insert (tnum tListStatusType) tListStatusType $
          LT.insert (tnum tNetGrossInd) tNetGrossInd $
          LT.insert (tnum tListOrderStatus) tListOrderStatus $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tListExecInstType) tListExecInstType $
          LT.insert (tnum tCxlRejResponseTo) tCxlRejResponseTo $
          LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
          LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
          LT.insert (tnum tContraTradeQty) tContraTradeQty $
          LT.insert (tnum tContraTradeTime) tContraTradeTime $
          LT.insert (tnum tClearingFirm) tClearingFirm $
          LT.insert (tnum tClearingAccount) tClearingAccount $
          LT.insert (tnum tLiquidityNumSecurities) tLiquidityNumSecurities $
          LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType $
          LT.insert (tnum tStrikeTime) tStrikeTime $
          LT.insert (tnum tListStatusText) tListStatusText $
          LT.insert (tnum tEncodedListStatusTextLen) tEncodedListStatusTextLen $
          LT.insert (tnum tEncodedListStatusText) tEncodedListStatusText $
          LT.new
