{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Optics.Core qualified as Optics
import Plutus.Script.Utils.Scripts qualified as PSU
import PlutusLedgerApi.V1.Address qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.Internal qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.Prelude
import PlutusTx.TH qualified as PlutusTx
import Prettyprinter qualified as PP
import Prettyprinter.Extras qualified as PP
import Prelude qualified as HS

class ValidatorTypes a where
  -- Minting purpose type variables with default
  type MintingRedeemer a
  type MintingTxInfo a

  type MintingRedeemer a = ()
  type MintingTxInfo a = Api.TxInfo

  -- Spending purpose type variables with default
  type SpendingRedeemer a
  type SpendingTxInfo a
  type Datum a

  type SpendingRedeemer a = ()
  type SpendingTxInfo a = Api.TxInfo
  type Datum a = ()

  -- Rewarding purpose type variables with default
  type RewardingRedeemer a
  type RewardingTxInfo a

  type RewardingRedeemer a = ()
  type RewardingTxInfo a = Api.TxInfo

  -- Certifying purpose type variables with default
  type CertifyingRedeemer a
  type CertifyingTxInfo a

  type CertifyingRedeemer a = ()
  type CertifyingTxInfo a = Api.TxInfo

  -- Voting purpose type variables with default
  type VotingRedeemer a
  type VotingTxInfo a

  type VotingRedeemer a = ()
  type VotingTxInfo a = Api.TxInfo

  -- Proposing purpose type variables with default
  type ProposingRedeemer a
  type ProposingTxInfo a

  type ProposingRedeemer a = ()
  type ProposingTxInfo a = Api.TxInfo

type ExplicitMintingScript mintingRed mintingTxInfo = Api.CurrencySymbol -> mintingRed -> mintingTxInfo -> Bool

type MintingScript a = ExplicitMintingScript (MintingRedeemer a) (MintingTxInfo a)

type ExplicitSpendingScript datum spendingRed spendingTxInfo = Api.TxOutRef -> Maybe datum -> spendingRed -> spendingTxInfo -> Bool

type SpendingScript a = ExplicitSpendingScript (Datum a) (SpendingRedeemer a) (SpendingTxInfo a)

type ExplicitRewardingScript rewardingRed rewardingTxInfo = Api.Credential -> rewardingRed -> rewardingTxInfo -> Bool

type RewardingScript a = ExplicitRewardingScript (RewardingRedeemer a) (RewardingTxInfo a)

type ExplicitCertifyingScript certifyingRed certifyingTxInfo = Integer -> Api.TxCert -> certifyingRed -> certifyingTxInfo -> Bool

type CertifyingScript a = ExplicitCertifyingScript (CertifyingRedeemer a) (CertifyingTxInfo a)

type ExplicitVotingScript votingRed votingTxInfo = Api.Voter -> votingRed -> votingTxInfo -> Bool

type VotingScript a = ExplicitVotingScript (VotingRedeemer a) (VotingTxInfo a)

type ExplicitProposingScript proposingRed proposingTxInfo = Integer -> Api.ProposalProcedure -> proposingRed -> proposingTxInfo -> Bool

type ProposingScript a = ExplicitProposingScript (ProposingRedeemer a) (ProposingTxInfo a)

data TypedMultiPurposeScript a = TypedMultiPurposeScript
  { mintingScript :: MintingScript a,
    spendingScript :: SpendingScript a,
    rewardingScript :: RewardingScript a,
    certifyingScript :: CertifyingScript a,
    votingScript :: VotingScript a,
    proposingScript :: ProposingScript a
  }

data
  ExplicitTypedMultiPurposeScript
    mintingRed
    mintingTxInfo
    datum
    spendingRed
    spendingTxInfo
    rewardingRed
    rewardingTxInfo
    certifyingRed
    certifyingTxInfo
    votingRed
    votingTxInfo
    proposingRed
    proposingTxInfo = ExplicitTypedMultiPurposeScript
  { explicitMintingScript :: ExplicitMintingScript mintingRed mintingTxInfo,
    explicitSpendingScript :: ExplicitSpendingScript datum spendingRed spendingTxInfo,
    explicitRewardingScript :: ExplicitRewardingScript rewardingRed rewardingTxInfo,
    explicitCertifyingScript :: ExplicitCertifyingScript certifyingRed certifyingTxInfo,
    explicitVotingScript :: ExplicitVotingScript votingRed votingTxInfo,
    explicitProposingScript :: ExplicitProposingScript proposingRed proposingTxInfo
  }

typedToExplicitTypedMultiPurposeScript ::
  TypedMultiPurposeScript a ->
  ExplicitTypedMultiPurposeScript
    (MintingRedeemer a)
    (MintingTxInfo a)
    (Datum a)
    (SpendingRedeemer a)
    (SpendingTxInfo a)
    (RewardingRedeemer a)
    (RewardingTxInfo a)
    (CertifyingRedeemer a)
    (CertifyingTxInfo a)
    (VotingRedeemer a)
    (VotingTxInfo a)
    (ProposingRedeemer a)
    (ProposingTxInfo a)
typedToExplicitTypedMultiPurposeScript TypedMultiPurposeScript {..} =
  ExplicitTypedMultiPurposeScript mintingScript spendingScript rewardingScript certifyingScript votingScript proposingScript

{-# INLINEABLE alwaysFalseTypedMultiPurposeScript #-}
alwaysFalseTypedMultiPurposeScript :: TypedMultiPurposeScript a
alwaysFalseTypedMultiPurposeScript =
  TypedMultiPurposeScript
    (\_ _ _ -> False)
    (\_ _ _ _ -> False)
    (\_ _ _ -> False)
    (\_ _ _ _ -> False)
    (\_ _ _ -> False)
    (\_ _ _ _ -> False)

{-# INLINEABLE alwaysTrueTypedMultiPurposeScript #-}
alwaysTrueTypedMultiPurposeScript :: TypedMultiPurposeScript a
alwaysTrueTypedMultiPurposeScript =
  TypedMultiPurposeScript
    (\_ _ _ -> True)
    (\_ _ _ _ -> True)
    (\_ _ _ -> True)
    (\_ _ _ _ -> True)
    (\_ _ _ -> True)
    (\_ _ _ _ -> True)

alwaysTrueMintingScript :: TypedMultiPurposeScript a
alwaysTrueMintingScript = alwaysFalseTypedMultiPurposeScript `withMintingPurpose` \_ _ _ -> True

alwaysTrueSpendingScript :: TypedMultiPurposeScript a
alwaysTrueSpendingScript = alwaysFalseTypedMultiPurposeScript `withSpendingPurpose` \_ _ _ _ -> True

-- * Working with the Minting purpose of a multipurpose script

-- | Adds (or overrides) the minting purpose to a V3 typed script
{-# INLINEABLE withMintingPurpose #-}
withMintingPurpose :: TypedMultiPurposeScript a -> MintingScript a -> TypedMultiPurposeScript a
withMintingPurpose ts ms = ts {mintingScript = ms}

-- | Adds a minting constraint to an existing minting purpose
{-# INLINEABLE addMintingConstraint #-}
addMintingConstraint ::
  TypedMultiPurposeScript a -> MintingScript a -> TypedMultiPurposeScript a
addMintingConstraint ts ms = ts `withMintingPurpose` \cs red txInfo -> mintingScript ts cs red txInfo && ms cs red txInfo

-- | Utility function to check that an input exists at a given script address
{-# INLINEABLE inputExistsAtScriptAddress #-}
inputExistsAtScriptAddress :: [Api.TxInInfo] -> BuiltinByteString -> Bool
inputExistsAtScriptAddress txInfo bs =
  bs
    `elem` [ h
             | Api.TxInInfo _ (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <- txInfo
           ]

-- | Getting the inputs from a TxInfo
{-# INLINEABLE txInfoInputsG #-}
txInfoInputsG :: Optics.Getter Api.TxInfo [Api.TxInInfo]
txInfoInputsG = Optics.to Api.txInfoInputs

-- | Minting policy that ensure a given validator is called in the transaction
{-# INLINEABLE withForwardingMintingScript #-}
withForwardingMintingScript ::
  TypedMultiPurposeScript a ->
  PSU.ValidatorHash ->
  Optics.Getter (MintingTxInfo a) [Api.TxInInfo] ->
  TypedMultiPurposeScript a
withForwardingMintingScript ts (PSU.ValidatorHash hash) getter =
  ts `withMintingPurpose` \_ _ txInfo -> inputExistsAtScriptAddress (Optics.view getter txInfo) hash

-- | Minting policy that ensure the own spending purpose check is called in the transaction
{-# INLINEABLE withOwnForwardingMintingScript #-}
withOwnForwardingMintingScript ::
  TypedMultiPurposeScript a ->
  Optics.Getter (MintingTxInfo a) [Api.TxInInfo] ->
  TypedMultiPurposeScript a
withOwnForwardingMintingScript ts getter =
  ts `withMintingPurpose` \(Api.CurrencySymbol cs) _ txInfo -> inputExistsAtScriptAddress (Optics.view getter txInfo) cs

-- * Working with the Spending purpose of a multipurpose script

-- | Adds (or overrides) the spending purpose to a V3 typed script
{-# INLINEABLE withSpendingPurpose #-}
withSpendingPurpose ::
  TypedMultiPurposeScript a -> SpendingScript a -> TypedMultiPurposeScript a
withSpendingPurpose ts ss = ts {spendingScript = ss}

-- | Adds a spending constraint to an existing spending purpose
{-# INLINEABLE addSpendingConstraint #-}
addSpendingConstraint ::
  TypedMultiPurposeScript a -> SpendingScript a -> TypedMultiPurposeScript a
addSpendingConstraint ts ss =
  ts `withSpendingPurpose` \oRef mDat red txInfo -> spendingScript ts oRef mDat red txInfo && ss oRef mDat red txInfo

-- | Getting the minted value from a TxInfo
{-# INLINEABLE txInfoMintValueG #-}
txInfoMintValueG :: Optics.Getter Api.TxInfo Api.Value
txInfoMintValueG = Optics.to Api.txInfoMint

-- | Spending purpose that ensures a given minting script is invoked in the transaction
{-# INLINEABLE withForwardSpendingScript #-}
withForwardSpendingScript ::
  TypedMultiPurposeScript a ->
  PSU.MintingPolicyHash ->
  Optics.Getter (SpendingTxInfo a) Api.Value ->
  TypedMultiPurposeScript a
withForwardSpendingScript ts (PSU.MintingPolicyHash hash) getter =
  ts `withSpendingPurpose` \_ _ _ txInfo -> Api.CurrencySymbol hash `Map.member` Api.getValue (Optics.view getter txInfo)

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE withOwnForwardSpendingScript #-}
withOwnForwardSpendingScript ::
  TypedMultiPurposeScript a ->
  Optics.Getter (SpendingTxInfo a) Api.Value ->
  Optics.Getter (SpendingTxInfo a) [Api.TxInInfo] ->
  TypedMultiPurposeScript a
withOwnForwardSpendingScript ts mintedValueGetter inputsGetter =
  ts `withSpendingPurpose` \oRef _ _ txInfo ->
    any
      ((`Map.member` Api.getValue (Optics.view mintedValueGetter txInfo)) . Api.CurrencySymbol)
      [ h
        | Api.TxInInfo ref (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <-
            Optics.view inputsGetter txInfo,
          ref == oRef
      ]

-- * Working with the Rewarding purpose of a multipurpose script

{-# INLINEABLE withRewardingPurpose #-}
withRewardingPurpose ::
  TypedMultiPurposeScript a -> RewardingScript a -> TypedMultiPurposeScript a
withRewardingPurpose ts rs = ts {rewardingScript = rs}

{-# INLINEABLE withCertifyingPurpose #-}
withCertifyingPurpose ::
  TypedMultiPurposeScript a -> CertifyingScript a -> TypedMultiPurposeScript a
withCertifyingPurpose ts cs = ts {certifyingScript = cs}

{-# INLINEABLE withVotingPurpose #-}
withVotingPurpose :: TypedMultiPurposeScript a -> VotingScript a -> TypedMultiPurposeScript a
withVotingPurpose ts vs = ts {votingScript = vs}

{-# INLINEABLE withProposingPurpose #-}
withProposingPurpose ::
  TypedMultiPurposeScript a -> ProposingScript a -> TypedMultiPurposeScript a
withProposingPurpose ts ps = ts {proposingScript = ps}

data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData,
    scriptContextRedeemer :: BuiltinData,
    scriptContextScriptInfo :: Api.ScriptInfo
  }

PlutusTx.unstableMakeIsData ''ScriptContextResolvedScriptInfo

newtype MultiPurposeScript a = MultiPurposeScript {getMultiPurposeScript :: PSU.Script}
  deriving stock (Generic)
  deriving newtype (HS.Eq, HS.Ord, Serialise)
  deriving (PP.Pretty) via (PP.PrettyShow (MultiPurposeScript a))

instance HS.Show (MultiPurposeScript a) where
  show _ = "Multi purpose script { <script> }"

multiPurposeToMintingPolicy :: MultiPurposeScript a -> PSU.MintingPolicy
multiPurposeToMintingPolicy = PSU.MintingPolicy . getMultiPurposeScript

multiPurposeToValidator :: MultiPurposeScript a -> PSU.Validator
multiPurposeToValidator = PSU.Validator . getMultiPurposeScript

multiPurposeToStakeValidator :: MultiPurposeScript a -> PSU.StakeValidator
multiPurposeToStakeValidator = PSU.StakeValidator . getMultiPurposeScript

multiPurposeToScriptHash :: MultiPurposeScript a -> PSU.ScriptHash
multiPurposeToScriptHash = PSU.scriptHash . (`PSU.Versioned` PSU.PlutusV3) . getMultiPurposeScript

multiPurposeScriptAddress :: MultiPurposeScript a -> Api.Address
multiPurposeScriptAddress = Api.scriptHashAddress . multiPurposeToScriptHash

multiPurposeToValidatorHash :: MultiPurposeScript a -> PSU.ValidatorHash
multiPurposeToValidatorHash = PSU.validatorHash . (`PSU.Versioned` PSU.PlutusV3) . multiPurposeToValidator

multiPurposeToStakeValidatorHash :: MultiPurposeScript a -> PSU.StakeValidatorHash
multiPurposeToStakeValidatorHash = PSU.stakeValidatorHash . (`PSU.Versioned` PSU.PlutusV3) . multiPurposeToStakeValidator

multiPurposeMintingPolicyHash :: MultiPurposeScript a -> PSU.MintingPolicyHash
multiPurposeMintingPolicyHash = PSU.mintingPolicyHash . (`PSU.Versioned` PSU.PlutusV3) . multiPurposeToMintingPolicy

multiPurposeScriptCurrencySymbol :: MultiPurposeScript a -> Api.CurrencySymbol
multiPurposeScriptCurrencySymbol = PSU.scriptCurrencySymbol . (`PSU.Versioned` PSU.PlutusV3) . multiPurposeToMintingPolicy

type UntypedMultiPurposeScript = BuiltinData -> PlutusTx.BuiltinUnit

compileUntypedMultiPurposeScript :: UntypedMultiPurposeScript -> MultiPurposeScript a
compileUntypedMultiPurposeScript script = MultiPurposeScript $ PSU.Script $ Api.serialiseCompiledCode $$(PlutusTx.compile [||script||])

{-# INLINEABLE typedToUntypedMultiPurposeScript #-}
typedToUntypedMultiPurposeScript ::
  ( Api.FromData mintingRed,
    Api.FromData mintingTxInfo,
    Api.FromData datum,
    Api.FromData spendingRed,
    Api.FromData spendingTxInfo,
    Api.FromData rewardingRed,
    Api.FromData rewardingTxInfo,
    Api.FromData certifyingRed,
    Api.FromData certifyingTxInfo,
    Api.FromData votingRed,
    Api.FromData votingTxInfo,
    Api.FromData proposingRed,
    Api.FromData proposingTxInfo
  ) =>
  ExplicitTypedMultiPurposeScript
    mintingRed
    mintingTxInfo
    datum
    spendingRed
    spendingTxInfo
    rewardingRed
    rewardingTxInfo
    certifyingRed
    certifyingTxInfo
    votingRed
    votingTxInfo
    proposingRed
    proposingTxInfo ->
  UntypedMultiPurposeScript
typedToUntypedMultiPurposeScript ExplicitTypedMultiPurposeScript {..} dat = either traceError check $ do
  ScriptContextResolvedScriptInfo {..} <- fromBuiltinDataEither "script info" dat
  case scriptContextScriptInfo of
    Api.MintingScript cur -> do
      (red, txInfo) <- deserializeContext "minting" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Minting" $ explicitMintingScript cur red txInfo
    Api.SpendingScript oRef mDat -> do
      (red, txInfo) <- deserializeContext "spending" scriptContextRedeemer scriptContextTxInfo
      mResolvedDat <- case mDat of
        Nothing -> return Nothing
        Just (Api.Datum bDat) -> Just <$> fromBuiltinDataEither "datum" bDat
      return $ traceRunning "Spending" $ explicitSpendingScript oRef mResolvedDat red txInfo
    Api.RewardingScript cred -> do
      (red, txInfo) <- deserializeContext "rewarding" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Rewarding" $ explicitRewardingScript cred red txInfo
    Api.CertifyingScript i cert -> do
      (red, txInfo) <- deserializeContext "certifying" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Certifying" $ explicitCertifyingScript i cert red txInfo
    Api.VotingScript voter -> do
      (red, txInfo) <- deserializeContext "voting" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Voting" $ explicitVotingScript voter red txInfo
    Api.ProposingScript i prop -> do
      (red, txInfo) <- deserializeContext "proposing" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Proposing" $ explicitProposingScript i prop red txInfo
  where
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " <> name) Right . PlutusTx.fromBuiltinData
    traceRunning name = trace ("Running the validator with the " <> name <> " script purpose")
    deserializeContext name redData txInfoData = do
      red <- fromBuiltinDataEither (name <> " redeemer") redData
      txInfo <- fromBuiltinDataEither (name <> " tx info") txInfoData
      return (red, txInfo)

compileTypedMultiPurposeScript ::
  ( PlutusTx.FromData (MintingRedeemer a),
    PlutusTx.FromData (MintingTxInfo a),
    PlutusTx.FromData (SpendingRedeemer a),
    PlutusTx.FromData (SpendingTxInfo a),
    PlutusTx.FromData (Datum a),
    PlutusTx.FromData (RewardingRedeemer a),
    PlutusTx.FromData (RewardingTxInfo a),
    PlutusTx.FromData (CertifyingRedeemer a),
    PlutusTx.FromData (CertifyingTxInfo a),
    PlutusTx.FromData (VotingRedeemer a),
    PlutusTx.FromData (VotingTxInfo a),
    PlutusTx.FromData (ProposingRedeemer a),
    PlutusTx.FromData (ProposingTxInfo a)
  ) =>
  TypedMultiPurposeScript a ->
  MultiPurposeScript a
compileTypedMultiPurposeScript =
  compileUntypedMultiPurposeScript
    . typedToUntypedMultiPurposeScript
    . typedToExplicitTypedMultiPurposeScript

class ToBuiltinUnit a where
  toBuiltinUnit :: a -> PlutusTx.BuiltinUnit

instance ToBuiltinUnit PlutusTx.BuiltinUnit where
  toBuiltinUnit = id

instance ToBuiltinUnit () where
  toBuiltinUnit = PlutusTx.BuiltinUnit

instance ToBuiltinUnit Bool where
  toBuiltinUnit = check

{-# INLINEABLE genericToUntypedMultiPurposeScript #-}
genericToUntypedMultiPurposeScript ::
  (PlutusTx.FromData a, ToBuiltinUnit b) => (a -> b) -> UntypedMultiPurposeScript
genericToUntypedMultiPurposeScript script dat = case PlutusTx.fromBuiltinData dat of
  Nothing -> traceError "Unable to deserialize to the desired type"
  Just ctx -> toBuiltinUnit $ script ctx

compileGenericMultiPurposeScript ::
  (PlutusTx.FromData a, ToBuiltinUnit b) => (a -> b) -> MultiPurposeScript c
compileGenericMultiPurposeScript = compileUntypedMultiPurposeScript . genericToUntypedMultiPurposeScript
