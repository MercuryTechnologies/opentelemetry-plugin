{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module OpenTelemetry.Plugin.Compat (mkSampler) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import OpenTelemetry.Context (Context)
import OpenTelemetry.Trace (Attribute, SpanArguments)
import OpenTelemetry.Trace.Sampler (Sampler(..), SamplingResult)
import OpenTelemetry.Trace.TraceState (TraceState)

#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
import OpenTelemetry.Trace.Sampler (SamplingDecision(..))
#endif

{-| Construct a `Sampler` from a description and a sampling function.

    This papers over the API changes between the pre- and post-1.0 API changes
    with a common type signature and CPP in the implementation body.
-}
mkSampler
    :: Text
    -> (Context -> SpanArguments -> IO (SamplingResult, HashMap Text Attribute, TraceState))
    -> Sampler
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
mkSampler description f =
    CustomSampler description \context _traceId _name spanArguments _scope -> do
        (result, attrs, traceState_) <- f context spanArguments
        pure (SamplingDecision result attrs traceState_)
#else
mkSampler description f =
    Sampler
        { getDescription = description
        , shouldSample = \context _traceId _name spanArguments ->
            f context spanArguments
        }
#endif
