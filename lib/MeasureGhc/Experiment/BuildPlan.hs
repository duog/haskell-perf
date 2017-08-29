{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedLabels, OverloadedStrings, FlexibleContexts, DataKinds #-}
module MeasureGhc.Experiment.BuildPlan where

import Stackage.Types
import Stackage.BuildPlan
import Stackage.PerformBuild
import Distribution.Types.PackageName hiding (mkPackageName)
import Distribution.Version
import Data.Generics.Product
import Data.Generics.Labels
import Debug.Trace
import MeasureGhc.Prelude
import qualified Data.Set as Set
import Control.Monad.State.Strict

adjustPackagePlan :: Set PackageName -> PackagePlan -> PackagePlan
adjustPackagePlan wanted_packages = foldr (.) id
  [ #ppGithubPings .~ Empty
  , #ppUsers %~ Set.intersection wanted_packages
  ]

adjustBuildPlan :: Maybe (Set PackageName) -> BuildPlan -> BuildPlan
adjustBuildPlan mb_packages bp = bp &~ do
  #bpGithubUsers .= Empty
  #bpPackages %= \packages_map0 ->
    let add_package_and_deps :: MonadState (Set PackageName) m => PackageName -> m ()
        add_package_and_deps pn = do
          let packages_deps :: Set  PackageName
              packages_deps = setOf
                (ix pn . #ppDesc . #sdPackages . ifolded . asIndex)
                packages_map0
              setup_deps = setOf
                (ix pn . #ppDesc . #sdSetupDeps . folded . folded)
                packages_map0
              deps :: Set PackageName
              deps = (packages_deps <> setup_deps) & contains pn .~ False
          already_done <- contains pn <<.= True
          unless already_done $ do
            for_ deps add_package_and_deps
        wanted_packages :: Set PackageName
        wanted_packages =
          mb_packages
            & maybe
              (setOf (ifolded . asIndex) packages_map0)
              (\ps -> execState (traverse_ add_package_and_deps ps) Empty)
            & contains (mkPackageName "Cabal") .~ True
        packages_map = packages_map0
          & _Wrapped %~ toListOf
            ( folded
            . filtered (\x -> wanted_packages ^. contains (x ^. _1))
            . to (_2 %~ adjustPackagePlan wanted_packages)
            )
    in packages_map
  #bpSystemInfo . #siGhcVersion .= mkVersion [8, 3]

performAdjustedBuild ::
     BuildPlan
  -> Int
  -> FilePath
  -> (ByteString -> IO ())
  -> FilePath
  -> (Text -> PBBuildType -> ([Text] -> IO ()) -> IO ())
  -> IO [Text]
performAdjustedBuild pbPlan pbJobs pbInstallDest pbLog pbLogDir pbBuildCallback =
  performBuild pb
  where
    pbGlobalInstall = False
    pbEnableTests = BuildOnly
    pbEnableBenches = BuildOnly
    pbEnableHaddock = False
    pbEnableLibProfiling = False
    pbEnableExecDyn = True
    pbVerbose = True
    pbAllowNewer = False
    pbBuildHoogle = False
    pbNoRebuildCabal = False
    pbCabalFromHead = False
    pb = PerformBuild {..}
