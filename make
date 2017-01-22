#!/bin/zsh
PACKAGES=(ghc-typelits-{natnormalise,extra,knownnat} clash-prelude base deepseq alg affine lenz lenz-template bytestring template-haskell)
PACKAGE_FLAGS=(-hide-all-packages ${:--package ${^PACKAGES}})
WARNING_FLAGS=(-Wall -Wcompat -Wredundant-constraints -Wno-name-shadowing -Wincomplete-record-updates -Wincomplete-uni-patterns -Wno-partial-type-signatures -Wno-orphans -Werror=incomplete-patterns -Werror=incomplete-uni-patterns -Werror=incomplete-record-updates -Werror=missing-fields -Werror=missing-methods)
EXTENSION_FLAGS=(-XUnicodeSyntax -XLambdaCase -XPartialTypeSignatures -XRankNTypes -XFlexibleContexts -XFlexibleInstances -XDerivingStrategies -XStandaloneDeriving -XGeneralizedNewtypeDeriving -XDeriveFunctor -XDeriveGeneric)
exec stack exec -- clash $PACKAGE_FLAGS -fconstraint-solver-iterations=256 $WARNING_FLAGS $EXTENSION_FLAGS -i -isrc "$@" Main
