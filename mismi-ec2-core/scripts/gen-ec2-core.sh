#!/bin/bash

EC2_TYPES=$(cat  ../../lib/amazonka/amazonka-ec2/gen/Network/AWS/EC2/Types/Sum.hs | sed -n '/data InstanceType/,/  deriving.*/{
   p
   }' \
 | grep -v 'data InstanceType' \
 | grep -v '.*deriving' \
 | sed -E 's/ +[|=] +(.*)/\1/g')

function str_type() {
  echo $1 | tr '[:upper:]' '[:lower:]' | sed 's/_/./'
}

function virt_type() {
  if [[ $1 =~ (C1_|CC1_|CG1_|CR1_|HI1_|HS1_|M1_|M2_|CC2_) ]]; then
    echo 'Paravirtual'
  else
    echo 'HVM'
  fi
}

cat <<EOF
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Core.Ec2Types (
    MismiInstanceType(..)
  , virtualizationFor
  , renderMismiInstanceType
  , parseMismiInstanceType
  ) where

import           Mismi.EC2.Core.MismiTypes

import           P

EOF

echo '-- | Mismi'"'"'s view of available EC2 instance types.
data MismiInstanceType ='
for T in $EC2_TYPES; do
  echo "  | $T"
done
echo '    deriving (Eq, Show, Ord, Enum, Bounded)'
echo
echo

echo 'virtualizationFor :: MismiInstanceType -> MismiVirtualizationType
virtualizationFor itype =
  case itype of'
for T in $EC2_TYPES; do
  echo "     $T ->"
  echo "      $(virt_type $T)"
done
echo
echo

echo 'renderMismiInstanceType :: MismiInstanceType -> Text
renderMismiInstanceType m =
  case m of'
for T in $EC2_TYPES; do
  echo "    $T ->"
  echo "      \"$(str_type $T)\""
done
echo
echo

echo 'parseMismiInstanceType :: Text -> Maybe MismiInstanceType
parseMismiInstanceType m =
  case m of'
for T in $EC2_TYPES; do
  echo "    \"$(str_type $T)\" ->"
  echo "      Just $T"
done
echo '    _ ->
      Nothing'
echo
echo
