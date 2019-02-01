EC2_TYPES=$(cat  ../../lib/amazonka/amazonka-ec2/gen/Network/AWS/EC2/Types/Sum.hs | sed -n '/data InstanceType/,/  deriving.*/{
   p
   }' \
 | grep -v 'data InstanceType' \
 | grep -v '.*deriving' \
 | sed -E 's/ +[|=] +(.*)/\1/g')

function str_type() {
  echo $1 | tr '[:upper:]' '[:lower:]' | sed 's/_/./'
}


echo 'fromMismiInstanceType :: MismiInstanceType -> A.InstanceType
fromMismiInstanceType m =
  case m of'
for T in $EC2_TYPES; do
  echo "     $T ->"
  echo "      A.$T"
done
echo
echo

echo 'toMismiInstanceType :: A.InstanceType -> MismiInstanceType
toMismiInstanceType i =
  case i of'
for T in $EC2_TYPES; do
  echo "     A.$T ->"
  echo "      $T"
done
echo
echo