EC2_TYPES=$(cat  ../../lib/amazonka/amazonka-ec2/gen/Network/AWS/EC2/Types/Sum.hs | sed -n '/data InstanceType/,/  deriving.*/{
   p
   }' \
 | grep -v 'data InstanceType' \
 | grep -v '.*deriving' \
 | sed -E 's/ +[|=] +(.*)/\1/g')

function str_type() {
  echo $1 | tr '[:upper:]' '[:lower:]' | sed 's/_/./'
}

./gen-ec2.sh > ec2-Data.hs

./gen-ec2-core.sh > ec2-core-Data.hs
