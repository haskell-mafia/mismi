mismi-autoscaling
=================


## Testing

### Permissions

```
"autoscaling:AttachLoadBalancers"
"autoscaling:CreateAutoScalingGroup"
"autoscaling:CreateLaunchConfiguration"
"autoscaling:CreateOrUpdateTags"
"autoscaling:DeleteAutoScalingGroup"
"autoscaling:DeleteLaunchConfiguration"
"autoscaling:DescribeAutoScalingGroups"
"autoscaling:DescribeAutoScalingInstances"
"autoscaling:DescribeLaunchConfigurations"
"autoscaling:DescribeLoadBalancers"
"autoscaling:DescribeTags"
"autoscaling:DetachLoadBalancers"
"autoscaling:SetDesiredHealth"
"autoscaling:UpdateAutoScalingGroup"
```

### Options

```
`AWS_TEST_SECURITY_GROUP` - defaults to "ci.ci.node"
`AWS_TEST_IMAGE_ID` - defaults to "ami-bc3611df"
`AWS_TEST_IAM_ROLE` - defaults to "ci.ci.node"
```
