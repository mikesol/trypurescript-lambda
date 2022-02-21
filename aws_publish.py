import boto3
import time
client = boto3.client('lambda')

def wait_till_updated():
  lus = client.get_function_configuration(
      FunctionName='trypurescript',
  )['LastUpdateStatus']
  if lus == 'Failed': raise ValueError('last update failed')
  if lus == 'InProgress':
    time.sleep(5.0)
    wait_till_updated()

client.update_function_code(FunctionName="trypurescript",
                            ImageUri="201242457561.dkr.ecr.eu-west-1.amazonaws.com/wags-lambda:latest")
wait_till_updated()
newVersion = client.publish_version(FunctionName="trypurescript")['Version']
wait_till_updated()
currency_configs = client.list_provisioned_concurrency_configs(
    FunctionName='trypurescript')['ProvisionedConcurrencyConfigs']
wait_till_updated()
print('new version', newVersion)
for c in currency_configs:
  print('deleting', c['FunctionArn'])
  client.delete_provisioned_concurrency_config(
      FunctionName='trypurescript', Qualifier=c['FunctionArn'].split(':')[-1])
  wait_till_updated()
client.put_provisioned_concurrency_config(FunctionName='trypurescript', Qualifier=newVersion, ProvisionedConcurrentExecutions=10)
