kubectl apply -f ../configmap/bakery-controller.yaml
kubectl apply -f ../cassandra-dc1.yaml
kubectl apply -f ../cassandra-dc2.yaml
kubectl apply -f ../example-client-app.yaml
kubectl apply -f ../kafka-event-sink.yaml
sleep 3000
kubectl exec cassandra-dc1-0 -- cqlsh --execute="CREATE KEYSPACE bakery WITH replication = {'class': 'NetworkTopologyStrategy', 'DC1': '3', 'DC2': '3'}  AND durable_writes = true;"
kubectl exec cassandra-dc1-0 -- cqlsh --execute="describe keyspaces;"
