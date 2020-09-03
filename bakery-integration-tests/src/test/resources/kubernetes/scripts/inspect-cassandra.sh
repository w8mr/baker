kubectl exec cassandra-dc1-0 -ti -- cqlsh --execute="SELECT COUNT(*) FROM bakery.messages;"
kubectl exec cassandra-dc1-0 -ti -- cqlsh --execute="SELECT COUNT(*) FROM bakery.snapshots;"