Below are some examples of accessing the data

# Sources

[source]
type = "N2N"
address = ["Tcp", "logicalmechanism.asuscomm.com:6001"]
magic = "mainnet"


[source]
type = "N2C"
address = ["Unix", "/home/logic/Documents/Work/LogicalMechanism/full-node-wallet/node/db-testnet/node.socket"]
magic = "testnet"

# Sinks

[sink]
type = "Webhook"
url = "http://localhost:8010/webhook/v1/oura/asset-metadata-sync"
timeout = 60000
error_policy = "Exit"

[sink]
type = "Terminal"
throttle_min_span_millis = 500
wrap = true

# Intersects

[source.intersect]
type = "Point"
value = [34117450, "529c536c02fa056874ff2e9aaa56d79b7e4d3b0fceafc1321cbd048dd9361808"]

[source.intersect]
type = "Point"
value = [34532183, "3169a6873e9191a39a50606efc15467b2a712f8f57156e9fd57b801843604b8d"]

[source.intersect]
type = "Origin"

[source.intersect]
type = "Tip"