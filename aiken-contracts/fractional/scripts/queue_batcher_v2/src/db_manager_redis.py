import json
import redis

class DatabaseManager:
    def __init__(self, host='localhost', port=6379, db=0):
        self.conn = redis.StrictRedis(host=host, port=port, db=db)

    def clear_database(self):
        self.conn.flushdb()
        
    def create_sale_record(self, tkn, txid, datum, value):
        self.conn.hset('sale', tkn, json.dumps({
            'txid': txid,
            'datum': datum,
            'value': value
        }))

    def create_queue_record(self, txid, tkn, datum, value, timestamp):
        self.conn.hset('queue', txid, json.dumps({
            'tkn': tkn,
            'datum': datum,
            'value': value,
            'timestamp': timestamp
        }))
    
    def delete_sale_record(self, tkn):
        self.conn.hdel('sale', tkn)

    def delete_queue_record(self, txid):
        self.conn.hdel('queue', txid)
        # try:
        # result = self.conn.hdel('queue', txid)
        #     print("Deletion result:", result)  # Debugging print to check the result of the deletion
        # except Exception as e:
        #     print("Error deleting record:", e) 
    
    def read_sale_record(self, tkn):
        record = self.conn.hget('sale', tkn)
        return json.loads(record) if record else None

    def find_sale_by_utxo(self, txid):
        records = self.conn.hgetall('sale')
        matches = [(key.decode('utf-8')) for key, value in records.items() if json.loads(value)['txid'] == txid]
        return matches
    
    def is_key_in_queue(self, txid):
        return self.conn.hexists('queue', txid)
        
    def read_all_sale_records(self):
        records = self.conn.hvals('sale')
        return [json.loads(record) for record in records]

    def read_all_queue_records(self):
        records = self.conn.hgetall('queue')
        matches = [(key.decode('utf-8')) for key, value in records.items()]
        return matches


