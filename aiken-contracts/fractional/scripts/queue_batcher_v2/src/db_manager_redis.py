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

    def create_queue_record(self, id, txid, tkn, datum, value, timestamp, tx_idx):
        self.conn.hset('queue', id, json.dumps({
            'txid': txid,
            'tkn': tkn,
            'datum': datum,
            'value': value,
            'timestamp': timestamp,
            'tx_idx': tx_idx
        }))
    
    def delete_sale_record(self, tkn):
        self.conn.hdel('sale', tkn)

    def delete_queue_record(self, txid):
        self.conn.hdel('queue', txid)
    
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
        records = self.conn.hgetall('sale')
        matches = [(key.decode('utf-8'), json.loads(value)) for key, value in records.items()]
        return matches

    def read_all_queue_records(self):
        records = self.conn.hgetall('queue')
        matches = [(key.decode('utf-8'), json.loads(value)) for key, value in records.items()]
        return matches


