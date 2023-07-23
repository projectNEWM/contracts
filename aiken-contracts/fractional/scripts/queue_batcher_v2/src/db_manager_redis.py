import json
import redis
from typing import Optional, List, Tuple

class DatabaseManager:
    def __init__(self, host: str = 'localhost', port: int = 6379, db: int = 0):
        self.conn = redis.StrictRedis(host=host, port=port, db=db)

    def clear_database(self) -> None:
        self.conn.flushdb()
        
    def create_sale_record(self, tkn: str, txid: str, datum: dict, value: dict) -> None:
        self.conn.hset('sale', tkn, json.dumps({
            'txid': txid,
            'datum': datum,
            'value': value
        }))

    def create_queue_record(self, id: str, txid: str, tkn: str, datum: dict, value: dict, timestamp: int, tx_idx: int) -> None:
        self.conn.hset('queue', id, json.dumps({
            'txid': txid,
            'tkn': tkn,
            'datum': datum,
            'value': value,
            'timestamp': timestamp,
            'tx_idx': tx_idx
        }))
    
    def create_batcher_record(self, id: str, txid: str, value: dict) -> None:
        self.conn.hset('batcher', id, json.dumps({
            'txid': txid,
            'value': value,
        }))
    
    def delete_sale_record(self, tkn: str) -> int:
        return self.conn.hdel('sale', tkn)

    def delete_queue_record(self, id: str) -> int:
        return self.conn.hdel('queue', id)
    
    def delete_batcher_record(self, id: str) -> int:
        return self.conn.hdel('batcher', id)
    
    def read_sale_record(self, tkn: str) -> Optional[dict]:
        record = self.conn.hget('sale', tkn)
        return json.loads(record) if record else None
    
    def read_queue_record(self, id: str) -> Optional[dict]:
        record = self.conn.hget('queue', id)
        return json.loads(record) if record else None
    
    def read_batcher_record(self, id: str) -> Optional[dict]:
        record = self.conn.hget('batcher', id)
        return json.loads(record) if record else None

    def find_sale_by_utxo(self, txid: str) -> List[str]:
        records = self.conn.hgetall('sale')
        matches = [key.decode('utf-8') for key, value in records.items() if json.loads(value)['txid'] == txid]
        return matches
    
    def is_key_in_queue(self, id: str) -> bool:
        return self.conn.hexists('queue', id)

    def is_key_in_batcher(self, id: str) -> bool:
        return self.conn.hexists('batcher', id)
        
    def read_all_sale_records(self) -> List[Tuple[str, dict]]:
        records = self.conn.hgetall('sale')
        matches = [(key.decode('utf-8'), json.loads(value)) for key, value in records.items()]
        return matches

    def read_all_queue_records(self) -> List[Tuple[str, dict]]:
        records = self.conn.hgetall('queue')
        matches = [(key.decode('utf-8'), json.loads(value)) for key, value in records.items()]
        return matches

    def read_all_batcher_records(self) -> List[Tuple[str, dict]]:
        records = self.conn.hgetall('batcher')
        matches = [(key.decode('utf-8'), json.loads(value)) for key, value in records.items()]
        return matches
