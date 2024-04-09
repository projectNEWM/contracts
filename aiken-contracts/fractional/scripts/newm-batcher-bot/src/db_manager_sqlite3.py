import json
import sqlite3


class DatabaseManagerSQLite:

    def __init__(self, db_file='batcher.db'):
        self.db_file = db_file
        self.conn = sqlite3.connect(db_file)
        self.init_db()

    def init_db(self):
        # Initialize database tables
        with self.conn:
            # Table for sale records
            self.conn.execute("""
                CREATE TABLE IF NOT EXISTS sale (
                    tkn TEXT PRIMARY KEY,
                    txid TEXT,
                    datum TEXT,
                    value TEXT
                )
            """)

            # Table for queue records
            self.conn.execute("""
                CREATE TABLE IF NOT EXISTS queue (
                    tag TEXT PRIMARY KEY,
                    txid TEXT,
                    tkn TEXT,
                    datum TEXT,
                    value TEXT,
                    timestamp INTEGER,
                    tx_idx INTEGER
                )
            """)

            # Table for batcher records
            self.conn.execute("""
                CREATE TABLE IF NOT EXISTS batcher (
                    tag TEXT PRIMARY KEY,
                    txid TEXT,
                    value TEXT
                )
            """)

            # Table for batcher records
            self.conn.execute("""
                CREATE TABLE IF NOT EXISTS status (
                    id TEXT PRIMARY KEY,
                    block_number INTEGER,
                    block_hash TEXT,
                    timestamp INTEGER
                )
            """)

            # Table for the seen records
            self.conn.execute("""
                CREATE TABLE IF NOT EXISTS seen (
                    id TEXT PRIMARY KEY,
                    tag TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)

    def cleanup(self):
        # Perform any necessary cleanup before closing the database
        conn = self.get_connection()
        try:
            conn.commit()
        finally:
            conn.close()

    def get_connection(self):
        return sqlite3.connect(self.db_file)

    def dict_to_json(self, dict_data):
        return json.dumps(dict_data)

    def json_to_dict(self, json_data):
        return json.loads(json_data)

    # CRUD for Status
    def init_status_record(self, constants):
        conn = self.get_connection()
        try:
            # use the default values in constants
            conn.execute(
                'INSERT OR IGNORE INTO status (id, block_number, block_hash, timestamp) VALUES (?, ?, ?, ?)',
                ("unique_status", constants["starting_block_number"],
                 constants["starting_blockhash"], constants["starting_timestamp"])
            )
            conn.commit()
        finally:
            conn.close()

    # it only gets created once, so the id is always known
    def update_status_record(self, block_number, block_hash, timestamp):
        conn = self.get_connection()
        try:
            conn.execute(
                'UPDATE status SET block_number = ?, block_hash = ?, timestamp = ? WHERE id = ?',
                (block_number, block_hash, timestamp, "unique_status")
            )
            conn.commit()
        finally:
            conn.close()

    def read_status_record(self):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            # ... rest of your code ...
            cursor.execute(
                'SELECT block_number, block_hash, timestamp FROM status WHERE id = ?', ("unique_status",))
            record = cursor.fetchone()
            if record:
                block_number, block_hash, timestamp = record
                return {'block_number': block_number, 'block_hash': block_hash, 'timestamp': timestamp}
            return {}
        finally:
            conn.close()

    # CRUD for Batcher
    def create_batcher_record(self, tag, txid, value):
        conn = self.get_connection()
        try:
            value_json = self.dict_to_json(value)
            conn.execute(
                'INSERT OR REPLACE INTO batcher (tag, txid, value) VALUES (?, ?, ?)',
                (tag, txid, value_json)
            )
            conn.commit()
        finally:
            conn.close()

    def read_batcher_record(self, tag):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT txid, value FROM batcher WHERE tag = ?', (tag,))
            record = cursor.fetchone()
            if record:
                txid, value_json = record
                value = self.json_to_dict(value_json)
                return {'txid': txid, 'value': value}
            return None
        finally:
            conn.close()

    # tag TEXT PRIMARY KEY,
    # txid TEXT,
    # value TEXT
    def read_all_batcher_records(self):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT tag, txid, value FROM batcher')
            records = cursor.fetchall()
            batcher_records = []
            for record in records:
                tag, txid, value_json = record
                value = self.json_to_dict(value_json)
                batcher_records.append((tag, {'tag': tag, 'txid': txid, 'value': value}))
            return batcher_records
        finally:
            conn.close()

    def delete_batcher_record(self, tag):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            # Check if the record exists
            cursor.execute('SELECT EXISTS(SELECT 1 FROM batcher WHERE tag = ?)', (tag,))
            exists = cursor.fetchone()[0]
            if exists:
                # If the record exists, delete it
                cursor.execute('DELETE FROM batcher WHERE tag = ?', (tag,))
                conn.commit()
                return True  # Record existed and was deleted
            else:
                return False  # Record did not exist
        finally:
            conn.close()

    # CRUD for Sale
    def create_sale_record(self, tkn, txid, datum, value):
        conn = self.get_connection()
        try:
            datum_json = self.dict_to_json(datum)
            value_json = self.dict_to_json(value)
            conn.execute(
                'INSERT OR REPLACE INTO sale (tkn, txid, datum, value) VALUES (?, ?, ?, ?)',
                (tkn, txid, datum_json, value_json)
            )
            conn.commit()
        finally:
            conn.close()

    def read_sale_record(self, tkn):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT txid, datum, value FROM sale WHERE tkn = ?', (tkn,))
            record = cursor.fetchone()
            if record:
                txid, datum_json, value_json = record
                datum = self.json_to_dict(datum_json)
                value = self.json_to_dict(value_json)
                return (tkn, {'txid': txid, 'datum': datum, 'value': value})
            return ('', {})
        finally:
            conn.close()

    # tkn TEXT PRIMARY KEY,
    # txid TEXT,
    # datum TEXT,
    # value TEXT
    def read_all_sale_records(self):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT tkn, txid, datum, value FROM sale')
            records = cursor.fetchall()
            sale_records = []
            for record in records:
                tkn, txid, datum_json, value_json = record
                datum = self.json_to_dict(datum_json)
                value = self.json_to_dict(value_json)
                # sale_records.append((tkn, txid, datum, value))
                sale_records.append((tkn, {'txid': txid, 'datum': datum, 'value': value}))
            return sale_records
        finally:
            conn.close()

    def delete_sale_record_by_txid(self, txid):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            # Check if the record exists with the given txid
            cursor.execute('SELECT EXISTS(SELECT 1 FROM sale WHERE txid = ?)', (txid,))
            exists = cursor.fetchone()[0]
            if exists:
                # If the record exists, delete it
                cursor.execute('DELETE FROM sale WHERE txid = ?', (txid,))
                conn.commit()
                return True  # Record with the given txid existed and was deleted
            else:
                return False  # No record with the given txid
        finally:
            conn.close()

    # CRUD for Queue
    def create_queue_record(self, tag, txid, tkn, datum, value, timestamp, tx_idx):
        conn = self.get_connection()
        try:
            datum_json = self.dict_to_json(datum)
            value_json = self.dict_to_json(value)
            conn.execute(
                'INSERT OR REPLACE INTO queue (tag, txid, tkn, datum, value, timestamp, tx_idx) VALUES (?, ?, ?, ?, ?, ?, ?)',
                (tag, txid, tkn, datum_json, value_json, timestamp, tx_idx)
            )
            conn.commit()
        finally:
            conn.close()

    def read_queue_record(self, tag):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT txid, tkn, datum, value, timestamp, tx_idx FROM queue WHERE tag = ?', (tag,))
            record = cursor.fetchone()
            if record:
                txid, tkn, datum_json, value_json, timestamp, tx_idx = record
                datum = self.json_to_dict(datum_json)
                value = self.json_to_dict(value_json)
                return (tag, {'tag': tag, 'txid': txid, 'tkn': tkn, 'datum': datum, 'value': value, 'timestamp': timestamp, 'tx_idx': tx_idx})
            return None
        finally:
            conn.close()

    # tag TEXT PRIMARY KEY,
    # txid TEXT,
    # tkn TEXT,
    # datum TEXT,
    # value TEXT,
    # timestamp INTEGER,
    # tx_idx INTEGER
    def read_all_queue_records(self, pointer: str):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT tag, txid, tkn, datum, value, timestamp, tx_idx FROM queue WHERE tkn = ?', (pointer,))
            records = cursor.fetchall()
            queue_records = []
            for record in records:
                tag, txid, tkn, datum_json, value_json, timestamp, tx_idx = record
                datum = self.json_to_dict(datum_json)
                value = self.json_to_dict(value_json)
                # queue_records.append({'tag': tag, 'txid': txid, 'tkn': tkn, 'datum': datum, 'value': value, 'timestamp': timestamp, 'tx_idx': tx_idx})
                queue_records.append((tag, {'tag': tag, 'txid': txid, 'tkn': tkn, 'datum': datum, 'value': value, 'timestamp': timestamp, 'tx_idx': tx_idx}))
                # queue_records.append((tag, txid, tkn, datum, value, timestamp, tx_idx))
            return queue_records
        finally:
            conn.close()

    # get all queue records by tkn

    def delete_queue_record_by_tag(self, tag):
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            # Check if the record exists with the given txid
            cursor.execute('SELECT EXISTS(SELECT 1 FROM queue WHERE tag = ?)', (tag,))
            exists = cursor.fetchone()[0]
            if exists:
                # If the record exists, delete it
                cursor.execute('DELETE FROM queue WHERE tag = ?', (tag,))
                conn.commit()
                return True  # Record with the given txid existed and was deleted
            else:
                return False  # No record with the given txid
        finally:
            conn.close()

    # CRUD for seen
    def create_seen_record(self, id: str):
        conn = self.get_connection()
        try:
            conn.execute('INSERT OR REPLACE INTO seen (id, tag) VALUES (?, ?)', (id, id))
            conn.commit()
        finally:
            conn.close()

    def read_seen_record(self, id: str) -> bool:
        conn = self.get_connection()
        try:
            cursor = conn.cursor()
            cursor.execute('SELECT 1 FROM seen WHERE id = ?', (id,))
            return cursor.fetchone() is not None
        finally:
            conn.close()
