import sqlite3

class DatabaseManager:
    def __init__(self, db_file):
        self.conn = sqlite3.connect(db_file)
        self.create_tables()

    def create_tables(self):
        # Create Table 1 if it doesn't exist
        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS sale (
                id TEXT PRIMARY KEY,
                txid TEXT,
                datum TEXT,
                value TEXT
            )
        """)

        # Create Table 2 if it doesn't exist
        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS queue (
                id TEXT PRIMARY KEY,
                tkn TEXT,
                datum TEXT,
                value TEXT
            )
        """)
        self.conn.commit()
    
    def close_connection(self):
        self.conn.close()
    
    def create_sale_record(self, tkn, txid, datum, value):
        self.conn.execute("INSERT INTO sale (id, txid, datum, value) VALUES (?, ?, ?, ?)", (tkn, txid, datum, value))
        self.conn.commit()

    def create_queue_record(self, txid, tkn, datum, value):
        self.conn.execute("INSERT INTO queue (id, tkn, datum, value) VALUES (?, ?, ?)", (txid, tkn, datum, value))
        self.conn.commit()
    
    def update_sale_record(self, tkn, txid, datum, value):
        self.conn.execute("UPDATE sale SET txid = ?, datum = ?, value = ? WHERE id = ?", (txid, datum, value, tkn))
        self.conn.commit()
    
    def delete_sale_record(self, tkn):
        self.conn.execute("DELETE FROM sale WHERE id = ?", (tkn,))
        self.conn.commit()

    def delete_queue_record(self, txid):
        self.conn.execute("DELETE FROM queue WHERE id = ?", (txid,))
        self.conn.commit()
    
    def read_sale_record(self, tkn):
        cursor = self.conn.execute("SELECT * FROM sale WHERE id = ?", (tkn,))
        record = cursor.fetchone()
        return record

    def read_all_queue_records(self):
        cursor = self.conn.execute("SELECT * FROM queue")
        return cursor.fetchall()