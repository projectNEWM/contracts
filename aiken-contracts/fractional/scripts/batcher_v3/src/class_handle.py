import os

from src import (db_manager_redis, parsing, query, queue_purchase,
                 queue_refund, sorting, transaction, validate)
from src.class_batcher import Batcher
from src.class_book import Book
from src.class_queue import Queue
from src.class_sale import Sale


class Handle:
    """
        General class to handle all queue and order book fulfillment.
    """
    
    @staticmethod
    def rollback(data: dict, debug: bool = True) -> bool:
        """TODO"""
        # do something here
        
        # if a rollback occurs we need to handle it
        if data['context']['block_hash'] is not None:
            print("\nROLLBACK")
            print("DO SOME THING HERE\n")
            print(data)
            exit(1)
        
        # assume rollback fails until we know how to properly handle this case for all cases.
        return False
    
    class Batcher(Batcher):
        # inherit the subclass into the handle class
        pass
    
    class Queue(Queue):
        # inherit the subclass into the handle class
        pass
    
    class Sale(Sale):
        # inherit the subclass into the handle class
        pass
    
    
    class Book(Book):
        # inherit the subclass into the handle class
        pass