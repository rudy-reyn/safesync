# daemon.py
import time
import logging
from typing import Callable, TypeName

from sqlalchemy import create_engine
from sqlalchemy.engine import Engine
from sqlalchemy.orm import Session

from watchdog.observers import Observer
from watchdog.events import (
    LoggingEventHandler,
    FileSystemEvent,
    FileMovedEvent,
    FileCreatedEvent
)

import safesync.files
from .files import FileType
import safesync.client_file_journal as cfj

T = TypeName("T")

def directory_event(directory_handler: Callable[T],
                    file_handler: Callable[T],
                    event: FileSystemEvent) -> T:
    if event.is_directory:
        return directory_handler(event)
    return file_handler(event)

class ClientFileHandler(LoggingEventHandler):
    def __init__(self, engine: Engine=None, **kwargs):
        self.engine = engine
        super().__init__(**kwargs)

    def on_moved(self, event):
        super().on_moved(event)
        match event:
            case FileMovedEvent(src, dest):
                cfj.lookup_file(src)
            case DirMovedEvent(src, dest):
                cfj.lookup_file(src)

    def on_created(self, event):
        super().on_created(event)
        match event:
            case FileCreatedEvent(src):
                cfj.lookup_file(src)
            case DirCreatedEvent(src):
                cfj.lookup_file(src)

    def on_deleted(self, event):
        super().on_deleted(event)
        match event:
            case FileDeletedEvent(src):
                cfj.lookup_file(src)
            case DirDeletedEvent(src):
                cfj.lookup_file(src)


    def on_modified(self, event):
        super().on_modified(event)
        match event:
            case FileModifiedEvent(src):
                cfj.lookup_file(src)
            case DirModifiedEvent(src):
                cfj.lookup_file(src)

def main(path=".", /, *args, **kwargs):
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S"
    )

    event_handler = LoggingEventHandler()
    observer = Observer()
    observer.schedule(event_handler, path, recursive=True)
    observer.start()

    try:
        while True:
            time.sleep(1)
    finally:
        observer.stop()
        observer.join()


if __name__ == "__main__":
    import sys

    help(FileSystemEvent)
    main(*sys.argv[1:])
