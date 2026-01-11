from abc import ABC, abstractmethod
from typing import Optional


class ScannerInterface(ABC):
    """Base interface for scanner implementations"""

    def __init__(self):
        self._connected = False

    @abstractmethod
    def connect(self) -> bool:
        """Initialize scanner connection

        Returns:
            bool: True if connection successful, False otherwise
        """
        pass

    @abstractmethod
    def read(self) -> Optional[str]:
        """Read from scanner

        Returns:
            Optional[str]: Scanned data, or None if no data available
        """
        pass

    @abstractmethod
    def disconnect(self) -> bool:
        """Close scanner connection

        Returns:
            bool: True if disconnection successful, False otherwise
        """
        pass

    @property
    def is_connected(self) -> bool:
        """Check if scanner is connected"""
        return self._connected

    def __enter__(self):
        """Context manager entry"""
        self.connect()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.disconnect()


class ScannerError(Exception):
    """Base exception for scanner errors"""

    pass


class ScannerConnectionError(ScannerError):
    """Exception raised when scanner connection fails"""

    pass


class ScannerReadError(ScannerError):
    """Exception raised when scanner read fails"""

    pass
