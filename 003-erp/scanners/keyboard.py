import sys
import select
import termios
import tty
from typing import Optional
from .base import ScannerInterface, ScannerConnectionError, ScannerReadError


class KeyboardScanner(ScannerInterface):
    """USB Keyboard Scanner (acts as keyboard input)

    Most USB barcode scanners and NFC readers act as keyboard devices.
    They scan the code and "type" it followed by an Enter key.

    This implementation reads from stdin in raw mode to capture scanner input.
    """

    def __init__(self, device_path: str = "/dev/input/event0"):
        """Initialize keyboard scanner

        Args:
            device_path: Path to input device (Linux)
        """
        super().__init__()
        self.device_path = device_path
        self._old_settings = None

    def connect(self) -> bool:
        """Connect to scanner (stdin for keyboard scanners)"""
        try:
            if sys.stdin.isatty():
                # Save old terminal settings
                self._old_settings = termios.tcgetattr(sys.stdin)
                # Set terminal to raw mode
                tty.setraw(sys.stdin.fileno())
            self._connected = True
            return True
        except Exception as e:
            raise ScannerConnectionError(f"Failed to connect to keyboard scanner: {e}")

    def read(self) -> Optional[str]:
        """Read from scanner (stdin)

        For keyboard scanners, this reads input until Enter key.
        Scanner input typically ends with carriage return (\\r).
        """
        if not self._connected:
            raise ScannerReadError("Scanner not connected")

        try:
            # Check if data is available to read
            if select.select([sys.stdin], [], [], 0.1)[0]:
                # Read input until newline
                data = ""
                while True:
                    char = sys.stdin.read(1)
                    if char == "\r" or char == "\n":
                        break
                    data += char
                return data if data else None
            return None
        except Exception as e:
            raise ScannerReadError(f"Failed to read from scanner: {e}")

    def disconnect(self) -> bool:
        """Disconnect from scanner and restore terminal settings"""
        try:
            if self._old_settings and sys.stdin.isatty():
                # Restore terminal settings
                termios.tcsetattr(sys.stdin, termios.TCSADRAIN, self._old_settings)
            self._connected = False
            return True
        except Exception as e:
            return False


class SimulatedKeyboardScanner(ScannerInterface):
    """Simulated scanner for testing without hardware

    Reads input from stdin in a friendly way for development/testing.
    """

    def __init__(self):
        super().__init__()

    def connect(self) -> bool:
        """Connect (no-op for simulated scanner)"""
        self._connected = True
        return True

    def read(self) -> Optional[str]:
        """Read input line from stdin"""
        if not self._connected:
            raise ScannerReadError("Scanner not connected")

        try:
            # Try to read without blocking
            import select
            if select.select([sys.stdin], [], [], 0.1)[0]:
                return sys.stdin.readline().strip()
            return None
        except Exception:
            return None

    def disconnect(self) -> bool:
        """Disconnect (no-op for simulated scanner)"""
        self._connected = False
        return True
