from unittest.mock import patch, MagicMock
import unittest
from subprocess import Popen, PIPE
import errno

from send_pipe import send_pipe  # Reemplaza "send_pipe" con el nombre del archivo donde está send_pipe.

class TestSendPipe(unittest.TestCase):
    @patch("send_pipe.Popen")  # Se sustituye Popen por un mock.
    def test_send_pipe_success(self, mock_popen):
        """Verifica que send_pipe escribe en stdin sin errores y finaliza correctamente."""
        mock_process = MagicMock()
        mock_popen.return_value = mock_process  # Simula el proceso Popen.

        send_pipe(b"Test data\n")

        mock_popen.assert_called_once_with("less", stdin=PIPE)
        mock_process.stdin.write.assert_called_once_with(b"Test data\n")
        mock_process.stdin.close.assert_called_once()
        mock_process.wait.assert_called_once()

    @patch("send_pipe.Popen")
    def test_send_pipe_broken_pipe(self, mock_popen):
        """Simula que la tubería se rompe (EPIPE) y verifica que el código lo maneja sin lanzar excepción."""
        mock_process = MagicMock()
        mock_popen.return_value = mock_process

        # Simular IOError con errno.EPIPE
        mock_process.stdin.write.side_effect = IOError(errno.EPIPE, "Broken pipe")

        try:
            send_pipe(b"Test data\n")  # No debería lanzar excepción.
        except Exception as e:
            self.fail(f"send_pipe lanzó una excepción inesperada: {e}")

    @patch("send_pipe.Popen")
    def test_send_pipe_invalid_argument(self, mock_popen):
        """Simula un error EINVAL y verifica que se maneja correctamente."""
        mock_process = MagicMock()
        mock_popen.return_value = mock_process

        mock_process.stdin.write.side_effect = IOError(errno.EINVAL, "Invalid argument")

        try:
            send_pipe(b"Test data\n")  # No debería lanzar excepción.
        except Exception as e:
            self.fail(f"send_pipe lanzó una excepción inesperada: {e}")

    @patch("send_pipe.Popen")
    def test_send_pipe_other_ioerror(self, mock_popen):
        """Simula un IOError desconocido y verifica que se propaga la excepción."""
        mock_process = MagicMock()
        mock_popen.return_value = mock_process

        mock_process.stdin.write.side_effect = IOError(errno.ENOENT, "File not found")  # Un error inesperado.

        with self.assertRaises(IOError):
            send_pipe(b"Test data\n")

if __name__ == "__main__":
    unittest.main()
