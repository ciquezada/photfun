import threading
import time
import os
import logging
from astropy.samp import SAMPIntegratedClient
from astropy.samp.errors import SAMPHubError

class Receiver:
    def __init__(self, client):
        self.client = client
        self.params = {}
        self.new_message = False
        self.lock = threading.Lock()
        
    def receive_call(self, private_key, sender_id, msg_id, mtype, params, extra):
        with self.lock:
            self.params = {**params, "mtype": mtype}
            self.new_message = True
            self.client.reply(msg_id, {"samp.status": "samp.ok", "samp.result": {}})
        
    def receive_notification(self, private_key, sender_id, mtype, params, extra):
        with self.lock:
            self.params = {**params, "mtype": mtype}
            self.new_message = True
        
    def bind_actions(self, client, actions):
        for mtype in actions:
            client.bind_receive_call(mtype, self.receive_call)
            client.bind_receive_notification(mtype, self.receive_notification)
    
    def reset_flag(self):
        with self.lock:
            self.new_message = False

class SAMPclient:
    def __init__(self):
        self.samp_client = SAMPIntegratedClient()
        self.samp_receiver = None
        self.is_connected = False
        # self.logger = logging.getLogger(__name__)
        # self.reconnect_attempts = 3
        self.reconnect_delay = 2  # segundos
        self.ping_interval = 5    # segundos
        self.ping_thread = None
        self.running = False

    def start_samp(self):
        """Intenta conectar al hub con reintentos automáticos"""
        # for attempt in range(1, self.reconnect_attempts + 1):
        try:
            if not self.is_connected:
                self.samp_client.connect()
                self.is_connected = True
                self._init_receiver()
                self._start_ping_thread()
                # self.logger.info("Conexión SAMP establecida")
                print(f"SAMP: succesfully connected")
            return True
                
        except SAMPHubError as e:
            print(f"SAMP error: connection error")
            # self.logger.warning(f"Intento de conexión {attempt}/{self.reconnect_attempts} fallido: {str(e)}")
            # time.sleep(self.reconnect_delay)
            self.is_connected = False
        
        # self.logger.error("No se pudo conectar al hub SAMP")
        return False

    def _init_receiver(self):
        """Inicializa el receptor de mensajes"""
        try:
            if self.is_connected and not self.samp_receiver:
                self.samp_receiver = Receiver(self.samp_client)
                self.samp_receiver.bind_actions(self.samp_client, [
                    "image.load.fits",
                    "table.load.votable"
                ])
        except Exception as e:
            # self.logger.error(f"Error inicializando receptor: {str(e)}")
            print(f"SAMP error: receiver error {str(e)}")
            self.is_connected = False

    def _start_ping_thread(self):
        """Inicia hilo para verificar conexión periódicamente"""
        self.running = True
        self.ping_thread = threading.Thread(target=self._ping_hub, daemon=True)
        self.ping_thread.start()

    def _ping_hub(self):
        """Verifica periodicamente la conexión al hub"""
        while self.running:
            try:
                if self.is_connected:
                    self.samp_client.ping()
            except Exception as e:
                print(f"SAMP error: not responding {str(e)}")
                # self.logger.warning(f"Error en ping: {str(e)}")
                self.is_connected = False
            time.sleep(self.ping_interval)

    def _check_connection(self):
        """Verifica y restablece la conexión si es necesario"""
        if not self.is_connected:
            return self.start_samp()
        return True

    def broadcast_fits(self, in_fits, alias):
        """Transmite archivo FITS con manejo de errores"""
        if not self._check_connection():
            return False

        try:
            url_path = os.path.abspath(in_fits).replace('\\', '/')
            params = {
                "url": f"file:///{url_path}",
                "name": alias
            }
            self.samp_client.notify_all({
                "samp.mtype": "image.load.fits",
                "samp.params": params
            })
            return True
        except (ConnectionRefusedError, SAMPHubError) as e:
            # self.logger.error(f"Error transmitiendo FITS: {str(e)}")
            print(f"SAMP error: broadcast error {str(e)}")
            self.is_connected = False
            return False
        except Exception as e:
            print(f"SAMP error: unexpected error {str(e)}")
            # self.logger.error(f"Error inesperado: {str(e)}")
            return False

    def broadcast_table(self, in_table, alias):
        """Transmite tabla VOTable con manejo de errores"""
        if not self._check_connection():
            return False

        try:
            url_path = os.path.abspath(in_table).replace('\\', '/')
            params = {
                "url": f"file:///{url_path}",
                "name": f"{alias}_{os.path.basename(url_path)}"
            }
            self.samp_client.notify_all({
                "samp.mtype": "table.load.votable",
                "samp.params": params
            })
            return True
        except (ConnectionRefusedError, SAMPHubError) as e:
            # self.logger.error(f"Error transmitiendo tabla: {str(e)}")
            print(f"SAMP error: broadcast error {str(e)}")
            self.is_connected = False
            return False
        except Exception as e:
            print(f"SAMP error: unexpected error {str(e)}")
            # self.logger.error(f"Error inesperado: {str(e)}")
            return False

    def stop_samp(self):
        """Desconexión segura con manejo de errores"""
        self.running = False
        try:
            if self.is_connected:
                self.samp_client.disconnect()
                self.is_connected = False
                print("SAMP disconnected")
                # self.logger.info("Desconexión SAMP exitosa")
        except Exception as e:
            print(f"SAMP error: disconnect error {str(e)}")
            # self.logger.error(f"Error durante desconexión: {str(e)}")
        finally:
            self.samp_receiver = None
            if self.ping_thread and self.ping_thread.is_alive():
                self.ping_thread.join(timeout=1)

# # Configuración básica de logging
# logging.basicConfig(
#     format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
#     level=logging.INFO
# )