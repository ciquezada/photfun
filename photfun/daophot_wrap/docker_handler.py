import os
from subprocess import Popen, PIPE
from pathlib import Path
import threading
import psutil
try:
    import docker
    HAS_DOCKER = True
except ImportError:
    HAS_DOCKER = False


def run_proc(cmd, workdir):
    process = Popen(cmd, shell=True, cwd=workdir, stdout=PIPE, stderr=PIPE)
    stdout, stderr = process.communicate()

def init_docker(working_dir, n_proc=1, prev=None, mem_fraction=0.4):
    n_proc = os.cpu_count() if n_proc==-1 else n_proc
    if prev:
        docker_stop_async(prev)

    if not HAS_DOCKER:
        print(f"[PhotFun] Docker not available. Running locally.\n -> Import error")
        return False

    container_names = []
    try:
         # Calcula memoria total y mem_limit por contenedor
        total_mem = psutil.virtual_memory().total
        usable = total_mem * mem_fraction
        per_container = usable / n_proc
        # convierte a gigabytes redondeados
        per_gb = per_container / (1024**3)
        mem_str = f"{per_gb:.1f}g"
        
        docker_client = docker.from_env()
        docker_client.ping()  # Verifica si el daemon responde
        image_name = "ciquezada/photfun-daophot_wrapper"
        images = docker_client.images.list(name=image_name)

        # Pull de imagen si no existe
        if not images:
            print(f"[PhotFun] Downloading docker image '{image_name}'...")
            docker_client.images.pull(image_name)

        for i in range(n_proc):
            container = docker_client.containers.run(
                image=image_name,
                command="/bin/bash",
                volumes={str(Path(working_dir).resolve()): {
                    'bind': "/workdir", 
                    'mode': 'rw'
                }},
                working_dir="/workdir",
                tty=True,
                detach=True,
                mem_limit=mem_str,
                memswap_limit=mem_str,
            )
            container_names.append(container.name)
        print("[PhotFun] Docker DAOPHOT available.")
        print(f"[PhotFun] {mem_str} RAM per core available ")

    except Exception as e:
        print(f"[PhotFun] Docker not available. Running locally.\n -> {e}")
        return False

    return container_names

def docker_run(container_name):
    def docker_runner(cmd, workdir):
        # Ejecutar contenedor
        docker_client = docker.from_env()
        container = docker_client.containers.get(container_name)
        # if container.status != 'running':
        #     container.start()

        exec_res = container.exec_run(cmd=cmd, workdir=f"/workdir/{Path(workdir).as_posix()}")
    
        # stdout, stderr = exec_res.output
        # if stdout:
        #     print("[STDOUT]\n", stdout.decode())
        # if stderr:
        #     print("[STDERR]\n", stderr.decode())
        if exec_res.exit_code == 137:
            raise MemoryError("DAOPHOT error OOM-killed (exit code 137)")
        elif exec_res.exit_code != 0:
            raise RuntimeError(f"DAOPHOT error (exit code {exec_res.exit_code})")

    return docker_runner

def _stop_and_remove(name):
    try:
        client = docker.from_env()
        container = client.containers.get(name)
        container.stop()
        container.remove()
        print(f"[PhotFun] Container '{name}' stopped and removed.")
    except Exception as e:
        print(f"[PhotFun] Error stopping/removing container '{name}': {e}. Manual intervention required.")

def docker_stop_async(container_names):
    """
    Para cada contenedor en container_names, lanza un hilo que
    haga stop() y remove(), pero NO espera a que terminen.
    """
    threads = []
    for name in container_names:
        t = threading.Thread(target=_stop_and_remove, args=(name,), daemon=True)
        t.start()
        threads.append(t)

    # Espera a que cada hilo termine
    for t in threads:
        t.join()

# def init_docker():
#     if not HAS_DOCKER:
#         print(f"[PhotFun] Docker not available. Running locally.\n -> Import error")
#         return False
#     try:
#         docker_client = docker.from_env()
#         docker_client.ping()  # Verifica si el daemon responde
#         image_name = "ciquezada/photfun-daophot_wrapper"
#         images = docker_client.images.list(name=image_name)

#         # Pull de imagen si no existe
#         if not len(images)>0:
#             print(f"[PhotFun] Downloading docker image '{image_name}'...")
#             docker_client.images.pull(image_name)

#         print("[PhotFun] Docker DAOPHOT available.")

#     except Exception as e:
#         print(f"[PhotFun] Docker not available. Running locally.\n -> {e}")
#         return False
#     return True

# def docker_run(cmd, workdir):
#     # Ejecutar contenedor
#     docker_client = docker.from_env()
#     container = docker_client.containers.run(
#         image="ciquezada/photfun-daophot_wrapper",
#         command="/bin/bash",
#         volumes={str(Path(workdir).resolve()): {
#                     'bind': "/workdir", 
#                     'mode': 'rw'
#                 }},
#         working_dir="/workdir",
#         tty=True,
#         detach=True
#     )
#     try:
#         exec_res = container.exec_run(cmd=cmd)
#         if exec_res.exit_code != 0:
#             raise RuntimeError(f"DAOPHOT error:\n{exec_res.exit_code}")
#     finally:
#         container.stop()
#         container.remove()