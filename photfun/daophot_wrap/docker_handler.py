import os
from subprocess import Popen, PIPE, STDOUT
from pathlib import Path
import threading
import psutil
from collections import deque
import uuid
try:
    import docker
    HAS_DOCKER = True
except ImportError:
    HAS_DOCKER = False


def run_proc(cmd, workdir, max_repeats=20, verbose=False):
    process = Popen(
        cmd, shell=True, cwd=workdir,
        stdout=PIPE, stderr=STDOUT, text=True
    )

    lines = deque(maxlen=max_repeats)
    killed_due_to_repeats = False

    def monitor():
        nonlocal killed_due_to_repeats
        for line in iter(process.stdout.readline, ''):
             # Muestra en consola
            lines.append(line.strip())
            if len(lines) == max_repeats and len(set(lines)) == 1:
                process.kill()
                killed_due_to_repeats = True
                break

    monitor_thread = threading.Thread(target=monitor)
    monitor_thread.start()
    process.wait()
    monitor_thread.join()

    if killed_due_to_repeats:
        raise RuntimeError(f"DAOPHOT error loop detected (exit code 137)")

    if process.returncode != 0:
        raise RuntimeError(f"DAOPHOT error (exit code {process.returncode})")

    return process.returncode


def init_docker(working_dir, n_proc=1, prev=[False], mem_fraction=0.4, force_docker=False):
    n_proc = os.cpu_count() if n_proc==-1 else n_proc
    if prev[0]:
        docker_stop_async(prev)
    
    if not force_docker:
        # Prefer local DAOPHOT if available
        try:
            run_proc("daophot << EOF\nexit\nEOF", workdir=working_dir, verbose=False)
            print("[PhotFun] Native DAOPHOT detected. Docker will not be used.")
            return [False]
        except Exception as e:
            print(f"[PhotFun] Native DAOPHOT not available:\n -> {e}")
            print("[PhotFun] Attempting to start Docker containers...")

    if not HAS_DOCKER:
        print(f"[PhotFun] Docker not available. Running locally.\n -> Import error")
        return [False]

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
                name = f"photfun_{uuid.uuid4().hex[:12]}",
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
        print(f"[Docker] {mem_str} RAM per container available ")

    except Exception as e:
        print(f"[PhotFun] Docker not available. Running locally.\n -> {e}")
        return [False]

    return container_names

def docker_run(container_name):
    def docker_runner(cmd, workdir, verbose=False):
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
    if not name:
        print(f"[PhotFun] Docker is disabled.")
        return 0
    try:
        client = docker.from_env()
        container = client.containers.get(name)
        container.stop()
        container.remove()
        print(f"[Docker] Container '{name}' stopped and removed.")
    except Exception as e:
        print(f"[Docker] Error stopping/removing container '{name}': {e}. Manual intervention required.")

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