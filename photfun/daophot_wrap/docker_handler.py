from subprocess import Popen, PIPE
try:
    import docker
    HAS_DOCKER = True
except ImportError:
    HAS_DOCKER = False


def run_proc(cmd, workdir):
    process = Popen(cmd, shell=True, cwd=workdir, stdout=PIPE, stderr=PIPE)
    stdout, stderr = process.communicate()

def init_docker():
    if not HAS_DOCKER:
        print(f"[PhotFun] Docker not available. Running locally.\n -> Import error")
        return False
    try:
        docker_client = docker.from_env()
        docker_client.ping()  # Verifica si el daemon responde
        image_name = "ciquezada/photfun-daophot_wrapper"
        images = docker_client.images.list(name=image_name)

        # Pull de imagen si no existe
        if not len(images)>0:
            print(f"[PhotFun] Downloading docker image '{image_name}'...")
            docker_client.images.pull(image_name)

        print("[PhotFun] Docker DAOPHOT available.")

    except Exception as e:
        print(f"[PhotFun] Docker not available. Running locally.\n -> {e}")
        return False
    return True

def docker_run(cmd, workdir):
    # Ejecutar contenedor
    docker_client = docker.from_env()
    container = docker_client.containers.run(
        image="ciquezada/photfun-daophot_wrapper",
        command="/bin/bash",
        volumes={workdir: {
            	'bind': "/workdir", 
          		'mode': 'rw'}
           },
        working_dir="/workdir",
        tty=True,
        detach=True
    )
    try:
        exec_res = container.exec_run(cmd=cmd)
        if exec_res.exit_code != 0:
            raise RuntimeError(f"DAOPHOT error:\n{exec_res.exit_code}")
    finally:
        container.stop()
        container.remove()
