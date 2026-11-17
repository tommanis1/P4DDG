
import docker
import sys
import os
import time

def get_container():
    client = docker.from_env()

    IMAGE_NAME = "p4c"
    CONTAINER_NAME = f"{IMAGE_NAME}-tunnel-ex-container"
    image_name = f"{IMAGE_NAME}:latest"

    # check if container exists
    try:
        container = client.containers.get(CONTAINER_NAME)
    except docker.errors.NotFound:
        # check if image exists
        if not any(image_name in image.tags for image in client.images.list()):
            print(f"Image {image_name} not found.")
            # print(client.images.list())
            sys.exit(1)

        else:
            print(f"Container {CONTAINER_NAME} not found. Creating new container...")
            container = client.containers.run(
                image=IMAGE_NAME,
                name=CONTAINER_NAME,
                detach=True,
                tty=True,
                volumes={f"{os.getcwd()}/tutorials": {"bind": "/tutorials", "mode": "rw"}},
                working_dir="/tutorials",
                command="bash"
            )
    
    # print (f"Container {CONTAINER_NAME} found.")
    return client, container

def stream_container_output(container, command):
    container.exec_run("apt update && apt install -y make")
    exit_code, output = container.exec_run(command, stream=True, demux=True)
    for stdout, stderr in output:
        if stdout:
            print(stdout.decode(errors='replace'), end='')
        if stderr:
            print(stderr.decode(errors='replace'), end='')
    return exit_code

def main():
    _, container = get_container()
    print(f"found {container.name}")

    # exit_code = stream_container_output(container, f"cd")
    # if exit_code != 0:
    #     print(f"Error running command in container.")


if __name__ == "__main__" : main()