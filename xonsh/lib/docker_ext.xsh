from lib.utils import mk_alias
import os
xontrib load docker_tabcomplete

mk_alias({
    'd': "docker",
    'di': "docker images",
    'drmi': "docker rmi",
    'dt': "docker tag",
    'dp': "docker ps",
    'dpa': "docker ps -a",
    'dl': "docker logs -ft",
    'dpl': "docker pull",
    'dps': "docker push",
    'drr': lambda x: f"docker run --rm -v {os.getcwd()}:/world {' '.join(x)}",
    'dr': lambda x: execx(f"docker run --rm -it -v {os.getcwd()}:/world {' '.join(x)}"),
    'dcs': "docker container stop",
    'dcr': "docker container rm",
    'dsp': "docker system prune -f",
    'dspa': "docker system prune --all --force --volumes",
    'dvi': "docker volume inspect",
    'dvr': "docker volume rm",
    'dvp': "docker volume prune",
    'dvl': "docker volume ls",
    'dvc': "docker volume create",
    'dsv': "docker save",
    'dld': "docker load",
    'dh': "docker history",
    'dhl': "docker history --no-trunc",
    'dis': "docker inspect",
    'dc': "docker-compose",
    'dcu': "docker-compose up",
    'dcud': "docker-compose up -d",
    'dcd': "docker-compose down",
})

