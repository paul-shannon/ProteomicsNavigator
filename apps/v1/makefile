ID = 12aug2020
APP_IMAGE_NAME = nav_app_$(ID)
BASE=$(APP_IMAGE_NAME)_base

build:
	docker build -t $(APP_IMAGE_NAME) -f Dockerfile .

quick:
	docker build -t $(APP_IMAGE_NAME) -f Dockerfile.quick .

base:
	docker build -t $(BASE) -f Dockerfile.base .

bash:
	docker run -it \
           -p 3838:3838 \
           --entrypoint /bin/bash \
           $(APP_IMAGE_NAME)

