DOCKER_TAG=openjdk-11-lein
test-cp:
	docker run -it --rm --name docker-cp -v $(PWD):/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  clojure:$(DOCKER_TAG) ./test-by-cp

test-git:
	docker run -it --rm --name docker-git -v $(PWD):/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  clojure:$(DOCKER_TAG) ./test-by-git
