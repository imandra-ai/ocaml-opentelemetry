tag=ocaml-otel-test:latest

docker-build:
	sudo docker buildx build .. -f ./Dockerfile --tag $(tag)

docker-push:
	sudo docker push $(tag)

kubectl-apply:
	kubectl apply -f emit-test-pod.yaml

kubectl-delete:
	kubectl delete -f emit-test-pod.yaml

all: docker-build docker-push kubectl-delete kubectl-apply
