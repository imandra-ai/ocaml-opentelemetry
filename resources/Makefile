tag=ocaml-otel-test:latest
platform=linux/amd64

docker-build:
	sudo docker buildx build .. -f ./Dockerfile --tag $(tag) --platform $(platform)

docker-push:
	sudo docker push $(tag)

kubectl-apply:
	sed s=%IMAGE%=$(tag)=g emit-test-pod.yaml | kubectl apply -f -

kubectl-delete:
	sed s=%IMAGE%=$(tag)=g emit-test-pod.yaml | kubectl delete -f - --ignore-not-found --grace-period 1

all: docker-build docker-push kubectl-delete kubectl-apply
