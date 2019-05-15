ID = 14may2019
TESTPORT=60054

build:
	docker build -t dtdemo_$(ID) -f Dockerfile .

bash:
	docker run -ti --rm -p $(TESTPORT):3838 dtdemo_$(ID) bash

dockerRun:
	docker run -p $(TESTPORT):3838 dtdemo_$(ID)

save:
	time docker save dtdemo_$(ID) >  ~/tmp/docker_dtdemo_$(ID).image
	ls -lat ~/tmp/docker_dtdemo_$(ID).image

scp:
	time scp ~/tmp/docker_dtdemo_$(ID).image pshannon@trena:/local/tmp/
	ssh pshannon@trena ls -l /local/tmp/docker_dtdemo_$(ID).image
#	ssh pshannon@trena docker load < /local/tmp/docker_dtdemo_$(ID).image


