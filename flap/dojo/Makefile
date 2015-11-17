.PHONY: submit

HJC=hjc

TARBALL_M1=$(wildcard ../flap-*-15.1.tar.gz)

submit: $(HJC)
	@echo 'Submit MILESTONE 1'
	./submit.sh $(TARBALL_M1) m1

$(HJC):
	curl -L https://github.com/yurug/hjc/releases/download/0.1/hjc-0.1.tar.gz -o hjc.tar.gz
	tar xvfz hjc.tar.gz
	cd hjc-0.1 && ./configure && make
	cp hjc-0.1/hjc .
