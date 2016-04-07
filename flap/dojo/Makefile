.PHONY: submit submit1 submit2 submit3

HJC=hjc

TARBALL_M1=$(wildcard ../flap-*-15.1.tar.gz)
TARBALL_M2=$(wildcard ../flap-*-15.2.tar.gz)
TARBALL_M3=$(wildcard ../flap-*-15.3.tar.gz)

submit: $(HJC) submit1 submit2 submit3

submit1:
	@echo 'Submit MILESTONE 1'
	./submit.sh $(TARBALL_M1) m1

submit2:
	@echo 'Submit MILESTONE 2'
	./submit.sh $(TARBALL_M2) m2

submit3:
	@echo 'Submit MILESTONE 3'
	./submit.sh $(TARBALL_M3) m3

$(HJC):
	curl -L https://github.com/yurug/hjc/releases/download/0.2/hjc-0.2.tar.gz -o hjc.tar.gz
	tar xvfz hjc.tar.gz
	cd hjc-0.2 && ./configure && make
	cp hjc-0.2/hjc .
