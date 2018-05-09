REBAR = ./rebar
COOKIE = e843

all:deps clean compile rund
 
compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps
   
clean:
	rm -rf erl_crash.dump
	rm -rf ebin/*
	#$(REBAR) delete-deps
	$(REBAR) clean
	
start:
	erl +K true +P 10240000 +A 10 +swt low +sbwt none -env ERL_MAX_PORTS 1000000 -env ERL_FULLSWEEP_AFTER 2 -pa ebin -name 'e843@127.0.0.1' -setcookie $(COOKIE) -config etc/app.config -s $(COOKIE) -detached
   
run:
	erl +K true +P 10240000 +A 10 +swt low +sbwt none -env ERL_MAX_PORTS 1000000 -env ERL_FULLSWEEP_AFTER 2 -pa ebin -name 'e843@127.0.0.1' -setcookie $(COOKIE) -config etc/app.config -s $(COOKIE)
	
rund:
	erl +K true +P 10240000 +A 10 +swt low +sbwt none -env ERL_MAX_PORTS 1000000 -env ERL_FULLSWEEP_AFTER 2 -pa ebin -name 'e843@127.0.0.1' -setcookie $(COOKIE) -config etc/app.config -s $(COOKIE)
	
test:
	erl +K true +P 10240000 +A 10 +swt low +sbwt none -env ERL_MAX_PORTS 1000000 -env ERL_FULLSWEEP_AFTER 2 -pa ebin -config etc/app.config -s lager

issue:deps clean compile
	rm -rf release
	rm -rf e843.tar.gz
	mkdir -p release/ebin
	mkdir -p release/etc
	cp -f -R ebin release
	cp -f -R etc release
	cp -f README.md release/
	echo "erl +K true +P 10240000 +A 10 +swt low +sbwt none -env ERL_MAX_PORTS 1000000 -env ERL_FULLSWEEP_AFTER 2 -pa ebin -name 'e843@127.0.0.1' -setcookie $(COOKIE) -config etc/app.config -s $(COOKIE)" >> release/e843
	chmod +x release/e843*
	tar cvf e843.tar.gz release
	
.PHONY:all deps compile clean start run rund cluster issue test


