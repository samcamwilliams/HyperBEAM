FROM gitpod/workspace-full

RUN git clone https://github.com/erlang/otp.git && cd otp && git checkout maint-27 && ./configure && make && sudo make install
RUN git clone https://github.com/erlang/rebar3.git && cd rebar3 && ./bootstrap && sudo mv rebar3 /usr/local/bin/
