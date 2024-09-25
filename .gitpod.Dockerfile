FROM gitpod/workspace-full

RUN git clone https://github.com/erlang/otp.git
RUN cd otp
RUN git checkout maint-27
RUN ./configure
RUN make
RUN sudo make install
RUN cd ..
RUN git clone https://github.com/erlang/rebar3.git
RUN cd rebar3
RUN ./bootstrap
RUN sudo mv rebar3 /usr/local/bin/
