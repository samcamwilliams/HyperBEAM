FROM ubuntu:22.04

# Install dependencies for building Erlang/OTP
RUN apt-get update && apt-get install -y \
    git build-essential libncurses-dev libssl-dev cmake \
    libboost-tools-dev libboost-thread-dev

# Clone and install Erlang/OTP
RUN git clone https://github.com/erlang/otp.git
RUN cd otp && git checkout maint-27 && ./configure \
    --without-wx --without-debugger --without-observer --with-crypto \
    --without-et && make -j8 && make install

# Install rebar3 for Erlang project management
RUN git clone https://github.com/erlang/rebar3.git && cd rebar3 && ./bootstrap && mv rebar3 /usr/local/bin/
RUN rebar3 compile

# Copy hyperbeam into the container
WORKDIR /hyperbeam
COPY . .

# Compile hyperbeam
RUN rebar3 compile

# Remove SSH server
RUN apt-get purge -y openssh-server && apt-get autoremove -y

# Disable any services that could interfere
RUN systemctl disable ssh || true && \
    rm -f /etc/init.d/ssh || true

# Set the command to run when the container starts
CMD ["rebar3", "shell"]