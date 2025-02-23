# otp
git clone https://github.com/erlang/otp.git 
cd otp
git checkout maint-27 
./configure 
make -j16 
sudo make install

cd /workspace/hyperbeam

git clone https://github.com/erlang/rebar3.git 
cd rebar3 
./bootstrap 
sudo mv rebar3 /usr/local/bin/

cd /workspace/hyperbeam

curl https://sh.rustup.rs -sSf | sh -s -- -y

export PATH="/root/.cargo/bin:${PATH}"

cd /workspace/hyperbeam

git clone https://github.com/permaweb/ao 
cd ao 
git checkout tillathehun0/cu-experimental 
cd servers/cu  
npm i

cat <<EOF > .env
NODE_CONFIG_ENV=development
PORT=6363
UNIT_MODE=hbu
HB_URL=http://localhost:10000
WALLET_FILE=/home/gitpod/.aos.json
EOF

cd /workspace/hyperbeam
echo "eol"