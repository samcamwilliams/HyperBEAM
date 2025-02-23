npm i -g pm2

cd ao/servers/cu
npm i
pm2 start 'npm start'

cd ../../..
pm2 start 'rebar3 shell --eval="hb:start_mainnet(10000)."'

