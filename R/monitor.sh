#!/bin/bash

echo "$(date) Restart monitor starting." >> ~/app-root/repo/R/restart.log

### waiting to be exist file
while [ ! -f ~/app-root/repo/R/restart.txt ]; # true if /your/file does not exist
do
  sleep 5
done

rm ~/app-root/repo/R/restart.txt

echo "$(date) Restart detected. Restarting gear." >> ~/app-root/repo/R/restart.log

yes "1" | gear restart >> ~/app-root/repo/R/restart.log
