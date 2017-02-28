#!/bin/bash

echo "removing old shiny-server.conf file"
rm -f $OPENSHIFT_REPO_DIR/shiny-server.conf
echo "generating shiny-server.conf file"
sed -e "s;USER_NAME;$OPENSHIFT_APP_UUID;g" $OPENSHIFT_REPO_DIR/shiny-server.conf.template > $OPENSHIFT_REPO_DIR/shiny-server.conf
sed -i -e "s;ADDRESS_IP;$OPENSHIFT_NODEJS_IP;g" $OPENSHIFT_REPO_DIR/shiny-server.conf
