#!/bin/bash

# To store login / password outside :
# mysql_config_editor set --login-path=client --user=insee --password

mysql insee $@
