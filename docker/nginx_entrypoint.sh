#!/bin/sh

echo "LOGIN_USER:<$LOGIN_USER>"
echo "LOGIN_PASSWORD: <$LOGIN_PASSWORD>"

# Generate the password file
htpasswd -cb /etc/nginx/.htpasswd $LOGIN_USER $LOGIN_PASSWORD

cat /etc/nginx/.htpasswd

sleep 1

# Start Nginx in the foreground
exec nginx -g 'daemon off;'
