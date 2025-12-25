#!/bin/bash
set -euo pipefail

# Configuration
REPO_ROOT="/home/rustkas/aigroup/apps/otp/router"
CERTS_DIR="$REPO_ROOT/_artifacts/certs"
DAYS=365

mkdir -p "$CERTS_DIR"
cd "$CERTS_DIR"

echo "=== Generating NATS TLS Certificates ==="

# 1. Create CA
echo "Creating Root CA..."
openssl genrsa -out ca-key.pem 2048
openssl req -x509 -new -nodes -key ca-key.pem -days $DAYS -out ca-cert.pem -subj "/CN=NATS-Test-CA"

# 2. Create Server Cert (NATS)
echo "Creating Server Certificate..."
openssl genrsa -out server-key.pem 2048
openssl req -new -key server-key.pem -out server.csr -subj "/CN=localhost"
# Add SAN (Subject Alternative Name) for localhost
cat > server.ext <<EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage = digitalSignature, nonRepudiation, keyEncipherment, dataEncipherment
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
IP.1 = 127.0.0.1
EOF
openssl x509 -req -in server.csr -CA ca-cert.pem -CAkey ca-key.pem -CAcreateserial -out server-cert.pem -days $DAYS -extfile server.ext

# 3. Create Client Cert (Erlang Router)
echo "Creating Client Certificate..."
openssl genrsa -out client-key.pem 2048
openssl req -new -key client-key.pem -out client.csr -subj "/CN=router-client"
openssl x509 -req -in client.csr -CA ca-cert.pem -CAkey ca-key.pem -CAcreateserial -out client-cert.pem -days $DAYS

# Cleanup
rm -f *.csr *.ext *.srl

echo "✅ Certificates generated in $CERTS_DIR"
ls -l "$CERTS_DIR"
