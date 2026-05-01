#!/usr/bin/env bash
set -euo pipefail

# ===================== CONFIGURATION =====================
ADDRESS_FILE="payment.addr"          # File containing your payment address
SKEY_FILE="payment.skey"             # Signing key file (private key)
MAGIC=2                              # Preview network magic number
AMOUNT_TO_SEND=1000000000            # Amount to send to yourself (1 ADA in lovelace)
MIN_FEE_GUESS=200000                 # Conservative fee guess (0.2 ADA) - used only as fallback

# Your address (hardcoded for convenience)
MY_ADDRESS="addr_test1vpk46w732z6htes8jdds22gr3wjzjmx8t4t44zktc5u6s2qwrxf8e"

# Check if required files exist
if [[ ! -f "$ADDRESS_FILE" ]]; then
  echo "ERROR: File $ADDRESS_FILE not found"
  exit 1
fi

if [[ ! -f "$SKEY_FILE" ]]; then
  echo "ERROR: Signing key file $SKEY_FILE not found"
  exit 1
fi

# ===================== GET AVAILABLE UTXOs =====================
echo "Querying available UTXOs..."
cardano-cli query utxo \
  --address "$MY_ADDRESS" \
  --testnet-magic $MAGIC \
  --out-file utxos.json

# Select the UTXO with the highest balance (at least 2 ADA recommended)
UTXO_LINE=$(jq -r 'to_entries[] | select(.value.value.lovelace >= 2000000000) | .key' utxos.json | head -n1)

if [[ -z "$UTXO_LINE" ]]; then
  echo "ERROR: No UTXO found with at least 2 ADA"
  exit 1
fi

TX_IN="$UTXO_LINE"   # Format: txhash#txix

echo "Using UTXO: $TX_IN"

# ===================== GET BALANCE OF SELECTED UTXO =====================
LOVELACE=$(jq -r --arg txin "$TX_IN" '.[$txin].value.value.lovelace' utxos.json)

echo "UTXO balance: $((LOVELACE / 100000000)) ADA"

# ===================== BUILD TRANSACTION (INITIAL) =====================
echo "Building transaction..."

cardano-cli transaction build \
  --change-address "$MY_ADDRESS" \
  --tx-in "$TX_IN" \
  --tx-out "$MY_ADDRESS+$AMOUNT_TO_SEND lovelace" \
  --testnet-magic $MAGIC \
  --out-file tx.raw

# ===================== CALCULATE REAL FEE =====================
# Requires protocol.json (get it with: cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.json)
FEE=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file tx.raw \
  --testnet-magic $MAGIC \
  --protocol-params-file protocol.json \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --reference-script-size 0 \
  | awk '{print $2}')

echo "Calculated fee: $FEE lovelace"

# ===================== BUILD FINAL TRANSACTION WITH REAL FEE =====================
cardano-cli transaction build \
  --change-address "$MY_ADDRESS" \
  --tx-in "$TX_IN" \
  --tx-out "$MY_ADDRESS+$AMOUNT_TO_SEND lovelace" \
  --testnet-magic $MAGIC \
  --fee "$FEE" \
  --out-file tx.final.raw

# ===================== SIGN TRANSACTION =====================
echo "Signing transaction..."
cardano-cli transaction sign \
  --tx-body-file tx.final.raw \
  --signing-key-file "$SKEY_FILE" \
  --testnet-magic $MAGIC \
  --out-file tx.signed

# ===================== SUBMIT TRANSACTION =====================
echo "Submitting transaction..."
cardano-cli transaction submit \
  --tx-file tx.signed \
  --testnet-magic $MAGIC

echo -e "\nTransaction submitted successfully!"
echo "You can check it here:"
echo "https://preview.cexplorer.io/tx/$(cardano-cli transaction txid --tx-file tx.signed)"

# Optional cleanup
# rm -f utxos.json tx.raw tx.final.raw tx.signed