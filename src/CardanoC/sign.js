// 1. Conectar a la wallet (devuelve la API CIP-30)
async function enableWallet(walletName = null) {
  if (!window.cardano) {
    throw new Error("No Cardano wallet found. Install Eternl, Nami, Lace, etc.");
  }

  const wallets = Object.keys(window.cardano)
    .filter(key => window.cardano[key].enable && window.cardano[key].isEnabled !== false);

  if (wallets.length === 0) throw new Error("No wallet available");

  const selected = walletName || wallets[0];  // Usa la primera si no se especifica

  if (!window.cardano[selected]) {
    throw new Error(`Wallet ${selected} not found. Available: ${wallets.join(", ")}`);
  }

  const api = await window.cardano[selected].enable();
  return api;
}

// 2. Firmar transacción (recibe cborHex del backend)
async function signTransaction(cborHex, partialSign = true) {
  const wallet = await enableWallet();
  const witnessesHex = await wallet.signTx(cborHex, partialSign);
  return witnessesHex;  // String hex con los witnesses
}

// 3. Enviar transacción firmada al backend para submit
async function submitSignedTx(signedCborHex) {
  const response = await fetch('/tx/submit', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ cborHex: signedCborHex })
  });

  if (!response.ok) throw new Error("Submit failed");
  const { txId } = await response.json();
  return txId;
}

// 4. Flujo completo: construir → firmar → submit
async function sendTransaction(txBodyContentJson) {
  try {
    // 1. Pedir tx sin firmar al backend
    const res = await fetch('/tx/unsigned', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(txBodyContentJson)
    });
    const { cborHex } = await res.json();

    // 2. Firmar en wallet del usuario
    const witnessesHex = await signTransaction(cborHex, true);

    // 3. Combinar body + witnesses (algunas wallets lo hacen por ti)
    // Si tu wallet soporta submitTx directamente:
    const wallet = await enableWallet();
    if (wallet.submitTx) {
      const txId = await wallet.submitTx(witnessesHex);
      console.log("Tx enviada directamente por wallet:", txId);
      return txId;
    }

    // 4. Si no, enviar al backend para que combine y submita
    const txId = await submitSignedTx(witnessesHex);
    console.log("Tx enviada por backend:", txId);
    return txId;

  } catch (err) {
    console.error("Error en flujo de transacción:", err);
    throw err;
  }
}

//return the  sign data
async function handleUnsignedTxRequest(unsignedTx) {
  const wallet = await enableWallet();

  // Firma parcial: solo devuelve los witnesses
  const witnessesHex = await wallet.signTx(unsignedTx.cborHex, true);

  // Devolvemos directamente los witnesses al backend en la misma respuesta
  return { witnessesHex };
}