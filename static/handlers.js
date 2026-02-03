// Mensaje inicial
window.initialRequestMessage = {
  msg: "Init the program",
  req: {
    requrl: "http://localhost:8080/init/S0/u"
  }
};

// Contrato: cada handler devuelve SpecialVar[] = [{ name, value, type? }]
window.specialVarHandlers = {
  "/addrs/S0": async (_vars, item) => await connectWalletAndGetAddresses(),
  "/signIt/S0": async (vars, item) => await signTransaction(vars, item),
  "/saludo/S0": async (vars, item) => await saludo(vars, item)
};

// =========================
//  Utils de wallet (se mantienen)
// =========================

async function selectWallet() {
  const available = window.cardano || {};
  const supported = ["eternl", "nami", "lace", "yoroi"];
  const options = supported.filter(
    name => available[name] && available[name].enable
  );

  if (options.length === 0) {
    throw new Error("No CIP-30 wallets available in this browser");
  }

  let chosen;
  if (options.length === 1) {
    chosen = options[0];
  } else {
    const name = prompt(
      "Choose wallet: " + options.join(", "),
      options[0]
    );
    if (!name || !options.includes(name)) {
      throw new Error("Wallet no vÃ¡lida");
    }
    chosen = name;
  }
  return chosen;
}

async function enableWallet(walletName) {
  const provider = window.cardano && window.cardano[walletName];
  if (!provider || !provider.enable) {
    throw new Error(`Wallet ${walletName} not available`);
  }
  const api = await provider.enable();
  return api;
}

let cachedWalletApi = null;
function getWalletApi() {
  return cachedWalletApi;
}

// =========================
//  Handlers
// =========================

// /addrs/S0 -> obtiene direcciones de la wallet
async function connectWalletAndGetAddresses() {
  // 1) Choose wallet
  const walletName = await selectWallet();

  // 2) Enable CIP-30 API
  const api = await enableWallet(walletName);
  cachedWalletApi = api;

  // 3) Get addresses
  const changeAddressCbor = await api.getChangeAddress();
  const usedAddrs = await api.getUsedAddresses();
  const unusedAddrs = await api.getUnusedAddresses();
  const rewardAddrs = await api.getRewardAddresses();

  const changeAddress = changeAddressCbor;
  const listPayAddresses = [...usedAddrs, ...unusedAddrs];
  const listStakeAddresses = rewardAddrs;

  // 4) Devolver SpecialVar[]
  return [
    { name: "changeAddress", type: "changeAddress", value: changeAddress },
    { name: "listPayAddresses", type: "payAddressList", value: listPayAddresses },
    { name: "listStakeAddresses", type: "stakeAddressList", value: listStakeAddresses }
  ];
}

// /signIt/S0 -> firma una transacciÃ³n y devuelve el witness
async function signTransaction(_vars, item) {
  const api = getWalletApi();
  if (!api) {
    throw new Error("Wallet API not initialized");
  }

  const txCborHex = item && item.msg && item.msg.cborHex;
  if (!txCborHex || typeof txCborHex !== "string") {
    throw new Error("No valid msg.cborHex in the message to sign");
  }

  let witnessSetCbor;
  try {
    witnessSetCbor = await api.signTx(txCborHex, false);
    alert(witnessSetCbor);
  } catch (e) {
    console.error("Error in signTx", e);
    throw e;
  }

  return [
    { name: "body", type: "text", value: witnessSetCbor }
  ];
}

// /saludo/S0 -> ejemplo simple
async function saludo(_vars, item) {
  const nombre =
    (item && item.msg && item.msg.nombre) ||
    (item && item.msg && item.msg.name) ||
    "mundo";

  const saludo = `Hola, ${nombre}`;

  return [
    { name: "saludo", type: "text", value: saludo }
  ];
}


// ====== userInputHandler NUEVO ======
let pendingResolve = null;

async function userInputHandler(vars, item) {
  const form = item._form;
  const req = item._req;

  // 1) Si es la primera vez, crear inputs según vars y body
  if (!item._inputsInitialized) {
    pendingResolve = null;
    const varNames = Object.keys(vars);
    const reqBody = req.reqbody;

    if (varNames.length > 0) {
      // Inputs por cada var detectada ($foo, etc.)
      varNames.forEach(varName => {
        const label = document.createElement("label");
        label.textContent = `${varName}: `;
        label.style.display = "block";
        label.style.marginTop = "4px";

        const inp = document.createElement("input");
        inp.dataset.var = varName;
        inp.id = `var_${varName}_${Math.random().toString(16).slice(2)}`;
        inp.type = varName.toLowerCase().includes("int") ? "number" : "text";
        item._varInputIds = item._varInputIds || {};
        item._varInputIds[varName] = inp.id;

        form.appendChild(label);
        form.appendChild(inp);
      });
    } else {
      // Sin vars pero body primitivo -> input único "body"
      const isPrimitiveBody =
        reqBody !== null &&
        reqBody !== undefined &&
        (typeof reqBody === "number" ||
          typeof reqBody === "string" ||
          typeof reqBody === "boolean");

      if (isPrimitiveBody) {
        const label = document.createElement("label");
        label.textContent = "body:";
        label.style.display = "block";
        label.style.marginTop = "4px";

        const inp = document.createElement("input");
        inp.dataset.var = "body";
        inp.id = `var_body_${Math.random().toString(16).slice(2)}`;
        item._bodyInputId = inp.id;

        if (typeof reqBody === "number") {
          inp.type = "number";
          inp.value = String(reqBody);
        } else if (typeof reqBody === "boolean") {
          inp.type = "checkbox";
          inp.checked = reqBody;
        } else {
          inp.type = "text";
          inp.value = reqBody;
        }

        form.appendChild(label);
        form.appendChild(inp);
      }
    }

    item._inputsInitialized = true;
  }

  // 2) MOSTRAR un botón "Enviar" propio del formulario (si no existe)
  if (!item._userSubmitButton) {
    const submitBtn = document.createElement("button");
    submitBtn.textContent = "Enviar";
    submitBtn.style.cssText = "margin-top:6px;padding:4px 8px;";
    submitBtn.type = "button";
    form.appendChild(submitBtn);
    item._userSubmitButton = submitBtn;

    submitBtn.onclick = () => {
      // aquí leemos inputs y resolvemos la promesa pendiente
      if (!pendingResolve) return;

      const updatedVars = { ...vars };
      const specials = [];

      // Leer todas las vars declaradas ($foo, etc.)
      Object.keys(vars).forEach(name => {
        // let bodyInput = null;
        // if (item._bodyInputId) {
        //   bodyInput = document.getElementById(item._bodyInputId);
        // }
        // if (!bodyInput) return;

        // let val;
        // if (bodyInput.type === "number") {
        //   val = Number(bodyInput.value);
        // } else if (bodyInput.type === "checkbox") {
        //   val = bodyInput.checked;
        // } else {
        //   val = bodyInput.value;
        // }
        let input = null;
        if (item._varInputIds && item._varInputIds[name]) {
          input = document.getElementById(item._varInputIds[name]);
        }
        if (!input) return;

        let val;
        if (input.type === "number") {
          const raw = input.value;
          val = raw === "" ? null : Number(raw);
        } else if (input.type === "checkbox") {
          val = input.checked;
        } else {
          val = input.value;
        }

        updatedVars[name] = val;
        specials.push({
          name,
          type: "manualInput",
          value: val,
        });
      });

      // Leer body primitivo como var "body"
      // const bodyInput = form.querySelector('input[data-var="body"]');
      let bodyInput = null;
      if (item._bodyInputId) {
          bodyInput = document.getElementById(item._bodyInputId);


      }
      if (bodyInput) {
        let val;
        if (bodyInput.type === "number") {
              
              const raw = bodyInput.value;
              const num = raw === "" ? null : Number(raw);
              val = num;
        } else if (bodyInput.type === "checkbox") {
          val = bodyInput.checked;
        } else {
          val = bodyInput.value;
        }

        updatedVars.body = val;
        specials.push({
          name: "body",
          type: "manualBody",
          value: val,
        });
      }

      pendingResolve(specials);
      pendingResolve = null;
    };
  }

  // 3) Devolver una PROMESA que se resolverá cuando se pulse "Enviar"
  return new Promise(resolve => {
    pendingResolve = resolve;
  });
}

let pendingObjectResolve = null;

async function objectBodyInputHandler(vars, item) {
  const form = item._form;
  const req = item._req;
  const body = req.reqbody;

  // Crear inputs solo la primera vez
  if (!item._objectInputsInitialized) {
    Object.entries(body).forEach(([key, value]) => {
      const label = document.createElement("label");
      label.textContent = `${key}: `;
      label.style.display = "block";
      label.style.marginTop = "4px";

      const inp = document.createElement("input");
      inp.dataset.bodyKey = key;
      inp.id = `body_${key}_${Math.random().toString(16).slice(2)}`;

      if (typeof value === "number") {
        inp.type = "number";
        inp.value = String(value);
      } else if (typeof value === "boolean") {
        inp.type = "checkbox";
        inp.checked = value;
      } else {
        inp.type = "text";
        inp.value = value ?? "";
      }

      form.appendChild(label);
      form.appendChild(inp);

      // opcional: guardar ids por clave
      item._bodyFieldIds = item._bodyFieldIds || {};
      item._bodyFieldIds[key] = inp.id;
    });

    // botón propio de envío para este caso (si quieres compartir con el otro, puedes)
    const submitBtn = document.createElement("button");
    submitBtn.textContent = "Enviar body";
    submitBtn.style.cssText = "margin-top:6px;padding:4px 8px;";
    submitBtn.type = "button";
    form.appendChild(submitBtn);
    item._objectSubmitButton = submitBtn;

    submitBtn.onclick = () => {
      if (!pendingObjectResolve) return;

      const updatedBody = { ...body };

      Object.entries(updatedBody).forEach(([key, oldVal]) => {
        let input = null;
        if (item._bodyFieldIds && item._bodyFieldIds[key]) {
          input = document.getElementById(item._bodyFieldIds[key]);
        } else {
          input = form.querySelector(`input[data-body-key="${key}"]`);
        }
        if (!input) return;

        let val;
        if (input.type === "number") {
          const raw = input.value;
          val = raw === "" ? null : Number(raw);
        } else if (input.type === "checkbox") {
          val = input.checked;
        } else {
          val = input.value;
        }

        updatedBody[key] = val;
      });

      pendingObjectResolve([
        {
          name: "body",
          type: "manualBodyObject",
          value: updatedBody,
        },
      ]);
      pendingObjectResolve = null;
    };

    item._objectInputsInitialized = true;
  }

  return new Promise(resolve => {
    pendingObjectResolve = resolve;
  });
}


// // Firma igual que el resto de handlers: (vars, item) => SpecialVar[]
// async function userInputHandler(vars, item) {
//   const form = item._form;
//   const req = item._req;
//   const updatedVars = { ...vars };
//   const specials = [];

//   // 1) Si es la primera vez, crear inputs según vars y body
//   if (!item._inputsInitialized) {
//     const varNames = Object.keys(vars);
//     const reqBody = req.reqbody;

//     if (varNames.length > 0) {
//       // Inputs por cada var detectada ($foo, etc.)
//       varNames.forEach(varName => {
//         const label = document.createElement("label");
//         label.textContent = `${varName}: `;
//         label.style.display = "block";
//         label.style.marginTop = "4px";

//         const inp = document.createElement("input");
//         inp.dataset.var = varName;
//         inp.id = `var_${varName}_${Math.random().toString(16).slice(2)}`;
//         inp.type = varName.toLowerCase().includes("int") ? "number" : "text";

//         form.appendChild(label);
//         form.appendChild(inp);
//       });
//     } else {
//       // Sin vars pero body primitivo -> input único "body"
//       const isPrimitiveBody =
//         reqBody !== null &&
//         reqBody !== undefined &&
//         (typeof reqBody === "number" ||
//           typeof reqBody === "string" ||
//           typeof reqBody === "boolean");

//       if (isPrimitiveBody) {
//         const label = document.createElement("label");
//         label.textContent = "body:";
//         label.style.display = "block";
//         label.style.marginTop = "4px";

//         const inp = document.createElement("input");
//         inp.dataset.var = "body";
//         inp.id = `var_body_${Math.random().toString(16).slice(2)}`;

//         if (typeof reqBody === "number") {
//           inp.type = "number";
//           inp.value = String(reqBody);
//         } else if (typeof reqBody === "boolean") {
//           inp.type = "checkbox";
//           inp.checked = reqBody;
//         } else {
//           inp.type = "text";
//           inp.value = reqBody;
//         }

//         form.appendChild(label);
//         form.appendChild(inp);
//       }
//     }

//     item._inputsInitialized = true;
//   }

//   // 2) Leer todas las vars declaradas ($foo, etc.)
//   Object.keys(vars).forEach(name => {
//     const input = form.querySelector(`input[data-var="${name}"]`);
//     if (!input) return;

//     let val;
//     if (input.type === "number") {
//       val = Number(input.value);
//     } else if (input.type === "checkbox") {
//       val = input.checked;
//     } else {
//       val = input.value;
//     }

//     updatedVars[name] = val;
//     specials.push({
//       name,
//       type: "manualInput",
//       value: val,
//     });
//   });

//   // 3) Leer body primitivo como var "body"
//   const bodyInput = form.querySelector('input[data-var="body"]');
//   if (bodyInput) {
//     let val;
//     if (bodyInput.type === "number") {
//       val = Number(bodyInput.value);
//     } else if (bodyInput.type === "checkbox") {
//       val = bodyInput.checked;
//     } else {
//       val = bodyInput.value;
//     }

//     updatedVars.body = val;
//     specials.push({
//       name: "body",
//       type: "manualBody",
//       value: val,
//     });
//   }

//   // Salida EXACTAMENTE igual que un handler normal
//   return specials;
// }
