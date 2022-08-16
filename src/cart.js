import "dotenv/config";
import { Elm } from "./Cart.elm";

const endpoint = process.env.SHOPIFY_ENDPOINT;
const token = process.env.SHOPIFY_ACCESS_TOKEN;

// User does not have a cart
// const cartId = null;

// User has a missing cart
// const cartId = "gid://shopify/Cart/546afca7e868f12bc644b9af3057bd0";

// User has an existing cart
// const cartId = "gid://shopify/Cart/546afca7e868f12bc644b9af3057bd0f";

const cartKey = "cartId";
const localStorage = window.localStorage;

const cartId = localStorage.getItem(cartKey);

const checkoutWindowConfig = {
  width: 400,
  height: 600,
  left: (window.outerWidth / 2) - 200,
  top: (window.outerHeight / 2) - 300
};

const checkoutWindowParams = Object.keys(checkoutWindowConfig).reduce((acc, key) => `${acc}${key}=${checkoutWindowConfig[key]},`, '');

const app = Elm.Cart.init({
  node: document.getElementById("main"),
  flags: {
    endpoint: endpoint,
    token: token,
    cartId: cartId
  }
});

window.app = app;

app.ports.events.subscribe((message) => {
  console.log("Event", message);
  if (message && message.type === "CartCreated") {
    localStorage.setItem(cartKey, message.value);
  };
});

app.ports.commands.subscribe((message) => {
  console.log("Command", message);
  if (message && message.type === "Checkout") {
    window.open(
      message.url,
      "Checkout",
      checkoutWindowParams
    );
  };
});

// We use this to get the order state after checkout is complete
// via the checkout pop up postMessage calls
window.addEventListener("message", (msg) => {

  let data;
  try {
    data = JSON.parse(msg.data);
  } catch (err) {
    data = {};
  };

  console.log("data", data);

  app.ports.messages.send(data);

  // {
  //   current_checkout_page: "/checkout/contact_information"
  // }

  // {
  //   "current_checkout_page": "/checkout/shipping"
  // }

  // {
  //   "current_checkout_page": "/checkout/payment"
  // }

  // {
  //     "current_checkout_page": "/checkout/processing"
  // }

  // {
  //     "current_checkout_page": "/checkout/thank_you"
  // }

  // current thank_you page message with data
  // {
  //   orderData: {
  //     value: 0,
  //     currency: 'GBP',
  //     transaction_id: '1035'
  //   }
  // }

  // new thank_you page message with data
  // {
  //   type: "CheckoutCompleted",
  //   order: {
  //     value: 0,
  //     currency: 'GBP',
  //     transactionId: '1035'
  //   },
  //   orderData: {
  //     value: 0,
  //     currency: 'GBP',
  //     transaction_id: '1035'
  //   }
  // }

  // if (data.orderData) {
  //   trackPurchase(data.orderData);
  //   // unfortunately we have to reload the page to reset the cart
  //   location.reload();
  // };
});

// The code below needs to be added to shopify settings
// checkout > order status page > additional scripts
// <script>
// if (window.opener) {
//   window.opener.postMessage(JSON.stringify({
//     type: "CheckoutCompleted",
//     order: {
//       subTotal: {
//         amount: "{{ checkout.subtotal_price | divided_by: 100.0 }}",
//         currencyCode: "{{ currency }}"
//       },
//       transactionId: "{{ order_number }}"
//     },
//     orderData: {
//       value: {{ checkout.subtotal_price | divided_by: 100.0 }},
//       currency: "{{ currency }}",
//       transaction_id: "{{ order_number }}"
//     }
//   }), "*");
// };
// </script>
