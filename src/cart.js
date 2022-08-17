import "dotenv/config";
import { Elm } from "./Cart.elm";
import { EventEmitter } from 'events';

const endpoint = process.env.SHOPIFY_ENDPOINT;
const token = process.env.SHOPIFY_ACCESS_TOKEN;

const cartKey = "cartId";
const localStorage = window.localStorage;

const cartId = localStorage.getItem(cartKey);

// User does not have a cart
// const cartId = null;

// User has a missing cart
// const cartId = "gid://shopify/Cart/546afca7e868f12bc644b9af3057bd0";

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

const eventEmitter = new EventEmitter();

app.ports.events.subscribe((message) => {
  if (message && message.type) {
    eventEmitter.emit(message.type, message.value);
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


// Conversion Tracking

const analyticsPropertyId = process.env.GA_PROPERTY;
const adsPropertyId = process.env.AW_PROPERTY;
const addToBasketEventId = process.env.AW_ADD_TO_BASKET_EVENT_ID;
const beginCheckoutEventId = process.env.AW_BEGIN_CHECKOUT_EVENT_ID;
const purchaseEventId = process.env.AW_PURCHASE_EVENT_ID;

const defaultConsent = {
  ad_storage: "denied",
  analytics_storage: "denied"
};

window.dataLayer = window.dataLayer || [];
function gtag() { dataLayer.push(arguments); }

// gtag("consent", "default", defaultConsent);

gtag("js", new Date());

gtag("config", analyticsPropertyId);

if (adsPropertyId) {
  gtag("config", adsPropertyId);
};

eventEmitter.on("CartCreated", (value) => {
  localStorage.setItem(cartKey, value);
});

eventEmitter.on("AddedToCart", (value) => {
  // Analytics
  gtag("event", "add_to_cart", {
    send_to: analyticsPropertyId,
    ...value
  });
  // Google Ads
  gtag("event", "conversion", {
    send_to: `${adsPropertyId}/${addToBasketEventId}`
  });
});

eventEmitter.on("RemovedFromCart", (value) => {
  // Analytics
  gtag("event", "remove_from_cart", {
    send_to: analyticsPropertyId,
    ...value
  });
});

eventEmitter.on("CheckoutStarted", (value) => {
  // Analytics
  gtag("event", "begin_checkout", {
    send_to: analyticsPropertyId,
    ...value
  });
  // Google Ads
  gtag("event", "conversion", {
    send_to: `${adsPropertyId}/${beginCheckoutEventId}`
  });
});

eventEmitter.on("CheckoutCompleted", (value) => {
  // Analytics
  gtag("event", "purchase", {
    send_to: analyticsPropertyId,
    ...value
  });
  // Google Ads
  gtag("event", "conversion", {
    send_to: `${adsPropertyId}/${beginCheckoutEventId}`,
    ...value
  });
});
