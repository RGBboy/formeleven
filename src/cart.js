import { Elm } from "../frontend/elm.js";
import { EventEmitter } from 'events';

const cartKey = "cartId";

export const createCart = (config) => {

  const {
    endpoint,
    token,
    element,
    productVariantId,
    window
  } = config;

  const eventEmitter = new EventEmitter();

  const localStorage = window.localStorage;
  const cartId = localStorage.getItem(cartKey);

  const checkoutWindowConfig = {
    width: 400,
    height: 600,
    left: (window.outerWidth / 2) - 200,
    top: (window.outerHeight / 2) - 300
  };

  const checkoutWindowParams = Object.keys(checkoutWindowConfig).reduce((acc, key) => `${acc}${key}=${checkoutWindowConfig[key]},`, '');

  const cart = Elm.Cart.init({
    node: element,
    flags: {
      endpoint,
      token,
      cartId,
      productVariantId
    }
  });

  // TEMP to be able to trigger events
  // window.cart = cart;

  cart.ports.events.subscribe((message) => {
    if (message && message.type) {
      eventEmitter.emit(message.type, message.value);
    };
  });

  cart.ports.commands.subscribe((message) => {
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

    cart.ports.messages.send(data);

  });

  eventEmitter.on("CartCreated", (value) => {
    localStorage.setItem(cartKey, value);
  });

  return eventEmitter;
};

// The script below needs to be added to shopify settings
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
//     }
//   }), "*");
// };
// </script>



// Example message data from checkout post message

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
