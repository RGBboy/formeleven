import "dotenv/config";
import { Elm } from "../frontend/elm.js";

Elm.Mailer.init({ node: document.getElementById("newsletter") });

// Util

const logger = (label) => (e) => console.log(label, e);

// Google Analytics

const defaultConsent = {
  ad_storage: "denied",
  analytics_storage: "denied",
  wait_for_update: 500
};

window.dataLayer = window.dataLayer || [];
function gtag() { dataLayer.push(arguments); }

gtag("consent", "default", defaultConsent);

gtag("js", new Date());

gtag("config", process.env.GA_PROPERTY);

if (process.env.AW_PROPERTY) {
  gtag("config", process.env.AW_PROPERTY);
}

const trackConversion = (conversionID, eventID) => {
  const track = trackConversionWithData(conversionID, eventID);
  return () => track();
};

const trackConversionWithData = (conversionID, eventID) => (data) => {
  const sendTo = `${conversionID}/${eventID}`;
  const conversionData = {
    "send_to": sendTo,
    ...data
  };
  gtag("event", "conversion", conversionData);
};

const conversionID = process.env.AW_PROPERTY;
const addToBasketEventID = process.env.AW_ADD_TO_BASKET_EVENT_ID;
const beginCheckoutEventID = process.env.AW_BEGIN_CHECKOUT_EVENT_ID;
const purchaseEventID = process.env.AW_PURCHASE_EVENT_ID;

const trackAddToBasket = trackConversion(conversionID, addToBasketEventID);
const trackBeginCheckout = trackConversion(conversionID, beginCheckoutEventID);
const trackPurchase = trackConversionWithData(conversionID, purchaseEventID);

// Local Storage

const localStorage = window.localStorage;

const consentApp = Elm.Consent.init({
  node: document.getElementById("cookie-consent"),
  flags: localStorage.getItem("consent")
});

consentApp.ports.consentUpdate.subscribe((value) => {

  // store change in local storage
  localStorage.setItem("consent", JSON.stringify(value));

  // Send updated consent to GA
  let gaConsent = defaultConsent;
  if (value && value.performance) {
    gaConsent = {
      ad_storage: "granted",
      analytics_storage: "granted"
    };
  };

  gtag("consent", "update", gaConsent);

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

  if (data.orderData) {
    trackPurchase(data.orderData);
    // unfortunately we have to reload the page to reset the cart
    location.reload();
  };
});

// The code below needs to be added to shopify settings
// checkout > order status page > additional scripts
// <script>
//   if (window.opener) {
//     window.opener.postMessage(JSON.stringify({
//       orderData: {
//         "value": {{ checkout.subtotal_price | divided_by: 100.0 }},
//         "currency": "{{ currency }}",
//         "transaction_id": "{{ order_number }}"
//       }
//     }), "*");
//   };
// </script>

// Note: requires shopify buy button js to be loaded on the web page
// The one that we load has been modified to remove window.reload()
const client = ShopifyBuy.buildClient({
  domain: "formeleven.myshopify.com",
  storefrontAccessToken: "be1c6fb5a5922fe3da659e0e48d6e157"
});

ShopifyBuy.UI.onReady(client).then(function (ui) {
  const buttonElements = document.querySelectorAll("div[data-buy-button]");
  const cartElement = document.getElementById("cart");

  if (buttonElements.length == 0 && cartElement) {
    ui.createComponent("cart", {
      node: cartElement,
      moneyFormat: moneyFormat,
      options: shopifyOptions
    });
  } else {
    buttonElements.forEach((element) => {
      const id = element.getAttribute("data-buy-button");
      ui.createComponent("product", {
        id: id,
        node: element,
        moneyFormat: moneyFormat,
        options: shopifyOptions
      });
    });
  };

});

const moneyFormat = "%C2%A3%7B%7Bamount%7D%7D";

const fontSize = "1rem";

const fontColor = "#333333";

const interactiveButtonStyles = {
  "background-color": "#000000",
  "opacity": "1",
  "transition": "opacity .15s ease-in",
  ":hover": {
    "background-color": "#000000",
    "opacity": "0.5",
    "transition": "opacity .15s ease-in"
  },
  ":focus": {
    "background-color": "#000000",
    "opacity": "0.5",
    "transition": "opacity .15s ease-in"
  }
};

const buttonStyles = {
  "border-radius": "0.5rem",
  "padding-top": "1rem",
  "padding-bottom": "1rem",
  "padding-left": "2rem",
  "padding-right": "2rem",
  "font-family": "Raleway, sans-serif",
  "font-weight": "bold",
  "font-size": "1.25rem"
};

const shopifyOptions = {
  "product": {
    "events": {
      addVariantToCart: trackAddToBasket,
      openCheckout: trackBeginCheckout
    },
    "styles": {
      "product": {
        "text-align": "left"
      },
      "buttonWrapper": {
        "margin-top": "0px"
      },
      "button": {
        ...buttonStyles,
        ...interactiveButtonStyles
      }
    },
    "contents": {
      "img": false,
      "button": true,
      "buttonWithQuantity": false,
      "title": false,
      "price": false
    },
    "text": {
      "button": "Add to cart"
    },
    "googleFonts": [
      "Raleway"
    ]
  },
  "cart": {
    "events": {
      openCheckout: trackBeginCheckout
    },
    "styles": {
      "button": {
        ...buttonStyles,
        ...interactiveButtonStyles
      },
      "title": {
        "color": fontColor
      },
      "header": {
        "color": fontColor
      },
      "lineItems": {
        "color": fontColor
      },
      "subtotalText": {
        "color": fontColor
      },
      "subtotal": {
        "color": fontColor
      },
      "notice": {
        "color": fontColor
      },
      "currency": {
        "color": fontColor
      },
      "close": {
        "color": fontColor,
        ":hover": {
          "color": fontColor
        }
      },
      "empty": {
        "color": fontColor
      },
      "noteDescription": {
        "color": fontColor
      },
      "discountText": {
        "color": fontColor
      },
      "discountIcon": {
        "fill": fontColor
      },
      "discountAmount": {
        "color": fontColor
      }
    },
    "text": {
      "total": "Subtotal",
      "button": "Checkout"
    },
    "googleFonts": [
      "Raleway"
    ]
  },
  "toggle": {
    "styles": {
      "toggle": {
        "font-family": "Raleway, sans-serif",
        "font-weight": "bold",
        ...interactiveButtonStyles
      },
      "count": {
        "font-size": fontSize
      }
    },
    "googleFonts": [
      "Raleway"
    ]
  },
  "lineItem": {
    "styles": {
      "variantTitle": {
        "color": fontColor
      },
      "title": {
        "color": fontColor
      },
      "price": {
        "color": fontColor
      },
      "fullPrice": {
        "color": fontColor
      },
      "discount": {
        "color": fontColor
      },
      "discountIcon": {
        "fill": fontColor
      },
      "quantity": {
        "color": fontColor
      },
      "quantityIncrement": {
        "color": fontColor,
        "border-color": fontColor
      },
      "quantityDecrement": {
        "color": fontColor,
        "border-color": fontColor
      },
      "quantityInput": {
        "color": fontColor,
        "border-color": fontColor
      }
    }
  }
};
