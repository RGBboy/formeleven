import { Elm } from "./Main.elm";

Elm.Main.init({ node: document.getElementById("newsletter") });

// Note: requires shopify buy library to be loaded on the web page
// <script src="https://sdks.shopifycdn.com/buy-button/latest/buy-button-storefront.min.js"></script>

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
