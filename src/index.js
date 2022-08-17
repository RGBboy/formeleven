import "dotenv/config";
import { Elm } from "../frontend/elm.js";
import { createCart } from "./cart.js";
import { createTracking } from "./tracking.js";

// Mailing List

Elm.Mailer.init({ node: document.getElementById("newsletter") });

// Cart

const endpoint = process.env.SHOPIFY_ENDPOINT;
const token = process.env.SHOPIFY_ACCESS_TOKEN;
const buyButtonElement = document.getElementById("buy-button");
const productVariantId = buyButtonElement.getAttribute("data-buy-button");

const cartConfig = {
  endpoint,
  token,
  element: buyButtonElement,
  productVariantId,
  window
};

const eventEmitter = createCart(cartConfig);

// Tracking + Consent

const analyticsPropertyId = process.env.GA_PROPERTY;
const adsPropertyId = process.env.AW_PROPERTY;
const addToBasketEventId = process.env.AW_ADD_TO_BASKET_EVENT_ID;
const beginCheckoutEventId = process.env.AW_BEGIN_CHECKOUT_EVENT_ID;
const purchaseEventId = process.env.AW_PURCHASE_EVENT_ID;

const trackingConfig = {
  analyticsPropertyId,
  adsPropertyId,
  addToBasketEventId,
  beginCheckoutEventId,
  purchaseEventId,
  element: document.getElementById("cookie-consent"),
  window,
  eventEmitter
};

createTracking(trackingConfig);
