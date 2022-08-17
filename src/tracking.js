import { Elm } from "../frontend/elm.js";

const consentKey = "consent";

const defaultConsent = {
  ad_storage: "denied",
  analytics_storage: "denied"
};

export const createTracking = (config) => {

  const {
    analyticsPropertyId,
    adsPropertyId,
    addToBasketEventId,
    beginCheckoutEventId,
    purchaseEventId,
    element,
    window,
    eventEmitter
  } = config;

  // Local Storage

  const localStorage = window.localStorage;

  window.dataLayer = window.dataLayer || [];
  function gtag() { window.dataLayer.push(arguments); }

  gtag("consent", "default", defaultConsent);

  gtag("js", new Date());

  gtag("config", analyticsPropertyId);

  if (adsPropertyId) {
    gtag("config", adsPropertyId);
  };

  const consentApp = Elm.Consent.init({
    node: element,
    flags: localStorage.getItem(consentKey)
  });

  consentApp.ports.consentUpdate.subscribe((value) => {
    // store change in local storage
    localStorage.setItem(consentKey, JSON.stringify(value));
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

  // Conversion Tracking

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

  eventEmitter.on("CheckoutCompleted", (config) => (value) => {
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

};
