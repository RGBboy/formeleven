import * as dotenv from 'dotenv'
import fs from 'filendir';
import { GraphQLClient, gql } from 'graphql-request';
import path, { dirname } from 'path';
import { fileURLToPath } from 'url';
import template from './template.mjs';
import xmlTemplate from './xml-template.mjs';
import Elm from './elm.js';

const envString = process.env.NODE_ENV ? "." + process.env.NODE_ENV : "";

dotenv.config({ path: path.resolve(process.cwd(), ".env" + envString)})

const render = ([ key, { body, title, description } ]) => {
  const ext = path.extname(key);
  const filePath = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'frontend', key);

  switch (ext) {
    case ".html":
        fs.writeFileSync(filePath, template(title, description, body, process.env.GA_PROPERTY));
        console.log(`Successfully generated ${filePath}`);
      break;
    case ".xml":
        fs.writeFileSync(filePath, xmlTemplate(body));
        console.log(`Successfully generated ${filePath}`);
      break;
    default:
      console.log(`Uknown extension to format`);
  }

};

const query = gql`
{
  collection(id: "gid://shopify/Collection/291476668611") {
    id
    handle
    title
    descriptionHtml
    products(first: 21) {
      nodes {
        id
        handle
        title
        description
        descriptionHtml
        availableForSale
        priceRange {
          maxVariantPrice {
            amount
            currencyCode
          }
        }
        featuredImage {
          altText
          url(transform: {
            maxWidth: 512
            maxHeight: 512
          })
        }
        images(first:10){
          nodes {
            altText
            url(transform: {
              maxWidth: 512
              maxHeight: 512
            })
          }
        }
        productHighlights: metafield(namespace: "formeleven", key: "productHighlights") {
          value
        }
        productDetails: metafield(namespace: "formeleven", key: "productDetails") {
          value
        }
    	}
  	}
	}

  feed: collection(id: "gid://shopify/Collection/291476668611") {
    products(first: 21) {
      nodes {
        id
        handle
        title
        description
        productType
        availableForSale
        priceRange {
          maxVariantPrice {
            amount
            currencyCode
          }
        }
        images(first:10){
          nodes {
            url(transform: {
              maxWidth: 1024
              maxHeight: 1024
            })
          }
        }
        productHighlights: metafield(namespace: "formeleven", key: "productHighlights") {
          value
        }
        productDetails: metafield(namespace: "formeleven", key: "productDetails") {
          value
        }
        tags
      }
    }
  }

  shop {
    refundPolicy {
      body
    }
  }

}
`

const gqlClient = new GraphQLClient(process.env.SHOPIFY_ENDPOINT, {
  headers: {
    "X-Shopify-Storefront-Access-Token": process.env.SHOPIFY_ACCESS_TOKEN
  },
});

gqlClient.request(query).then((data) => {
  const builder = Elm.Elm.Build.init({ flags: data });
  const response = builder.ports.htmlOut;
  response.subscribe((pages) => {
    Object.entries(pages).map(render);
  });
});
