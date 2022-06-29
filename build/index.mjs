import 'dotenv/config';
import fs from 'filendir';
import { GraphQLClient, gql } from 'graphql-request';
import path, { dirname } from 'path';
import { fileURLToPath } from 'url';
import template from './template.mjs';
import feedTemplate from './feed-template.mjs';
import Elm from './elm.js';

const render = ([key, value]) => {
  const ext = path.extname(key);
  const filePath = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'frontend', key);

  switch (ext) {
    case ".html":
        fs.writeFileSync(filePath, template(value));
        console.log(`Successfully generated ${filePath}`);
      break;
    case ".xml":
        fs.writeFileSync(filePath, feedTemplate(value));
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
      }
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
  // console.log(data)
  const builder = Elm.Elm.Build.init({ flags: data });
  const response = builder.ports.htmlOut;
  response.subscribe((pages) => {
    Object.entries(pages).map(render);
  });
});
