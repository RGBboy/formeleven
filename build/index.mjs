import 'dotenv/config';
import fs from 'filendir';
import { GraphQLClient, gql } from 'graphql-request';
import path, { dirname } from 'path';
import { fileURLToPath } from 'url';
import template from './template.mjs';
import Elm from './elm.js';

const render = ([key, value]) => {
  key = (key === '/') ? '/index.html' : key + '.html';
  const filePath = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'frontend', key);
  fs.writeFileSync(filePath, template(value));
  console.log(`Successfully generated ${filePath}`);
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
