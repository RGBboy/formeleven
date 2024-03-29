import React from 'react';
import ReactDOM from 'react-dom';
import GraphiQL from 'graphiql';
import { createGraphiQLFetcher } from '@graphiql/toolkit';

const fetcher = createGraphiQLFetcher(
  { url: process.env.SHOPIFY_ENDPOINT
  , headers : {
      "X-Shopify-Storefront-Access-Token": process.env.SHOPIFY_ACCESS_TOKEN
    }
  });

ReactDOM.render(
  React.createElement(GraphiQL, { fetcher: fetcher }),
  document.getElementById('graphiql'),
);
