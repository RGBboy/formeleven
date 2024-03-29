const tagsToReplace = {
  '&': '&amp;',
  '<': '&lt;',
  '>': '&gt;'
};

function replaceTag(tag) {
  return tagsToReplace[tag] || tag;
}

function sanitize(str) {
  return str.replace(/[&<>]/g, replaceTag);
}

const template = (title, description, body, gaProperty) =>
`<!doctype html>
<html class="no-js" lang="en">

<head>
  <meta charset="utf-8">
  <title>${sanitize(title)}</title>
  <meta name="description" content="${sanitize(description)}">
  <meta name="viewport" content="width=device-width, initial-scale=1">

  <meta property="og:title" content="Form Eleven">
  <meta property="og:type" content="">
  <meta property="og:url" content="">
  <meta property="og:image" content="">

  <link rel="manifest" href="/frontend/site.webmanifest">
  <link rel="apple-touch-icon" href="/frontend/icon.png">

  <link rel="stylesheet" href="https://unpkg.com/tachyons@4/css/tachyons.min.css">

  <meta name="theme-color" content="#fafafa">

  <style>
    @import url('/frontend/fonts.css');
    @import url('/frontend/style.css');
  </style>

  <script type="module" src="/src/index.js"></script>

  <!-- Global site tag (gtag.js) - Google Analytics -->
  <script type="text/javascript" async src="https://www.googletagmanager.com/gtag/js?id=${gaProperty}"></script>

</head>
${body}
</html>
`;

export default template;
