const template = (body) =>
`<!doctype html>
<html class="no-js" lang="en">

<head>
  <meta charset="utf-8">
  <title>Form Eleven</title>
  <meta name="description" content="Combining digital fabrication, handmade ceramics and luminaires.">
  <meta name="viewport" content="width=device-width, initial-scale=1">

  <meta property="og:title" content="Form Eleven">
  <meta property="og:type" content="">
  <meta property="og:url" content="">
  <meta property="og:image" content="">

  <link rel="manifest" href="/site.webmanifest">
  <link rel="apple-touch-icon" href="/icon.png">

  <link rel="stylesheet" href="https://unpkg.com/tachyons@4/css/tachyons.min.css">

  <meta name="theme-color" content="#fafafa">

  <style>
    @import url('/fonts.css');
  </style>

  <script type="module" src="/index.js"></script>

  <!-- Global site tag (gtag.js) - Google Analytics -->
  <script async src="https://www.googletagmanager.com/gtag/js?id=G-8G0BV1N2JQ"></script>
  <script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}

    gtag('consent', 'default', {
      'ad_storage': 'denied',
      'analytics_storage': 'denied'
    });

    gtag('js', new Date());

    gtag('config', 'G-8G0BV1N2JQ');
  </script>

</head>
${body}
</html>
`;

exports = module.exports = template;
