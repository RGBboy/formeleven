var path = require('path'),
    fs = require('fs'),
    template = require('./template'),
    data = require('./data').data,
    builder = require('./elm').Elm.Build.init({ flags: data }),
    response = builder.ports.htmlOut;

const render = ([key, value]) => {
  key = (key === '/') ? '/index.html' : key + '.html';
  const filePath = path.join(__dirname, '..', 'frontend', key);
  fs.writeFileSync(filePath, template(value));
  console.log(`Successfully generated ${filePath}`);
};

response.subscribe((pages) => {
  Object.entries(pages).map(render);
});
