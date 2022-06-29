const template = (body) =>
`<?xml version="1.0"?>
<rss xmlns:g="http://base.google.com/ns/1.0" version="2.0">
${body}
</rss>
`;

export default template;
