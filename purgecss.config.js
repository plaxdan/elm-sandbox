module.exports = {
    content: [
        './src/index.html',
        './src/**/*.elm'
    ],
    defaultExtractor: content => content.match(/[A-Za-z0-9-_:/]+/g) || []
};
