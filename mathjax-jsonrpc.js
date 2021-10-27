#!/usr/bin/env node -r esm
// Author: Shaq Xu <shaqxu@163.com>
// Maintainer: Shaq Xu <shaqxu@163.com>
// Version: 1.0
//const mathjax = require('mathjax-full/es5/node-main.js');

const mathjax = require('mathjax-full/components/src/node-main/node-main.js');

const rpc = require('vscode-jsonrpc');

mathjax.init({
    loader: {
        load: ['input/asciimath', 'input/tex', 'output/svg'],
        source: require('mathjax-full/components/src/source.js').source
    },
    svg: {
        scale: 2,
        minScale: 1.5,
        exFactor: 2
    }
}).then((MathJax) => {
    let connection = rpc.createMessageConnection(
        new rpc.StreamMessageReader(process.stdin),
        new rpc.StreamMessageWriter(process.stdout));

    connection.onRequest(new rpc.RequestType('asciimath2svg'), (param) => {
        let svg = MathJax.asciimath2svg(param, { display: false });
        return MathJax.startup.adaptor.innerHTML(svg);
    });
    connection.onRequest(new rpc.RequestType('tex2svg'), (param) => {
        let svg = MathJax.tex2svg(param, { display: false });
        return MathJax.startup.adaptor.innerHTML(svg);
    });

    connection.listen();
});
