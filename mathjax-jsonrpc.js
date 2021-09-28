#!/usr/bin/env node
const mathjax = require('mathjax');

const rpc = require('vscode-jsonrpc');

mathjax.init({
    loader: { load: ['input/asciimath', 'input/tex', 'output/svg'] }
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
