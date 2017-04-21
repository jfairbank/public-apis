require('./main.css');

var Elm = require('./Main.elm');
var apis = require('./apis');

var root = document.getElementById('root');

Elm.Main.embed(root, apis);
