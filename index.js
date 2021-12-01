var Elm = require('./build/main').Elm;
var main = Elm.Main.init();

main.ports.get.send("Hello World!")

main.ports.put.subscribe(data => {
    console.log(JSON.stringify(data))
})