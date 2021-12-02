const fs = require('fs');

const day = process.argv[2]
const Elm = require(`./build/${day}.js`).Elm;

const puzzleInput = fs.readFileSync(`input/2021-${day}.txt`).toString()

var main = Elm[`Day${day}`].init();
main.ports.receiveInput.send(puzzleInput)

main.ports.output.subscribe(data => {
    console.log(JSON.stringify(data))
})