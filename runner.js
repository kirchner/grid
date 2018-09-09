const process = require('process')
if  (typeof process.argv[2] === 'undefined') {
  console.log('no elm.js specified')
  process.exit()
}
const {Elm} = require('./' + process.argv[2])
const seed = Math.floor(1000*Math.random())

const getStdin = () => {
  return new Promise((resolve, reject) => {
    const readline = require('readline').createInterface({
      input: process.stdin,
      output: process.stdout,
    })
    let inputs = []
    readline.on('line', (str) => { inputs.push(str) })
    readline.on('error', (err) => { reject(err) })
    readline.on('close', (str) => { resolve(inputs) })
  })
}

(async () => {
  const inputs = await getStdin()
  const app = Elm.Main.init({flags: { inputs: inputs }})
  app.ports.notifyWon.subscribe(() => {
    console.log("WON")
  })
  app.ports.notifyLost.subscribe(() => {
    console.log("LOST")
  })
  app.ports.notifyFail.subscribe(() => {
    console.log("FAIL")
  })
})()
