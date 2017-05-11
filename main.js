const server = require('http').createServer();
const WebSocketServer = require('elm-websocket-server');
const app = require('./server.js').Server.worker();

let port = (process.env.PORT || 8080);
let wss = new WebSocketServer(
	server,
	app.ports.inputPort,
	app.ports.outputPort);

server.listen(port);

console.log(`Listening on :${port}`);
