import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

//Add Message///////////////////
var addMessageWS = new WebSocket("ws://192.168.0.10:5000/addMessage");

addMessageWS.onmessage = function(message) {
  app.ports.addMessageIn.send(message.data);
};

app.ports.addMessageOut.subscribe(function(msg) { addMessageWS.send(msg); });
///////////////////////////////

//Delete Message//////////////
var deleteMessageWS = new WebSocket("ws://192.168.0.10:5000/deleteMessage");

deleteMessageWS.onmessage = function(message) {
  app.ports.deleteMessageIn.send(message.data);
}

app.ports.deleteMessageOut.subscribe(function(msg) { deleteMessageWS.send(msg) });
///////////////////////////////

//Edit Message////////////////
var editMessageWs = new WebSocket("ws://192.168.0.10:5000/editMessage");

editMessageWs.onmessage = function(message) {
  app.ports.editMessageIn.send(message.data);
}

app.ports.editMessageOut.subscribe(function(msg) { editMessageWs.send(msg) });
/////////////////////////////

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
