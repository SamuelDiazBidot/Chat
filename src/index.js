import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

//Add Message
var addMessageWS = new WebSocket("ws://localhost:5000/addMessage");

addMessageWS.onmessage = function(message) {
  console.log(message.data);
  app.ports.addMessageIn.send(message.data);
};

app.ports.addMessageOut.subscribe(function(msg) { addMessageWS.send(msg); });

//Delete Message
var deleteMessageWS = new WebSocket("ws://localhost:5000/deleteMessage");

deleteMessageWS.onmessage = function(message) {
  //console.log(message.data);
  app.ports.deleteMessageIn.send(message.data);
}

app.ports.deleteMessageOut.subscribe(function(msg) { deleteMessageWS.send(msg) });


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
