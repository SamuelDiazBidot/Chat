port module WebSockets exposing (..)

-- JavaScript usage: app.ports.addMessageIn.send(response);
port addMessageIn : (String -> msg) -> Sub msg
-- JavaScript usage: app.ports.addMessageOut.subscribe(handler);
port addMessageOut : String -> Cmd msg


--JavaScript usage: app.ports.deleteMessageIn.send(response);
port deleteMessageIn : (String -> msg) -> Sub msg
-- JavaScript usage: app.ports.deleteMessageOut.subscribe(handler);
port deleteMessageOut : String -> Cmd msg