/* elm-pkg-js
port wire3WsConnect : String -> Cmd msg
port wire3WsDisconnect : () -> Cmd msg
port wire3WsSend : List Int -> Cmd msg
port wire3WsReceived : (List Int -> msg) -> Sub msg
port wire3WsStatus : (String -> msg) -> Sub msg
*/

window.elmPkgJs = window.elmPkgJs || {};
window.elmPkgJs['wire3-ws'] = {
    init: function(app) {
        if (!app.ports || !app.ports.wire3WsConnect || !app.ports.wire3WsSend ||
            !app.ports.wire3WsReceived || !app.ports.wire3WsStatus) {
            console.warn('elm-pkg-js [wire3-ws]: required ports not found');
            return;
        }

        var ws = null;
        var reconnectTimer = null;
        var attempt = 0;
        var currentUrl = null;
        var intentionalClose = false;

        function sendStatus(status) {
            app.ports.wire3WsStatus.send(status);
        }

        function connect(url) {
            if (ws && (ws.readyState === WebSocket.CONNECTING || ws.readyState === WebSocket.OPEN)) {
                ws.close();
            }
            if (reconnectTimer) {
                clearTimeout(reconnectTimer);
                reconnectTimer = null;
            }

            currentUrl = url;
            intentionalClose = false;
            ws = new WebSocket(url);
            ws.binaryType = 'arraybuffer';

            ws.onopen = function() {
                attempt = 0;
                sendStatus('connected');
            };

            ws.onmessage = function(event) {
                if (event.data instanceof ArrayBuffer) {
                    app.ports.wire3WsReceived.send(Array.from(new Uint8Array(event.data)));
                }
            };

            ws.onclose = function() {
                ws = null;
                if (intentionalClose) {
                    sendStatus('disconnected');
                    return;
                }
                attempt++;
                var delay = Math.min(1000 * Math.pow(1.5, attempt), 30000);
                sendStatus('reconnecting');
                reconnectTimer = setTimeout(function() {
                    reconnectTimer = null;
                    if (currentUrl && !intentionalClose) {
                        connect(currentUrl);
                    }
                }, delay);
            };

            ws.onerror = function() {
                // onclose fires after onerror, handles reconnect
            };
        }

        app.ports.wire3WsConnect.subscribe(function(url) {
            connect(url);
        });

        if (app.ports.wire3WsDisconnect) {
            app.ports.wire3WsDisconnect.subscribe(function() {
                intentionalClose = true;
                if (reconnectTimer) {
                    clearTimeout(reconnectTimer);
                    reconnectTimer = null;
                }
                if (ws) { ws.close(); }
                currentUrl = null;
                sendStatus('disconnected');
            });
        }

        app.ports.wire3WsSend.subscribe(function(byteArray) {
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(new Uint8Array(byteArray).buffer);
            }
        });
    }
};
