"use strict";

// import libraries
const { performance } = require("perf_hooks");
const dgram = require("dgram");

// set some constants
const HEARTBEAT_INTERVAL = 10000; // 10 seconds
const HTTP_PORT = 1234;

const server = dgram.createSocket("udp4");
const client = dgram.createSocket("udp4");

let users = {};

server.on("listening", function () {
    let address = server.address();
    console.log(
        "UDP Server listening on " + address.address + ":" + address.port
    );
});

server.on("message", (data, remote) => {
    let msg_code = data.readUInt8(0);
    console.log(`receive: ${msg_code}`);

    users[`${remote.address}:${remote.port}`] = { address: remote.address, port: remote.port, lastSeen: performance.now() };

    if(msg_code > 0) {
        // Send the message to all users:
        for (const [key, info] of Object.entries(users)) {
            console.log(`send to ${key}: ${msg_code}`);
            client.send(new Uint8Array([msg_code]), info.port, info.address);
        }
    }
});

// start listening
server.bind(HTTP_PORT);

// heartbeat handler
const interval = setInterval(function ping() {
    for (const [key, info] of Object.entries(users)) {
        // check when we saw the user for the last time
        if (performance.now() - info.lastSeen > HEARTBEAT_INTERVAL) {
            delete users.key;
            console.log(
                `${address} has been disconnected (${
                    (performance.now() - info.lastSeen) / 1000
                }).`
            );
        }
    }
}, HEARTBEAT_INTERVAL);

// helpers to convert ArrayBuffer <=> String
// source: http://stackoverflow.com/a/11058858
function ab2str(buf) {
    return String.fromCharCode.apply(null, new Uint8Array(buf));
}
function str2ab(str) {
    var buf = new ArrayBuffer(str.length);
    var bufView = new Uint8Array(buf);
    for (var i = 0, strLen = str.length; i < strLen; i++) {
        bufView[i] = str.charCodeAt(i);
    }
    return buf;
}
