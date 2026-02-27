// Node.js runner for the Wire3 test harness.
// Bridges Elm ports to stdin/stdout for the Rust test runner.

const { Elm } = require('./harness.js');

const app = Elm.Harness.init();

// Elm → stdout
app.ports.responsePort.subscribe((response) => {
    process.stdout.write(response + '\n');
});

// stdin → Elm (line-buffered)
let buffer = '';
process.stdin.setEncoding('utf8');
process.stdin.on('data', (chunk) => {
    buffer += chunk;
    let lines = buffer.split('\n');
    // Keep the last incomplete line in the buffer
    buffer = lines.pop();
    for (const line of lines) {
        if (line.trim()) {
            app.ports.requestPort.send(line.trim());
        }
    }
});

process.stdin.on('end', () => {
    if (buffer.trim()) {
        app.ports.requestPort.send(buffer.trim());
    }
    // Give Elm a tick to process the last message
    setTimeout(() => process.exit(0), 100);
});
