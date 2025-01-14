'use strict';

const http = require('http');
const https = require('https');
const express = require('express');
const expressStaticGzip = require('express-static-gzip');
const bodyparser = require('body-parser');
const path = require('path');
const helmet = require('helmet');
const compression = require('compression');
const fs = require('fs'); // Needed to save and read files from the system
const crypto = require('crypto'); // Needed for hashing the sent code
const bodyParser = require('body-parser');
const RateLimit = require('express-rate-limit'); // This module helps limiting excessive access to the APIs
const config = require(__dirname + '/../config.js');
const cors = require('cors');
// const QRCode = require('../SOSML-frontend/frontend/src/components/QRCode');

var limiter = new RateLimit(config.shareLimits);

const server = express();
server.enable('trust proxy');
server.use(cors());
// TODO: reenable helmet but allows prog2.de, uni saarland domains
// server.use(helmet())
server.use(compression())
server.use(bodyParser.json({ limit: '5mb' }));
server.use('/static/', expressStaticGzip('SOSML-frontend/frontend/build/static', {
    enableBrotli: true,
    orderPreference: ['br', 'gz'],
    setHeaders: function (res, path) {
        res.setHeader("Cache-Control", "public, max-age=31536000");
    }
}));
// server.use('/share/', express.static('shares'));
// server.use('/code/', express.static('code'));

function readFile(name, callback) {
    fs.readFile(name, 'utf8', function (err, data) {
        if (err) {
            return console.log(err);
        }
        callback(data);
    });
}

function outputFile(name, response) {
    readFile(name, function (data) {
        response.set('Content-Type', 'text/plain');
        response.end(data);
    });
}

function listDir(dir, prefix, response) {
    var results = [];
    fs.readdir(dir, function (err, items) {
        if (err) {
            response(err);
        }
        var pending = items.length;
        if (!pending) {
            response(null, results);
        }
        items.forEach(function (file) {
            file = path.resolve(dir, file);
            fs.stat(file, function (err, stat) {
                if (stat && stat.isDirectory()) {
                    listDir(file, prefix, function (err, res) {
                        results = results.concat(res);
                        if (!--pending) {
                            response(null, results);
                        }
                    });
                } else {
                    results.push(file.substr(prefix.length));
                    if (!--pending) {
                        response(null, results);
                    }
                }
            });
        });
    });
}

// The first server call that matches will be executed
// In this path handles the uploaded code. The 'limiter' limits the how many API accesses per minute can be done per IP
server.put('/api/share/', limiter,
    // 'request' is the from the client sent http request
    // 'response' is the answer from this server, which is still to be edited
    // 'next' can be called so the rest of the function is skipped
    function (request, response, next) {
        // making sure sharing is enabled
        if (config.serveSharing) {
            const payload = request.body.code;
            // The sent code is hashed, so it can get stored on the server
            const hash = crypto.createHash('sha256').update(payload).digest("hex");
            if (fs.existsSync(config.sharePath + hash + ".sml")) {
                response.set('Content-Type', 'text/plain');
                response.end(hash);
                return;
            }
            fs.writeFile(config.sharePath + hash + ".sml", payload, function (err) {
                if (err) {
                    return console.log(err);
                }
                console.log("A file with hash " + hash + " was saved");
                response.set('Content-Type', 'text/plain');
                response.end(hash);
            });
        } else {
            next();
            return;
        }
    }
);

server.put('/api/wishare/', limiter,
    // 'request' is the from the client sent http request
    // 'response' is the answer from this server, which is still to be edited
    // 'next' can be called so the rest of the function is skipped
    function (request, response, next) {
        // making sure sharing is enabled
        if (config.serveSharing) {
            const payload = request.body.code;
            // The sent code is hashed, so it can get stored on the server
            const hash = crypto.createHash('sha256').update(payload).digest("hex");
            if (fs.existsSync(config.wisharePath + hash + ".wish..json")) {
                response.set('Content-Type', 'text/plain');
                response.end(hash);
                return;
            }
            fs.writeFile(config.wisharePath + hash + ".wish.json", payload, function (err) {
                if (err) {
                    return console.log(err);
                }
                console.log("A wish with hash " + hash + " was saved");
                response.set('Content-Type', 'text/plain');
                response.end(hash);
            });
        } else {
            next();
            return;
        }
    }
);

server.get('/api/share/:code',
    function (request, response, next) {
        if (config.serveSharing) {
            const code = request.params.code;
            if (/^[\d\w]+$/g.test(code)) {
                outputFile(config.sharePath + code + ".sml", response);
            } else {
                response.sendStatus(400);
            }
        } else {
            next();
            return;
        }
    }
);

server.get('/api/wishare/:code',
    function (request, response, next) {
        if (config.serveSharing) {
            const code = request.params.code;
            if (/^[\d\w]+$/g.test(code)) {
                outputFile(config.wisharePath + code + ".wish.json", response);
            } else {
                response.sendStatus(400);
            }
        } else {
            next();
            return;
        }
    }
);

server.get('/api/list/',
    function (request, response, next) {
        if (config.serveExamples) {
            listDir(config.examplePath, path.resolve(config.examplePath, '.') + '/',
                function (err, results) {
                    if (err) {
                        console.log(err);
                    }
                    response.set('Content-Type', 'text/json');
                    response.end(JSON.stringify({ codes: results }));
                });
        } else {
            next();
            return;
        }
    }
);

server.get('/api/wishlist/',
    function (request, response, next) {
        if (config.serveWishExamples) {
            listDir(config.wishPath, path.resolve(config.wishPath, '.') + '/',
                function (err, results) {
                    if (err) {
                        console.log(err);
                    }
                    response.set('Content-Type', 'text/json');
                    response.end(JSON.stringify({ codes: results }));
                });
        } else {
            next();
            return;
        }
    }
);

server.get('/api/code/:code',
    function (request, response, next) {
        if (config.serveExamples) {
            const code = request.params.code;
            console.log('Trying to read file ' + request.params.code);
            if (/^[\d\w](\/[\d\w\%]+|.[\d\w\%]+|[\d\w\%])*$/g.test(code)) {
                outputFile(config.examplePath + code, response);
            } else {
                response.sendStatus(400);
            }
        } else {
            next();
            return;
        }
    }
);

server.get('/api/wish/:code',
    function (request, response, next) {
        if (config.serveWishExamples) {
            const code = request.params.code;
            console.log('Trying to read wish ' + request.params.code);
            if (/^[\d\w](\/[\d\w\%]+|.[\d\w\%]+|[\d\w\%])*$/g.test(code)) {
                outputFile(config.wishPath + code + ".wish.json", response);
            } else {
                response.sendStatus(400);
            }
        } else {
            next();
            return;
        }
    }
);


server.use('/api',
    function (request, response) {
        response.sendStatus(404);
    }
)

server.get('/webworker.js', function (request, response, next) {
    if (config.serveFrontend) {
        response.sendFile(path.resolve(config.frontendPath + '/webworker.js'));
    } else {
        next();
        return;
    }
});

server.get('/toplevel.js', function (request, response, next) {
    if (config.serveFrontend) {
        response.sendFile(path.resolve(config.frontendPath + '/toplevel.js'));
    } else {
        next();
        return;
    }
});

server.get('/logo.png', function (request, response, next) {
    if (config.serveFrontend) {
        response.sendFile(path.resolve(config.frontendPath + '/logo.png'));
    } else {
        next();
        return;
    }
});

server.get('/favicon.png', function (request, response, next) {
    if (config.serveFrontend) {
        response.sendFile(path.resolve(config.frontendPath + '/favicon.png'));
    } else {
        next();
        return;
    }
});

server.get('/', function (request, response, next) {
    if (config.serveFrontend) {
        response.sendFile(path.resolve(config.frontendPath + '/index.html'));
    } else {
        next();
        return;
    }
});

server.use(function (request, response, next) {
    if (config.serveFrontend) {
        response.sendFile(path.resolve(config.frontendPath + '/index.html'));
    } else {
        next();
        return;
    }
});


const domain = 'v2202201167307177506.ultrasrv.de';
const privateKey = fs.readFileSync(`/etc/letsencrypt/live/${domain}/privkey.pem`, 'utf8');
const certificate = fs.readFileSync(`/etc/letsencrypt/live/${domain}/cert.pem`, 'utf8');
const ca = fs.readFileSync(`/etc/letsencrypt/live/${domain}/chain.pem`, 'utf8');

const credentials = {
    key: privateKey,
    cert: certificate,
    ca: ca
};


// server.listen(config.port, function () {
//     console.log('==== Server started ==== Port ' + config.port);
// });
const httpServer = http.createServer(server);
const httpsServer = https.createServer(credentials, server);

httpServer.listen(80, () => {
    console.log('HTTP Server running on port 80');
});

httpsServer.listen(443, () => {
    console.log('HTTPS Server running on port 443');
});
