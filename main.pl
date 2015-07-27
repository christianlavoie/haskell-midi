#!/usr/bin/env perl

use strict;
use warnings;

use local::lib './ext';

use Mojolicious::Lite;

# Template with browser-side code
get '/' => 'index';

app->start;
__DATA__

@@ index.html.ep
<!DOCTYPE html>
<html>
  <head>
    <title>Echo</title>
    <link rel="stylesheet" href="/pure-min.css">
    <script src="/zepto.min.js"></script>
    <script>
      var ws = new WebSocket('ws://10.0.0.2:30001/ws');

      // Incoming messages
      ws.onmessage = function (event) {
        document.body.innerHTML += event.data + '<br/>';
      };

      // Outgoing messages
      ws.onopen = function (event) {
        document.body.innerHTML += 'Socket opened <br/>';
      };
    </script>
  </head>
  <body>
  </body>
</html>
