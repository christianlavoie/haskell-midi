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
    <script src="/haste.js"></script>
    <script>
        function fullscreen() {
            var w = $(window).width();
            var h = $(window).height();

            $("#drawArea").css("width", w + "px");
            $("#drawArea").css("height", h + "px"); 
        }

        $(window).bind("resize", fullscreen);
        $(document).bind("ready", fullscreen);
    </script>
  </head>
  <body>
  <canvas id="drawArea" width="640" height="480"></canvas>
  </body>
</html>
