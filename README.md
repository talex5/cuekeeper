CueKeeper
=========

Copyright Thomas Leonard, 2025

CueKeeper is a web-based [GTD][] system (a fancy TODO list) that runs entirely in your browser (the data is stored on your computer, in your browser).

<a href='https://raw.githubusercontent.com/talex5/cuekeeper/gh-bpages/CueKeeper-0.2.png'><img src="https://raw.githubusercontent.com/talex5/cuekeeper/gh-bpages/CueKeeper-0.2.png" width="50%" height="50%"></a>

Installation
------------

Download the latest `cuekeeper-bin` zip from the [releases list][], e.g.

https://github.com/talex5/cuekeeper/releases/download/v0.4/cuekeeper-bin-0.4.zip

Extract it somewhere permanent (not your Downloads folder) and open the `index.html` file inside in a web browser. Most browsers will allow you to "pin" the tab so that it is always available. e.g. In Firefox, right-click on the browser tab and choose "Pin Tab" from the menu. The tab icon will go red when something becomes due.

Instructions for using CueKeeper can be found here:

http://roscidus.com/blog/blog/2015/04/28/cuekeeper-gitting-things-done-in-the-browser/

Upgrading: You can just unpack the new archive over the old one and refresh the browser tab. If you just get a blank window, you probably have another CueKeeper tab open somewhere and will need to close that first so it can update the database schema to the latest version. If upgrading from versions earlier than 0.3, you will need to upgrade to version 0.3 first (it will prompt you to do this if needed).

Chromium note: If it says "The user denied permission to access the database." try turning off `Block third-party cookies` (I don't know why it thinks a local HTML file writing to a local database is a third-party cookie). Also, it seems that all `file://...` pages will see the same database on Chromium, whereas on Firefox each page gets its own one.

Backups
-------

All data will be stored locally in your web browser, so make sure you're backing up your browser's data! Also, the data will be stored based on the location of the `index.html` file - if you move the `cuekeeper` directory, you will get a fresh database (and it will look as if your data has gone - don't panic!).

For example, I use Firefox on Linux. The data is stored at

    ~/.mozilla/firefox/XXX.default/storage/default/file++++home+user+cuekeeper+index.html/


Building (using Docker)
-----------------------

The easiest way to build CueKeeper is using Docker:

    make docker-build

Then load `test.html` in a browser to test locally (no server required).


Building (without Docker)
-------------------------

You'll need the [opam](http://opam.ocaml.org/) package manager.
It should be available through your distribution, but you can use a [generic opam binary](http://tools.ocaml.org/opam.xml) if it's missing or too old (I use opam 2.2.1).
Ensure you're using OCaml 4.14.2 (check with `ocaml -version`). If not, switch to that version:

    opam switch create 4.14.2

Install the dependencies (`-t` includes the test dependencies too):

    opam install --deps-only -t .

Build:

    make

Load `test.html` in a browser to test locally (no server required).

Note that this defaults to "dev" mode, where the Javascript generated will be very large (about 12 MB) and not optimised.
To get a smaller file, use `dune build --profile=release ./js/client.bc.js` (should be about 1.3 MB).


Running a server
----------------

While `test.html` can be opened directly in a browser, as above, you can also build a server.
This allows you to sync between devices (e.g. a laptop and mobile phone).

**Warning: This is a work-in-progress**:

- The server does not yet persist the data itself
  (the client sends the whole history the first time it connects after the service is restarted).
- You have to sync manually by clicking the `Sync` button - it does not send or fetch changes automatically.

First, generate an access token (a *long* random string that grants access to the server).
The `pwgen` command is useful for this:

    $ pwgen -s 32 1
    dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb

To avoid storing the secret itself, generate its SHA256 hash:

    $ echo -n dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb | sha256sum
    774400f3384a6f37cc2bc54b2fd0280193b613a5bc401c0e54fd17fe4ec19572

Create a `devices` file with the hash(es) you generated above, e.g.:

    774400f3384a6f37cc2bc54b2fd0280193b613a5bc401c0e54fd17fe4ec19572 Laptop

The string at the end ("Laptop") is just used for logging.
You can generate a different access token for each device you want to sync and list them all here, one per line.

To build the server component:

    make server

You will be prompted to create a self-signed X.509 certificate. Just enter your server's hostname
as the "Common Name" (for testing, you could use "localhost" here and generate a proper one later).

To run the server:

    dune exec -- cuekeeper --devices=./devices

By default the server listens on TCP port 8443, but this can be changed by editing `server/main.ml`.

Open the URL in a browser, e.g.

    https://localhost:8443/

You'll probably now get some scary-looking warning about the certificate not being trusted.
To get rid of the warning, add your newly-generated server.pem as follows:

In Firefox:

1. Firefox will say "This Connection is Untrusted".
2. Expand the **I Understand the Risks** section.
3. Click **Add Exception**, then **Confirm Security Exception** (and "Permanently store this exception").

In Chrome:

1. It will say "Your connection is not private" (in fact, the opposite is true; if encryption wasn't being used it wouldn't have complained at all).
2. Go to **Settings** -> **Show advanced settings**.
3. Click the **Manage certificates** button (in the HTTPS/SSL section).
4. In the **Authorities** tab, click **Import...** and select your `server/conf/server.pem` file.
5. Select **Trust this certificate for identifying websites**.

Finally, you should be prompted for your access key.
Paste in the token you generated above (e.g. `dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb` in the example above - *not* the hash).


Bugs
----

Please any send questions or comments to the mirage mailing list:

http://lists.xenproject.org/cgi-bin/mailman/listinfo/mirageos-devel

Bugs can be reported on the mailing list or as GitHub issues:

https://github.com/talex5/cuekeeper/issues


Conditions
----------

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
USA


This project includes Foundation (http://foundation.zurb.com). These files
are released under the MIT license.


This project includes the Pikaday date picker (https://github.com/dbushell/Pikaday).
These files are released under the BSD & MIT licenses.


This project includes FileSaver.js (https://github.com/eligrey/FileSaver.js), which
is released under a permissive license.


Full details of all licenses can be found in the LICENSE file.


[GTD]: https://en.wikipedia.org/wiki/Getting_Things_Done
[mirage]: http://openmirage.org/
[releases list]: https://github.com/talex5/cuekeeper/releases
