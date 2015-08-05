CueKeeper
=========

Copyright Thomas Leonard, 2015


Installation
------------

You'll need the [opam](http://opam.ocaml.org/) package manager.
It should be available through your distribution, but you can use a [generic opam binary](http://tools.ocaml.org/opam.xml) if it's missing or too old (I use opam 1.2).
Ensure you're using OCaml 4.01 or later (check with `ocaml -version`).
If not, switch to 4.01.0 or later:

    opam sw 4.01.0

Pin a few patches we require:

    opam pin add -n sexplib 'https://github.com/talex5/sexplib.git#js_of_ocaml'
    opam pin add -n reactiveData https://github.com/hhugo/reactiveData.git
    opam pin add -n bin_prot 'https://github.com/talex5/bin_prot.git#js_of_ocaml'
    opam pin add -n dolog 'https://github.com/UnixJunkie/dolog.git#no_unix'

    opam update

Install the dependencies:

    opam install sexplib uuidm irmin tyxml reactiveData js_of_ocaml omd base64 tar-format crunch cohttp irmin-indexeddb ounit

Build:

    make

Load `test.html` in a browser to test locally (no server required).


Instructions
------------

Instructions for using CueKeeper can be found here:

http://roscidus.com/blog/blog/2015/04/28/cuekeeper-gitting-things-done-in-the-browser/


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

To avoid storing the secret in the server binary, generate its SHA256 hash:

    $ echo -n dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb | sha256sum
    774400f3384a6f37cc2bc54b2fd0280193b613a5bc401c0e54fd17fe4ec19572

Copy the file `server/devices.ml.example` as `server/devices.ml` and add the hash
you generated above, e.g.:

    let lookup = function
      | "774400f3384a6f37cc2bc54b2fd0280193b613a5bc401c0e54fd17fe4ec19572" -> Some "Laptop"
      | _ -> None

The string at the end ("Laptop") is just used for logging.
You can generate a different access token for each device you want to sync and list them all here, one per line.
Make sure the `None` line comes last - this rejects all unknown tokens.

To build the server component:

    opam install mirage
    make server

You will be prompted to create a self-signed X.509 certificate. Just enter your server's hostname
as the "Common Name" (for testing, you could use "localhost" here and generate a proper one later).

To run the server:

    ./server/mir-cuekeeper

By default the server listens on TCP port 8443, but this can be changed by editing `server/unikernel.ml`.

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
4. In the **Authorities** tab, click **Import...** and select your `server/conf/tls/server.pem` file.
5. Select **Trust this certificate for identifying websites**.

Finally, you should be prompted for your access key.
Paste in the token you generated above (e.g. `dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb` in the example above - *not* the hash).

Deploying as a Xen VM
---------------------

In fact, the server is a [Mirage unikernel][mirage] and can also be compiled and booted as a Xen virtual machine:

    opam pin add mirage-xen-posix 'https://github.com/talex5/mirage-platform.git#times'
    make server MIRAGE_FLAGS="--xen"
    cd server
    xl create -c cuekeeper.xl


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


[mirage]: http://openmirage.org/
