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

    opam pin add sexplib 'https://github.com/talex5/sexplib.git#js_of_ocaml'
    opam pin add irmin 'https://github.com/mirage/irmin.git'
    opam pin add reactiveData https://github.com/hhugo/reactiveData.git
    opam pin add bin_prot 'https://github.com/talex5/bin_prot.git#js_of_ocaml'

Install the dependencies:

    opam install sexplib uuidm irmin tyxml reactiveData js_of_ocaml omd base64 tar-format crunch

Build:

    make

Load `test.html` in a browser to test locally (no server required).


Instructions
------------

Instructions for using CueKeeper can be found here:

http://roscidus.com/blog/blog/2015/04/28/cuekeeper-gitting-things-done-in-the-browser/


Server
------

While `test.html` can be opened directly in a browser, as above, you can also build a server to host the files:

    opam install mirage
    make server

To run the server:

    ./server/mir-cuekeeper

By default the server listens on TCP port 8080, but this can be changed by editing `server/config.ml`.

In fact, the server is a [Mirage unikernel][mirage] and can also be compiled and booted as a Xen virtual machine:

    make server MIRAGE_FLAGS="--xen"
    cd server
    xl create -c cuekeeper.xl

Note that currently the server simply serves up the static resources (the HTML, CSS, Javascript files),
with the data still being stored only in the browser.
In future, the browser will sync with the server, allowing use from multiple devices.

Tip: if you make changes to the server code, you only need to run make in the `server` directory.
However, if you change the client you must run `make server` in the top-level directory to update `server/static.ml`.


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
