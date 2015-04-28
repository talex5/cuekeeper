CueKeeper
=========

Copyright Thomas Leonard, 2015


Installation
------------

You'll need the [opam](http://opam.ocaml.org/) package manager (should be
available through your distribution).
Ensure you're using OCaml 4.01 or later:

    opam sw 4.01.0

Pin a few patches we require:

    opam pin add sexplib 'https://github.com/talex5/sexplib.git#js_of_ocaml'
    opam pin add irmin 'https://github.com/talex5/irmin.git#js-sha'
    opam pin add reactiveData https://github.com/hhugo/reactiveData.git

Install the dependencies:

    opam install sexplib uuidm irmin tyxml reactiveData js_of_ocaml omd base64 tar-format crunch

Build:

    make

Load `test.html` in a browser to test locally (no server required).


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
