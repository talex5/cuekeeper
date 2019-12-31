## v0.3

- CueKeeper now stores its data in standard Git format, rather than the old
  Irmin 0.10 custom format. This will (finally) allow upgrading to newer
  versions of Irmin in the future.

  When upgrading from an older release, you will see a migration box on the
  first run, showing the progress of the migration. On my machine, converting
  my CueKeeper history from 2015-03-09 to now (15,193 commits) took about 20
  minutes in Firefox.

- Open all links in a new window. Mac users can't just middle-click links,
  because they only have one mouse button. If you don't like this behaviour,
  just edit the HTML file and remove the `<base>` element.

- Convert build system to dune and upgrade to newer versions of various
  packages.
