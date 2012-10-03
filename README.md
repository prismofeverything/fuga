# fuga

This is a library to read notes from a midi file and translate them into a form suitable for analysis.  

Also included are the midi files for books I and II from Bach's Well-Tempered Clavier.

## Usage

```clj
(require '[fuga.core :as fuga])
(def midi (fuga/read-midi "path/to/midi/file"))
(def tracks (fuga/extract-tracks midi))
(def notes (fuga/filter-notes tracks))
(def processed (fuga/process-notes notes))
```

## License

Copyright © 2012 Ryan Spangler

Distributed under the Eclipse Public License, the same as Clojure.
