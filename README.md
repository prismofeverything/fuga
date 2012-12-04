# fuga

This is a library to read notes from a midi file, analyze them and use the result of that analysis to generate new sequences of notes!

Also included are the midi files for books I and II from Bach's Well-Tempered Clavier.

## Usage

To hear notes:

```clj
(require '[fuga.core :as fuga])
(def notes (fuga/full-circle))
```

To process notes:

```clj
(def midi (fuga/read-midi "path/to/midi/file"))
(def tracks (fuga/extract-tracks midi))
(def notes (fuga/filter-notes tracks))
(def processed (fuga/process-notes notes))
```

## License

Copyright © 2012 Ryan Spangler

Distributed under the Eclipse Public License, the same as Clojure.
