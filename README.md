# honeysql-h2

## Example

```clojure
(require '[honeysql-h2.core :refer :all])

(upsert [{:aa 11 :bb 22}] :table)
;; ["MERGE INTO table (aa, bb) VALUES (?, ?)" 11 22]
(upsert [{:aa 11 :bb 22}] :table :aa)
;; ["MERGE INTO table (aa, bb) KEY(aa) VALUES (?, ?)" 11 22]
(upsert [{:aa 11 :bb 22}] :table [:p :s])
;; ["MERGE INTO table (aa, bb) KEY(p,s) VALUES (?, ?)" 11 22]
```

## License

© 2022 UPD Consulting Ltd

Distributed under the Eclipse Public License, the same as Clojure.
