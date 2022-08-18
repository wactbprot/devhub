#!/bin/sh

echo "========= coverage =========\n"
clojure -M:dev:coverage

echo "=========  codox   =========\n"
clojure -X:dev:codox
