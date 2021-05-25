#!/bin/sh

echo "========= clean up: ========\n"
rm -r target/default/doc/*
echo "rm -r target/default/doc/*\n"

echo "========= coverage =========\n"
clojure -M:dev:coverage
cp -r target/coverage/* docs/coverage
echo "cp -r target/coverage/* docs/coverage\n"

echo "=========  codox   =========\n"
clojure -X:dev:codox
cp -r target/default/doc/* docs/api
echo "cp -r target/default/doc/* docs/api\n"
