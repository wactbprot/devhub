#!/bin/sh

echo "========= clean up: ========\n"
rm -r target/default/doc/*
echo "rm -r target/default/doc/*\n"

echo "=======  graphviz  =========\n\n"
dot -Tsvg ./docs/req-res.dot > ./docs/req-res.svg
echo "dot -Tsvg ./docs/req-res.dot > ./docs/req-res.svg"

echo "=========  codox   =========\n\n"
lein codox
cp -r target/default/doc/* docs/api
