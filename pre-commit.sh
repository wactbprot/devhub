#!/bin/sh

echo "========= clean up: ========\n"
rm -r target/default/doc/*
rm req-res.png
echo "rm -r target/default/doc/*\n"

echo "=======  graphviz  =========\n\n"
dot -Tpng ./resources/req-res.dot > ./resources/req-res.png
cp ./resources/req-res.png ./docs

echo "=========  codox   =========\n\n"
lein codox
cp -r target/default/doc/* docs/api
