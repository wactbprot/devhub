#!/bin/sh

echo "========= clean up: ========\n"
rm -r target/default/doc/*
echo "rm -r target/default/doc/*\n"

echo "=========  codox   =========\n\n"
lein codox
cp -r target/default/doc/* docs/api
