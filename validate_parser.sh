#!/bin/bash

# Simple validation script for cremepat parser syntax
# This checks if the grammar files are syntactically correct

echo "=== CREMEPAT PARSER VALIDATION ==="
echo

echo "Checking Parse.mly syntax..."
if menhir --version > /dev/null 2>&1; then
    echo "✓ Menhir available"
    cd cremepat
    if menhir --only-preprocess Parse.mly > /dev/null 2>&1; then
        echo "✓ Parse.mly syntax is valid"
    else
        echo "✗ Parse.mly has syntax errors"
        menhir --only-preprocess Parse.mly
    fi
    cd ..
else
    echo "⚠ Menhir not available, cannot validate grammar"
fi

echo
echo "Checking ParseTree.ml syntax..."
if ocaml -version > /dev/null 2>&1; then
    echo "✓ OCaml available"
    if ocaml -i cremepat/ParseTree.ml > /dev/null 2>&1; then
        echo "✓ ParseTree.ml syntax is valid"
    else
        echo "✗ ParseTree.ml has syntax errors"
        ocaml -i cremepat/ParseTree.ml
    fi
else
    echo "⚠ OCaml not available, cannot validate syntax"
fi

echo
echo "=== VALIDATION COMPLETE ==="