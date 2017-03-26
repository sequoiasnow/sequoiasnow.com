#!/bin/bash

echo "Finding and deleteing .DS_Store..."

cd "$(dirname "${BASH_SOURCE[0]}")/.."
find . -name '.DS_Store' -type f -delete

echo "Done."
