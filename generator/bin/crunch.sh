#!/bin/bash

for filename in ../data/*.zst ; do
    basename=$(basename -s ".zst" $filename)
    varname=${basename//[-.]/_}
    echo "let $varname = {|"
    unzstd -q --stdout $filename
    echo "|}"
done

echo "let read = function"
for filename in ../data/*.zst ; do
    basename=$(basename -s ".zst" $filename)
    varname=${basename//[-.]/_}
    echo "  | \"$basename\" -> Some ($varname)"
done
echo "  | _ -> None"
