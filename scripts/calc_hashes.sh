#!/bin/bash

to_bencode() {
  local input="$1"
  local type="$2"
  if [ "$type" = "string" ]; then
    local len=$(echo -n "$input" | wc -c)
    echo "${len}:${input}"
  elif [ "$type" = "integer" ]; then
    echo "i${input}e"
  elif [ "$type" = "dict" ]; then
    echo "d${input}e"
  elif [ "$type" = "list" ]; then
    echo "l${input}e"
  else
    echo "Invalid type: $type" >&2
    return 1
  fi
}

sha1_hash() {
  local input="$1"
  echo -n "$input" | openssl dgst -sha1 | sed 's/^.* //g'
}

calculate_bencode_hash() {
  local bencode_input="$1"
  local encoded_bencode
  local raw_hash
  encoded_bencode=$(to_bencode "$bencode_input" "dict")
  raw_hash=$(sha1_hash "$encoded_bencode")
  echo "$raw_hash"
}

# Calculate hash of empty dictionary:
echo "Hash of empty dictionary:"
calculate_bencode_hash ""
# Calculate hash of dictionary with length zero
echo "Hash of dictionary with length 0:"
calculate_bencode_hash "6:lengthi0e"
# Calculate hash of dictionary with length not zero
echo "Hash of dictionary with length 12345 and name example.txt:"
calculate_bencode_hash "6:lengthi12345e4:name9:example.txt"
echo "Hash of dictionary with non-int length string"
calculate_bencode_hash "6:length9:not an integere"
# Calculate hash of a simple list of integers
echo "Hash of a list of integers [1, 2, 3]:"
calculate_bencode_hash "i1ei2ei3e" #encoded as a dictionary
# Calculate hash of a list of strings
echo "Hash of a list of strings ['abc', 'def']:"
calculate_bencode_hash "3:abc3:def" #encoded as a dictionary
# Calculate hash of a mixed list
echo "Hash of a mixed list [1, 'abc', 2]:"
calculate_bencode_hash "i1e3:abci2e" #encoded as a dictionary

