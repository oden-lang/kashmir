#!/bin/bash
prg=$0
command=$1

if [ -z $command ]; then
  echo "Usage: $prg (validate|save)"
  exit 1
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ODEN=$DIR/../dist/build/cli/cli
if ! hash $ODEN ; then
  ODEN=$DIR/../dist/oden/bin/oden
fi

cd $DIR
tests=$(find src -name *.oden)

print_success() {
  echo -e "\033[1;32m$*\033[0m"
}

print_err() {
  echo -e "\033[1;4;31m$*\033[0m"
}

for test in $tests; do
  tmp_oden_path=$(mktemp -d -t oden.XXXXXXXXXX)
  tmp_go_path=$(mktemp -d -t oden_out.XXXXXXXXXX)
  tmp_src_file=$tmp_oden_path/$test

  mkdir -p $(dirname $tmp_src_file)

  cp $test $tmp_src_file
  oden_out=$($ODEN -p$tmp_oden_path -o$tmp_go_path build 2>&1)
  oden_return=$?

  go_out_file=$()$(mktemp -t go_out.XXXXXXXXXX)
  GOPATH=$tmp_go_path go run $(find $tmp_go_path -name *.go) > $go_out_file 2>&1
  go_return=$?

  if [ ! -f "$test.expected.txt" ]; then
    if [[ $command == "validate" ]]; then
      print_err "✗ $test"
      echo "Missing file $test.expected.txt!"
      echo "Run \"$prg save\" to generate such a file."
      echo ""
      continue
    fi
  fi

  if [[ $oden_return -ne 0 ]]; then
    print_err "✗ $test"
    echo "$oden_out"
    echo ""
  elif [[ $go_return -ne 0 ]]; then
    print_err "✗ $test"
    cat $(find $tmp_go_path -name *.go)
    echo ""
    cat $go_out_file
    echo ""
  else
    expected_out_file="$test.expected.txt"
    if [[ $command == "save" ]]; then
      print_success "✓ $test"
      echo -e "\033[35mSaving results.\033[0m"
      echo -e $go_out_file > "$test.expected.txt"
    elif [[ $command == "validate" ]]; then
      if diff $go_out_file $expected_out_file ; then
        print_success "✓ $test"
      else
        print_err "✗ $test"
      fi
    fi
  fi

  rm -r $tmp_oden_path
  rm -r $tmp_go_path
done
