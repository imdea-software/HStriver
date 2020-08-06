#!/bin/bash

if [ $# -eq 0 ]; then
  echo "Running all the examples. Alternatively, you can use:"
  echo "  $ docker run imdeasoftware/powertrain [1-8]"
  echo "to run a specific example."
  for i in {1..8}
    do
      echo "Property $i:"
      HStriver --run "$i"
    done
    exit 0
fi

HStriver --run "$@"
