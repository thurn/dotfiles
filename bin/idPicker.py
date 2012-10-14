#!/usr/bin/env python3
import sys

if len(sys.argv) <= 2:
  print("Usage: idPicker.py INDEX WINDOW_IDS")
  exit()

numbers = [int(x) for x in sys.argv[2].split(" ")]
print(sorted(numbers)[int(sys.argv[1])])
