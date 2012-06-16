#!/usr/bin/env python

""" Generates passwords given an input master password and domain name. By
    default, all outputs are transformed to include punctuation, uppercase
    characters, and digits."""

import scrypt
import base64
import getpass
import string
import argparse

parser = argparse.ArgumentParser(
    description="Generates passwords from a domain name and master password.")
parser.add_argument("--max_length",
  type=int,
  default=10,
  help="Limit on how long the generated password can be")
parser.add_argument('--no_punctuation',
  action='store_true',
  help="Do not include punctuation in the result")
args = parser.parse_args()

password = getpass.getpass()
domain = input("Domain: ")
hashed = str(base64.b64encode(scrypt.hash(password, domain)), "ascii")

def ensure_contains(input, characters):
  """Ensures that 'input' contains at least one character from 'characters'. If
     it does not, adds the first character from 'characters' to the result."""
  if any([c in characters for c in input]): return input
  else: return (characters[0] + input)

def ensure_characters(output):
  """Ensure the output contains lowercase, uppercase, digits, and
     punctuation"""
  output = ensure_contains(output, string.ascii_lowercase)
  output = ensure_contains(output, string.ascii_uppercase)
  output = ensure_contains(output, string.digits)
  return ensure_contains(output, string.punctuation)

def ensure_length(output):
  """Ensures the output is of the correct max_length, even after adding special
     characters"""
  if not args.max_length: return ensure_characters(output)
  length = args.max_length
  result = output
  while len(result) > args.max_length:
    result = ensure_characters(output[:length])
    length -= 1
  return result

def strip_punctuation(output):
  """Replaces punctuation in the input string with the character 'a'"""
  result = ""
  for char in final_password:
    if char in string.punctuation:
      result += 'a'
    else:
      result += char
  return result

final_password = ensure_length(hashed)
if args.no_punctuation:
  final_password = strip_punctuation(final_password)

print(final_password)
