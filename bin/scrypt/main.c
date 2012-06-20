/*-
 * Copyright 2009 Colin Percival
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "scrypt_platform.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "readpass.h"
#include "scryptenc.h"
#include "warn.h"

static void
usage(void)
{

	fprintf(stderr, "usage: scrypt {domain} [--no_punctuation]\n");
	exit(1);
}

unsigned char
remove_punctuation(unsigned char c) {
  if (c >= 33 && c <= 47) {
    return c + 33;
  } else if (c >= 58 && c <= 64) {
    return c + 10;
  } else if (c >= 91 && c <=96) {
    return c - 10;
  } else if (c >= 123) {
    return c - 10;
  } else {
    return c;
  }
}

void
print_it(unsigned char* buffer, int allow_punctuation) {
  int i;
  for (i = 0; i < 10; ++i) {
    unsigned char c = buffer[i];
    if (c > 126) {
      c -= 126;
    }
    if (c > 126) {
      c -= 126;
    }
    if (c < 33) {
      c += 33;
    }
    if (!allow_punctuation) {
      c = remove_punctuation(c);
    }
    printf("%c", c);
  }
}

int
main(int argc, char* argv[])
{
	char * passwd;
  uint64_t N = 1 << 14;
  uint32_t r = 8;
  uint32_t p = 1;
  char buf[65];
  uint8_t buflen = 64;
  int result;
  int i = 0;
  int punctuation = 1;

  if (argc != 2) {
    usage();
  }

  // Disable punctuation in output for sites that are jerks
  if (strcmp(argv[1], "chase.com") == 0) {
    punctuation = 0;
  }

	/* Prompt for a password. */
	if (tarsnap_readpass(&passwd, "Please enter passphrase", NULL, 1)) {
		exit(1);
  }

  result = crypto_scrypt(passwd, strlen(passwd), argv[1], strlen(argv[1]), N, r,
                         p, buf, buflen);
  buf[64] = 0;

  if (result != 0) {
    fprintf(stderr, "Something went wrong!\n");
    exit(1);
  } else {
    print_it(buf, punctuation);
    printf("\n");
  }

	/* Zero and free the password. */
	memset(passwd, 0, strlen(passwd));
	free(passwd);

  return 0;
}
