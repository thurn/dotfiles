package ca.thurn.passwordmachine;

import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;

import com.lambdaworks.crypto.SCrypt;

public class Main {

	private static final int SCRYPT_LEN = 64;
	private static final int P = 1;
	private static final int R = 8;
	private static final int N = 1 << 14;
	private static final int DEFAULT_LENGTH = 10;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		boolean allowPunctuation = true;
		boolean addBang = false;
		boolean addFive = false;
		int length = DEFAULT_LENGTH;

		if (args.length < 1) {
			usage();
		}

		// Disable punctuation in output for sites that are jerks
		if (args[0].equals("chase.com") ||
        args[0].equals("fandango.com") ||
        args[0].equals("verizonwireless.com") ||
				args.length == 2) {
			allowPunctuation = false;
		} else if (args[0].equals("kaiserpermanente.org")) {
			addBang = true;
		} else if (args[0].equals("schwab.com")) {
			// Schwab may have the stupidest password requirements I've ever
			// encountered.
			length = 8;
			addFive = true;
			allowPunctuation = false;
		} else if (args[0].equals("deltadentalins.com")) {
			addFive = true;
		}
		
		String password = null;
		if (System.console() != null) {
			password = new String(System.console().readPassword("Please enter passphrase: "));
		} else {
			System.err.println("No system console allocated.");
			Runtime.getRuntime().halt(1);
		}

		try {
			byte[] result = SCrypt.scrypt(password.getBytes(),
					args[0].getBytes(), N, R, P, SCRYPT_LEN);
			printIt(new String(result, "ISO-8859-1").toCharArray(), length,
					allowPunctuation, addBang, addFive);
		} catch (GeneralSecurityException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		System.out.print("\n");
	}

	private static void usage() {
		System.err.println("usage: pwmachine {domain} [--no_punctuation]\n");
		System.exit(42);
	}

	private static void printIt(char[] buffer, int length,
			boolean allowPunctuation, boolean addBang, boolean addFive) {
		for (int i = 0; i < length; ++i) {
			char c = buffer[i];
			if (c > 126) {
				c -= 126;
			}
			if (c > 126) {
				c -= 126;
			}
			if (c < 33) {
				c += 33;
			}
			if (!allowPunctuation) {
				c = removePunctuation(c);
			}
			if (addBang && i == 0) {
				System.out.printf("!");
			} else if (addFive && i == 5) {
				System.out.printf("5");
			} else {
				System.out.printf("%c", c);
			}
		}
	}
	
	static char removePunctuation(char character) {
	      int c = (int)character;
		  if (c >= 33 && c <= 47) {
		    return (char)(c + 33);
		  } else if (c >= 58 && c <= 64) {
		    return (char)(c + 10);
		  } else if (c >= 91 && c <=96) {
		    return (char)(c - 10);
		  } else if (c >= 123) {
		    return (char)(c - 10);
		  } else {
		    return (char)c;
		  }
		}

}
