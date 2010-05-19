#!/usr/bin/env python

# Note: this script totally sucks, but it works (for the most part)

def main ():
	import subprocess
	import time
	try:
		last = None
		while True:
			output = subprocess.Popen('ls -lctTR src/'.split(),
				stdout=subprocess.PIPE).stdout.read()
			if last is None:
				last = output
			if output != last:
				print 'Files under src/ have changed, reloading...'
				print subprocess.Popen('rake build'.split(),
					stdout=subprocess.PIPE).stdout.read()
				last = output
			time.sleep(0.1)
	except KeyboardInterrupt:
		pass

if __name__ == '__main__':
	main()
