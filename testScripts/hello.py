import datetime
import time
import sys

while 1:
	now = datetime.datetime.now()
	print(now.isoformat())
	sys.stdout.flush()
	time.sleep(5)
