# Makefile push changes out to github

default:
	make clean
	git add --all
	git commit -m "Remote update"
	git push -u origin main

local:
	make clean
	git add --all
	git commit -m "Local update"

clean:
	rm -rf src/*.o
