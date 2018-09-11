aligned.html: aligned.md
	pandoc aligned.md --read=markdown --write=html5 -o aligned.html

.PHONY: deploy
deploy: aligned.html
	./deploy.sh
