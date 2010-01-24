1) symlink deps/mochiweb-src to your checkout of mochiweb
2) make
3) ./start-dev.sh

listener:

  curl http://localhost:8000/foo

poster:

  curl -d hello http://localhost:8000/foo
