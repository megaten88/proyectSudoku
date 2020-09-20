# Sudoku en Haskell

## Integrantes: 
* Andrés Elías Cruz Banegas - 11811228
* José Daniel Rodríguez Cardona - 11811379
* Rafael Eduardo Flores Cáceres - 31711187
* Mayra Alejandra Salazar Rodríguez - 11511306


## Dependencies
- Para el UI se necesitan los paquetes de GTK3
```
sudo add-apt-repository ppa:gnome3-team/gnome3-staging
sudo apt-get update
sudo apt-get install libgtk-3-dev libgirepository1.0-dev libcairo2-dev libgdk-pixbuf2.0-dev
```


- Para otras dependencias dentro del juego
```
cabal update
cabal install http-client
cabal install http-conduit
cabal install alex happy
cabal install hxt
```

