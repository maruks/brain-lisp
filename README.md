# Brain cellular automaton

https://en.wikipedia.org/wiki/Brian%27s_Brain

## Start

   make start

## Build executable

    make

## Docker image

    docker build -t brain .
    docker run --name brain --tty --network net -d brain
    docker stop brain
    docker start brain
