# Brain cellular automaton

https://en.wikipedia.org/wiki/Brian%27s_Brain

## Add project to ASDF

    cd ~/.roswell/local-projects
    ln -s ~/Projects/Lisp local

## Start

   ./brain.ros

## Build executable

    ros dump executable brain.ros

## Docker image

    docker build -t brain .
    docker run --name brain --tty --network net -d brain
    docker stop brain
    docker start brain
