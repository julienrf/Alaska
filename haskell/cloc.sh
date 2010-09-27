#!/bin/sh

echo "Décompte vitef' des lignes de codes dans le dossier src"


# on prend les lignes non vides, on enlève les lignes de commentaire,
# on compte

grep -v -e "^ *$" src/*.hs | grep -v -e "^--" | wc -l