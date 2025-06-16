# Forest Fire Simulation
Ce projet simule la propagation des feux de forêt à l’aide d’un modèle de type automate cellulaire et propose des outils de visualisation pour analyser les résultats.

## Contenu
- **Scala** : génération des données de simulation (`simulation.json` et `visu_simulation.json`).
- **Python** : visualisation des résultats sous forme de heatmap et de courbes.
- **HTML/JS** : visualisation interactive et animée des simulations.

## Génération des données
Le fichier `ForestFire.scala` génère :
- `simulation.json` : sans données étape par étape, permet de réaliser plus de simulations rapidement.
- `visu_simulation.json` : contient les données complètes pour l’animation des étapes.
  Commande :
```bash
sbt run
```

**Paramètres principaux :**
* `treeDensity` : densité initiale des arbres
* `waterDensity` : densité initiale de l’eau
* `fireStartChance` : probabilité qu’un arbre soit en feu au début de la simulation
* `treePropagationChance` : probabilité qu’un arbre pousse à côté d’un autre
* `firePropagationChance` : probabilité que le feu se propage
* `nbrStep` : nombre d’étapes simulées

## Visualisation Python
Le script Python (Jupyter Notebook `plots.ipynb`) affiche :
* Une heatmap des surfaces brûlées selon les paramètres.
* Des courbes du ratio brûlé en fonction des probabilités.

## Visualisation interactive (HTML + JS)
Ouvrir `index.html`.
Fonctionnalités :
* Sélection de la simulation (`visu_simulation.json`).
* Animation des étapes.
* Contrôles : Pause / Play / Step +/-.
* Graphe du ratio brûlé au fil du temps.

## Vidéo de présentation