09/08/2021
Je n'arrivais pas conserver les agents en temps que turtles et a leur envoyer des activité en fonction de leurs rôles. En discutant avec Cyril la semaine dernière, il me semble qu'on peut passer par des agents (non visible `hidden = TRUE`).

Il faudra que l'agent turtle demande à l'agent rôle de lui crée une instance avec laquelle il fera un lien. Une fois que l'agent turtle à une instance de son rôle il peut l’appeler pour lui demander de faire les action qu'il à dans `task-stack`. Il s'agira d'ordonnance ensuite les rôle.



05/08/2021
On défit un tick comme une année. Ce qui veux dire que toutes les taches qui sont contenu dans chaque rôle vont se jouer à la suite.
Il faudra peut être penser une manière de les faire jouer de manière aléatoire dans le même tick.

*notes personnelles*
Penser le modèle en essayant de suivre les spec de Jean-Pierre dans son papier n'est pas facile de manière général.
S'il y a des choses qui sont bien plus facile, par exemple ajouter une procedure liée à un rôle, ça complexifie dans ma tête la manière dont on consrtuit les rôles pour que les taches se remttent en place dans la `task-stack`.


#questions

1. combien un foyer est capable de défricher ? Comment caluler la force de travail en fonction de l'espace?
1. Est-ce que la relation sociale est médié par l'espace. C.a.d un agent sais que cette parcelle est à sa famille sans pour autant savoir a qui ?
  - J'ai l'impression que c'est en grande parti le cas, dans ce cas là l'agent va demander dans sa famille à qui appartient la parcelle.
