- Mettre l'accent sur la recréation de la sémantique de i-score à partir du graphe: 
 => création d'objets récursivement, etc
 => comment est-ce qu'on peut combiner de la meilleure manière possible l'univers des patchers et des séquenceurs.
 => pour toute contrainte, pour tout scénario, créer noeud qui fait le mixage
 => "cable créé par défaut" quand on rajoute un processus dont on marque l'entrée
 => messages: actuellement "peu" typés ; rajouter type de l'unité ? 
 => pbq du multicanal: pour l'instant non traitée, on ne gère que les cas mono / stereo pour le upmix / downmix
 => states du scénario: comment interviennent-ils ? faire un scénario fantôme *

 => décrire requirements pour qu'un autre système de contraintes temporelles puisse fonctionner 
   => au minimum : enable / disable
   => set_date
   => set_offset pour offset audio (p-ê pas nécessaire si on fait comme LAStream)

Pbq si on fait ça en fonctionnel: construction obligatoire en "top-down" ? on commence par les processus, puis les contraintes, etc. 
sinon on doit parcourir tout pour remplacer ce qu'on veut

- Langage: formalisation par ML ? utilisation par QML ?

- Tête d'un langage "ML" : quelles sont les opérations que l'on fait ? 

add_interval sc itv ev ev -> sc
add_event sc ev sync -> sc
add_sync sc sync -> sc

add_proc itv proc -> itv

tick itv t -> itv * state

tick sc t -> 
