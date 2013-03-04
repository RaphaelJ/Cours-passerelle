/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux et Jeremy Aussems - Novembre 2012.
 *
 * Ce fichier définit un algorithme pour générer des labyrinthes de 12 cases de
 * coté ainsi que la structure de données associée.
 */

#ifndef LABYRINTH_H
#define LABYRINTH_H

#include <stdio.h>
#include <stdlib.h>

#include <inttypes.h>
#include <stdbool.h>

const int LABYRINTH_SIZE; // Doit être pair

// Chaque case du labyrinthe est représentée par un entier non signé.
// L'entier non signé contient sur ses quatre bits les plus significatifs l'état
// des murs (ouvert = 0/fermé = 1), respectivement: haut, droite, bas et gauche.
// Le bit suivant indique si la cellule est partagée (1) ou pas (0) entre
// plusieurs processus (les cellules partagées sont les cellules limitrophes aux
// murs). Ce bit va permettre à l'algorithme de savoir s'il doit placer un
// verrou lorsqu'il travaille sur cette cellule (car elle peut être modifiée par
// d'autres processus ou non).
// Le reste identifie l'indice de la cellule "parente" du groupe dans le tableau
// du labyrinthe (l'algorithme qui génère le labyrinthe utilise une foret pour
// implémenter une structure d'union-find efficace plutôt qu'un remplissage
// en flood-fill, beaucoup plus lent). Chaque groupe/arbre existant dans le
// labyrinthe est une "couleur" de ce dernier. Chaque groupe est identifié par
// sa racine, que l'on peut retrouver en remontant les indices de chaque 
// cellule.
// Lorsque l'indice est le même que celui de la cellule, alors la cellule est sa
// propre parente, et la racine de son groupe.
typedef uint16_t CELL;
typedef CELL *LABYRINTH;

// Masques utilisés pour accéder aux deux valeurs des cellules.
const CELL WALLS_MASK;
const CELL SHARED_MASK;
const CELL GROUP_MASK;

// Liste les types de murs possibles autour d'une cellule.
typedef CELL WALL;
const WALL WALL_TOP;
const WALL WALL_RIGHT;
const WALL WALL_BOTTOM;
const WALL WALL_LEFT;

// Retourne la cellule de la x ième colonne de la y ième ligne du labyrinthe.
CELL *cell_index(LABYRINTH labyrinth, int x, int y);

// Retourne true si le mur spécifié existe pour la cellule, false s'il est
// ouvert (= n'existe pas).
bool is_wall(CELL cell, WALL wall_type);

// Ferme le mur donné d'une cellule. Ne fait rien si le mur existe déjà.
void close_wall(CELL *cell, WALL wall_type);

// Ouvre le mur donné d'une cellule. Ne fait rien si le mur n'existe pas.
void open_wall(CELL *cell, WALL wall_type);

// Retourne true si la cellule est limitrophe d'un des murs
bool is_shared(CELL cell);

// Change l'état de partage de la cellule en la définissant comme une cellule 
// partagée.
void set_shared(CELL *cell);

// Retourne l'indice du parent de la cellule.
CELL get_parent_index(CELL cell);

// Modifie les bits d'index du parent de la cellule.
void set_parent_index(CELL *cell, CELL parent_index);

// Retourne la cellule à la racine du groupe à laquelle appartient la cellule.
// Cette opération s'exécute en temps quasiment constant.
CELL *cell_root(LABYRINTH labyrinth, CELL *cell);

// Attache la cellule source au groupe de la cellule de destination.
// Cette opération s'exécute en temps constant quel que soit la hauteur des
// arbres des deux groupes.
void cell_attach_group(CELL *src, CELL dst);

// Génère un labyrinthe où toutes les cellules sont entièrement fermées.
// Chaque cellule s'est vue attribuée une couleur différente.
void init_labyrinth(LABYRINTH labyrinth);

// Structure permettant de récupérer les performances de la parallélisation
// lors de l'exécution de l'algorithme de génération du labyrinthe.
// hits contient le nombre de murs supprimés sans blocage global du labyrinthe
// et misses le nombre supprimés avec un blocage complet.
typedef struct {
    int hits, misses;
} PARAL_STATS;

// Génère un labyrinthe parfait de manière aléatoire sur quatre processus
// indépendants. La fonction peut fournir les statistiques de la
// parallélisation si le pointeur stats est non nul.
LABYRINTH gen_labyrinth(PARAL_STATS *stats);

// Affiche un labyrinthe sur la sortie standard.
void show_labyrinth(const LABYRINTH labyrinth);
#endif
