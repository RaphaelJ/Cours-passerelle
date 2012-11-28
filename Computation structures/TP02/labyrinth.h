/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux - Novembre 2012.
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

const int LABYRINTH_SIZE;

// Chaque case du labyrinthe est représentée par un entier non signé indiquant
// sa couleur et l'état (ouvert/fermé) des quatre murs entourant celle-ci.
// Les quatre bits les plus significatifs encodent les quatre murs (
// respectivement: haut, droite, bas et gauche), le reste identifie la couleur.
// Chaque bit des murs vaut 1 si le mur existe, 0 s'il est ouvert.
typedef uint32_t CELL;
typedef CELL *LABYRINTH;

// Masques utilisés pour accéder aux deux valeurs des cellules.
const uint32_t COLOR_MASK;
const uint32_t WALLS_MASK;

// Liste les murs possibles autour d'une cellule.
typedef uint32_t WALL;
const WALL WALL_TOP;
const WALL WALL_RIGHT;
const WALL WALL_BOTTOM;
const WALL WALL_LEFT;

// Retourne l'entier identifiant la couleur de la cellule.
uint32_t cell_color(CELL cell);

// Fixe la couleur d'une cellule.
void set_cell_color(CELL *cell, uint32_t color);

// Retourne true si le mur spécifié existe pour la cellule, false s'il est
// ouvert (== n'existe pas).
bool is_wall(CELL cell, WALL wall_type);

// Ferme le mur donné d'une cellule. Ne fait rien si le mur existe déjà.
void set_wall(CELL *cell, WALL wall_type);

// Ouvre le mur donné d'une cellule. Ne fait rien si le mur n'existe pas.
void remove_wall(CELL *cell, WALL wall_type);

// Génère un labyrinthe où toutes les cellules sont entièrement fermées.
// Chaque cellule s'est vue attribuée une couleur différente.
LABYRINTH init_labyrinth(void);

// Affiche un labyrinthe sur la sortie standard.
void show_labyrinth(const LABYRINTH labyrinth);
#endif
