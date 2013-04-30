<?php
if (!defined("IN_INDEX"))
    die();
?>

<div class="well">

<?php

if (isset($_POST['date_debut']) && isset($_POST['date_fin']))
    $validInterval = isValidInterval($_POST['date_debut'], $_POST['date_fin']);

if (isset($_POST['id_cat']) && isset($validInterval) && $validInterval)
{
    // Affiche les véhicules disponibles.

    $my_query = $db->prepare("
        SELECT v.*, m.*
        FROM vehicule AS v
        INNER JOIN modele AS m
            ON m.id_mod = v.id_mod
        WHERE m.id_cat = :id_cat
          AND NOT EXISTS (
            -- Disponible s'il n'existe pas un contrat
            -- qui se superpose aux dates souhaitées.
            SELECT *
            FROM contrat AS c
            WHERE     :date_debut <= c.date_fin
                  AND :date_fin   >= c.date_debut
                  AND c.id_veh = v.id_veh
            );");
    $my_query->execute($_POST);

    ?>
      <h3>Véhicules disponibles pour les critères spécifiés:</h3>
      <table class="table table-striped">
        <thead>
          <tr>
            <th>ID</th>
            <th>Année</th>
            <th>Plaque</th>
            <th>Modèle</th>
            <th>Marque</th>
            <th>Places</th>
            <th>Carburant</th>
            <th>Cylindrée</th>
            <th>Puissance</th>
          </tr>
        </thead>

        <?php
        while ($row = $my_query->fetch(PDO::FETCH_ASSOC))
        {
            echo '<tr>';
            echo '<td>'.$row["id_veh"].'</td>';
            echo '<td>'.$row["annee"].'</td>';
            echo '<td>'.$row["plaque"].'</td>';
            echo '<td>'.$row["id_mod"].'</td>';
            echo '<td>'.$row["marque"].'</td>';
            echo '<td>'.$row["places"].'</td>';
            echo '<td>'.$row["carburant"].'</td>';
            echo '<td>'.$row["cylindree"].' cc</td>';
            echo '<td>'.$row["puissance"].' cv</td>';
            echo '</tr>';
        }
        ?>
      </table>
    <?php
}
else
{
    // Affiche le formulaire pour sélectionner la catégorie et l'intervalle.
    ?>

    <h3>Rechercher les véhicules disponibles</h3>
    <div class="row-fluid">
      <div class="span12">
        <form class="form-horizontal" method="POST"
              action="index.php?page=vehicule_availability">
            <label for="id_cat">Catégorie du véhicule</label>
            <select id="id_cat" name="id_cat">
                <?php
                $result = $db->query("SELECT id_cat FROM categorie;");
                while ($row = $result->fetch(PDO::FETCH_NUM))
                    echo "<option>$row[0]</option>";
                ?>
            </select>

            <?php
            if (isset($validInterval))
            {
                ?>
                <p class="alert alert-error" align="center">
                    Entrez une intervalle correcte.
                </p>
                <?php
            }
            ?>

            <label for="date_debut">Date de début de la location</label>
            <input type="date" id="date_debut" name="date_debut"
                   placeholder="YYYY-MM-DD"/>

            <label for="date_fin">Date de fin de la location</label>
            <input type="date" id="date_fin" name="date_fin"
                   placeholder="YYYY-MM-DD"/>

            <button class="btn" type="submit">Rechercher</button>
        </form>
      </div>
    </div>
    <?php
}
?>

</div>

