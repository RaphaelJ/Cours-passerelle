<?php
if (!defined("IN_INDEX"))
    die();
?>

<div class="well">

<?php

if (isset($_POST['date_debut']) && isset($_POST['date_fin']))
    $validInterval = isValidInterval($_POST['date_debut'], $_POST['date_fin']);

if (isset($validInterval) && $validInterval)
{
    $my_query = $db->prepare("
        SELECT c.id_veh, SUM(c.km_fin - c.km_debut) AS kms
        FROM contrat AS c
        WHERE c.date_debut >= :date_debut AND c.date_fin <= :date_fin
        GROUP BY c.id_veh
        ORDER BY kms DESC
        LIMIT 1;");
    $my_query->execute($_POST);
    $res = $my_query->fetch(PDO::FETCH_ASSOC);
    ?>

    <h3>Résultat</h3>
    Le véhicule ayant parcouru le plus de kilomètres est le véhicule N°
    <?php echo $res["id_veh"]; ?> avec <?php echo $res["kms"]; ?> km.

    <?php
}
else
{
    ?>
    <h3>Rechercher le véhicule ayant parcouru la plus grande distance</h3>
    <div class="row-fluid">
      <div class="span12">
        <form class="form-horizontal" method="POST"
              action="index.php?page=search_id">

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

            <label for="date_debut">Date de début</label>
            <input type="date" id="date_debut" name="date_debut"/>

            <label for="date_fin">Date de fin</label>
            <input type="date" id="date_fin" name="date_fin"/>

            <button class="btn" type="submit">Rechercher</button>
        </form>
      </div>
    </div>
    <?php
}
?>

</div>
