<?php
if (!defined("IN_INDEX"))
    die();
?>

<div class="well">

<?php

// Exécute cette page dans une transaction pour éviter que le contrat ne soit
// pas validé/supprimé entre le moment où nous vérifions son existance et le
// moment ou on enregistre sa facture.
$db->beginTransaction();

// Vérifie que le contrat sélectionné existe (toujours) et n'est pas facturé.
if (isset($_POST["id_cont"])) {
    $my_query = $db->prepare("SELECT km_debut, km_fin
        FROM contrat AS c
        WHERE c.id_cont = :id_cont
          AND NOT EXISTS (
            SELECT *
            FROM facture AS f
            WHERE f.id_cont = c.id_cont)");
    $my_query->bindParam(':id_cont', $_POST["id_cont"]);
    $my_query->execute();

    if ($row = $my_query->fetch(PDO::FETCH_ASSOC)) {
        $cont = $row;

        // Vérifie que le kilométrage est correctement spécifié.
        if ($_POST["km_debut"] && $_POST["km_fin"])
        {
            $debut = intval($_POST["km_debut"]);
            $fin   = intval($_POST["km_fin"]); 
            if ($debut <= $fin) {
                $km_debut = $debut;
                $km_fin   = $fin;
            }
        }
    }
}

if (isset($cont) && isset($km_debut) && isset($km_fin))
{
    // Facture le contrat.

    // Met à jour le kilométrage du contrat.
    $my_query = $db->prepare("UPDATE contrat
        SET km_debut = :km_debut, km_fin = :km_fin
        WHERE id_cont = :id_cont;");
    $my_query->bindParam(":id_cont" , $_POST["id_cont"]);
    $my_query->bindParam(":km_debut", $km_debut);
    $my_query->bindParam(":km_fin"  , $km_fin);
    $my_query->execute();

    // Ajoute la facture.
    $my_query = $db->prepare("INSERT INTO facture (id_cont, prix)
        SELECT con.id_cont,
                 DATEDIFF(con.date_fin, con.date_debut) * cat.prix_jour
               + (con.km_fin - con.km_debut) * cat.prix_km AS prix
        FROM contrat AS con
        INNER JOIN vehicule AS v
            ON v.id_veh = con.id_veh
        INNER JOIN modele AS m
            ON m.id_mod = v.id_mod
        INNER JOIN categorie AS cat
            ON cat.id_cat = m.id_cat
        WHERE con.id_cont = :id_cont;");
    $my_query->bindParam(":id_cont", $_POST["id_cont"]);
    $my_query->execute();

    ?>
    <h3>Contrat de location facturé</h3>
    Montant facture : 
    <?php
    $my_query = $db->prepare("SELECT prix
        FROM facture
        WHERE id_cont = :id_cont;");
    $my_query->bindParam(":id_cont" , $_POST["id_cont"]);
    $my_query->execute();

    echo $my_query->fetchColumn(0) . ' €';
}
else if (isset($cont))
{
    // Entre les données de kilomètrage

    ?>
    <h3>Données de location</h3>
    <div class="row-fluid">
      <div class="span12">
        <form class="form-horizontal" method="POST"
              action="index.php?page=finish_contract">
            <?php
            if ($_POST["km_debut"] && $_POST["km_fin"]) {
                ?>
                <p class="alert alert-error" align="center">
                    Entrez une intervalle kilométrique correcte.
                </p>
                <?php
            }

            if ($cont["km_debut"]) {
                ?>
                <input type="hidden" name="km_debut"
                       value="<?php echo $cont["km_debut"]; ?>" />

                <label for="km_fin">
                    Kilomètrage de fin
                    (kilomètrage de début: <?php echo $cont["km_debut"]; ?>) :
                </label>
                <input type="number" id="km_fin" name="km_fin" />
                <?php
            } else {
                ?>
                <label for="km_debut">Kilomètrage de début : </label>
                <input type="number" id="km_debut" name="km_debut" />

                <label for="km_fin">Kilomètrage de fin : </label>
                <input type="number" id="km_fin" name="km_fin" />
                <?php
            }
            ?>

            <input type="hidden" name="id_cont"
                       value="<?php echo $_POST["id_cont"] ?>" />

            <button class="btn" type="submit">Facturer la location</button>
        </form>
      </div>
    </div>
    <?php
}
else
{
    // Sélectionne le contrat à facturer.

    ?>
    <h3>Sélection d'un contrat à facturer</h3>
    <div class="row-fluid">
      <div class="span12">
        <form class="form-inline" method="POST"
              action="index.php?page=finish_contract">
          <div class="input-append">
            <label for="id_cont">Numéro du contrat : </label>
            <select id="id_cont" name="id_cont">
                <?php
                $result = $db->query("
                    SELECT c.id_cont
                    FROM contrat AS c
                    WHERE NOT EXISTS (
                        SELECT *
                        FROM facture AS f
                        WHERE f.id_cont = c.id_cont)
                    ORDER BY c.id_cont;");
                while ($row = $result->fetch(PDO::FETCH_NUM))
                    echo "<option value=\"$row[0]\">$row[0]</option>";
                ?>
            </select>

            <button class="btn" type="submit">Rechercher</button>
          </div>
        </form>
      </div>
    </div>
    <?php
}

$db->commit();
?>

</div>
