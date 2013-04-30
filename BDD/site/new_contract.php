<?php
if (!defined("IN_INDEX"))
    die();
?>

<div class="well">

<?php

if (isset($_POST['date_debut']) && isset($_POST['date_fin']))
    $validInterval = isValidInterval($_POST['date_debut'], $_POST['date_fin']);

// Exécute cette page dans une transaction pour éviter que la vérification de
// l'existance du conducteur et de la disponibilité du véhicule soit toujours
// valide à la fin de l'exécution de la page.
$db->beginTransaction();

// Vérifie que le client sélectionné existe (toujours).
if (isset($_POST["id_client"])) {
    $my_query = $db->prepare("SELECT 1
        FROM client
        WHERE id_client = :id_client;");
    $my_query->bindParam(':id_client', $_POST["id_client"]);
    $my_query->execute();

    if ($my_query->rowCount() > 0)
        $id_client = $_POST["id_client"];
}

// Vérifie que chaque conducteur sélectionné existe (toujours).
if (isset($_POST["drivers"])) {
    $my_query = $db->prepare("SELECT 1
        FROM conducteur AS c
        WHERE c.no_permis = :no_permis;");

    $drivers = array();
    foreach ($_POST["drivers"] as $no_permis) {
        $my_query->bindParam(':no_permis', $no_permis);
        $my_query->execute();

        if ($my_query->rowCount() > 0)
            $drivers[] = $no_permis;
    }
}

// Vérifie que le véhicule sélectionné est (toujours) disponible pour
// l'intervalle choise.
if (isset($validInterval) && $validInterval && isset($_POST["id_veh"])) {
    $my_query = $db->prepare("SELECT 1
        FROM vehicule AS v
        INNER JOIN modele AS m
            ON m.id_mod = v.id_mod
        WHERE v.id_veh = :id_veh
         AND NOT EXISTS (
            -- Disponible s'il n'existe pas un contract
            -- qui se superpose aux dates souhaitées.
            SELECT *
            FROM contrat AS c
            WHERE     :date_debut <= c.date_fin
                  AND :date_fin   >= c.date_debut
                  AND c.id_veh = v.id_veh
            );");
    $my_query->bindParam(":id_veh", $_POST["id_veh"]);
    $my_query->bindParam(":date_debut", $_POST["date_debut"]);
    $my_query->bindParam(":date_fin", $_POST["date_fin"]);
    $my_query->execute();

    if ($my_query->rowCount() > 0)
        $id_veh = $_POST["id_veh"];
}

if (isset($id_veh) && isset($drivers) && count($drivers) > 0
    && isset($id_client) && isset($validInterval) && $validInterval)
{
    // Enregistre un nouveau contrat.

    // Ajoute le contrat.
    $my_query = $db->prepare("INSERT INTO contrat
        (id_veh, id_client, date_debut, date_fin, km_debut, km_fin)
        VALUES (:id_veh, :id_client, :date_debut, :date_fin, NULL, NULL);");
    $my_query->bindParam(":id_veh", $id_veh);
    $my_query->bindParam(":id_client", $id_client);
    $my_query->bindParam(":date_debut", $_POST["date_debut"]);
    $my_query->bindParam(":date_fin", $_POST["date_fin"]);
    $my_query->execute();

    $id_contrat = $db->lastInsertId();

    // Ajoute les conducteurs
    foreach ($drivers as $driver) {
        $my_query = $db->prepare("INSERT INTO conduit (id_cont, no_permis)
            VALUES (:id_contrat, :no_permis);");
        $my_query->bindParam(":id_contrat", $id_contrat);
        $my_query->bindParam(":no_permis", $driver);
        $my_query->execute();
    }

    ?>
    <h3>Contrat de location enregistré</h3>
    Numéro de contrat : <?php echo $id_contrat; ?>
    <?php
}
else if (isset($drivers) && count($drivers) > 0 && isset($id_client)
         && isset($validInterval) && $validInterval)
{
    // Sélectionne le véhicule en listant les véhicules disponibles.

    ?>
    <h3>Selection de la voiture de la location :</h3>
    <div class="row-fluid">
      <div class="span12">
        <form class="form-inline" method="POST"
              action="index.php?page=new_contract">
          <div class="input-append">
            <select id="id_veh" name="id_veh">
                <?php
                $my_query = $db->prepare("
                    SELECT v.*, m.*
                    FROM vehicule AS v
                    INNER JOIN modele AS m
                        ON m.id_mod = v.id_mod
                    WHERE NOT EXISTS (
                        -- Disponible s'il n'existe pas un contrat
                        -- qui se superpose aux dates souhaitées.
                        SELECT *
                        FROM contrat AS c
                        WHERE   :date_debut <= c.date_fin
                            AND :date_fin   >= c.date_debut
                            AND c.id_veh = v.id_veh
                        );");
                $my_query->bindParam(":date_debut", $_POST["date_debut"]);
                $my_query->bindParam(":date_fin", $_POST["date_fin"]);
                $my_query->execute();

                while ($row = $my_query->fetch(PDO::FETCH_ASSOC))
                {
                    echo "<option value=\"".$row['id_veh']."\">";
                    echo $row['id_veh']." - ".$row['marque']." ".$row['id_mod'];
                    echo " (".$row['annee'].")";
                    echo "</option>";
                }
                ?>
            </select>

            <input type="hidden" name="id_client"
                   value="<?php echo $id_client; ?>" />
            <input type="hidden" name="date_debut"
                   value="<?php echo $_POST["date_debut"]; ?>" />
            <input type="hidden" name="date_fin"
                   value="<?php echo $_POST["date_fin"]; ?>" />
            <?php
            foreach ($drivers as $driver) {
                ?>
                <input type="hidden" name="drivers[]"
                       value="<?php echo $driver; ?>" />
                <?php
            }
            ?>

            <button class="btn" type="submit">Valider la location</button>
          </div>
        </form>
      </div>
    </div>
    <?php
}
else
{
    // Sélectionne le client, les conducteurs et la période de location.

    ?>
    <h3>Selection d'un client, des conducteurs et de la période de location</h3>
    <div class="row-fluid">
      <div class="span12">
        <form class="form-horizontal" method="POST"
              action="index.php?page=new_contract">
            <label for="id_client">Client</label>
            <select id="id_client" name="id_client">
                <?php
                $result = $db->query("
                    SELECT p.id_client,
                           CONCAT(p.prenom_part, ' ', p.nom_part) AS nom
                    FROM particulier AS p
                    UNION
                    SELECT s.id_client, s.nom_societe AS nom
                    FROM societe AS s;");
                while ($row = $result->fetch(PDO::FETCH_NUM))
                    echo "<option value=\"$row[0]\">$row[0] - $row[1]</option>";
                ?>
            </select>

            <?php
            if (isset($validInterval) && !$validInterval)
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

            <?php
            if (isset($drivers) && count($drivers) == 0)
            {
                ?>
                <p class="alert alert-error" align="center">
                    Sélectionnez au moins un conducteur.
                </p>
                <?php
            }
            ?>

            <label for="drivers">Conducteurs</label>
            <select id="drivers" name="drivers[]" multiple="multiple">
                <?php
                $result = $db->query("
                    SELECT cc.no_permis,
                           CONCAT(p.prenom_part, ' ', p.nom_part) AS nom
                    FROM particulier AS p
                    INNER JOIN cond_client AS cc
                        ON cc.id_client = p.id_client
                    UNION
                    SELECT cc.no_permis, s.nom_societe AS nom
                    FROM societe AS s
                    INNER JOIN cond_client AS cc
                        ON cc.id_client = s.id_client
                    UNION
                    SELECT ca.no_permis,
                           CONCAT(ca.prenom_conducteur_autre, ' ', 
                                  ca.nom_conducteur_autre) AS nom
                    FROM cond_autre AS ca;");

                while ($row = $result->fetch(PDO::FETCH_NUM))
                    echo "<option value=\"$row[0]\">$row[0] - $row[1]</option>";
                ?>
            </select>
            <span class="help-block">
                Sélectionnez au moins un conducteur. Maintenez CTRL pour
                sélectionner plusieurs conducteurs.
            </span>

            <button class="btn" type="submit">Rechercher un véhicule</button>
        </form>
      </div>
    </div>
    <?php
}

$db->commit();
?>

</div>

