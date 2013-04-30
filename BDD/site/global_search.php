<?php
if (!defined("IN_INDEX"))
    die();
?>

<div class="well">
<?php
if(isset($_GET['tablename']) && isClean($_GET['tablename'], $db) && isset($_GET['final']))
{
    // On retire les champs clairement inutile (en espérant que final ne soit pas une clef ..)
    $tablename = $_GET['tablename'];
    unset($_GET['final']); unset($_GET['tablename']);

    // On retire tout ce qui est *
    foreach($_GET as $k => $v)
      if(strcmp("*", $v) == 0 || empty($v))
	unset($_GET[$k]);

    // Soit la requête fetch tous les résultat
    if(empty($_GET))
    {
      $my_query = "SELECT * FROM ". $tablename." WHERE 1";
      $final_query = $db->prepare($my_query);
      $final_query->execute();
    }
    // Soit on ajoute les champs non * dans la requête
    else
    {
      $sub_query = "";
      $result = $db->query("SHOW KEYS FROM ".$tablename." WHERE Key_name = 'PRIMARY'");
      while ($row = $result->fetch(PDO::FETCH_ASSOC))
	if(array_key_exists($row['Column_name'], $_GET))
	  $sub_query .= $row['Column_name'] . " = :" . $row['Column_name'] . " AND ";
      $my_query = "SELECT * FROM ". $tablename." WHERE " . substr($sub_query, 0, strlen($sub_query) - 5);
      $final_query = $db->prepare($my_query);
      $final_query->execute($_GET);
    }
    
    ?>
      <h3>Résultat:</h3>
      <table class="table table-striped">
	<?php  
	$first = true;
	while ($row = $final_query->fetch(PDO::FETCH_ASSOC))
	{
	  if($first)
	  {
	    echo '<thead>';
	    echo '<tr>';
	    foreach($row as $key => $value)
	      echo '<th>'.$key.'</th>';
	    echo '</tr>';
	    echo '</thead>';
	    echo '</tbody>';
	    $first = false;
	  }

	  echo '<tr>';
	  foreach($row as $key => $value)
	    echo '<td>'.$value.'</td>';
	  echo '</tr>';
	}
	echo '</tbody>';
	?>
      </table>
    <?php

    if($first)
    {
      echo '<div class="alert alert-error"><strong>WUT?</strong> Aucun résultat!.</div>';
    }
}
elseif(isset($_GET['tablename']) && isClean($_GET['tablename'], $db))
{
    // Affiche le formulaire de selection de la clé.

    ?>
    <h3>Spécification des clefs</h3>
    <div class="row-fluid">
      <div class="span12">
	<form class="form-inline" method="GET" >
	  <div class="input-append">
	    <?php
	    $result = $db->query("SHOW KEYS FROM ".$_GET['tablename']." WHERE Key_name = 'PRIMARY'");
	    while ($row = $result->fetch(PDO::FETCH_ASSOC))
	    {
	      echo '<input type="text" class="input-small" name="'.$row['Column_name'].'" placeholder="'.$row['Column_name'].'">';
	    }
	    ?>
	    <input type="hidden" name="tablename" value="<?php echo $_GET['tablename']; ?>">
	    <input type="hidden" name="final">
	    <button type="submit" class="btn">Rechercher</button>
	  </div>
	</form>
      </div>
    </div>
    <?php
}
else
{
    // Affiche le formulaire de selection de la table.

    ?>
    <h3>Sélectionnez une table</h3>
    <div class="row-fluid">
      <div class="span12">
	<form class="form-inline" method="GET">
	  <div class="input-append">
	    <select name="tablename">
	      <?php
	      $result = $db->query("show tables");
	      while ($row = $result->fetch(PDO::FETCH_NUM))
		echo "<option>$row[0]</option>";
	      ?>
	    </select>
	    <button class="btn" type="submit">Utiliser</button>
	  </div>
	</form>
      </div>
    </div>
    <?php
}

// Retourne true si le nom de la table est valide.
function isClean($tablename, $dbcon) {
  $result = $dbcon->query("show tables");
  while ($row = $result->fetch(PDO::FETCH_NUM))
    if(strcmp($tablename, $row[0]) == 0)
      return true;
  return false;
}
?>

</div>

