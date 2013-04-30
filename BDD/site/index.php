<?php
define("IN_INDEX", true);

session_start();
include 'config.php';

if(!isset($_SESSION['auth']) || $_SESSION['auth'] == false)
  gotoSignin();
if(isset($_GET['page']) && strcmp($_GET['page'], "logout") == 0)
{
  session_destroy();
  gotoSignin();
}
$curPage = (isset($_GET['page'])? $_GET['page'] : "global_search");
$db = new PDO($DSN, $_SESSION['username'], $_SESSION['password']);

/* FONCTIONS */

// Retourne true si les deux chaines de caractères représentent une intervalle
// correcte.

function isValidInterval($start, $stop)
{
    return strtotime($start) < strtotime($stop);
}

function gotoSignin() {
  header('Location: signin.php');
  die();
}

?>
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Projet base de données</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">

    <!-- Le styles -->
    <link href="css/bootstrap.css" rel="stylesheet">
    <style type="text/css">
      body {
        padding-top: 20px;
        padding-bottom: 60px;
      }

      /* Custom container */
      .container {
        margin: 0 auto;
        max-width: 1000px;
      }
      .container > hr {
        margin: 60px 0;
      }

      /* Customize the navbar links to be fill the entire space of the .navbar */
      .navbar .navbar-inner {
        padding: 0;
      }
      .navbar .nav {
        margin: 0;
        display: table;
        width: 100%;
      }
      .navbar .nav li {
        display: table-cell;
        width: 1%;
        float: none;
      }
      .navbar .nav li a {
        font-weight: bold;
        text-align: center;
      }
      .navbar .nav li:first-child a {
        border-left: 0;
        border-radius: 3px 0 0 3px;
      }
      .navbar .nav li:last-child a {
        border-right: 0;
        border-radius: 0 3px 3px 0;
      }
    </style>
  </head>

  <body>

    <div class="row-fluid">
      <div class="span10 offset1">

        <div class="masthead">
          <h3 class="muted">Projet de base de données</h3>
          <div class="navbar">
            <div class="navbar-inner">
              <ul class="nav">
                <li <?php if(strcmp($curPage, "global_search") == 0) echo 'class="active"'; ?>>
                    <a href="index.php?page=global_search">Recherche globale</a></li>
                <li <?php if(strcmp($curPage, "vehicule_availability") == 0) echo 'class="active"'; ?>>
                    <a href="index.php?page=vehicule_availability">Disponibilité véhicule</a></li>
                <li <?php if(strcmp($curPage, "new_contract") == 0) echo 'class="active"'; ?>>
                    <a href="index.php?page=new_contract">Nouveau contrat</a></li>
                <li <?php if(strcmp($curPage, "finish_contract") == 0) echo 'class="active"'; ?>>
                    <a href="index.php?page=finish_contract">Cloturer contrat</a></li>
                <li <?php if(strcmp($curPage, "search_id") == 0) echo 'class="active"'; ?>>
                    <a href="index.php?page=search_id">Rechercher identifiant</a></li>
                <li><a href="index.php?page=logout">Déconnexion</a></li>
              </ul>
            </div>
          </div>
        </div>

        <hr>

        <div class="row-fluid">
          <div class="span12">
            <?php
              if(isset($_GET['page']) && strcmp($_GET['page'], "new_contract") == 0)
                include 'new_contract.php';
              elseif(isset($_GET['page']) && strcmp($_GET['page'], "vehicule_availability") == 0)
                include 'vehicule_availability.php';
              elseif(isset($_GET['page']) && strcmp($_GET['page'], "finish_contract") == 0)
                include 'finish_contract.php';
              elseif(isset($_GET['page']) && strcmp($_GET['page'], "search_id") == 0)
                include 'search_id.php';
              else
                include 'global_search.php';
            ?>
          </div>
        </div>

        <hr>

        <div class="footer">
          <p>Par Raphael Javaux &amp; Marien Bourguignon.</p>
        </div>

      </div>
    </div>

    <script src="js/bootstrap.js"></script>
  </body>
</html>
