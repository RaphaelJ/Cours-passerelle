<?php
session_start();
include 'config.php';

if(isset($_SESSION['auth']) && $_SESSION['auth'] == true)
{
  header('Location: index.php');
  die();
}
else if(isset($_POST['Username']) && isset($_POST['Password']))
{
  try
  {
    $db = new PDO($DSN, $_POST['Username'], $_POST['Password']);
    $_SESSION['auth'] = true;
    $_SESSION['username'] = $_POST['Username'];
    $_SESSION['password'] = $_POST['Password'];

    header('Location: index.php');
    die();
  }
  catch (Exception $ex)
  {
    $error = true;
  }
}
?>
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Sign in</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <!-- Bootstrap -->
    <link href="css/bootstrap.min.css" rel="stylesheet" media="screen">

    <style type="text/css">
      body {
        padding-top: 40px;
        padding-bottom: 40px;
        background-color: #f5f5f5;
      }

      .form-signin {
        padding: 19px 29px 29px;
        margin-top: 20px;
        background-color: #fff;
        border: 1px solid #e5e5e5;
        -webkit-border-radius: 5px;
           -moz-border-radius: 5px;
                border-radius: 5px;
        -webkit-box-shadow: 0 1px 2px rgba(0,0,0,.05);
           -moz-box-shadow: 0 1px 2px rgba(0,0,0,.05);
                box-shadow: 0 1px 2px rgba(0,0,0,.05);
      }

    </style>

  </head>
  <body>
    <div class="row-fluid">
      <div class="offset4 span4">
        <form class="form-signin" method="POST">
          <h2>Connexion</h2>
          <input type="text" class="input-block-level" placeholder="Username" name="Username">
          <input type="password" class="input-block-level" placeholder="Password" name="Password">
          <button class="btn btn-primary" type="submit">Sign in</button> 
        </form>

        <?php 
        if(isset($error))
        {
            ?>
            <p class="alert alert-error" align="center">
                Mauvais identifiants
            </p>
            <?php
        }
        ?>

      </div>
    </div>

    <script src="js/bootstrap.min.js"></script>
  </body>
</html>
