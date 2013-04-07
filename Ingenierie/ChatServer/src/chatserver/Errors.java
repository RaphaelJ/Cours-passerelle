package chatserver;

/**
 * Liste toutes les instances des commandes liées aux codes des erreurs.
 */
public final class Errors {
    // Erreurs de syntaxe.
    public static final Command SyntaxError         = error("1.1");
    
    // Erreurs liées à une commande non autorisée.
    public static final Command NotAuthenticated    = error("2.1");
    
    // Erreurs liées à l'autentification et aux utilisateurs.
    public static final Command AlreadyUsedUsername = error("3.1");
    public static final Command UnknownUsername     = error("3.2");
    
    // Erreurs liées à la connexion à un salon.
    public static final Command ChanNotJoined       = error("4.1");
   
    /**
     * Instancie une commande d'erreur à partir d'un code d'erreur.
     */
    private static Command error(String code)
    {
        return new Command('N', new String[] { code });
    }
}