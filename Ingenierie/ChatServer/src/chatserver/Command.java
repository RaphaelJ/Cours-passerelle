package chatserver;

/**
 * Encapsule une commande recue ou envoyée au client.
 * Une commande est constituée d'un caractère identifiant l'opérateur et
 * d'un vecteur de paramètres séparés par des espaces.
 * Fournit les méthodes de parsing et de génération de commandes.
 */
class Command {
    /** 
     * Commande utilisée pour signaler le succès d'une opération.
     */
    public static final Command Ack = new Command('K', new String[0]);
    
    private final char _operator;
    private final String[] _args;
    
    /** 
     * Construit une nouvelle commande.
     */
    public Command(char operator, String[] args)
    {
        this._operator = operator;
        this._args = args;
    }
    
    /** 
     * Construit une nouvelle commande en parsant sa représentation en chaine
     * de caractères.
     */
    public Command(String str)
    {
        this._operator = str.charAt(0);
        this._args     = str.substring(1).split(" ");
    }
    
    public char getOperator() {
        return _operator;
    }
    
    public String[] getArgs() {
        return _args;
    }
    
    /**
     * Construit la représentation de la chaine de caractère pouvant être
     * transmise.
     */
    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();
        
        builder.append(this._operator);
        
        for (String arg : this._args) {
            builder.append(' ');
            builder.append(arg);
        }
        
        return builder.toString();
     }
}
