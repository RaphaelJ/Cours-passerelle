package chatserver;

import java.util.Iterator;

/**
 * Encapsule une commande recue ou envoyée au client.
 * Une commande est constituée d'un caractère identifiant l'opérateur et
 * d'un vecteur de paramètres séparés par des espaces.
 * Fournit les méthodes de parsing et de génération de commandes.
 */
class Command implements Iterable<String> {
    /** 
     * Commande utilisée pour signaler le succès d'une opération.
     */
    public static final Command Ack = new Command('K',"");
    
    private final char _operator;
    private final String _args;
    
    /** 
     * Construit une nouvelle commande.
     */
    public Command(char operator, String args)
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

        if (str.length() >= 3)
            this._args = str.substring(2);
        else
            this._args = "";
    }
    
    public char getOperator() 
    {
        return _operator;
    }
    
    public String getArgs() 
    {
        return _args;
    }
    
    /**
     * Construit la représentation de la chaine de caractère pouvant être
     * transmise.
     */
    @Override
    public String toString()
    {
        return this._operator + " " + this._args;
     }

    @Override
    public Iterator<String> iterator()
    {
        return new CommandArgsIterator(this._args);
    }
}
