package chatserver;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Itérateur utilisable pour itérer les arguments d'une commande séparés par
 * des espaces.
 */
public class CommandArgsIterator implements Iterator<String> {
    private String _args;
    
    public CommandArgsIterator(String args)
    {
        this._args = args;
    }
    
    @Override
    public boolean hasNext()
    {
        return this._args.length() > 0;
    }
    
    /**
     * Retourne la plus longe chaine de caractères sans espaces dans la commande
     * itérée.
     */
    @Override
    public String next()
    {
        if (this.hasNext()) {
            int index = this._args.indexOf(' ');
            if (index >= 0) {
                String arg = this._args.substring(0, index);
                this._args = this._args.substring(index + 1);
                return arg;
            } else {
                String arg = this._args;
                this._args = "";
                return arg;
            }
        } else
            throw new NoSuchElementException();
    }
    
    /**
     * Retourne le reste de la chaine d'argument. Ne modifie pas l'état de
     * l'itérateur. Utilisé dans les messages où toute la fin de la commande
     * constitue le contenu du message.
     */
    public String remainder()
    {
        return this._args;
    }

    @Override
    public void remove() 
    {
        throw new UnsupportedOperationException("Not supported.");
    }
}
