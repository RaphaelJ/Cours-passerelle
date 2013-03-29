package chatserver;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

/**
 * Encapsule un stream de données de telle façon qu'il soit possible d'y lire
 * des commandes séparées par un '\0'.
 */
public class CommandReader {
    private final InputStreamReader _in;
    
    public CommandReader(InputStream in) throws UnsupportedEncodingException
    {
        this._in = new InputStreamReader(in, "UTF-8");
    }
    
    /** 
     * Lit, parse et retourne une commande depuis le flux d'entrée.
     * Verrouille le flux durant la réception.
     */
    public synchronized Command readCommand() throws IOException
    {
        StringBuilder builder = new StringBuilder();
        
        char c;
        
        // Ignore les commandes vides.
        while ((c = (char) this._in.read()) == Config.COMMAND_SEP) { }
        
        // Lit jusqu'au séparateur des commandes.
        do 
            builder.append(c);
        while ((c = (char) this._in.read()) != Config.COMMAND_SEP);
        
        return new Command(builder.toString());
    }
}
