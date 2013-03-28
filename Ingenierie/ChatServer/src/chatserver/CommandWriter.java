package chatserver;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

/**
 * Encapsule un stream de données de telle façon qu'il soit possible d'y écrire
 * des commandes séparées par un '\0'.
 */
public class CommandWriter {
    private final OutputStreamWriter _out;
    
    public CommandWriter(OutputStream out) throws UnsupportedEncodingException
    {
        this._out = new OutputStreamWriter(out, "UTF-8");
    }
    
    /** 
     * Ecrit une commande sur le flux de sortie.
     */
    public void writeCommand(Command cmd) throws IOException
    {
        this._out.write(cmd.toString());
        this._out.write(Config.COMMAND_SEP);
        this._out.flush();
    }
}
