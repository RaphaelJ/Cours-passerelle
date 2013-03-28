package chatserver;

import java.io.IOException;
import java.net.Socket;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Gère un client connecté au serveur.
 */
public class Client extends Thread {
    private final ChatServer _server;
    private final Socket _sock;
    private final CommandReader _in;
    private final CommandWriter _out;

    public Client(ChatServer server, Socket sock) throws IOException
    {
        this._server = server;
        this._sock = sock;
        
        this._in = new CommandReader(sock.getInputStream());
        this._out = new CommandWriter(sock.getOutputStream());
    }
    
    @Override
    public void run()
    {
        try {
            try {
                this.authentification();
            } finally {
                this._sock.close(); // close() peut émettre une IOException.
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
    
    /** 
     * Gère l'état d'un utilisateur connecté au serveur mais dont
     * l'authentification n'a pas encore été validée.
     */
    public void authentification() throws IOException 
    {
        Command cmd = this._in.readCommand();
        String[] args = cmd.getArgs();
        
        char op = cmd.getOperator();
        if (cmd.getOperator() == 'C' && args.length == 1) {
            // Vérifie atomiquement si le nom d'utilisateur est déjà enregistré.
            String user = args[0];
            boolean existing;
            Set<String> users = this._server.getUsers();
            synchronized (users) {
                if (users.contains(user))
                    existing = true;
                else {
                    existing = false;
                    users.add(user);
                }             
            }
            
            if (existing) {
                this._out.writeCommand(Errors.AlreadyUsedUsername);
                this.authentification();     
            } else {
                // Authentification réussite. Change l'état du client.
                try {
                    this._out.writeCommand(Command.Ack);
                    this.MainLoop(user);
                } finally { 
                    // Supprime de la liste des utilisateurs connectés lors
                    // de la fin de la connexion.
                    synchronized (users) {
                        users.remove(user);
                    }
                }
            }
        } else if (op != 'D') {
            this._out.writeCommand(Errors.SyntaxError);
            this.authentification();
        }
    }
 
    /**
     * Gère les commandes d'un client autentifié.
     */
    private void MainLoop(String user) throws IOException 
    {
        Command cmd = this._in.readCommand();
        
        switch (cmd.getOperator()) {
        
        }
    }
}
