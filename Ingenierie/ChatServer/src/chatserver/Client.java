package chatserver;

import java.io.IOException;
import java.net.Socket;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Gère un client connecté au serveur.
 */
public class Client extends Thread {
    private final ChatServer _server;
    private final Socket _sock;
    private final CommandReader _in;
    private final CommandWriter _out;
    
    /**
     * Salons auxquels l'utilisateur est connecté.
     */
    private final Set<Chan> _chans = new TreeSet<>();

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
                System.out.println(
                    "Client " + this._sock.getInetAddress().toString() +
                    "déconnecté."
                );
                this._sock.close(); // close() peut émettre une IOException.
            }
        } catch (IOException ex) {
            System.err.println(
                "Exception sur le client " + 
                this._sock.getInetAddress().toString() + ":"
            );
            System.err.println("Client ");
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
        
        char op = cmd.getOperator();
        CommandArgsIterator args_it = (CommandArgsIterator) cmd.iterator();
        if (op == 'C' && args_it.hasNext()) {
            // Vérifie atomiquement si le nom d'utilisateur est déjà enregistré.
            String user = args_it.remainder();
            boolean existing;
            Map<String, Client> users = this._server.getUsers();
            synchronized (users) {
                if (users.containsKey(user))
                    existing = true;
                else {
                    existing = false;
                    users.put(user, this);
                }             
            }
            
            if (existing) {
                this._out.writeCommand(Errors.AlreadyUsedUsername);
                this.authentification();     
            } else {
                // Authentification réussie. Change l'état du client.
                try {
                    this._out.writeCommand(Command.Ack);
                    this.mainLoop(user);
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
    private void mainLoop(String user) throws IOException 
    {
        for (;;) {
            Command cmd = this._in.readCommand();
        
            char op = cmd.getOperator();
            CommandArgsIterator args_it = (CommandArgsIterator) cmd.iterator();
            if (op == 'J' && args.length == 1 && args[0].charAt(0) == '#')
                this.joinChan(user, args[0].substring(0));
            else if (op == 'Q' && args.length == 1 && args[0].charAt(0) == '#')
                this.quitChan(user, args[0].substring(0));
            else if (op == 'M' && args.length >= 2)
                this.message(user, args)
            else if (op == 'W' && args.length >= 2)
                // Whois
            else if (op == 'L' && args.length >= 2)
                // List Chan
            else if (op == 'U' && args.length >= 2)
                // List Chan Users
            else if (op == 'E')
               this.serverExtensions(args_it);
            else if (op == 'D')
                break;
            else
                this._out.writeCommand(Errors.SyntaxError);
        }
    }

    private void serverExtensions(CommandArgsIterator args_it) 
            throws IOException 
    {
        if (args_it.hasNext() && args_it.remainder().equals("version")) {
            this._out.writeCommand(new Command(
                'E', "Raphael Javaux " + Config.VERSION
            ));
        } else
            this._out.writeCommand(Errors.SyntaxError);
    }
}
