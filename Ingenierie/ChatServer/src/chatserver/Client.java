package chatserver;

import java.io.IOException;
import java.net.Socket;
import java.util.Map;
import java.util.TreeMap;

/**
 * Gère un client connecté au serveur en manipulant son socket TCP.
 */
public class Client extends Thread {
    private final ChatServer _server;
    private final Socket _sock;
    private final CommandReader _in;
    private final CommandWriter _out;
    
    /**
     * Salons auxquels l'utilisateur est connecté.
     */
    private final Map<String, Chan> _chans = new TreeMap<>();

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
                System.out.println(
                    "Client " + this._sock.getInetAddress().toString() +
                    " connecté."
                );
                
                this.authentification();
            } finally {
                System.out.println(
                    "Client " + this._sock.getInetAddress().toString() +
                    " déconnecté."
                );
                this._sock.close(); // close() peut émettre une IOException.
            }
        } catch (IOException ex) {
            System.err.println(
                "Exception sur le client " + 
                this._sock.getInetAddress().toString() + " : "
            );
            ex.printStackTrace();
        }
    }
    
    public Map<String, Chan> getChans() 
    {
        return _chans;
    }
    
    /**
     * Envoie la commande à l'utilisateur. Appel bloquant et réantrant.
     * @param cmd 
     */
    public void writeCommand(Command cmd) throws IOException
    {
        this._out.writeCommand(cmd);
    }
    
    /** 
     * Envoie un message au client. Appel bloquant et réantrant.
     * @param user_name Nom de l'utilisateur à l'origine du message.
     */
    public void sendMessage(String user_name, String message) throws IOException
    {
        Command cmd = new Command('M', user_name + " " + message);
        this.writeCommand(cmd);
    }
    
    /** 
     * Gère l'état d'un utilisateur connecté au serveur mais dont
     * l'authentification n'a pas encore été validée.
     */
    private void authentification() throws IOException 
    {
        Command cmd = this._in.readCommand();
        
        char op = cmd.getOperator();
        CommandArgsIterator args_it = (CommandArgsIterator) cmd.iterator();
        if (op == 'C' && args_it.hasNext()) {
            // Vérifie atomiquement si le nom d'utilisateur est déjà enregistré.
            String user_name = args_it.next();
            if (!args_it.hasNext()) {
                boolean existing;
                Map<String, Client> users = this._server.getUsers();
                synchronized (users) {
                    if (users.containsKey(user_name))
                        existing = true;
                    else {
                        existing = false;
                        users.put(user_name, this);
                    }             
                }

                if (existing) {
                    this.writeCommand(Errors.AlreadyUsedUsername);
                    this.authentification();     
                } else {
                    // Authentification réussie. Change l'état du client.
                    try {
                        this.ack();
                        this.mainLoop(user_name);
                    } finally { 
                        this.disconnect();
                    }
                }
            } else {
                this.syntaxError();
                this.authentification();
            }
        } else if (op != 'D') {
            this.syntaxError();
            this.authentification();
        }
    }
 
    /**
     * Gère les commandes d'un client autentifié.
     */
    private void mainLoop(String user_name) throws IOException 
    {
        for (;;) {
            Command cmd = this._in.readCommand();
        
            char op = cmd.getOperator();
            CommandArgsIterator args_it = (CommandArgsIterator) cmd.iterator();
            if (op == 'J')
                this.joinChan(user_name, args_it);
            else if (op == 'Q')
                this.quitChan(user_name, args_it);
            else if (op == 'M')
                this.message(user_name, args_it);
            else if (op == 'W')
                this.whois(args_it);
            else if (op == 'L')
                this.listChan(args_it);
            else if (op == 'U')
                this.listUsers(args_it);
            else if (op == 'E')
               this.serverExtensions(args_it);
            else if (op == 'D')
                break;
            else
                this.syntaxError();
        }
    }
    
    /* GESTION DES COMMANDES */

    private void joinChan(String user_name, CommandArgsIterator args_it) 
            throws IOException
    {
        if (args_it.hasNext()) {
            String chan_arg = args_it.next();
            if (!args_it.hasNext() && chan_arg.length() >= 2
                    && chan_arg.charAt(0) == '#') {
                String chan_name = chan_arg.substring(1);
                if (!this._chans.containsKey(chan_name)) {
                    Map<String, Chan> chans = this._server.getChans();
                
                    synchronized (chans) {
                        Chan chan = chans.get(chan_name);
                        if (chan != null)
                            chan.joinChan(user_name, this);
                        else {
                            new Chan(
                                this._server, chan_name, user_name, this
                            );
                        }
                    }
                    this.ack();
                } else
                    this.writeCommand(Errors.ChanAlreadyJoined);
            } else 
                this.syntaxError();
        } else
            this.syntaxError();
    }
    
    private void quitChan(String user_name, CommandArgsIterator args_it)
            throws IOException
    {
        if (args_it.hasNext()) {
            String chan_arg = args_it.next();
            if (!args_it.hasNext() && chan_arg.length() >= 2
                    && chan_arg.charAt(0) == '#') {
                String chan_name = chan_arg.substring(1);
                Chan chan = this.getChans().get(chan_name);
                if (chan != null) {
                    chan.quitChan(user_name);
                    this.ack();
                } else
                    this.writeCommand(Errors.ChanNotJoined);
            } else 
                this.syntaxError();
        } else
            this.syntaxError();
    }

    private void message(String user_name, CommandArgsIterator args_it)
            throws IOException
    {
        if (args_it.hasNext()) {
            String dest    = args_it.next();
            String message = args_it.remainder();
            
            if (dest.length() >= 1 && !message.isEmpty()) {
                if (dest.charAt(0) == '#') { // Message sur un salon
                    String chan_name = dest.substring(1);
                    if (!chan_name.isEmpty()) {
                        Chan chan = this.getChans().get(chan_name);
                        if (chan != null)
                            chan.sendMessage(user_name, message);
                        else
                            this.writeCommand(Errors.ChanNotJoined);
                    } else
                        this.syntaxError();
                } else { // Message privé 
                    Map<String, Client> users = this._server.getUsers();
                    Boolean success; // Pour envoyer le feedback hors du lock
                    synchronized (users) {
                        Client dest_user = users.get(dest);
                        if (users.containsKey(dest)) {
                            dest_user.sendMessage(user_name, message);
                            success = true;
                        } else
                            success = false;
                    }
                    
                    if (success) 
                        this.ack();
                    else
                        this.writeCommand(Errors.UnknownUsername);
                }
            } else 
                this.syntaxError();
        } else {
            this.syntaxError();
        }
    }

    private void whois(CommandArgsIterator args_it) throws IOException
    {
        if (args_it.hasNext()) {    
            String user_name = args_it.next();
            if (!args_it.hasNext()) {
                Map<String, Client> users = this._server.getUsers();
                
                String[] chan_names = null;
                synchronized (users) {
                    Client user = users.get(user_name);
                    if (user != null) {
                        Map<String, Chan> chans = user.getChans();
                        synchronized (chans) {
                            chan_names = chans.keySet().toArray(chan_names);
                        }
                    }
                }
                
                // Envoie la réponse hors des verrous.
                if (chan_names != null) {
                    StringBuilder args = new StringBuilder("C");
                    for (String name : chan_names) {
                        args.append(" #");
                        args.append(name);
                    }
                        
                    this.writeCommand(new Command('I', args.toString()));
                } else
                    this.writeCommand(new Command('I', "D"));
                
                this.writeCommand(Command.Ack);
            } else
                this.syntaxError();
        } else
            this.syntaxError();
    }

    private void listChan(CommandArgsIterator args_it)
    {
        
    }

    private void listUsers(CommandArgsIterator args_it) 
    {
        
    }

    private void serverExtensions(CommandArgsIterator args_it) 
            throws IOException 
    {
        if (args_it.hasNext() && args_it.remainder().equals("version")) {
            this.writeCommand(new Command(
                'E', "Raphael Javaux " + Config.VERSION
            ));
        } else
            this.syntaxError();
    }
    
    /* UTILITAIRES */
    
    private void syntaxError() throws IOException
    {
        this.writeCommand(Errors.SyntaxError);
    }
    
    private void ack() throws IOException
    {
        this.writeCommand(Command.Ack);
    }     

    // Déconnecte l'utilisateur du serveur et des salons
    private void disconnect()
    {
        Map<String, Client> users = this._server.getUsers();
        synchronized (users) {
            Map<String, Chan>
            users.remove(user_name);
        }
    }
}
