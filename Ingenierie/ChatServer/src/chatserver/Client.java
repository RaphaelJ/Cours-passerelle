package chatserver;

import java.io.IOException;
import java.net.InetSocketAddress;
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
                this.traceEvent("Client connecté");
                this.authentification();
            } finally {
                this.traceEvent("Client déconnecté");
                this._sock.close(); // close() peut émettre une IOException.
            }
        } catch (IOException ex) {
            this.traceEvent("Exception sur le client");
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
    public void sendMessage(String author, String message) throws IOException
    {
        Command cmd = new Command('M', author + " " + message);
        this.writeCommand(cmd);
    }
    
    /* GESTION DE LA CONNEXION */
    
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
                    this.traceEvent(
                        "Client authentifié en tant que " + user_name
                    );
                    try {
                        this.ack();
                        this.mainLoop(user_name);
                    } finally { 
                        this.disconnect(user_name);
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
                this.listChans(args_it);
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
                
                Map<String, Chan> chans = this._server.getChans();
                boolean already_joined;
                synchronized (chans) {
                    synchronized (this._chans) {
                        if (!this._chans.containsKey(chan_name)) {
                            Chan chan = chans.get(chan_name);
                            if (chan != null)
                                chan.joinChan(user_name, this);
                            else {
                                new Chan(
                                    this._server, chan_name, user_name, this
                                );
                            }
                            already_joined = false;
                        } else
                            already_joined = true;
                    }
                }
                
                if (!already_joined) {
                    this.traceEvent("Rejoint le salon #" + chan_name);
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
                boolean not_joined;
                synchronized (this._server.getChans()) {
                    synchronized (this._chans) {
                        Chan chan = this._chans.get(chan_name);
                        if (chan != null) {
                            chan.quitChan(user_name, this);
                            not_joined = false;
                        } else
                            not_joined = true;
                    }
                }
                
                if (!not_joined) {
                    this.traceEvent("Quitte le salon #" + chan_name);
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
                        Chan chan;
                        synchronized (this._chans) {
                            chan = this._chans.get(chan_name);
                        }
                        
                        if (chan != null) {
                            this.traceEvent("Message sur #" + chan_name);
                            chan.sendMessage(user_name, message);
                            this.ack();
                        } else
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
                    
                    if (success) {
                        this.traceEvent("Message à " + dest);
                        this.ack();
                    } else
                        this.writeCommand(Errors.UnknownUsername);
                }
            } else 
                this.syntaxError();
        } else {
            this.syntaxError();
        }
    }

    /**
     * Liste les salons auxquels un utilisateur est connecté.
     */
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
                            chan_names = chans.keySet().toArray(new String[0]);
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
                
                this.traceEvent("Whois sur " + user_name);
                this.writeCommand(Command.Ack);
            } else
                this.syntaxError();
        } else
            this.syntaxError();
    }

    /**
     * Liste les salons du serveur.
     */
    private void listChans(CommandArgsIterator args_it) throws IOException
    {
        if (!args_it.hasNext()) {
            Map<String, Chan> chans = this._server.getChans();
            String[] chan_names;
            synchronized (chans) {
                chan_names = chans.keySet().toArray(new String[0]);
            }
            
            Command cmd;
            if (chan_names.length > 0) {
                StringBuilder args = new StringBuilder();
                for (String name : chan_names) {
                    args.append(" #");
                    args.append(name);
                }

                cmd = new Command('I', args.toString().substring(1));
            } else
                cmd = new Command('I', "");

            this.traceEvent("Listage des salons");
            this.writeCommand(cmd);
            this.ack();
        } else
            this.syntaxError();
    }

    /**
     * Liste les utilisateurs d'un salon.
     */
    private void listUsers(CommandArgsIterator args_it) throws IOException 
    {
        if (args_it.hasNext()) {
            String chan_arg = args_it.next();
            if (!args_it.hasNext() && chan_arg.length() >= 2
                    && chan_arg.charAt(0) == '#') {
                String chan_name = chan_arg.substring(1);
                Map<String, Chan> chans = this._server.getChans();
                String[] user_names;
                synchronized (chans) {
                    Chan chan = chans.get(chan_name);
                    if (chan != null) {
                        Map<String, Client> users = chan.getUsers();
                        synchronized (users) {
                            user_names = users.keySet().toArray(new String[0]);
                        }
                    } else
                        user_names = null;
                }
                
                if (user_names != null) {
                    Command cmd;
                    if (user_names.length > 0) {
                        StringBuilder args = new StringBuilder();
                        for (String name : user_names) {
                            args.append(' ');
                            args.append(name);
                        }

                        cmd = new Command('I', args.toString().substring(1));
                    } else
                        cmd = new Command('I', "");
                    
                    this.traceEvent(
                        "Listage des utilisateurs de #" + chan_name
                    );
                    this.writeCommand(cmd);
                    this.ack();
                } else
                    this.writeCommand(Errors.ChanNotFound);
            } else 
                this.syntaxError();
        } else
            this.syntaxError();
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

    /**
     * Déconnecte l'utilisateur du serveur et des salons.
     */
    private void disconnect(String user_name)
    {
        Map<String, Client> users = this._server.getUsers();
        synchronized (users) {
            synchronized (this._server.getChans()) {
                synchronized (this._chans) {
                    for (Chan chan : this._chans.values())
                        chan.quitChan(user_name, this);
                    
                    users.remove(user_name);
                }
            }
        }
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
    
    /**
     * Affiche un message lié d'information avec les informations du socket.
     */
    private void traceEvent(String desc)
    {
        InetSocketAddress addr = 
                (InetSocketAddress) this._sock.getRemoteSocketAddress();
        
        // Génère une couleur unique à partir du port du client
        int color_code = addr.getPort() % 6 + 1;

        System.out.println(
            "\033[1;3"+ color_code +"m[" + addr.toString() + "]\033[0m " +
            desc);
    }
}
