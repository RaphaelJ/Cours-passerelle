package chatserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * 
 */
public class ChatServer  {
    /**
     * Clients autentifiés.
     */
    private Map<String, Client> _users = new TreeMap<>();
    /**
     * Salons actifs.
     */
    private Set<Chan> _chans = new TreeSet<>();

    public static void main(String[] args) throws IOException 
    {
        new ChatServer().run();
    }

    /**
     * Ouvre une socket TCP sur le port dédié et lance la gestion de chaque 
     * client dans un nouveau thread. Ne termine jamais.
     */
    public void run() throws IOException
    {
        ServerSocket server_sock = new ServerSocket(Config.SERVER_PORT);

        System.out.println(
            "Socket serveur en écoute sur le port " + Config.SERVER_PORT + "."
        );

        for (;;)
            new Client(this, server_sock.accept()).start();
    }

    public Map<String, Client> getUsers()
    {
        return _users;
    }

    public Set<Chan> getChans()
    {
        return _chans;
    }
}
