package chatserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Set;
import java.util.TreeSet;

/**
 * 
 */
public class ChatServer  {
    private Set<String> _users = new TreeSet<>();

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

        for (;;) {
            Socket sock = server_sock.accept();
            System.out.println(
                "Nouveau client (" + sock.getInetAddress().toString() + ")."
            );
            new Client(this, sock).start();
        }
    }

    public Set<String> getUsers() {
        return _users;
    }
}
