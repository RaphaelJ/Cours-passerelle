package chatserver;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

/**
 * Gère un salon et les clients qui s'y trouvent.
 */
class Chan {
    private final ChatServer _server;
    private final String _name;
    private final Map<String, Client> _users = new TreeMap<>();
    
    /**
     * Crée une nouveau salon avec un utilisateur.
     * Rajoute le salon à la liste des salons du serveur et du client.
     */
    public Chan(
        ChatServer server, String name, String client_name, Client client
    )
    {
        this._server = server;
        this._name = name;
        this.joinChan(name, client);
        
        // Ajoute le salon à la liste des salons du serveur.
        Map<String, Chan> chans = this._server.getChans();
        synchronized (chans) {
            chans.put(name, this);
        }
            
    }
    
    /**
     * Enregistre un utilisateur sur un salon.
     * Met à jour la liste des utilisateurs du salon et la liste des salons de 
     * l'utilisateur.
     */
    public void joinChan(String name, Client client)
    {
        Map<String, Chan> user_chans = client.getChans();
        synchronized (user_chans) {
            synchronized (this._users) {
                user_chans.put(name, this);
                this._users.put(name, client);
            }
        }
    }
    
    /**
     * Retire un utilisateur du salon.
     * Met à jour la liste des utilisateur du salon, la liste des salons du
     * serveur et la liste des salons de l'utilisateur.
     * Retourne true si le salon est à présent vide et a été supprimé.
     */
    public boolean quitChan(String name, Client client)
    {
        // Verrouille les utilisateurs du chan et la liste des channels
        // pour garantir la coérence des informations (aucun autre thread ne
        // pourait accèder à un Chan vide).
        Map<String, Chan> chans = this._server.getChans();
        synchronized (chans) {
            Map<String, Chan> user_chans = client.getChans();
            synchronized (user_chans) {
                synchronized (this._users) { 
                    this._users.remove(name);
                    user_chans.remove(this._name);
                    
                    if (this._users.isEmpty()) {
                        chans.remove(this._name);
                        return true;
                    } else
                        return false;
                } 
            }
        }
    }
    
    /**
     * Envoie un message à l'ensemble des utilisateurs du salon.
     * Appel bloquant.
     * @param user_name
     */
    void sendMessage(String user_name, String message) throws IOException
    {
        Command cmd = new Command(
            'M', '#' + this._name + " " + user_name + " " + message
        );

        synchronized (this._users) {
            for (Map.Entry<String, Client> u : this._users.entrySet()) {
                if (!u.getKey().equals(user_name))
                    u.getValue().writeCommand(cmd);
            }
        }
    }
    
    public String getName() 
    {
        return _name;
    }

    public Map<String, Client> getUsers() 
    {
        return _users;
    }
}
