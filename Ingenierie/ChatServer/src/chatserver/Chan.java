package chatserver;

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
     * Rajoute le salon à la liste des salons du serveur.
     */
    public Chan(
        ChatServer server, String name, String client_name, Client client
    )
    {
        this._server = server;
        this._name = name;
        this.joinChan(name, client);
        
        Map<String, Chan> chans = this._server.getChans();
        synchronized (chans) {
            chans.put(name, this);
        }
            
    }
    
    /**
     * Enregistre un utilisateur sur un salon. Ne modifie pas l'objet Client.
     */
    public void joinChan(String name, Client client)
    {
        synchronized (this._users) {
            this._users.put(name, client);
        }
    }
    
    /**
     * Retire un utilisateur du salon. Ne modifie pas l'objet Client.
     * Retourne true si le salon est à présent vide et a été supprimé.
     */
    public boolean quitChan(String name)
    {
        // Verrouille les utilisateurs du chan et la liste des channels
        // pour garantir la coérence des informations (aucun autre thread ne
        // pourait accèder à un Chan vide).
        Map<String, Chan> chans = this._server.getChans();
        synchronized (chans) {            
            synchronized (this._users) { 
                this._users.remove(name);
                if (this._users.isEmpty()) {
                    chans.remove(this._name);
                    return true;
                } else
                    return false;
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
