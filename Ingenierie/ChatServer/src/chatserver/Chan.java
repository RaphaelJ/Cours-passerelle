package chatserver;

import java.util.Map;
import java.util.TreeMap;

/**
 * Gère un salon et les clients qui s'y trouvent.
 */
class Chan implements Comparable<Chan> {
    private final String _name;
    private final Map<String, Client> _users = new TreeMap<>();
    
    public Chan(String name)
    {
        this._name = name;
    }

    @Override
    public int compareTo(Chan t) 
    {
        return this.getName().compareTo(t.getName());
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
