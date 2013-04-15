package chatserver;

import java.io.IOException;

public class MainClass {
    public static void main(String[] args) throws IOException 
    {
        new ChatServer().run();
    }
}
