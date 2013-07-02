import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

public class Main {
    public static void main(String[] args) throws InterruptedException
    {
        int  nMen     = 300;
        int  nWomen   = 300;
        long duration = 500; // Shower duration in milliseconds.

        Shower shower = new Shower();

        // Allocate an array which will contain every human which will enter the
        // shower.
        // The way this array is ordered will determine the order in which
        // humans will enter the shower.
        // Uses a Vector instead of a primitive array as primitive arrays
        // don't implement the List<> interface needed to shuffle those.
        Vector<Human> humans = new Vector<Human>(nMen + nWomen);
        for (int i = 0; i < nMen; i++)
            humans.add(new Man(shower, duration));

        for (int i = 0; i < nWomen; i++)
            humans.add(new Woman(shower, duration));

        // Test 1.
        // Optimal case : humans enter the shower in two distincts groups.
        // Doesn't change the original order of the array.

        // Test 2.
        // Pragmatic case : humans enter the shower in a random order.
        Collections.shuffle(humans);

        // Runs the simulation.
        long start = System.currentTimeMillis();
        for (Human human : humans)
            human.start();

        // Waits for the end of the simulation.
        for (Human human : humans)
            human.join();

        // Results
        double eslaped = (double) (System.currentTimeMillis() - start);
        System.out.println(
            (eslaped / 1000) + " sec needed to run the simulation."
        );

        double waitMin = Double.POSITIVE_INFINITY
             , waitMax = Double.NEGATIVE_INFINITY
             , waitSum = 0;
        for (Human human : humans) {
            double waiting = (double) human.getWaiting();
            waitMin = Math.min(waitMin, waiting);
            waitMax = Math.max(waitMax, waiting);
            waitSum = waitSum + waiting;
        }

        System.out.println(
              "min/max/avg waiting time in : "
            + waitMin + "/" + waitMax + "/" + (waitSum / (nMen + nWomen)) 
            + " ms."
        );
    }
}
