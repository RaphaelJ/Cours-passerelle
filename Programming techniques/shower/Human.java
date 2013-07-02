/** Abstact class which will be overloaded by Woman and Man.
 * The class will run an human taking a shower in a Java thread. */
abstract class Human extends Thread {
    final Shower _shower;
    final long _milliSec;

    long _waiting;

    public Human(Shower shower, long milliSec)
    {
        _shower = shower;
        _milliSec = milliSec;
    }

    /** Tries to enter the shower and then leave after a while using the right
     * overloaded virtual methods. */
    public void run()
    {
        try {
            long start = System.currentTimeMillis();
            this.enterShower();
            _waiting = System.currentTimeMillis() - start;
            _shower.have(_milliSec);
            this.leaveShower();
        } catch (InterruptedException ex) {
            ex.printStackTrace();
        }
    }

    /** Returns the number of milliseconds eslaped during the call to
     * enterShower(). */
    public long getWaiting()
    {
        return _waiting;
    }

    /** Virtual methods which will be overloaded by Woman and Man and which will
     * be used by the thread to enter or leave the shower using the right method
     * of the Shower class. */
    abstract void enterShower() throws InterruptedException;

    abstract void leaveShower() throws InterruptedException;
}
