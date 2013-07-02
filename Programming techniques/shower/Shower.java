import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/** Manages a shower by forbidding two people of the opposite sex to take a
 * shower at the same time.
 *
 * Queuing is done as follow :
 * - If the shower is empty, then anybody can go inside without any queuing ;
 * - If the showed is used by someone of the opposite sex, then the person will
 *   wait until the shower is empty ;
 * - If the shower is used by someone of the same sex and if there is nobody
 *   of the opposite sex waiting outside, then the person is allowed to
 *   immediately join the shower ;
 * - If the shower is used by someone of the same sex but if there is at least
 *   one person of the opposite sex waiting outside, then the person will be
 *   enqueued after people of the opposite sex.
 * The last condition is there to ensure that someone of the sex A who arrives
 * after someone of the sex B doesn't bypass this last.
 */
public class Shower {
    enum UserType { MAN, WOMAN };

    final Lock _lock = new ReentrantLock();

    // Conditions used to signal when the shower is free for someone to enter.
    final Condition _noMan   = _lock.newCondition();
    final Condition _noWoman = _lock.newCondition();

    // True if at least a man/woman is waiting in the queue.
    boolean _manWaiting   = false;
    boolean _womanWaiting = false;

    UserType _currentUser;
    int _count = 0; // Counts the number of people currently in the shower.

    public void enterMale() throws InterruptedException
    {
        _lock.lock();

        try {
            if (_count > 0 && (_womanWaiting || _currentUser != UserType.MAN)) {
                _manWaiting = true;
                _noWoman.await();
                _manWaiting = false;
            }

            _currentUser = UserType.MAN;
            _count++;
        } finally {
            _lock.unlock();
        }
    }
    public void leaveMale()
    {
        _lock.lock();

        try {
            _count--;

            if (_count == 0)
                _noMan.signalAll();
        } finally {
            _lock.unlock();
        }
    }

    public void enterFemale() throws InterruptedException
    {
        _lock.lock();

        try {
            if (_count > 0 &&  (_manWaiting || _currentUser != UserType.WOMAN)) {
                _womanWaiting = true;
                _noMan.await();
                _womanWaiting = false;
            }

            _currentUser = UserType.WOMAN;
            _count++;
        } finally {
            _lock.unlock();
        }
    }
    public void leaveFemale()
    {
        _lock.lock();

        try {
            _count--;

            if (_count == 0)
                _noWoman.signalAll();
        } finally {
            _lock.unlock();
        }
    }

    public void have(long showerTimeMillis) throws InterruptedException
    {
        Thread.sleep(showerTimeMillis);
    }
}
