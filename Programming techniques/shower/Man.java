public class Man extends Human {
    public Man(Shower shower, long milliSec)
    {
        super(shower, milliSec);
    }

    public void enterShower() throws InterruptedException
    {
        _shower.enterMale();
    }

    public void leaveShower() throws InterruptedException
    {
        _shower.leaveMale();
    }
}
