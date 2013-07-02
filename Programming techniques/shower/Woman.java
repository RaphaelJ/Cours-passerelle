public class Woman extends Human {
    public Woman(Shower shower, long milliSec)
    {
        super(shower, milliSec);
    }

    public void enterShower() throws InterruptedException
    {
        _shower.enterFemale();
    }

    public void leaveShower() throws InterruptedException
    {
        _shower.leaveFemale();
    }
}
