public class ParallelCellGenerator implements ILifeGenerator {
    public byte[][] compute(Life origin) throws Exception
    {
        int size = origin.getSize();
        final byte[][] res = new byte[size][size];

        Thread[][] tids = new Thread[size][size];
        for (int y = 0; y < size; y++) {
            for (int x = 0; x < size; x++) {
                tids[y][x] = new Thread(new GeneratorTask(res, origin, x, y));
                tids[y][x].start();
            }
        }

        // Waits for childs to finish.
        for (Thread[] line : tids)
            for (Thread tid : line)
                tid.join();

        return res;
    }

    /**
     * Computes the next state of only one cell.
     */
    private class GeneratorTask implements Runnable {
        private final byte[][] _res;
        private final Life _origin;
        private final int _x;
        private final int _y;

        public GeneratorTask(byte[][] res, Life origin, int x, int y)
        {
            this._res = res;
            this._origin = origin;
            this._x = x;
            this._y = y;
        }

        public void run()
        {
            this._res[this._y][this._x] = 
                this._origin.nextCellState(this._x, this._y);
        }
    }
}