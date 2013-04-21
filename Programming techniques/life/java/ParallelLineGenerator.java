public class ParallelLineGenerator implements ILifeGenerator {
    private final int _n_threads;

    public ParallelLineGenerator(int n_threads)
    {
        this._n_threads = n_threads;
    }

    public short[][] compute(Life origin) throws Exception
    {
        int w = origin.getW(), h = origin.getH();
        short[][] res = new short[h][w];
        int n = w * h;

        // Doesn't start more threads than the number of cells.
        int n_threads = Math.min(this._n_threads, n);
        int n_per_threads = n / n_threads;

        int i_start = 0;
        Thread[] tids = new Thread[n_threads - 1];
        for (int i = 0; i < n_threads - 1; i++) {
            tids[i] = new Thread(
                new Generator(res, origin, i_start, n_per_threads)
            );
            tids[i].start();
            i_start += n_per_threads;
        }

        // If the number of cells is not a multiple of the number of threads,
        // the last segment will contain the remainder of cells.
        // Doesn't execute the last segment in a new thread: uses the current
        // context.
        new Generator(res, origin, i_start, n - i_start).run();

        // Waits for childs to finish.
        for (Thread tid : tids)
            tid.join();

        return res;
    }

    private class Generator implements Runnable
    {
        private short[][] _res;
        private final Life _origin;
        private final int _start_i;
        private final int _n;

        /**
         * Computes a segment of the board of n cells starting at start_i.
         */
        public Generator(short[][] res, Life origin, int start_i, int n)
        {
            this._res = res;
            this._origin = origin;
            this._start_i = start_i;
            this._n = n;
        }

        public void run()
        {
            int w = this._origin.getW();
            int x = this._start_i % w
              , y = this._start_i / w;

            for (int i = 0; i < this._n; i++) {
                this._res[y][x] = this._origin.nextCellState(x, y);

                x++;
                if (x >= w) {
                    x = 0;
                    y++;
                }
            }
        }
    }
}
