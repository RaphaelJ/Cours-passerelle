public class ParallelLineGenerator implements ILifeGenerator {
    private final int _num_threads;

    public ParallelLineGenerator(int num_threads)
    {
        this._num_threads = num_threads;
    }

    public boolean[][] compute(Life origin)
    {
        int w = origin.getW(), h = origin.getH();
        boolean[][] res = new boolean[h][w];
        int n = w + h;

        // Doesn't start more threads than the number of cells.
        int n_threads = Math.min(this.num_threads, n);
        int n_per_threads = n / n_threads;

        int i_start = 0;
        Thread[] tids = new Thread[n_threads - 1];
        for (int i = 0; i < n_threads - 1; i++) {
            tids[i] = new GeneratorThread(res, w, h, i_start, n_per_threads);
            tids[i].start();
            i_start += n_per_threads;
        }

        // If the number of cells is not a multiple of the number of threads,
        // the last segment will contain the remainder of cells.
        // Doesn't execute the last segment in a new thread: uses the current
        // context.
        new GeneratorThread(res, w, h, i_start, n - i_start).run();

        // Waits for childs to finish.
        for (Thread tid : tids)
            tid.join();

        return res;
    }

    private class GeneratorThread extends Thread ()
    {
        private boolean[][] _res;
        private int _w;
        private int _h;
        private int _start_x;
        private int _start_y;
        private int _n;

        public GeneratorThread(
            boolean[][] res, int w, int h, int start_x, int start_y, int n
        )
        {
            this._res = res;
            this._w = w;
            this._h = h;
            this._start_x = start_x;
            this._start_y = start_y;
            this._n = n;
        }

        public void run()
        {
            
        }
    }
}
